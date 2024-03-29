package pipedsl.passes

import pipedsl.common.DAGSyntax._
import pipedsl.common.Locks.eliminateLockRegions
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{andExpr, getReachableStages, getUsedVars, isReceivingCmd, updateListMap}
import pipedsl.passes.Passes.StagePass

import scala.collection.immutable.ListMap

/**
 * The SplitStages pass forces IF statements to create
 * a new stage for each of their branches. In the event
 * that one or both of those branches are combinational,
 * we can actually merge those stages together.
 *
 * Furthermore, all IF stages can be combined with their
 * predecessor, since the SplitStages pass also introduces
 * new stages whenever there is a conditional branch.
 *
 */
object CollapseStagesPass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    stgs.foreach(s => simplifyIfs(s))
    eliminateEmptyStages(stgs.head)
  }

  private def simplifyIfs(stg: PStage): Unit = stg match {
    case s: IfStage =>
      s.condStages.foreach(stg => stg.foreach(t => simplifyIfs(t)))
      s.defaultStages.foreach(f => simplifyIfs(f))
      val isBranchesComb = s.condStages.forall(stg => stg.head.outEdges.exists(e => e.to == s.joinStage))
      val isDefaultComb = s.defaultStages.head.outEdges.exists(e => e.to == s.joinStage)
      //Merge in the first true and false stages since that delay is artificial
      mergeStages(s, s.condStages.map(stg => stg.head) :+ s.defaultStages.head, isForward = false)
      //Update IF stage metadata
      s.condStages = s.condStages.map(stg => stg.tail)
      s.defaultStages= s.defaultStages.tail
      //Get the inputs to the join stage from any conditional edge
      val joinStgInputs = s.joinStage.inEdges.filter(e => e.condRecv.isDefined).head.values
      //Special case here, where we can merge all of the output edges into one
      if (isBranchesComb && isDefaultComb) {
        //join stage can be eliminated completely in this case
        val newOutEdge = PipelineEdge(None, None, s, s.joinStage, joinStgInputs)
        s.removeEdgesTo(s.joinStage)
        s.addEdge(newOutEdge)
        mergeStages(s, List(s.joinStage), isForward = false)
      } else {
        //merge join stage into its successor, as long as its successor isn't another If stage
        //and that stage ALSO only has one predecessor
        if (s.joinStage.succs.size == 1 && s.joinStage.succs.exists(p => p match {
          case _: IfStage => false
          case nstg => nstg.preds.size == 1
        })) {
          mergeStages(s.joinStage, List(s.joinStage.succs.head), isForward = false);
        }
      }
      //there must only be one by construction
      val priorstg = s.inEdges.head.from
      //merge this into the prior stage since that delay was added unnecessarily
      mergeStages(priorstg, List(s), isForward = false)
    case _ =>
  }

  private def eliminateEmptyStages(start: PStage): List[PStage] = {
    var result = getReachableStages(start)
    var done = false
    while (!done) {
      //reverse will just speed this up a bit
      result.reverse.foreach(s => {
        //This stage does nothing! Make it unreachable by removing all input edges
        if (s.getCmds.isEmpty && s.outEdges.isEmpty) {
          s.inEdges.foreach(e => s.removeEdge(e))
        }
      })
      val tmp = getReachableStages(start)
      //done once we stop removing stages
      done = tmp.equals(result)
      result = tmp
    }
    result
  }

  //Ensures that ICondCommands don't contain any ICondCommands
  //Try to minimize the number of ICondCommands we generate in order to avoid confusingly generated code
  private def flattenCondStmts(cond: Expr, stmts: Iterable[Command]): List[Command] = {
    var resultList: List[Command] = List()
    val lastCmd = stmts.foldLeft(ICondCommand(cond, List()).setPos(cond.pos))((ic, s) => {
      s match {
        case ICondCommand(cex, cs) if ic.cond != cex =>
          val condition = andExpr(Some(cond), Some(cex)).get
          condition.setPos(cond.pos)
          resultList = resultList :+ ic //push last command to output list
          ICondCommand(condition, cs).setPos(s.pos) //return new conditional command with composite condition
        case ICondCommand(_, cs) => //condition matches, just add our subcommands to it
          ICondCommand(ic.cond, ic.cs ++ cs).setPos(ic.pos)
        case c if ic.cond != cond =>
          resultList = resultList :+ ic //push last command to output list
          ICondCommand(cond, List(c)).setPos(c.pos) //start new command with original condition
        case c =>
          ICondCommand(ic.cond, ic.cs :+ c).setPos(ic.pos) //just add to command list
      }
    })
    if (lastCmd.cs.nonEmpty) {
      resultList :+ lastCmd //add in the remaining command if its subcommands not empty
    } else {
      resultList
    }
  }

  //split the given commands into a pair (receiving, normal) commands
  //also properly splits conditional commands into multiple conditional commands
  private def splitReceivingStmts(stmts: Iterable[Command]): (List[Command], List[Command]) = {
    var receivingList = List[Command]()
    var normalList = List[Command]()
    stmts.foreach {
      case ICondCommand(cex, cs) =>
        val (rs, ns) = splitReceivingStmts(cs)
        if (rs.nonEmpty) receivingList = receivingList :+ ICondCommand(cex, rs)
        if (ns.nonEmpty) normalList = normalList :+ ICondCommand(cex, ns)
      case c =>
        if (isReceivingCmd(c)) {
          receivingList = receivingList :+ c
        } else {
          normalList = normalList :+ c
        }
    }
    (receivingList, normalList)
  }
  /**
   * Merge src into target, but execute it conditionally based on the
   * condition of their connecting edge.
   * This requires target to have direct communication edges to src.
   * This modifies both of the stages in place.
   * @param target The earlier stage, which is being merged into and modified.
   * @param srcs The later stages, which are being removed from the stage graph.
   * @param isForward If this is true, target is the _later_ stage and srcs are the earlier ones.
   */
  private def mergeStages(target: PStage, srcs: Iterable[PStage], isForward: Boolean): Unit = {
    var newstmts = List[Command]()
    srcs.foreach(src => {
      // Check for each src, if there exists connection with other stages in the merging direction.
      val hasIncorrectEdges = if (isForward) {
        (s: PStage) => s.outEdges.exists(e => e.to != target) ||
          !s.outEdges.exists(e => e.to == target)
      } else {
        (s: PStage) => s.inEdges.exists(e => e.from != target) ||
          !s.inEdges.exists(e => e.from == target)
      }
      if (hasIncorrectEdges(src)) {
        throw new RuntimeException(s"Cannot merge ${src.name.v} since it has inputs other than ${target.name.v}")
      }
      val existingedges = if (isForward) {
        target.inEdges.filter(e => e.from == src)
      } else {
        target.outEdges.filter(e => e.to == src)
      }
      // Check if multi-edge.
      if (existingedges.size > 1) {
        throw new RuntimeException(s"Cannot merge ${src.name.v} into ${target.name.v} since they have multiple edges")
      }
      // Check if exists conditional statements for n1 in n1 -> n2
      val cond = if (isForward) {
        existingedges.head.condRecv
      } else {
        existingedges.head.condSend
      }
      //merge in the commands
      var receivingStmts = List[Command]() // Create empty receiving stmts list
      if (cond.isDefined) {
        val flattenedCmds = flattenCondStmts(cond.get, src.getCmds)
        val (recvstmts, normalstmts) = splitReceivingStmts(flattenedCmds)
        newstmts = newstmts ++ normalstmts
        if (isForward) {
          //when going forward, we don't treat these two separately
          newstmts = recvstmts ++ newstmts
        }
        receivingStmts = recvstmts
      } else {
        val (recvstmts, normalstmts) = splitReceivingStmts(src.getCmds)
        newstmts = newstmts ++ normalstmts
        if (isForward) {
          //when going forward, we don't treat these two separately
          newstmts = recvstmts ++ newstmts
        }
        receivingStmts = recvstmts
      }
      //reconnect edges from other side of src to target directly
      //use the old condition to/from src as the new condition
      if (isForward) {
        src.removeEdgesTo(target)
      } else {
        target.removeEdgesTo(src)
      }
      val connEdges = if (isForward) src.inEdges else src.outEdges
      val connDests = connEdges.foldLeft(Set[PStage]())(
        if (isForward) {
          (s, e) => s + e.from
        } else {
          (s, e) => s + e.to
        }
      )
      connDests.foreach(dest => {
        if (isForward) {
          dest.removeEdgesTo(src)
        } else {
          src.removeEdgesTo(dest)
        }
      })
      // done removing edges
      connEdges.foreach(e => {
        if (!isForward) {
          //only in this case we need to propagate receiving statements forward instead of back
          val nstmts = if (e.condRecv.isDefined) {
            flattenCondStmts(e.condRecv.get, receivingStmts)
          } else {
            receivingStmts
          }
          e.to.mergeStmts(nstmts, false)
        } // in else case, we're already moving receiving statements forward by merging with target
        val newedge = if (isForward) {
          PipelineEdge(e.condSend, andExpr(cond, e.condRecv), e.from, target, e.values)
        } else {
          //in this case we need to add new values to the edge based on the receiving statments we moved
          PipelineEdge(andExpr(cond, e.condSend), e.condRecv, target, e.to, e.values ++ getUsedVars(receivingStmts))
        }
        target.addEdge(newedge)
      })
    })
    target.mergeStmts(newstmts, addAfter = !isForward) //merge all subsequent stages at the same time
    eliminateLockRegions(target) //after merging then remove any unnecessary lock region statements immediately
  }
}
