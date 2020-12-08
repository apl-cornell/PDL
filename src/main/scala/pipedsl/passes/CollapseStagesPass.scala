package pipedsl.passes

import pipedsl.common.DAGSyntax._
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{andExpr, getReachableStages, getUsedVars, isReceivingCmd, updateListMap}
import pipedsl.passes.Passes.StagePass

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
      s.trueStages.foreach(t => simplifyIfs(t))
      s.falseStages.foreach(f => simplifyIfs(f))
      //Check if the branches are combinational
      val isTrueComb = s.trueStages.head.outEdges.exists(e => e.to == s.joinStage)
      val isFalseComb = s.falseStages.head.outEdges.exists(e => e.to == s.joinStage)
      //Merge in the first true and false stages since that delay is artificial
      mergeStages(s, List(s.trueStages.head, s.falseStages.head))
      //Update IF stage metadata
      s.trueStages = s.trueStages.tail
      s.falseStages = s.falseStages.tail
      //Get the inputs to the join stage from any conditional edge
      val joinStgInputs = s.joinStage.inEdges.filter(e => e.condRecv.isDefined).head.values
      //Special case here, where we can merge all of the output edges into one
      if (isTrueComb && isFalseComb) {
        //join stage can be eliminated completely in this case
        val newOutEdge = PipelineEdge(None, None, s, s.joinStage, joinStgInputs)
        s.removeEdgesTo(s.joinStage)
        s.addEdge(newOutEdge)
        mergeStages(s, List(s.joinStage))
      }
      //there must only be one by construction
      val priorstg = s.inEdges.head.from
      //merge this into the prior stage since that delay was added unnecessarily
      mergeStages(priorstg, List(s))
    case _: SpecStage =>
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
  private def flattenCondStmts(cond: Expr, stmts: Iterable[Command]): List[Command] = {
    var condMap: Map[Expr, List[Command]] = Map()
    stmts.foreach {
      case ICondCommand(cex, cs) =>
        val mapcondition = andExpr(Some(cond), Some(cex)).get
        condMap = updateListMap(condMap, mapcondition, cs)
      case c =>
        val mapcondition = cond
        condMap = updateListMap(condMap, mapcondition, c)
    }
    condMap.keys.foldLeft(List[Command]())((l, k) => {
        l :+ ICondCommand(k, condMap(k))
    })
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
   */
  private def mergeStages(target: PStage, srcs: Iterable[PStage]): Unit = {
    var newstmts = List[Command]()
    val lockids = srcs.foldLeft(Set[LockArg]())((ids, s) => {
      ids ++ getLockIds(s.getCmds)
    })
    srcs.foreach(src => {
      if (src.inEdges.exists(e => e.from != target) ||
        !src.inEdges.exists(e => e.from == target)) {
        throw new RuntimeException(s"Cannot merge ${src.name.v} since it has inputs other than ${target.name.v}")
      }
      val existingedges = target.outEdges.filter(e => e.to == src)
      if (existingedges.size > 1) {
        throw new RuntimeException(s"Cannot merge ${src.name.v} into ${target.name.v} since they have multiple edges")
      }
      val cond = existingedges.head.condSend
      //merge in the commands
      var receivingStmts = List[Command]()
      if (cond.isDefined) {
        val needIds = lockids.diff(getLockIds(src.getCmds).toSet)
        val noops = needIds.foldLeft(List[Command]())((l, id) => {
          l :+ ILockNoOp(id)
        })
        val flattenedCmds = flattenCondStmts(cond.get, src.getCmds ++ noops)
        val (recvstmts, normalstmts) = splitReceivingStmts(flattenedCmds)
        newstmts = newstmts ++ normalstmts
        receivingStmts = recvstmts
      } else {
        val (recvstmts, normalstmts) = splitReceivingStmts(src.getCmds)
        newstmts = newstmts ++ normalstmts
        receivingStmts = recvstmts
      }
      //merge in the output edge from src with target as the new from stage
      //use the old condsend to src as the new condsend
      target.removeEdgesTo(src)
      val outedges = src.outEdges
      val outdests = outedges.foldLeft(Set[PStage]())((s, e) => s + e.to)
      outdests.foreach(dest => {
        src.removeEdgesTo(dest)
      })
      outedges.foreach(e => {
        // also move recv stmts into each of the successors
        // merge conditional receive from the edge into the recv statements
        val nstmts = if (e.condRecv.isDefined) {
         flattenCondStmts(e.condRecv.get, receivingStmts)
        } else {
          receivingStmts
        }
        e.to.addCmds(nstmts)
        //also add necessary values to this edge
        val newedge = PipelineEdge(andExpr(cond, e.condSend), e.condRecv, target, e.to, e.values ++ getUsedVars(nstmts))
        target.addEdge(newedge)
      })
    })
    //merge all subsequent stages at the same time
    target.mergeStmts(newstmts)
  }
}
