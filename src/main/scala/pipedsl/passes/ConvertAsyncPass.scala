package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Errors.UnexpectedExpr
import pipedsl.common.Syntax
import pipedsl.common.Utilities.flattenStageList
import pipedsl.passes.Passes.StagePass

/**
 * Convert the recv commands inside stages into a pair
 * of Send and Recv pairs. The Send produces a reference
 * which the Recv uses to request the result.
 */
class ConvertAsyncPass extends StagePass[List[PStage]] {

  private var msgCount = 0
  override def run(stgs: List[PStage]): List[PStage] = {
    flattenStageList(stgs).foreach(s => {
      convertAsyncCmds(s)
    })
    stgs
  }

  private def convertAsyncCmds(stg: PStage): Unit = {
    val recvs = getRecvs(stg)
    val calls = getCalls(stg)
    val newRecvs = recvs.map(r => convertRecv(r))
    val newCalls = calls.map(c => convertCall(c))
    val newCmds = stg.cmds.filterNot(c => recvs.contains(c) || calls.map(cexp => CExpr(cexp)).contains(c)) ++
      //only take the send parts of the receives. Calls only have a send part
      newRecvs.map(t => t._1) ++ newCalls
    stg.cmds = newCmds
    stg.outEdges.foreach(e => {
      val nstg = e.to
      val nrecvs = if (e.condRecv.isDefined) {
        newRecvs.map(t => ICondCommand(e.condRecv.get, t._2))
      } else {
        newRecvs.map(t => t._2)
      }
      nstg.cmds = nrecvs ++ nstg.cmds
    })
  }

  private def convertRecv(c: CRecv): (Command, Command) = {
    (c.lhs, c.rhs) match {
        //Mem Read
      case (lhs@EVar(_), EMemAccess(mem, index@EVar(_))) => {
        val send = IMemSend(isWrite = false, mem, None, index)
        val recv = IMemRecv(mem, Some(lhs))
        (send, recv)
      }
        //Mem Write
      case (EMemAccess(mem, index@EVar(_)), data@EVar(_)) => {
        val send = IMemSend(isWrite = true, mem, Some(data), index)
        val recv = IMemRecv(mem, None)
        (send, recv)
      }
      case (lhs@EVar(_), call@ECall(_, _)) => {
        val send = convertCall(call)
        val recv = IRecv(send.handle, send.receiver, lhs)
        (send, recv)
      }
      case _ => throw UnexpectedExpr(c.lhs)
    }
  }

  private def convertCall(c: ECall): ISend = {
    val arglist: List[EVar] = c.args match {
      case l: List[EVar] => l
      case _ => throw UnexpectedExpr(c)
    }
    val msghandle = freshMessage
    ISend(msghandle, c.mod, arglist)
  }

  private def getRecvs(stg: PStage): List[CRecv] = {
   stg.cmds.foldLeft(List[CRecv]())((l, c) => {
     c match {
       case cmd: CRecv => l :+ cmd
       case _ => l
     }
   })
  }

  private def getCalls(stg: PStage): List[ECall] = {
    stg.cmds.foldLeft(List[ECall]())((l, c) => {
      c match {
        case CExpr(call@ECall(_,_)) => l :+ call
        case _ => l
      }
    })
  }

  private def freshMessage: EVar = {
    val res = EVar(Id("_request_" + msgCount))
    msgCount += 1
    res
  }
}
