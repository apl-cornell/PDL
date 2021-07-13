package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Errors.{UnexpectedExpr, UnexpectedType}
import pipedsl.common.Utilities.flattenStageList
import pipedsl.passes.Passes.StagePass

/**
 * Convert the recv commands inside stages into a pair
 * of Send and Recv pairs. The Send produces a reference
 * which the Recv uses to request the result.
 */
class ConvertAsyncPass(modName: Id) extends StagePass[List[PStage]] {

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
    val newCmds = stg.getCmds.filterNot(c => recvs.contains(c) || calls.map(cexp => CExpr(cexp)).contains(c)) ++
      //only take the send parts of the receives. Calls only have a send part
      newRecvs.map(t => t._1) ++ newCalls
    stg.setCmds(newCmds)
    //Nowhere to put the receive statements!!
    if (newRecvs.nonEmpty && stg.outEdges.isEmpty) {
      throw new RuntimeException("invalid pipeline graph!!! Missing final stage")
    }
    stg.outEdges.foreach(e => {
      val nstg = e.to
      val nrecvs = if (e.condRecv.isDefined) {
        newRecvs.map(t => ICondCommand(e.condRecv.get, List(t._2)))
      } else {
        newRecvs.map(t => t._2)
      }
      nstg.setCmds(nrecvs ++ nstg.getCmds)
    })
  }

  private def convertRecv(c: CRecv): (Command, Command) = {

    (c.lhs, c.rhs) match {
        //Mem Read
      case (lhs@EVar(_), e@EMemAccess(mem, index@EVar(_), _)) =>
        val handle = freshMessage(mem)
        val send = IMemSend(handle, writeMask = None, mem, None, index)
        val recv = IMemRecv(mem, handle, Some(lhs))
        send.memOpType = e.memOpType
        send.granularity = e.granularity
        send.portNum = c.portNum
        recv.memOpType = e.memOpType
        recv.granularity = e.granularity
        recv.portNum = c.portNum
        (send, recv)
      //Mem Write
      case (e@EMemAccess(mem, index@EVar(_), wm), data@EVar(_)) => mem.typ.get match {
        case TLockedMemType(TMemType(_, _, _, Latency.Asynchronous, _, _),_, _) =>
          val handle = freshMessage(mem)
          val send = IMemSend(handle, writeMask = wm, mem, Some(data), index)
          val recv = IMemRecv(mem, handle, None)
          send.memOpType = e.memOpType
          send.granularity = e.granularity
          send.portNum = c.portNum
          recv.memOpType = e.memOpType
          recv.granularity = e.granularity
          recv.portNum = c.portNum
          (send, recv)
          //if the memory is sequential we don't use handle since it
          //is assumed to complete at the end of the cycle
        case TLockedMemType(_,_,_) =>
          val write = IMemWrite(mem, index, data).setPos(e.pos)
          write.memOpType = e.memOpType
          write.granularity = e.granularity
          write.portNum = c.portNum
          (write, CEmpty())
        case _ => throw UnexpectedType(mem.pos, "Memory Write Statement", "Memory Type", mem.typ.get)
      }
      //module calls
      case (lhs@EVar(_), call@ECall(_, _)) =>
        val send = convertCall(call)
        val recv = IRecv(send.handle, send.receiver, lhs)
        (send, recv)
      case _ => throw UnexpectedExpr(c.lhs)
    }
  }

  private def convertCall(c: ECall): ISend = {
    val arglist: List[EVar] = c.args.foldLeft(List[EVar]())((l, a) => a match {
      case av: EVar => l :+ av
        //should be unreachable if the SimplifyRecv pass was executed
      case _ => throw UnexpectedExpr(c)
    })
    val msghandle = freshMessage(c.mod)
    ISend(msghandle, c.mod, arglist)
  }

  private def getRecvs(stg: PStage): List[CRecv] = {
   stg.getCmds.foldLeft(List[CRecv]())((l, c) => {
     c match {
       case cmd: CRecv => l :+ cmd
       case _ => l
     }
   })
  }

  private def getCalls(stg: PStage): List[ECall] = {
    stg.getCmds.foldLeft(List[ECall]())((l, c) => {
      c match {
        case CExpr(call@ECall(_,_)) => l :+ call
        case _ => l
      }
    })
  }

  private def freshMessage(m: Id): EVar = {
    val res = EVar(Id("_request_" + msgCount))
    res.typ = Some(TRequestHandle(m, RequestType.Module))
    res.id.typ = res.typ
    msgCount += 1
    res
  }
}
