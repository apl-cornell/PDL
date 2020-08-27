package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Errors.{UnexpectedExpr}
import pipedsl.passes.Passes.StagePass

/**
 * Convert the recv commands inside stages into a pair
 * of Send and Recv pairs. The Send produces a reference
 * which the Recv uses to request the result.
 */
class ConvertRecvPass extends StagePass[List[PStage]] {

  private var msgCount = 0
  override def run(stgs: List[PStage]): List[PStage] = {
    stgs.foreach(s => {
      convertRecvs(s)
    })
    stgs
  }

  private def convertRecvs(stg: PStage) = {
    val recvs = getRecvs(stg)
    val newRecvs = recvs.map(r => convertRecv(r))
    val newCmds = stg.cmds.filterNot(c => recvs.contains(c)) ++ newRecvs.map(t => t._1)
    stg.cmds = newCmds
    stg.succs.foreach(s => {
      s.cmds = newRecvs.map(t => t._2) ++ s.cmds
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
      //TODO throw better error, also handle CALLs to other modules
      case _ => throw UnexpectedExpr(c.lhs)
    }
  }
  private def getRecvs(stg: PStage): List[CRecv] = {
   stg.cmds.foldLeft(List[CRecv]())((l, c) => {
     c match {
       case cmd: CRecv => l :+ cmd
       case _ => l
     }
   })
  }
}
