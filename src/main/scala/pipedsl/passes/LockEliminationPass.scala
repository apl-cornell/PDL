package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Locks.Free
import pipedsl.common.Syntax
import pipedsl.common.Syntax.{CLockEnd, CLockStart, Command, ICheckLockFree, ICheckLockOwned, IReleaseLock, IReserveLock, Id}
import pipedsl.passes.Passes.StagePass

object LockEliminationPass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    stgs
  }

  private def eliminateLockReservations(stg: PStage): Unit = {
    val lockcmds = stg.getCmds.foldLeft(Map[Id, List[Command]]())((m, c) => c match {
      case ICheckLockFree(l) => m.updated(l, m.get(l) match {
        case Some(lc) => lc :+ c
        case None => List(c)
      })
      case ICheckLockOwned(l, lv) =>  m.updated(l, m.get(l) match {
        case Some(lc) => lc :+ c
        case None => List(c)
      })
      case IReserveLock(lv, l) =>  m.updated(l, m.get(l) match {
        case Some(lc) => lc :+ c
        case None => List(c)
      })
      case IReleaseLock(l, lv) =>  m.updated(l, m.get(l) match {
        case Some(lc) => lc :+ c
        case None => List(c)
      })
      case _ => m
    })
    lockcmds.keys.foreach(k => {
      val cmds = lockcmds(k)
      var newcmds = List[Command]()
      //reserve + release => checkfree
      //reserve + checkowned => reserved + checkfree
      //
    })
  }

}
