package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Dataflow.{DFMap, MaybeReservedHandles, worklist}
import pipedsl.common.Locks.LockHandleInfo
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.{flattenStageList, updateSetMap}
import pipedsl.passes.Passes.StagePass

object RemoveReentrantPass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    val (maybeResIns, _) = worklist(flattenStageList(stgs), MaybeReservedHandles)
    flattenStageList(stgs).foreach(s => convertLockOps(s, maybeResIns(s.name)))
    stgs
  }

  private def convertLockOps(stg: PStage, maybeRes: DFMap[Set[LockHandleInfo]]): Unit = {
    var newRes = maybeRes
    var newCmds = List[Command]()
    stg.getCmds.foreach {
      case c@IReserveLock(handle, larg) if larg.evar.isDefined =>
        //reserve if no alias, otherwise assign to matching handle
        val aliases = newRes.getOrElse(larg.id, Set()).filterNot(alias => alias._1 == larg.evar.get)
        if (aliases.nonEmpty) {
          val aliasnone = aliasNone(larg.evar.get, aliases)
          //if none are valid aliases then run original command
          newCmds = newCmds :+ ICondCommand(aliasnone, List(c))
          //otherwise, for each alias, overwrite the handle
          aliases.foreach(alias => {
            newCmds = newCmds :+ assignAlias(larg.evar.get, handle, alias)
          })
        } else {
          newCmds = newCmds :+ c
        }
        //then update the set of maybe reserved addrs
        newRes = updateSetMap(newRes, larg.id, (larg.evar.get, handle))
      case c@ICheckLockFree(larg) if larg.evar.isDefined =>
        //checkowned if alias with locks acquired before this cycle, otherwise still checkfree
        val aliases = maybeRes.getOrElse(larg.id, Set()).filterNot(alias => alias._1 == larg.evar.get)
        if (aliases.nonEmpty) {
          val aliasnone = aliasNone(larg.evar.get, aliases)
          //if none are valid aliases then run original command
          newCmds = newCmds :+ ICondCommand(aliasnone, List(c))
          //otherwise, for each alias, check the alias' handle
          aliases.foreach(alias => {
            newCmds = newCmds :+ checkOwned(larg, alias)
          })
        } else {
          newCmds = newCmds :+ c
        }
      case c@IReleaseLock(larg, handle) if larg.evar.isDefined =>
        val aliases = newRes.getOrElse(larg.id, Set()).filterNot(alias => alias._1 == larg.evar.get)
        if (aliases.nonEmpty) {
          //release if no alias other than self, otherwise set handle to invalid
          val aliasAny = validAliasAny(larg.evar.get, aliases)
          val aliasnone = aliasNone(larg.evar.get, aliases)
          newCmds = newCmds :+ ICondCommand(aliasAny, List(IAssignLock(handle, EInvalid)))
          newCmds = newCmds :+ ICondCommand(aliasnone, List(c))
        } else {
          //just run the command as normal
          newCmds = newCmds :+ c
        }
        //then remove from the set of maybe reserved addrs
        newRes.get(larg.id) match {
          case Some(s) => newRes = newRes.updated(larg.id, s.filter(info => info._1 != larg.evar.get))
          case None => ()
        }
      //checklockowned is always unmodified
      //all other commands (including general lock ops) are unmodified
      case c@_ => newCmds = newCmds :+ c
    }
    stg.setCmds(newCmds)
  }

  private def validAliasAny(addr: EVar, aliases: Iterable[LockHandleInfo]): Expr = {
    aliases.tail.foldLeft[Expr](AndOp(EqOp(addr, aliases.head._1), EIsValid(aliases.head._2)))((exp, alias) => {
      OrOp(exp, AndOp(EqOp(addr, alias._1), EIsValid(alias._2)))
    })
  }

  private def aliasNone(addr: EVar, aliases: Iterable[LockHandleInfo]): Expr = {
    EUop(NotOp(), validAliasAny(addr, aliases))
  }

  private def noneValid(handles: Iterable[EVar]): Expr = {
    EUop(NotOp(),
      handles.tail.foldLeft[Expr](EIsValid(handles.head))((exp, h) => {
      OrOp(exp, EIsValid(h))
    }))
  }
  //if (addr == aliasaddr && isValid(aliasHandle) {
  //  newHandle = fromMaybe(0, aliasHandle)
  //}
  private def assignAlias(addr: EVar, newHandle: EVar, aliasInfo: LockHandleInfo): Command = {
    val aliasCond = AndOp(EIsValid(aliasInfo._2), EqOp(addr, aliasInfo._1))
    ICondCommand(aliasCond, List(
      IAssignLock(newHandle, EFromMaybe(aliasInfo._2))
    ))
  }

  private def checkOwned(lockArg: LockArg, aliasInfo: LockHandleInfo): Command = {
    val addr = lockArg.evar.get
    val aliasCond = AndOp(EIsValid(aliasInfo._2), EqOp(addr, aliasInfo._1))
    ICondCommand(aliasCond, List(
      ICheckLockOwned(lockArg, aliasInfo._2)
    ))
  }
}
