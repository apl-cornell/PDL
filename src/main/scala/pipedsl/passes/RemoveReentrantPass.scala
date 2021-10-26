package pipedsl.passes

import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Dataflow.{DFMap, MaybeReservedHandles, worklist}
import pipedsl.common.Locks.LockHandleInfo
import pipedsl.common.Syntax._
import pipedsl.common.Utilities
import pipedsl.common.Utilities.{flattenStageList, updateSetMap}
import pipedsl.passes.Passes.StagePass

object RemoveReentrantPass extends StagePass[List[PStage]] {

  override def run(stgs: List[PStage]): List[PStage] = {
    val (maybeResIns, _) = worklist(flattenStageList(stgs), MaybeReservedHandles)
    var maybeResInfo = maybeResIns
    flattenStageList(stgs).foreach(s => maybeResInfo = convertLockOps(s, maybeResInfo))
    stgs
  }

  //returns the updated mappings for new handle variables (releasing locks requires
  //updating handle variables and thus renaming them
  private def convertLockOps(stg: PStage, resMap: DFMap[Map[Id, Set[LockHandleInfo]]]):
    DFMap[Map[Id, Set[LockHandleInfo]]] = {
    var result = resMap
    val maybeRes = resMap(stg.name)
    var newRes = maybeRes
    var newCmds = List[Command]()
    stg.getCmds.foreach {
      case c@IReserveLock(handle, larg) if larg.evar.isDefined =>
        //reserve if no alias, otherwise assign to matching handle
        //don't compare w/ new reservations -> library must handle concurrent reservation reentrancy issues.
        //use the original (maybeRes) instead to look for aliases
        val aliases = maybeRes.getOrElse(larg.id, Set()).filterNot(alias => alias._1 == larg.evar.get)
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
      case c@IReleaseLock(larg, handle) if larg.evar.isDefined =>
        val aliases = newRes.getOrElse(larg.id, Set()).filterNot(alias => alias._1 == larg.evar.get)
        if (aliases.nonEmpty) {
          //release if no alias other than self, rename handle and set to invalid
          val aliasnone = aliasNone(larg.evar.get, aliases)
          val newHandle = getReassignedHandle(handle)
          result = renameHandleVariable(handle, newHandle, stg, result)
          newCmds = newCmds :+ IAssignLock(newHandle, EInvalid, Some(handle))
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
    result
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
  //  newHandle = aliasHandle
  //}
  private def assignAlias(addr: EVar, newHandle: EVar, aliasInfo: LockHandleInfo): Command = {
    val aliasCond = AndOp(EIsValid(aliasInfo._2), EqOp(addr, aliasInfo._1))
    ICondCommand(aliasCond, List(
      IAssignLock(newHandle, aliasInfo._2, None)
    ))
  }

  private def checkOwned(lockArg: LockArg, aliasInfo: LockHandleInfo): Command = {
    val addr = lockArg.evar.get
    val aliasCond = AndOp(EIsValid(aliasInfo._2), EqOp(addr, aliasInfo._1))
    ICondCommand(aliasCond, List(
      ICheckLockOwned(lockArg, aliasInfo._2, aliasInfo._2)
    ))
  }

  private def getReassignedHandle(handle: EVar): EVar = {
    val newId = Id(handle.id.v + "_done")
    newId.typ = handle.id.typ
    val newVar = EVar(newId)
    newVar.typ = handle.typ
    newVar
  }

  //changes the name of handle variables used to generate alias checks in all stages
  private def renameHandleVariable(oldN: EVar, newN: EVar, curstg: PStage, map: DFMap[Map[Id, Set[LockHandleInfo]]]):
    DFMap[Map[Id, Set[LockHandleInfo]]] = {
    var result: DFMap[Map[Id, Set[LockHandleInfo]]] = Map()
    val reachable = Utilities.getReachableStages(curstg).map(s => s.name).toSet
    map.foreachEntry((stg, mayberes) => {
      var maybeResInfo: Map[Id, Set[LockHandleInfo]] = Map()
      if (reachable.contains(stg)) {
        //only update handles in reachable future stages
        mayberes.foreachEntry((largid, aliases) => {
          val renamedAliasHandles = aliases.map( linfo => {
            val addr = linfo._1
            val handle = linfo._2
            val newhandle = if (handle == oldN) { newN } else { handle }
            (addr, newhandle)
          })
          maybeResInfo = maybeResInfo + (largid -> renamedAliasHandles)
          })
      } else {
        //else leave the alias info as is
        maybeResInfo = mayberes
      }
      result = result + (stg -> maybeResInfo)
    })
    result
  }
}
