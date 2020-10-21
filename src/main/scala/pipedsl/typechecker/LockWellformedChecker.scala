package pipedsl.typechecker

import pipedsl.common.Errors.MalformedLockTypes
import pipedsl.common.Locks.{General, LockType, Specific}
import pipedsl.common.Syntax.{CIf, CLockOp, CSeq, CSpeculate, CSplit, CTBar, Command, Id, LockArg, ModuleDef, Prog}

/**
 * A class to check whether a program's locks are well formed. Locks are well formed if 
 * each memory module only has one type of lock (location-specific or not)
 *
 */
class LockWellformedChecker() {

  private var memLockTypeMap: Map[Id, Map[Id, LockType]] = Map().withDefaultValue(Map())
  private var currentMod: Id = Id("-invalid-")

  def getModLockTypeMap: Map[Id, Map[Id, LockType]] = {
    memLockTypeMap
  }

  private def getLockTypeMap: Map[Id, LockType] = {
    memLockTypeMap(currentMod)
  }

  private def updateLockTypeMap(lid: Id, lt: LockType): Unit = {
    memLockTypeMap = memLockTypeMap.updated(currentMod, getLockTypeMap + (lid -> lt))
  }

  /** 
   * Checks if the program has well formed locks
   * as defined in the comment at the top of this class.
   * @param p the program to be checked
   * @throws MalformedLockTypes error if locks are not well formed
   * @return A map containing the set of lock arguments used in each module.
   */
  def check(p:Prog) : Map[Id, Set[LockArg]] = {
    val Prog(_, moddefs, _) = p
    moddefs.foldLeft[Map[Id, Set[LockArg]]](Map())((map, mod) => map + (mod.name -> checkModule(mod, Set())))
  }
  
  private def checkModule(moduleDef: ModuleDef, set: Set[LockArg]): Set[LockArg] = {
    currentMod = moduleDef.name
    checkCommand(moduleDef.body, set)
  }
  
  private def checkCommand(command: Command, lockArgs: Set[LockArg]): Set[LockArg] = command match {
    case CSeq(c1, c2) => val s1 = checkCommand(c1, lockArgs); checkCommand(c2, s1)
    case CTBar(c1, c2) => val s1 = checkCommand(c1, lockArgs); checkCommand(c2, s1)
    case CIf(_, cons, alt) => val s1 = checkCommand(cons, lockArgs); checkCommand(alt, s1)
    case CSpeculate(_, _, verify, body) => val s1 = checkCommand(verify, lockArgs); checkCommand(body, s1)
    case CSplit(cases, default) =>
      val s1 = cases.foldLeft[Set[LockArg]](lockArgs)((s, c) => checkCommand(c.body, s))
      checkCommand(default, s1)
    case CLockOp(mem, _) =>
      if (getLockTypeMap.contains(mem.id) && !getLockTypeMap(mem.id).equals(getLockType(mem)))
        throw MalformedLockTypes("Memory modules can only have location specific locks or general locks, but not both")
      else updateLockTypeMap(mem.id, getLockType(mem))
      lockArgs + mem
    case _ => lockArgs
  }
  
  private def getLockType(lockArg: LockArg): LockType = lockArg.evar match {
    case Some(_) => Specific
    case None => General
  }
}
