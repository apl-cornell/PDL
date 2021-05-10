package pipedsl.typechecker

import pipedsl.common.Errors.MalformedLockTypes
import pipedsl.common.Locks.{General, LockGranularity, Specific}
import pipedsl.common.Syntax.{CIf, CLockOp, CSeq, CSpeculate, CSplit, CTBar, Command, Id, LockArg, LockType, ModuleDef, Prog}

/**
 * A class to check whether a program's locks are well formed. Locks are well formed if 
 * each memory module only has one granularity of lock (location-specific or not)
 *
 */
class LockWellformedChecker() {

  private var memLockGranularityMap: Map[Id, Map[Id, LockGranularity]] = Map().withDefaultValue(Map())
  private var currentMod: Id = Id("-invalid-")

  def getModLockGranularityMap: Map[Id, Map[Id, LockGranularity]] = {
    memLockGranularityMap
  }

  private def getLockGranularityMap: Map[Id, LockGranularity] = {
    memLockGranularityMap(currentMod)
  }

  private def updateLockGranularityMap(lid: Id, lt: LockGranularity): Unit = {
    memLockGranularityMap = memLockGranularityMap.updated(currentMod, getLockGranularityMap + (lid -> lt))
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
    case c@CLockOp(mem, _, _) =>
      if (getLockGranularityMap.contains(mem.id) && !getLockGranularityMap(mem.id).equals(getLockGranularity(mem)))
        throw MalformedLockTypes("Memory modules can only have location specific locks or general locks, but not both")
      else updateLockGranularityMap(mem.id, getLockGranularity(mem))
      c.granularity = getLockGranularity(mem)
      lockArgs + mem
    case _ => lockArgs

  }
  
  private def getLockGranularity(lockArg: LockArg): LockGranularity = lockArg.evar match {
    case Some(_) => Specific
    case None => General
  }
}
