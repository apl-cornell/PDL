package pipedsl.typechecker

import pipedsl.common.Errors.DeformedLockTypes
import pipedsl.common.Syntax.{CIf, CLockOp, CSeq, CSpeculate, CSplit, CTBar, Circuit, Command, FuncDef, Id, LockArg, ModuleDef, Prog}

import scala.collection.immutable

/**
 * An object to check whether a program's locks are well formed. Locks are well formed if 
 * each memory module only has one type of lock (location-specific or not)
 *
 */

object LockWellformedChecker {
  sealed trait LockType
  case class Specific() extends LockType
  case class General() extends LockType
  
  var MemLockTypeMap: Map[Id, LockType] = new immutable.HashMap[Id, LockType]()
  
  /** 
   * Checks if the program has well formed locks
   * 
   * @param p the program to be checked
   * @throws DeformedLockTypes error if locks are not well formed         
   */
  def check(p:Prog) : Unit = {
    val Prog(_, moddefs, _) = p
    moddefs.foreach(m => checkModule(m))
  }
  
  private def checkModule(moduleDef: ModuleDef): Unit = {
    checkCommand(moduleDef.body)
  }
  
  private def checkCommand(command: Command): Unit = command match {
    case CSeq(c1, c2) => {checkCommand(c1); checkCommand(c2)}
    case CTBar(c1, c2) => {checkCommand(c1); checkCommand(c2)}
    case CIf(cond, cons, alt) => {checkCommand(cons); checkCommand(alt)}
    case CSpeculate(predVar, predVal, verify, body) => {checkCommand(verify); checkCommand(body)}
    case CSplit(cases, default) => {cases.foreach(c => checkCommand(c.body)); checkCommand(default)}
    case CLockOp(mem, op) => {
      if (MemLockTypeMap.get(mem.id).isDefined && !MemLockTypeMap(mem.id).equals(getLockType(mem)))
        throw DeformedLockTypes("Memory modules can only have location specific locks or general locks, but not both")
      else MemLockTypeMap = MemLockTypeMap + (mem.id -> getLockType(mem))
    }
    case _ =>
  }
  
  private def getLockType(lockArg: LockArg): LockType = lockArg.expr match {
    case Some(_) => Specific()
    case None => General()
  }
}
