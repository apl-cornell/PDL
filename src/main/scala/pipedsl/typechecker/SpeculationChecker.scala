package pipedsl.typechecker

import TypeChecker._
import Environments._
import pipedsl.common.Errors.{AlreadyResolvedSpeculation, UnresolvedSpeculation}
import pipedsl.common.Syntax._

object SpeculationChecker extends TypeChecks[Type] {

  override def emptyEnv(): Environment[Type] = EmptyTypeEnv

  //No Speculation in Functions
  override def checkFunc(f: FuncDef, env: Environment[Type]): Environment[Type] = env


  override def checkModule(m: ModuleDef, env: Environment[Type]): Environment[Type] = {
   val canBeSpec = env(m.name).matchOrError(m.pos, "Module Definition", "Module TYpe")(
     { case TModType(_, _, specVars) => specVars.nonEmpty })
    checkCommand(m.body, canBeSpec)
    env
  }

  /**
   * Speculation for the current thread must be resolved before any of the following
   * commands are executed:
   *  - Writes to memories (CRECV with memories on LHS)
   *  - Updating the results of speculation made by this thread (CCHeck commands)
   * @param c The command to check
   * @param isSpec Whether or not the command starts in a speculative region
   * @return
   */
  def checkCommand(c: Command, isSpec: Boolean): Boolean = c match {
    case CSeq(c1, c2) =>
      val isSpec1 = checkCommand(c1, isSpec)
      checkCommand(c2, isSpec1)
    case CTBar(c1, c2) =>
      val isSpec1 = checkCommand(c1, isSpec)
      checkCommand(c2, isSpec1)
    case CIf(_, cons, alt) =>
      val isSpecT = checkCommand(cons, isSpec)
      val isSpecF = checkCommand(alt, isSpec)
      isSpecT || isSpecF //if either branch *doesn't* resolve speculation we are still speculative
    case CRecv(lhs, _) => (lhs, isSpec) match {
      case (EMemAccess(_, _), true) => throw UnresolvedSpeculation(lhs.pos, "Memory Write")
      case (_,_) => isSpec
    }
    case COutput(_) => throw UnresolvedSpeculation(c.pos, "Module Output")
    case CLockOp(_, _) => isSpec //TODO can you reserve a lock while speculative?
    case CSpeculate(_, _, verify, body) => { //TODO limit check statements to only happen on one path
      val isSpecAfterVerify = checkCommand(verify, isSpec)
      val isSpecAfterSpec = checkCommand(body, true)
      isSpecAfterVerify && isSpecAfterSpec
    }
    case CCheck(predVar) =>
      if (!isSpec) throw AlreadyResolvedSpeculation(c.pos)
      else false
    case _ => isSpec
  }

  override def checkCircuit(c: Circuit, env: Environment[Type]): Environment[Type] = env
}
