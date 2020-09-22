package pipedsl.typechecker

import TypeChecker._
import Environments._
import pipedsl.common.Errors.{AlreadyResolvedSpeculation, MismatchedSpeculationState, UnresolvedSpeculation}
import pipedsl.common.Syntax._

object SpeculationChecker extends TypeChecks[Type] {

  override def emptyEnv(): Environment[Type] = EmptyTypeEnv

  //No Speculation in Functions
  override def checkFunc(f: FuncDef, env: Environment[Type]): Environment[Type] = env


  override def checkModule(m: ModuleDef, env: Environment[Type]): Environment[Type] = {
    val canBeSpec: List[Id] = m.inputs.foldLeft(List[Id]())((l, in) => {
      if (in.typ.maybeSpec) l :+ in.name else l
    })
    val finalSpec = checkCommand(m.body, canBeSpec, canBeSpec.nonEmpty)
    if (finalSpec.nonEmpty) throw UnresolvedSpeculation(m.pos, "end of pipeline")
    env
  }

  /**
   * Speculation for the current thread must be resolved before any of the following
   * commands are executed:
   *  - Writes to memories (CRECV with memories on LHS)
   *  - Updating the results of speculation made by this thread (CCHeck commands)
   * @param c The command to check
   * @param specVars Set of Variables which are currently speculative
   * @param modIsSpec True if the pipeline can be called speculatively
   * @return
   */
  def checkCommand(c: Command, specVars: List[Id], modIsSpec: Boolean): List[Id] = c match {
    case CSeq(c1, c2) =>
      val specVars1 = checkCommand(c1, specVars, modIsSpec)
      checkCommand(c2, specVars1, modIsSpec)
    case CTBar(c1, c2) =>
      val specVars1 = checkCommand(c1, specVars, modIsSpec)
      checkCommand(c2, specVars1, modIsSpec)
    case CIf(_, cons, alt) =>
      val specVarsT = checkCommand(cons, specVars, modIsSpec)
      val specVarsF = checkCommand(alt, specVars, modIsSpec)
      if (specVarsT != specVarsF) {
        throw MismatchedSpeculationState(c.pos)
      } else {
        specVarsT
      }
    case CRecv(lhs, _) => (lhs) match {
      case (EMemAccess(_, _)) if specVars.nonEmpty =>
        throw UnresolvedSpeculation(lhs.pos, "Memory Write")
      case _ => specVars
    }
    case COutput(_) if (specVars.nonEmpty) => throw UnresolvedSpeculation(c.pos, "Module Output")
    case CLockOp(_, _) => specVars
    case CSpeculate(predVar, _, verify, body) => {
      val specVarsVerify = checkCommand(verify, specVars, modIsSpec)
      val specVarsWithPred = predVar.id +: specVars
      val specVarsSpec = checkCommand(body, specVarsWithPred, modIsSpec)
      if (specVarsSpec != specVarsWithPred) {
        throw MismatchedSpeculationState(c.pos)
      }
      specVarsVerify
    }
    case CCheck(predVar) => specVars match {
      case v :: tail if v == predVar=> tail
      case _ => throw AlreadyResolvedSpeculation(c.pos)
    }
    case CCall(_, _) => if (!modIsSpec && specVars.nonEmpty)
        throw UnresolvedSpeculation(c.pos, "pipeline call statement")
      else
        specVars
    case _ => specVars
  }

  override def checkCircuit(c: Circuit, env: Environment[Type]): Environment[Type] = env
}
