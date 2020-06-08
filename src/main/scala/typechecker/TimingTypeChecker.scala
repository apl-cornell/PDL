package pipedsl.typechecker

import pipedsl.common.Syntax._
import Environments.TypeEnvironment
import TypeChecker.TypeChecks
import pipedsl.common.Errors.{UnavailableArgUse, UnexpectedAsyncReference, UnexpectedLVal}
import pipedsl.common.Syntax

/*
 * Currently this checks that variables set by receive statements
 * are not used until after a `---` separator. It also checks that
 * any access to memory or an external module happens as part of a
 * "receive" statement rather than a normal assign.
 * This checker should maybe check more timing related behavior in the future
 * (such as lock acquisition)
 */
object TimingTypeChecker extends TypeChecks {

  type Available = Set[Id]
  val NoneAvailable: Available = Set[Id]()

  //Functions are combinational, this is checked in their well-formedness check
  override def checkFunc(f: FuncDef, env: TypeEnvironment): TypeEnvironment = env

  override def checkModule(m: ModuleDef, env: TypeEnvironment): TypeEnvironment = {
    val inputs = m.inputs.foldLeft[Available](NoneAvailable)((av,p) => {
      av + p.name
    })
    val allAvailable = m.modules.foldLeft[Available](inputs)((av, m) => {
      av + m.name
    })
    checkCommand(m.body, allAvailable, NoneAvailable)
    env
  }

  def checkCommand(c: Command, vars: Available, nextVars: Available): (Available, Available) = c match {
    case CSeq(c1, c2) => {
      val (v2, nv2) = checkCommand(c1, vars, nextVars)
      checkCommand(c2, v2, nv2)
    }
    case CTBar(c1, c2) =>{
      val (v2, nv2) = checkCommand(c1, vars, nextVars)
      checkCommand(c2, v2 ++ nv2, NoneAvailable)
    }
    case CIf(cond, cons, alt) => {
      if(checkExpr(cond, vars)) {
        throw UnexpectedAsyncReference(cond.pos, cond.toString)
      }
      val (vt, nvt) = checkCommand(cons, vars, nextVars)
      val (vf, nvf) = checkCommand(alt, vars, nextVars)
      (vt.intersect(vf), nvt.intersect(nvf))
    }
    case CAssign(lhs, rhs) => {
      if (checkExpr(rhs, vars)) {
        throw UnexpectedAsyncReference(rhs.pos, rhs.toString)
      }
      lhs match {
        case EVar(id) => (vars + id, nextVars)
        case _: EMemAccess => throw UnexpectedAsyncReference(lhs.pos, lhs.toString)
        case _ => (vars, nextVars)
      }
    }
    case CRecv(lhs, rhs) =>{
      checkExpr(rhs, vars)
      lhs match {
        case EVar(id) => (vars, nextVars + id)
        case _ => (vars, nextVars)
      }
    }
    case CCall(id, args) => {
      args.foreach(a => if(checkExpr(a, vars)) {
        throw UnexpectedAsyncReference(a.pos, a.toString)
      })
      (vars, nextVars)
    }
    case COutput(exp) => {
      if (checkExpr(exp, vars)) {
        throw UnexpectedAsyncReference(exp.pos, exp.toString)
      }
      (vars, nextVars)
    }
    case CReturn(exp) => {
      if (checkExpr(exp, vars)) {
        throw UnexpectedAsyncReference(exp.pos, exp.toString)
      }
      (vars, nextVars)
    }
    case CExpr(exp) => {
      if (checkExpr(exp, vars)) {
        throw UnexpectedAsyncReference(exp.pos, exp.toString)
      }
      (vars, nextVars)
    }
    case Syntax.CEmpty => (vars, nextVars)
  }

  //Returns true if any subexpression references memory or an external module
  def checkExpr(e: Expr, vars: Available): Boolean = e match {
    case EBinop(op, e1, e2) => {
      checkExpr(e1, vars) || checkExpr(e2, vars)
    }
    case ERecAccess(rec, _) => checkExpr(rec, vars)
    case EMemAccess(_, index) => {
      checkExpr(index, vars)
      true
    }
    case EBitExtract(num, _, _) => checkExpr(num, vars)
    case ETernary(cond, tval, fval) => {
      checkExpr(cond, vars) || checkExpr(tval, vars) || checkExpr(fval, vars)
    }
    case EApp(func, args) => args.foldLeft[Boolean](false)((usesMem, a) => checkExpr(a, vars) || usesMem)
    case EVar(id) => if(!vars(id)) { throw UnavailableArgUse(e.pos, id.toString)} else { false }
    case _ => false
  }
  //No timing in the circuit, just connections
  override def checkCircuit(c: Circuit, env: TypeEnvironment): TypeEnvironment = env
}
