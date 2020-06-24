package pipedsl.typechecker

import pipedsl.common.Syntax._
import TypeChecker.TypeChecks
import pipedsl.common.Errors.{UnavailableArgUse, UnexpectedAsyncReference, UnexpectedPipelineStatement}
import pipedsl.common.Syntax
import Environments.Environment

/**
 * - Checks that variables set by receive statements
 *   are not used until after a `---` separator (additionally checking
 *   that any reference to a variable happens after it has been assigned).
 * - Variables introduced via speculation can only be used inside the body
 *   of speculate blocks and as LHS references for Check and Resolve statements
 * - Checks that any access to memory or an external module happens as part of a
 *   "receive" statement rather than a normal assign or other combinational expression.
 * - Ensures that no pipeline splitting operations are conducted inside an if statement
 */
object TimingTypeChecker extends TypeChecks[Type] {

  type Available = Set[Id]
  val NoneAvailable: Available = Set[Id]()

  override def emptyEnv(): Environment[Type] = Environments.EmptyTypeEnv

  //Functions are combinational, this is checked in their well-formedness check
  override def checkFunc(f: FuncDef, env: Environment[Type]): Environment[Type] = env

  override def checkModule(m: ModuleDef, env: Environment[Type]): Environment[Type] = {
    val inputs = m.inputs.foldLeft[Available](NoneAvailable)((av,p) => {
      av + p.name
    })
    val allAvailable = m.modules.foldLeft[Available](inputs)((av, m) => {
      av + m.name
    })
    checkCommand(m.body, allAvailable, NoneAvailable, insideCond = false)
    env
  }

  def checkCommand(c: Command, vars: Available, nextVars: Available, insideCond: Boolean): (Available, Available) = c match {
    case CSeq(c1, c2) => {
      val (v2, nv2) = checkCommand(c1, vars, nextVars, insideCond)
      checkCommand(c2, v2, nv2, insideCond)
    }
    case CTBar(c1, c2) => {
      if (insideCond) { throw UnexpectedPipelineStatement(c.pos, "time barrier") }
      val (v2, nv2) = checkCommand(c1, vars, nextVars, insideCond)
      checkCommand(c2, v2 ++ nv2, NoneAvailable, insideCond)
    }
    case CSplit(cases, default) => {
      if (insideCond) { throw UnexpectedPipelineStatement(c.pos, "pipeline split")}
      var (endv, endnv) = checkCommand(default, vars, nextVars, insideCond)
      for (c <- cases) {
        if(checkExpr(c.cond, vars)) {
          throw UnexpectedAsyncReference(c.cond.pos, c.cond.toString)
        }
        val (v2, nv2) = checkCommand(c.body, vars, nextVars, insideCond)
        endv = endv.intersect(v2)
        endnv = endnv.intersect(nv2)
      }
      (endv, endnv)
    }
    case CDecl(id,_,n) => if (n) { (vars, nextVars + id) } else { (vars, nextVars) }
    case CIf(cond, cons, alt) => {
      if(checkExpr(cond, vars)) {
        throw UnexpectedAsyncReference(cond.pos, cond.toString)
      }
      val (vt, nvt) = checkCommand(cons, vars, nextVars, insideCond = true)
      val (vf, nvf) = checkCommand(alt, vars, nextVars, insideCond = true)
      (vt.intersect(vf), nvt.intersect(nvf))
    }
    case CAssign(lhs, rhs) => {
      if (checkExpr(rhs, vars)) {
        throw UnexpectedAsyncReference(rhs.pos, rhs.toString)
      }
      (vars + lhs.id, nextVars)
    }
    case CRecv(lhs, rhs) => {
      checkExpr(rhs, vars)
      (lhs, rhs) match {
        case (EVar(id), EMemAccess(_, _)) => (vars, nextVars + id)
        case (EVar(id), _) => throw UnexpectedAsyncReference(lhs.pos, s"rhs of '$id <-' but be a memory or module reference")
        case (EMemAccess(_,_), EMemAccess(_,_)) => throw UnexpectedAsyncReference(lhs.pos, "Both sides of <- cannot be memory or modules references")
        case _ => (vars, nextVars)
      }
    }
    case CLockOp(_, _) => (vars, nextVars)
    case CSpeculate(predVar, predVal, body) => {
      if(checkExpr(predVal, vars)) {
        throw UnexpectedAsyncReference(predVal.pos, "Speculative value must be combinational")
      }
      //Only allow speculative variables to be referenced in speculative body
      val (vars2, nvars2) = checkCommand(body, vars + predVar.id, nextVars, insideCond)
      (vars2 - predVar.id, nvars2)
    }
    case CCheck(predVar, realVal) => {
      if (checkExpr(realVal, vars)) {
        throw UnexpectedAsyncReference(realVal.pos, "Check clause must be combinational")
      }
      //now that the variable is *not* speculative it can be used
      (vars + predVar, nextVars)
    }
    case CResolve(_) => (vars, nextVars)
    case CCall(_, args) => {
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
    case EUop(_, e) => checkExpr(e, vars)
    case EBinop(_, e1, e2) => {
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
    case EApp(_, args) => args.foldLeft[Boolean](false)((usesMem, a) => checkExpr(a, vars) || usesMem)
    case EVar(id) => if(!vars(id)) { throw UnavailableArgUse(e.pos, id.toString)} else { false }
    case ECast(_, exp) => checkExpr(exp, vars)
    case _ => false
  }
  //No timing in the circuit, just connections
  override def checkCircuit(c: Circuit, env: Environment[Type]): Environment[Type] = env

}
