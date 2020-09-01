package pipedsl.typechecker

import pipedsl.common.Syntax._
import TypeChecker.TypeChecks
import pipedsl.common.Errors.{MissingType, UnavailableArgUse, UnexpectedAsyncReference, UnexpectedPipelineStatement, UnexpectedSyncReference, UnexpectedType}
import pipedsl.common.Syntax
import Environments.Environment
import pipedsl.common.Syntax.Latency.{Combinational, Latency, join}

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
    checkCommand(m.body, allAvailable, NoneAvailable)
    env
  }

  /**
   *
   * @param c
   * @param vars
   * @param nextVars
   * @return
   */
  def checkCommand(c: Command, vars: Available, nextVars: Available): (Available, Available) = c match {
    case CSeq(c1, c2) => {
      val (v2, nv2) = checkCommand(c1, vars, nextVars)
      checkCommand(c2, v2, nv2)
    }
    case CTBar(c1, c2) => {
      val (v2, nv2) = checkCommand(c1, vars, nextVars)
      checkCommand(c2, v2 ++ nv2, NoneAvailable)
    }
    case CSplit(cases, default) => {
      var (endv, endnv) = checkCommand(default, vars, nextVars)
      for (c <- cases) {
        if(checkExpr(c.cond, vars) != Latency.Combinational) {
          throw UnexpectedAsyncReference(c.cond.pos, c.cond.toString)
        }
        val (v2, nv2) = checkCommand(c.body, vars, nextVars)
        endv = endv.intersect(v2)
        endnv = endnv.intersect(nv2)
      }
      (endv, endnv)
    }
    case CIf(cond, cons, alt) => {
      if(checkExpr(cond, vars) != Latency.Combinational) {
        throw UnexpectedAsyncReference(cond.pos, cond.toString)
      }
      val (vt, nvt) = checkCommand(cons, vars, nextVars)
      val (vf, nvf) = checkCommand(alt, vars, nextVars)
      (vt.intersect(vf), nvt.intersect(nvf))
    }
    case CAssign(lhs, rhs) => {
      if (checkExpr(rhs, vars) != Latency.Combinational) {
        throw UnexpectedAsyncReference(rhs.pos, rhs.toString)
      }
      (vars + lhs.id, nextVars)
    }
    case CRecv(lhs, rhs) => {
      val rhsLat = checkExpr(rhs, vars)
      val lhsLat = checkExpr(lhs, vars, isRhs = false)
      if (rhsLat == Combinational && lhsLat == Combinational) {
        throw UnexpectedSyncReference(c.pos, s"<- statements expect a noncombinational reference")
      }
      (lhs, rhs) match {
        case (EVar(id), EMemAccess(_, _)) => (vars, nextVars + id)
        case (EVar(id), _) => throw UnexpectedAsyncReference(lhs.pos, s"rhs of '$id <-' but be a memory or module reference")
        case (EMemAccess(_,_), EMemAccess(_,_)) => throw UnexpectedAsyncReference(lhs.pos, "Both sides of <- cannot be memory or modules references")
        case _ => (vars, nextVars)
      }
    }
    case CLockOp(_, _) => (vars, nextVars)
    case CSpeculate(predVar, predVal, verify, body) => {
      if(checkExpr(predVal, vars) != Combinational) {
        throw UnexpectedAsyncReference(predVal.pos, "Speculative value must be combinational")
      }
      val (varsv, nvarsv) = checkCommand(verify, vars ++ nextVars, NoneAvailable)
      val (vars2, nvars2) = checkCommand(body, vars + predVar.id ++ nextVars, NoneAvailable)
      (varsv ++ vars2, nvarsv ++ nvars2)
    }
    case CCheck(_) => {
      (vars, nextVars)
    }
    case CCall(_, args) => {
      args.foreach(a => if(checkExpr(a, vars) != Combinational) {
        throw UnexpectedAsyncReference(a.pos, a.toString)
      })
      (vars, nextVars)
    }
    case COutput(exp) => {
      if (checkExpr(exp, vars) != Combinational) {
        throw UnexpectedAsyncReference(exp.pos, exp.toString)
      }
      (vars, nextVars)
    }
    case CReturn(exp) => {
      if (checkExpr(exp, vars) != Combinational) {
        throw UnexpectedAsyncReference(exp.pos, exp.toString)
      }
      (vars, nextVars)
    }
    case CExpr(exp) => {
      if (checkExpr(exp, vars) != Combinational) {
        throw UnexpectedAsyncReference(exp.pos, exp.toString)
      }
      (vars, nextVars)
    }
    case Syntax.CEmpty => (vars, nextVars)
  }

  def checkExpr(e: Expr, vars: Available, isRhs: Boolean = true): Latency = e match {
    case EUop(_, e) => checkExpr(e, vars, isRhs)
    case EBinop(_, e1, e2) => {
      join(checkExpr(e1, vars, isRhs), checkExpr(e2, vars, isRhs))
    }
    case ERecAccess(rec, _) => checkExpr(rec, vars, isRhs)
    case EMemAccess(m, index) => m.typ.get match {
      case TMemType(_, _, rLat, wLat) =>{
        val memLat = if (isRhs) { rLat } else { wLat }
        val indexExpr = checkExpr(index, vars, isRhs)
        join(indexExpr, memLat)
      }
      case _ => throw UnexpectedType(m.pos, m.v, "Mem Type", m.typ.get)
    }
    case EBitExtract(num, _, _) => checkExpr(num, vars, isRhs)
    case ETernary(cond, tval, fval) => {
      join(join(checkExpr(cond, vars, isRhs), checkExpr(tval, vars, isRhs)), checkExpr(fval, vars, isRhs))
    }
    case EApp(_, args) => args.foldLeft[Latency](Combinational)((lat, a) => join(checkExpr(a, vars), lat))
    case EVar(id) => if(!vars(id) && isRhs) { throw UnavailableArgUse(e.pos, id.toString)} else { Combinational }
    case ECast(_, exp) => checkExpr(exp, vars, isRhs)
    case _ => Combinational
  }
  //No timing in the circuit, just connections
  override def checkCircuit(c: Circuit, env: Environment[Type]): Environment[Type] = env

}
