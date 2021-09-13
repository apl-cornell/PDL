package pipedsl.typechecker

import pipedsl.common.Syntax._
import TypeChecker.TypeChecks
import pipedsl.common.Errors.{UnavailableArgUse, UnexpectedAsyncReference, UnexpectedCommand, UnexpectedType}
import pipedsl.common.Syntax
import Environments.Environment
import pipedsl.common.Syntax.Latency.{Asynchronous, Combinational, Latency}

/**
 * - Checks that variables set by receive statements
 *   are not used until after a `---` separator (additionally checking
 *   that any reference to a variable happens after it has been assigned).
 * - Checks that any noncombinational access to memory or an external module happens as part of a
 *   "receive" statement rather than a normal assign or other combinational expression.
 * - Ensures that no pipeline splitting operations are conducted inside an if statement
 */
object TimingTypeChecker extends TypeChecks[Id, Type] {

  type Available = Set[Id]
  val NoneAvailable: Available = Set[Id]()

  override def emptyEnv(): Environment[Id, Type] = Environments.EmptyTypeEnv

  override def checkExt(e: ExternDef, env: Environment[Id, Type]): Environment[Id, Type] = env

  //Functions are combinational, this is checked in their well-formedness check
  override def checkFunc(f: FuncDef, env: Environment[Id, Type]): Environment[Id, Type] = env

  override def checkModule(m: ModuleDef, env: Environment[Id, Type]): Environment[Id, Type] = {
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
   * Check that the given command only uses variables which are currently available.
   * Produce a new environment that indicates the variables made available by this command,
   * both those available immediately and those available after a logical timestep.
   * @param c The command to check
   * @param vars The current set of available variables
   * @param nextVars The set of variables which will be available next timestep.
   * @return The updated sets of available variables
   */
  def checkCommand(c: Command, vars: Available, nextVars: Available): (Available, Available) = c match {
    case CSeq(c1, c2) =>
      val (v2, nv2) = checkCommand(c1, vars, nextVars)
      checkCommand(c2, v2, nv2)
    case CTBar(c1, c2) =>
      val (v2, nv2) = checkCommand(c1, vars, nextVars)
      checkCommand(c2, v2 ++ nv2, NoneAvailable)
    case CSplit(cases, default) =>
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
    case CIf(cond, cons, alt) =>
      if(checkExpr(cond, vars) != Latency.Combinational) {
        throw UnexpectedAsyncReference(cond.pos, cond.toString)
      }
      val (vt, nvt) = checkCommand(cons, vars, nextVars)
      val (vf, nvf) = checkCommand(alt, vars, nextVars)
      (vt.intersect(vf), nvt.intersect(nvf))
    case CAssign(lhs, rhs) =>
      if (checkExpr(rhs, vars) != Latency.Combinational) {
        throw UnexpectedAsyncReference(rhs.pos, rhs.toString)
      }
      (vars + lhs.id, nextVars)
    case CRecv(lhs, rhs) =>
      val rhsLat = checkExpr(rhs, vars)
      val lhsLat = checkExpr(lhs, vars, isRhs = false)
        (lhs, rhs) match {
        //TODO rewrite to reduce code maybe?
        case (EVar(id), EMemAccess(_, _, _)) => (vars, nextVars + id)
        case (EVar(id), ECall(_,_,_)) => (vars, nextVars + id)
        case (EVar(id), _) => (vars, nextVars + id)
        case (EMemAccess(_,_, _), EMemAccess(_,_, _)) =>
          throw UnexpectedAsyncReference(lhs.pos, "Both sides of <- cannot be memory or modules references")
        case _ => (vars, nextVars)
      }
    case CLockStart(_) => (vars, nextVars)
    case CLockEnd(_) => (vars, nextVars)
    case CLockOp(mem, _, _) =>
      if (mem.evar.isDefined) {
        checkExpr(mem.evar.get, vars, isRhs = true)
      }
      (vars, nextVars)
    case CSpecCall(handle, _, args) =>
    //args must be available, but handle is available next cycle
    args.foreach(a => if(checkExpr(a, vars) != Combinational) {
        throw UnexpectedAsyncReference(a.pos, a.toString)
      })
      (vars, nextVars + handle.id)
    case CVerify(handle, args, preds, upd) =>
      //handle and args must be available this cycle
      if(checkExpr(handle, vars) != Combinational) {
        throw UnexpectedAsyncReference(handle.pos, handle.toString)
      }
      args.foreach(a => if(checkExpr(a, vars) != Combinational) {
        throw UnexpectedAsyncReference(a.pos, a.toString)
      })
      //just don't check preds they get inserted by the compiler automatically
      if (upd.isDefined) {
        if (checkExpr(upd.get, vars) != Combinational) {
          throw UnexpectedAsyncReference(handle.pos, handle.toString)
        }
      }
      (vars, nextVars)
    case CUpdate(nh, handle, args, preds) =>
      if(checkExpr(handle, vars) != Combinational) {
        throw UnexpectedAsyncReference(handle.pos, handle.toString)
      }
      args.foreach(a => if(checkExpr(a, vars) != Combinational) {
        throw UnexpectedAsyncReference(a.pos, a.toString)
      })
      //just don't check preds they get inserted by the compiler automatically
      (vars, nextVars + nh.id)
    case CInvalidate(handle) =>
      if(checkExpr(handle, vars) != Combinational) {
        throw UnexpectedAsyncReference(handle.pos, handle.toString)
      }
      (vars, nextVars)
    case CCheckSpec(_) => (vars, nextVars)
    case COutput(exp) =>
      if (checkExpr(exp, vars) != Combinational) {
        throw UnexpectedAsyncReference(exp.pos, exp.toString)
      }
      (vars, nextVars)
    case CReturn(exp) =>
      if (checkExpr(exp, vars) != Combinational) {
        throw UnexpectedAsyncReference(exp.pos, exp.toString)
      }
      (vars, nextVars)
    case CExpr(exp) =>
      checkExpr(exp, vars)
      (vars, nextVars)
    case Syntax.CEmpty() => (vars, nextVars)
    case CPrint(args) =>
      args.foreach(a => {
        checkExpr(a, vars)
      })
      (vars, nextVars)
    case _ => throw UnexpectedCommand(c)
  }

  def checkExpr(e: Expr, vars: Available, isRhs: Boolean = true): Latency = e match {
    case EUop(_, e) => checkExpr(e, vars, isRhs) match {
      case Combinational => Combinational
      case _ => throw UnexpectedAsyncReference(e.pos, e.toString)
    }
    case EBinop(_, e1, e2) =>
      (checkExpr(e1, vars, isRhs), checkExpr(e2, vars, isRhs)) match {
        case (Combinational, Combinational) => Combinational
        case (Combinational, _) => throw UnexpectedAsyncReference(e2.pos, e2.toString)
        case (_, Combinational) => throw UnexpectedAsyncReference(e1.pos, e1.toString)
        case (_,_) => throw UnexpectedAsyncReference(e1.pos, e1.toString)
      }
    case ERecAccess(rec, _) => checkExpr(rec, vars, isRhs) match {
      case Combinational => Combinational
      case _ => throw UnexpectedAsyncReference(rec.pos, rec.toString)
    }
    case EMemAccess(m, index, wm) => m.typ.get match {
      case TMemType(_, _, rLat, wLat, _, _) =>
        val memLat = if (isRhs) { rLat } else { wLat }
        val indexExpr = checkExpr(index, vars, isRhs)
        if (wm.isDefined) {
          checkExpr(wm.get, vars, isRhs) match {
            case Combinational => ()
            case _ => throw UnexpectedAsyncReference(wm.get.pos, wm.get.toString)
          }
        }
        indexExpr match {
          case Combinational => memLat
          case _ => throw UnexpectedAsyncReference(index.pos, index.toString)
        }
      case TLockedMemType(TMemType(_, _, rLat, wLat, _, _),_,_) =>
        val memLat = if (isRhs) { rLat } else { wLat }
        val indexExpr = checkExpr(index, vars, isRhs)
        if (wm.isDefined) {
          checkExpr(wm.get, vars, isRhs) match {
            case Combinational => ()
            case _ => throw UnexpectedAsyncReference(wm.get.pos, wm.get.toString)
          }
        }
        indexExpr match {
          case Combinational => memLat
          case _ => throw UnexpectedAsyncReference(index.pos, index.toString)
        }
      case _ => throw UnexpectedType(m.pos, m.v, "Mem Type", m.typ.get)
    }
    case EBitExtract(num, _, _) => checkExpr(num, vars, isRhs) match {
      case Combinational => Combinational
      case _ => throw UnexpectedAsyncReference(num.pos, num.toString)
    }
    case ETernary(cond, tval, fval) =>
      (checkExpr(cond, vars, isRhs), checkExpr(tval, vars, isRhs), checkExpr(fval, vars, isRhs)) match {
        case (Combinational, Combinational, Combinational) => Combinational
        case (_, Combinational, Combinational) => throw UnexpectedAsyncReference(cond.pos, cond.toString)
        case (_, _, Combinational) => throw UnexpectedAsyncReference(tval.pos, tval.toString)
        case _ => throw UnexpectedAsyncReference(fval.pos, fval.toString)
      }
    case EApp(_, args) =>
      args.foreach(a => if(checkExpr(a, vars) != Combinational) {
        throw UnexpectedAsyncReference(a.pos, a.toString)
      })
      Combinational
    case ECall(_, name, args) =>
      args.foreach(a => if(checkExpr(a, vars) != Combinational) {
        throw UnexpectedAsyncReference(a.pos, a.toString)
      })
      //TODO methods are hacked
      if (name.isDefined) { Combinational } else { Asynchronous }
    case EVar(id) => if(!vars(id) && isRhs) { throw UnavailableArgUse(e.pos, id.toString)} else { Combinational }
    case ECast(_, exp) => checkExpr(exp, vars, isRhs)
    case _ => Combinational
  }
  //No timing in the circuit, just connections
  override def checkCircuit(c: Circuit, env: Environment[Id, Type]): Environment[Id, Type] = env

}
