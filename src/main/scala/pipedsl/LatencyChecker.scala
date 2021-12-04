/*
package pipedsl.typechecker

import pipedsl.common.Syntax._
import pipedsl.typechecker.TypeChecker.TypeChecks
import Environments._
import pipedsl.common.Syntax
import pipedsl.common.Syntax.Latency.{Combinational, Latency}

object LatencyChecker
 {
  def checkProg(p :Prog) = p.moddefs.foreach(checkModule)

  private val empty = Set[Id]()
  private def checkModule(m: ModuleDef): Unit =
   checkCommand(m.body, empty)

  private def checkCommand(c :Command, env :Set[Id]) :Set[Id] = c match
  {
   case CSeq(c1, c2) =>  checkCommand(c2, checkCommand(c1, env))
   case CTBar(c1, c2) =>
    checkCommand(c1, empty)
    checkCommand(c2, empty)
    empty
   case CIf(cond, cons, alt) =>
    checkExpr(cond, env)
    checkCommand(alt, checkCommand(cons, env))
   case CAssign(lhs, rhs) =>
    checkExpr(rhs, env) match {
     case Combinational => env
     case _ => env + lhs.id
    }
   case CRecv(lhs, rhs) =>
    lhs match {
     case EVar(id) => checkExpr(rhs, env) match {
      case Combinational => env
      case _ => env + id
     }
     case _ => checkExpr(rhs, env); env
    }
   case CSpecCall(handle, pipe, args) =>

   case CCheckSpec(isBlocking) =>
   case CVerify(handle, args, preds, update) =>
   case CUpdate(newHandle, handle, args, preds) =>
   case CInvalidate(handle) =>
   case CPrint(args) =>
   case COutput(exp) =>
   case CReturn(exp) =>
   case CExpr(exp) =>
   case CLockStart(mod) =>
   case CLockEnd(mod) =>
   case CLockOp(mem, op, lockType, args, ret) =>
   case CSplit(cases, default) =>
   case CEmpty() =>
   case IReserveLock(outHandle, _) => env + outHandle.id
  }
  private def checkExpr(expr: Expr, delayed: Set[Id]) :Latency = expr match
  {
   case EIsValid(ex) => checkExpr(ex, delayed)
   case EFromMaybe(ex) => checkExpr(ex, delayed)
   case EToMaybe(ex) => checkExpr(ex, delayed)
   case EUop(_, ex) => checkExpr(ex, delayed)
   case EBinop(_, e1, e2) =>
    Latency.join(checkExpr(e1, delayed), checkExpr(e2, delayed))
   case ERecAccess(rec, _) => checkExpr(rec, delayed)
   case ERecLiteral(fields) => fields.foreach(idexpr => checkExpr(idexpr._2, delayed))
   case EMemAccess(mem, index, wmask, inHandle, outHandle) =>
   case EBitExtract(num, start, end) =>
   case ETernary(cond, tval, fval) =>
   case EApp(func, args) =>
   case ECall(mod, method, args, isAtomic) =>
   case EVar(id) =>
   case ECast(ctyp, exp) =>
   case expr: CirExpr =>
  }
 }
*/
