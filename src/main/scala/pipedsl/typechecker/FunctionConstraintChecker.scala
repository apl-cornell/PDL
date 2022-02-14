package pipedsl.typechecker

import com.microsoft.z3.{Status, Context => Z3Context, Solver => Z3Solver}
import pipedsl.common.Constraints.ImplicitConstraints._
import pipedsl.common.Constraints._
import pipedsl.common.Errors.{BadConstraintsAtCall, MissingType}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.degenerify
import scala.collection.mutable
import scala.language.implicitConversions

object FunctionConstraintChecker
 {
  def check(p: Prog): Unit =
   {
    val cons_map = mutable.HashMap[Id, FuncDef]()
    p.fdefs.foreach(f => cons_map.addOne(f.name, f))
    p.fdefs.foreach(checkFunc(cons_map, _))
    p.moddefs.foreach(checkMod(cons_map, _))
   }

  def checkFunc(cons_map: mutable.HashMap[Id, FuncDef], f: FuncDef): Unit =
   {
    val ctxt = new Z3Context()
    val solv = ctxt.mkSolver()
    f.constraints.foreach(c => solv.add(to_z3(ctxt, c)))
    checkCmd(f.body, ctxt, solv, cons_map)
   }

  def checkMod(cons_map: mutable.HashMap[Id, FuncDef], m: ModuleDef): Unit =
   {
    val ctxt = new Z3Context()
    val solv = ctxt.mkSolver()
    checkCmd(m.body, ctxt, solv, cons_map)
   }

  def checkCmd(c: Command, ctxt: Z3Context, solv: Z3Solver, cons_map: mutable.HashMap[Id, FuncDef]): Unit =
   {
    def _checkCmd(cmd: Command): Unit = checkCmd(cmd, ctxt, solv, cons_map)

    def _checkExpr(ex: Expr): Unit = checkExpr(ex, ctxt, solv, cons_map)

    c match
    {
     case CSeq(c1, c2) => _checkCmd(c1); _checkCmd(c2)
     case CTBar(c1, c2) => _checkCmd(c1); _checkCmd(c2)
     case CIf(cond, cons, alt) => _checkExpr(cond); _checkCmd(cons); _checkCmd(alt)
     case CAssign(lhs, rhs) => _checkExpr(rhs)
     case CRecv(lhs, rhs) => _checkExpr(rhs)
     case CSpecCall(handle, pipe, args) => args.foreach(_checkExpr)
     case CVerify(handle, args, preds, update) => args.foreach(_checkExpr)
     case CUpdate(newHandle, handle, args, preds) => args.foreach(_checkExpr)
     case CPrint(args) => args.foreach(_checkExpr)
     case COutput(exp) => _checkExpr(exp)
     case CReturn(exp) => _checkExpr(exp)
     case CExpr(exp) => _checkExpr(exp)
     case CSplit(cases, default) => cases.foreach(c =>
      {
       _checkExpr(c.cond);
       _checkCmd(c.body)
      });
      _checkCmd(default)
     case _ => ()
    }
   }

  def extract_width(t: Type): Option[IntExpr] = t match
  {
   case TSizedInt(len, _) => Some(len)
   case _ => None
  }

  private implicit class PipelineContainer[F](val value: F)
   {
    def |>[G](f: F => G): G = f(value)
   }

  def type_of_fdef(f: FuncDef): TFun = TFun(f.args.map(a => a.typ), f.ret)

  def checkExpr(e: Expr, ctxt: Z3Context, solv: Z3Solver, cons_map: mutable.HashMap[Id, FuncDef]): Unit =
   {
    def _checkExpr(ex: Expr): Unit = checkExpr(ex, ctxt, solv, cons_map)

    e match
    {
     case EIsValid(ex) => _checkExpr(ex)
     case EFromMaybe(ex) => _checkExpr(ex)
     case EToMaybe(ex) => _checkExpr(ex)
     case EUop(_, ex) => _checkExpr(ex)
     case EBinop(_, e1, e2) => _checkExpr(e1); _checkExpr(e2)
     case ERecAccess(rec, _) => _checkExpr(rec)
     case ERecLiteral(fields) => fields.foreach((ex) => _checkExpr(ex._2))
     case EMemAccess(_, index, wmask, _, _, _) => _checkExpr(index); wmask.foreach(_checkExpr)
     case EBitExtract(num, _, _) => _checkExpr(num)
     case ETernary(cond, tval, fval) => _checkExpr(cond); _checkExpr(tval); _checkExpr(fval)
     case ea@EApp(func, args) =>

      type_of_fdef(cons_map(func)).matchOrError(e.pos, "func type", "func type")
      { case TFun(targs, ret) => solv.push()
       val contraints_here = targs.zip(args.map(e => e.typ.getOrElse(throw MissingType(e.pos, "arg type")))).map(pair =>
        {
         (pair._1 |> extract_width, pair._2 |> degenerify |> extract_width) match
         {
          case (Some(a), Some(b)) => Some(ReEq(a, b))
          case _ => None
         }
        }).collect
       { case Some(cons) => cons
       }.prependedAll((ret |> extract_width, e.typ.getOrElse(throw MissingType(e.pos, "ret type")) |> degenerify |> extract_width) match
       { case (Some(a), Some(b)) => List(ReEq(a, b))
        case _ => List()
       }).map(degenerify_constr)
       val called_cons = cons_map(func).constraints.map(degenerify_constr)
       val constraints = called_cons prependedAll contraints_here
       constraints.foreach(c => solv.add(to_z3(ctxt, c)))
       solv.check() match
       {
        case Status.UNSATISFIABLE | Status.UNKNOWN => throw BadConstraintsAtCall(ea)
        case Status.SATISFIABLE => ()
       }
       solv.pop()
      }
     case ECall(_, _, args) => args.foreach(_checkExpr)
     case ECast(_, exp) => _checkExpr(exp)
     case _ => ()
    }
   }
 }
