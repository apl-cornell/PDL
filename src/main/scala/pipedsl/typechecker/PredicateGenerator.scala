package pipedsl.typechecker

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context, Expr => Z3Expr}
import pipedsl.common.Syntax
import pipedsl.common.Syntax.{BoolOp, BoolUOp, CSeq, Command, EVar, EqOp, Expr, ModuleDef, Prog}
import pipedsl.common.Utilities.mkAnd

import scala.collection.mutable

class PredicateGenerator(ctx: Z3Context) {

  private val intArray = ctx.mkArraySort(ctx.getIntSort, ctx.getIntSort)
  private val predicates: mutable.Stack[Z3AST] = mutable.Stack(ctx.mkTrue())
  private var incrementer = 0

  def checkProgram(p: Prog): Unit = {
    p.moddefs.foreach(m => checkModule(m))
  }

  def checkModule(m: ModuleDef):Unit = {
    //no need to reset any state here, at the end of a module predicates will be just true
    annotateCommand(m.body)
  }

  def annotateCommand(c: Command): Unit =  {
    c match {
      case CSeq(c1, c2) => c.predicateCtx = Some(mkAnd(ctx, predicates.toSeq: _*)); annotateCommand(c1); annotateCommand(c2)
      case Syntax.CTBar(c1, c2) => c.predicateCtx = Some(mkAnd(ctx, predicates.toSeq: _*)); annotateCommand(c1); annotateCommand(c2)
      case Syntax.CIf(cond, cons, alt) =>
        c.predicateCtx = Some(mkAnd(ctx, predicates.toSeq: _*))
        abstractInterpExpr(cond) match {
          case Some(value) => predicates.push(value);
          case None => predicates.push(ctx.mkEq(ctx.mkBoolConst("__TOPCONSTANT__" + incrementer), ctx.mkTrue()))
        }
        incrementer += 1
        annotateCommand(cons)
        val trueBranch = predicates.pop()
        predicates.push(ctx.mkNot(trueBranch.asInstanceOf[Z3BoolExpr]))
        annotateCommand(alt)
        predicates.pop()
      case Syntax.CSplit(cases, default) =>
        c.predicateCtx = Some(mkAnd(ctx, predicates.toSeq: _*))
        var runningPredicates: Z3AST = null
        for (caseObj <- cases) {
          //get abstract interp of condition
          var currentCond: Z3AST = null
          abstractInterpExpr(caseObj.cond) match {
            case Some(value) => currentCond = value
            case None => currentCond = ctx.mkEq(ctx.mkBoolConst("__TOPCONSTANT__" + incrementer), ctx.mkTrue())
          }
          //Get the not of the current condition
          val notCurrentCond = ctx.mkNot(currentCond.asInstanceOf[Z3BoolExpr])
          if (runningPredicates == null) {
            predicates.push(currentCond)
            runningPredicates = notCurrentCond
          } else {
            val runningNot = runningPredicates
            //need to add the current condition and the running Not of the previous cases to the predicates
            predicates.push(mkAnd(ctx, runningNot, currentCond))
            //add to the current running not
            runningPredicates = mkAnd(ctx, runningNot, notCurrentCond)
          }
          annotateCommand(caseObj.body)
          //remove the predicate used for this case statement to reset for the next case
          predicates.pop()
        }
        //For default, all the case statements must be false, so add this to the predicates
        predicates.push(runningPredicates)
        annotateCommand(default)
        predicates.pop()
      case _ => c.predicateCtx = Some(mkAnd(ctx, predicates.toSeq: _*))
    }
  }

  def abstractInterpExpr(e: Expr): Option[Z3Expr] = e match {
    case evar: EVar => Some(declareConstant(evar))
    case Syntax.EInt(v, base, bits) => Some(ctx.mkInt(v))
    case Syntax.EBool(v) => if (v) Some(ctx.mkTrue()) else Some(ctx.mkFalse())
    case Syntax.EUop(op, ex) =>
      val absex = abstractInterpExpr(ex)
      (op, absex) match {
        case (BoolUOp(o), Some(v)) if o == "!" => Some(ctx.mkNot(v.asInstanceOf[Z3BoolExpr]))
        case _ => None
      }
    case Syntax.EBinop(op, e1, e2) =>
      val abse1 = abstractInterpExpr(e1)
      val abse2 = abstractInterpExpr(e2)
      (op, abse1, abse2) match {
        case (EqOp(o), Some(v1), Some(v2)) if o == "==" => Some(ctx.mkEq(v1, v2))
        case (EqOp(o), Some(v1), Some(v2)) if o == "!=" => Some(ctx.mkNot(ctx.mkEq(v1, v2)))
        case (BoolOp(o, _), Some(v1), Some(v2)) if o == "&&" =>
          Some(ctx.mkAnd(v1.asInstanceOf[Z3BoolExpr], v2.asInstanceOf[Z3BoolExpr]))
        case (BoolOp(o, _), Some(v1), Some(v2)) if o == "||" =>
          Some(ctx.mkOr(v1.asInstanceOf[Z3BoolExpr], v2.asInstanceOf[Z3BoolExpr]))
        case _ => None
      }
    case Syntax.ETernary(cond, tval, fval) =>
      val abscond = abstractInterpExpr(cond)
      val abstval = abstractInterpExpr(tval)
      val absfval = abstractInterpExpr(fval)
      (abscond, abstval, absfval) match {
        case (Some(vcond), Some(vtval), Some(vfval)) =>
          Some(ctx.mkITE(vcond.asInstanceOf[Z3BoolExpr], vtval, vfval))
        case _ =>
          None
      }
    case _ => None
  }

  def declareConstant(evar: EVar): Z3Expr =
    evar.typ match {
      case Some(value) => value match {
        case _: Syntax.TSizedInt => ctx.mkIntConst(evar.id.v);
        case _: Syntax.TBool => ctx.mkBoolConst(evar.id.v)
        case _: Syntax.TMemType => ctx.mkConst(evar.id.v, intArray)
        case _ => throw new RuntimeException("Unexpected type")
      }
      case None => throw new RuntimeException("Missing type")
    }
}
