package pipedsl.typechecker

import com.microsoft.z3.{AST => Z3AST, Expr => Z3Expr, BoolExpr => Z3BoolExpr, Context => Z3Context}
import pipedsl.common.Syntax
import pipedsl.common.Syntax.{BoolOp, BoolUOp, CSeq, Command, EVar, EqOp, Expr}

class PredicateGenerator(ctx: Z3Context) {

  private val intArray = ctx.mkArraySort(ctx.getIntSort, ctx.getIntSort)
  
  def generatePostcondition(c: Command,  preCondition: Set[Z3AST]): Set[Z3AST] =
    c match {
      case CSeq(c1, c2) =>
        val p1 = generatePostcondition(c1, preCondition)
        generatePostcondition(c2, p1)
      case Syntax.CTBar(c1, c2) =>
        val p1 = generatePostcondition(c1, preCondition)
        generatePostcondition(c2, p1)
      case Syntax.CIf(cond, cons, alt) => 
        val abscond = abstractInterpExpr(cond)
        abscond match {
          case Some(value: Z3BoolExpr) =>
            generatePostcondition(cons, preCondition + value)
              .intersect(generatePostcondition(alt, preCondition + ctx.mkNot(value)))
          case None =>
            generatePostcondition(cons, preCondition)
              .intersect(generatePostcondition(alt, preCondition))
        }
      case Syntax.CAssign(lhs, rhs) => (lhs, abstractInterpExpr(rhs)) match {
        case (evar: EVar, Some(value)) =>
          val declare = ctx.mkEq(declareConstant(evar), value)
          preCondition + declare
        case (evar: EVar, None) => preCondition
        case _ => preCondition
      }
      case Syntax.CRecv(lhs, rhs) => (lhs, abstractInterpExpr(rhs)) match {
        case (evar: EVar, Some(value)) =>
          val declare = ctx.mkEq(declareConstant(evar), value)
          preCondition + declare
        case (evar: EVar, None) => preCondition
        case _ => preCondition
      }
      case _ => preCondition
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
