package pipedsl.typechecker

import pipedsl.common.Syntax
import pipedsl.common.Syntax.{BoolOp, BoolUOp, CSeq, Command, EVar, EqOp, Expr}
import z3.scala.{Z3AST, Z3Context}

class PredicateGenerator(ctx: Z3Context) {

  val int = ctx.mkIntSort()
  val bool = ctx.mkBoolSort()
  val array = ctx.mkArraySort(int, int)
  
  def generatePostcondition(c: Command,  preCondition: Set[Z3AST]): Set[Z3AST] =
    c match {
      case CSeq(c1, c2) => {
        val p1 = generatePostcondition(c1, preCondition)
        generatePostcondition(c2, p1)
      }
      case Syntax.CTBar(c1, c2) => {
        val p1 = generatePostcondition(c1, preCondition)
        generatePostcondition(c2, p1)
      }
      case Syntax.CIf(cond, cons, alt) => 
        val abscond = abstractInterpExpr(cond)
        abscond match {
          case Some(value) => generatePostcondition(cons, preCondition + value).intersect(generatePostcondition(alt, preCondition + ctx.mkNot(value)))
          case None => generatePostcondition(cons, preCondition).intersect(generatePostcondition(alt, preCondition))
        }
      case Syntax.CAssign(lhs, rhs) => (lhs, abstractInterpExpr(rhs)) match {
        case (evar: EVar, Some(value)) => {
          val declare = ctx.mkEq(declareConstant(evar), value)
          preCondition + declare
        }
        case (evar: EVar, None) => preCondition
        case _ => preCondition
      }
      case Syntax.CRecv(lhs, rhs) => (lhs, abstractInterpExpr(rhs)) match {
        case (evar: EVar, Some(value)) => {
          val declare = ctx.mkEq(declareConstant(evar), value)
          preCondition + declare
        }
        case (evar: EVar, None) => preCondition
        case _ => preCondition
      }
      case _ => preCondition
    }
  
  def abstractInterpExpr(e: Expr): Option[Z3AST] = e match {
    case evar: EVar =>  Some(declareConstant(evar))
    case Syntax.EInt(v, base, bits) => Some(ctx.mkInt(v, int))
    case Syntax.EBool(v) => if (v) Some(ctx.mkTrue()) else Some(ctx.mkFalse())
    case Syntax.EUop(op, ex) => {
      val absex = abstractInterpExpr(ex)
      (op, absex) match {
        case (BoolUOp(o), Some(v)) if o == "!" => Some(ctx.mkNot(v))
        case _ => None
      }
    }
    case Syntax.EBinop(op, e1, e2) => {
      val abse1 = abstractInterpExpr(e1)
      val abse2 = abstractInterpExpr(e2)
      (op, abse1, abse2) match {
        case (EqOp(o), Some(v1), Some(v2)) if o == "==" => Some(ctx.mkEq(v1, v2))
        case (EqOp(o), Some(v1), Some(v2)) if o == "!=" => Some(ctx.mkNot(ctx.mkEq(v1, v2)))
        case (BoolOp(o, fun), Some(v1), Some(v2)) if o == "&&" => Some(ctx.mkAnd(v1, v2))
        case (BoolOp(o, fun), Some(v1), Some(v2)) if o == "||" => Some(ctx.mkOr(v1, v2))
        case _ => None
      }
    }
    case Syntax.ETernary(cond, tval, fval) => {
      val abscond = abstractInterpExpr(cond)
      val abstval = abstractInterpExpr(tval)
      val absfval = abstractInterpExpr(fval)
      (abscond, abstval, absfval) match {
        case (Some(vcond), Some(vtval), Some(vfval)) => Some(ctx.mkITE(vcond, vtval, vfval))
        case _ => None
      }
    }
    case _ => None
  }
  def declareConstant(evar: EVar): Z3AST = 
    evar.typ match {
      case Some(value) => value match {
        case Syntax.TSizedInt(len, unsigned) => ctx.mkIntConst(evar.id.v); 
        case Syntax.TBool() => ctx.mkBoolConst(evar.id.v)
        case Syntax.TMemType(elem, addrSize, readLatency, writeLatency) => ctx.mkConst(evar.id.v, array)
        case _ => throw new RuntimeException("Unexpected type")
      }
      case None => throw new RuntimeException("Missing type")
    } 
}
