/* Constraints.scala */
package pipedsl.common

import pipedsl.common.Syntax.{EIndAdd, EIndConst, EIndSub, EIndex, Id, TBitWidth, TNamedType}
import com.microsoft.z3.{Status, AST => Z3AST, ArithExpr => Z3ArithExpr, BoolExpr => Z3BoolExpr, Context => Z3Context, Expr => Z3Expr, IntExpr => Z3IntExpr, Solver => Z3Solver}
import pipedsl.common.Errors.UnsatisfiableConstraint
import pipedsl.common.Utilities.{degenerify, generic_type_prefix, not_gen_pref}

import scala.collection.mutable
import scala.language.implicitConversions


object Constraints
 {
  sealed trait Constraint
  sealed trait IntRel extends Constraint

  sealed trait IntExpr

  sealed trait IntValue extends IntExpr

  case class IntConst(v :Int) extends IntValue
  {
   override def toString: String = v.toString
  }
  case class IntVar(id :Id) extends IntValue
  {
   override def toString: String = id.v
  }
  case class IntAdd(a :IntExpr, b :IntExpr) extends IntExpr
  case class IntSub(a :IntExpr, b :IntExpr) extends IntExpr
  case class IntMax(a :IntExpr, b :IntExpr) extends IntExpr

  object IntAdd
   {
    def apply(b1 :IntExpr, b2 :IntExpr) :IntAdd =
     {
      (b1, b2) match {
       case _ if (b1.toString < b2.toString) => new IntAdd(b2, b1)
       case _ => new IntAdd(b1, b2)
      }
     }
   }


  case class RelLt(a :IntExpr, b :IntExpr) extends Constraint
  case class ReGe(a :IntExpr, b :IntExpr) extends  Constraint
  case class ReEq(a :IntExpr, b :IntExpr) extends Constraint

  def degenerify_expr(i :IntExpr) :IntExpr = i match
  {
   case _ :IntConst => i
   case IntVar(id) => IntVar(Id(not_gen_pref + id.v))
   case IntAdd(a, b) => IntAdd(degenerify_expr(a), degenerify_expr(b))
   case IntSub(a, b) => IntSub(degenerify_expr(a), degenerify_expr(b))
   case IntMax(a, b) => IntMax(degenerify_expr(a), degenerify_expr(b))
  }

  def degenerify_constr(c :Constraint) :Constraint = c match
  {
   case RelLt(a, b) => RelLt(degenerify_expr(a), degenerify_expr(b))
   case ReGe(a, b) => ReGe(degenerify_expr(a), degenerify_expr(b))
   case ReEq(a, b) => ReEq(degenerify_expr(a), degenerify_expr(b))
  }


  def to_z3(ctxt : Z3Context, cons :Constraint) :Z3BoolExpr =
  {
   cons match
   {
    case RelLt(a, b) => ctxt.mkLt(to_z3(ctxt, a), to_z3(ctxt, b))
    case ReEq(a, b) => ctxt.mkEq(to_z3(ctxt, a), to_z3(ctxt, b))
    case ReGe(a, b) => ctxt.mkGe(to_z3(ctxt,a ), to_z3(ctxt, b))
   }
  }

  def to_z3(ctxt :Z3Context, expr :IntExpr) :Z3ArithExpr = expr match
  {
   case IntConst(v) => ctxt.mkInt(v)
   case IntVar(id) => ctxt.mkIntConst(id.v)
   case IntAdd(a, b) => ctxt.mkAdd(to_z3(ctxt, a), to_z3(ctxt, b))
   case IntSub(a, b) => ctxt.mkSub(to_z3(ctxt, a), to_z3(ctxt, b))
   //(max a b) = (if (> a b) a b)
   case IntMax(a, b) =>
   val z3a = to_z3(ctxt, a); val z3b = to_z3(ctxt, b)
    ctxt.mkITE(ctxt.mkGt(z3a, z3b), z3a, z3b).asInstanceOf[Z3ArithExpr]

  }

  object ImplicitConstraints
  {
   implicit def toConstraint(i :Int) :IntExpr = IntConst(i)

   implicit def toConstraint(w :TBitWidth) :IntExpr = w match
   {
    case Syntax.TBitWidthVar(name) =>IntVar(name)
    case Syntax.TBitWidthLen(len) => IntConst(len)
    case Syntax.TBitWidthAdd(b1, b2) => IntAdd(toConstraint(b1), toConstraint(b2))
    case Syntax.TBitWidthSub(b1, b2) => IntSub(toConstraint(b1), toConstraint(b2))
    case Syntax.TBitWidthMax(b1, b2) => IntMax(toConstraint(b1), toConstraint(b2))
   }

   implicit def toConstraint(i : EIndex) :IntExpr = i match
   {
    case EIndConst(v) => IntConst(v)
    case Syntax.EIndAdd(l, r) => IntAdd(toConstraint(l), toConstraint(r))
    case Syntax.EIndSub(l, r) => IntSub(toConstraint(l), toConstraint(r))
    case Syntax.EIndVar(id) => IntVar(Id(id.v))
   }
  }

  case class NotConst() extends RuntimeException

  def eval_const(expr: IntExpr) :Int = expr match
  {
   case value: IntValue => value match
   {
    case IntConst(v) => v
    case IntVar(_) => throw NotConst()
   }
   case IntAdd(a, b) => eval_const(a) + eval_const(b)
   case IntSub(a, b) => eval_const(a) - eval_const(b)
   case IntMax(a, b) => math.max(eval_const(a), eval_const(b))
  }

  def check_const(c :Constraint) :Unit = c match
  {
   case RelLt(a, b) => if (! (eval_const(a) < eval_const(b)))
    throw UnsatisfiableConstraint(c)
   case ReGe(a, b) => if (! (eval_const(a) >= eval_const(b)))
    throw UnsatisfiableConstraint(c)
   case ReEq(a, b) => if (! (eval_const(a) == eval_const(b)))
    throw UnsatisfiableConstraint(c)
  }

  def reduce_constraint_list(lst :List[Constraint]) :List[Constraint] =
  {
   val set = mutable.HashSet[Constraint]()
   lst.foreach(c =>
   {
    if(!set.contains(c))
     {
      try
       {
        check_const(c)
       } catch
       {
        case _: NotConst => set.add(c)
       }
     }
   })
   set.toList
  }

 }
