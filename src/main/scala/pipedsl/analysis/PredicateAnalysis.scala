package pipedsl.analysis

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context, Expr => Z3Expr}
import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.relation.Tree
import pipedsl.common.Syntax
import pipedsl.common.Syntax.{BoolOp, BoolUOp, CIf, CSeq, CSplit, CaseObj, Command, EVar, EqOp, Expr, ModuleDef, Prog, ProgramNode}
import pipedsl.common.Utilities.{mkAnd, mkOr}

class PredicateAnalysis(program: Tree[ProgramNode, Prog], typeAnalysis: TypeAnalysis, ctx: Z3Context) extends Attribution{

  private val intArray = ctx.mkArraySort(ctx.getIntSort, ctx.getIntSort)
  private var incrementer = 0
  
  val increment: Unit => Int = {
    _ =>
      incrementer += 1
      incrementer
  }

  private def addPredicate(cond: Expr): Z3Expr = abstractInterp(cond) match {
    case Some(value) => value
    case None => ctx.mkEq(ctx.mkBoolConst("__TOPCONSTANT__" + increment()), ctx.mkTrue())
  }
  
  // Gives the condition for a branch
  val branchCond: ProgramNode => Z3BoolExpr = attr {
    case program.prev.pair(p: Command, program.parent.pair(prev: Expr, c@CIf(cond, cons, alt))) => 
      addPredicate(cond).asInstanceOf[Z3BoolExpr]
    case program.prev.pair(p: Command, program.parent.pair(prev: Command, c@CIf(cond, cons, alt)))  => 
      ctx.mkNot(branchCond(prev))
    case program.prev.pair(c@CaseObj(_, _), prev@CaseObj(_, _)) => mkAnd(ctx, ctx.mkNot(runningConds(prev)), caseCond(c))
    case program.prev.pair(default: Command, prev@CaseObj(_, _)) => ctx.mkNot(runningConds(prev))
    case program.parent.pair(c@CaseObj(_, _), p@CSplit(_, _)) => caseCond(c)
  }
  
  val caseCond: CaseObj => Z3BoolExpr = attr {
    case c@CaseObj(cond, body) => addPredicate(cond).asInstanceOf[Z3BoolExpr]
  }
  
  val runningConds: CaseObj => Z3BoolExpr = attr {
    case program.prev.pair(c@CaseObj(_,_), cprev@CaseObj(_, _)) => mkOr(ctx, runningConds(cprev), caseCond(c))
    case c: CaseObj => caseCond(c)
  }
  
  //TODO: Add error in case of unexpected case, this is only called in lock constraint checker though
  val predicate: ProgramNode => Z3BoolExpr = {
    attr {
      case program.parent.pair(c: Command, cif@CIf(_, _, _)) => mkAnd(ctx, predicate(cif), branchCond(c))
      case program.parent.pair(p, c: CSplit) => mkAnd(ctx, predicate(c), branchCond(p))
      case program.parent(_:ModuleDef) => ctx.mkTrue()
      case program.parent(p) => predicate(p)
    }
  }
  
  val abstractInterp: Expr => Option[Z3Expr] = 
    attr {
      case evar: EVar => Some(declareConstant(evar))
      case Syntax.EInt(v, base, bits) => Some(ctx.mkInt(v))
      case Syntax.EBool(v) => if (v) Some(ctx.mkTrue()) else Some(ctx.mkFalse())
      case Syntax.EUop(op, ex) =>
        val absex = abstractInterp(ex)
        (op, absex) match {
          case (BoolUOp(o), Some(v)) if o == "!" => Some(ctx.mkNot(v.asInstanceOf[Z3BoolExpr]))
          case _ => None
        }
      case Syntax.EBinop(op, e1, e2) =>
        val abse1 = abstractInterp(e1)
        val abse2 = abstractInterp(e2)
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
        val abscond = abstractInterp(cond)
        val abstval = abstractInterp(tval)
        val absfval = abstractInterp(fval)
        (abscond, abstval, absfval) match {
          case (Some(vcond), Some(vtval), Some(vfval)) =>
            Some(ctx.mkITE(vcond.asInstanceOf[Z3BoolExpr], vtval, vfval))
          case _ =>
            None
        }
      case _ => None
  }

  def declareConstant(evar: EVar): Z3Expr =
    typeAnalysis.typeCheck(evar) match {
        case _: Syntax.TSizedInt => ctx.mkIntConst(evar.id.v);
        case _: Syntax.TBool => ctx.mkBoolConst(evar.id.v)
        case _: Syntax.TMemType => ctx.mkConst(evar.id.v, intArray)
        case _ => throw new RuntimeException("Unexpected type")
    }
}
object PredicateAnalysis extends Attribution {
  val instance: Z3Context => Prog => PredicateAnalysis =
    paramAttr {
      ctx => {
        case p => new PredicateAnalysis(new Tree[ProgramNode, Prog](p), TypeAnalysis.get(p), ctx)
      }
    }
  def get(program: Prog, ctx: Z3Context): PredicateAnalysis = instance(ctx)(program)
}

