package pipedsl.typechecker

import pipedsl.common.Errors._
import pipedsl.common.Syntax._
import Environments.TypeEnvironment
import Subtypes._


//TODO kinds of typechecking we need to do:

//Normal stuff
//  Assignment
//  Function calls and returns
//  Module Instantiations
//
//Pipeline Stuff
//  Call statment exactly once in each path w/ typed args
//  Dynamic Lock Checking
//  More complex lock types (indexed / dependent)
//  Speculation Restrictions
//
//Security Types
//  Normal IFC
//  How to Check "call"s
//  Speculation Labels
object TypeChecker {

  def typeCheck(p: Prog): Unit = {
    val Prog(fdefs, mdefs, cir) = p
  }


  def checkFuncDef(f: FuncDef) = {

  }

  def checkModuleDef(m: ModuleDef) = {

  }

  def checkCommand(c: Command, tenv: TypeEnvironment): TypeEnvironment = c match {
    case CSeq(c1, c2) => {
      val e2 = checkCommand(c1, tenv)
      checkCommand(c2, e2)
    }
    case CTBar(c1, c2) => {
      val e2 = checkCommand(c1, tenv)
      checkCommand(c2, e2)
    }
    case CIf(cond, cons, alt) => {
      val condTyp = checkExpression(cond, tenv)
      condTyp.matchOrError(cond.pos, "if condition", "boolean") { case _: TBool => () }
      val etrue = checkCommand(cons, tenv)
      val efalse = checkCommand(alt, tenv)
      etrue.intersect(efalse)
    }
    case CAssign(lhs, rhs) => {
      val lTyp = checkExpression(lhs, tenv)
      val rTyp = checkExpression(rhs, tenv)
      if (isSubtype(rTyp, lTyp)) tenv
      else throw UnexpectedSubtype(rhs.pos, "assignment", lTyp, rTyp)
    }
    case CRecv(lhs, rhs) => {
      val lTyp = checkExpression(lhs, tenv)
      val rTyp = checkExpression(rhs, tenv)
      if (isSubtype(rTyp, lTyp)) tenv
      else throw UnexpectedSubtype(rhs.pos, "recv", lTyp, rTyp)
    }
    case CCall(id, args) => tenv//TODO need module types first
    case COutput(exp) => {
      checkExpression(exp, tenv)
      tenv
    }
    case CReturn(exp) => tenv//TODO need context for being in a function definition
    case CExpr(exp) =>{
      checkExpression(exp, tenv)
      tenv
    }
    case CEmpty => tenv
  }

  def checkExpression(e: Expr, tenv: TypeEnvironment): Type = {
    val typ = _checkE(e, tenv);
    if (e.typ.isDefined) {
      if (e.isLVal) {
        if (!isEquiv(e.typ.get, typ)) throw UnexpectedType(e.pos, e.toString(), typ.toString(), e.typ.get)
      } else {
        throw AlreadyBoundType(e.pos, e.toString(), e.typ.get, typ)
      }
    }
    e.typ = Some(typ)
    typ
  }

  def _checkE(e: Expr, tenv: TypeEnvironment): Type = e match {
    case EInt(v, base, bits) => TSizedInt(bits, true)
    case EBool(v) => TBool()
    case EBinop(op, e1, e2) => {
      val t1 = checkExpression(e1, tenv)
      val t2 = checkExpression(e2, tenv)
      //TODO this is overly pessimistic, can concat different size integers
      if (!isEquiv(t1, t2)) {
        throw UnexpectedType(e2.pos, e2.toString(), t1.toString(), t2)
      }
      op match {
        case EqOp(_) => TBool()
        case CmpOp(_) => TBool()
        case BoolOp(_, _) => t1.matchOrError(t1.pos, "boolean op", "boolean") { case _: TBool => TBool() }
        case NumOp(_, _) => t1.matchOrError(t1.pos, "number op", "number") { case t: TSizedInt => t }
        case BitOp(n, _) => n match {
          case "++" => (t1, t2) match {
            case (TSizedInt(l1, u1), TSizedInt(l2, u2)) if u1 == u2 => TSizedInt(l1 + l2, u1)
            case (_, _) => throw UnexpectedType(e.pos, "concat", "sized number", t1)
          }
          case _ => t1.matchOrError(t1.pos, "bit op", "sized number") { case t: TSizedInt => t }
        }
      }
    }
    case ERecAccess(rec, fieldName) => {
      val rt = checkExpression(rec, tenv)
      rt match {
        case TRecType(n, fs) => fs.get(fieldName) match {
          case Some(t) => t
          case _ => throw MissingType(fieldName.pos, s"Field $n. ${fieldName.v}")
        }
        case _ => throw UnexpectedType(e.pos, "record access", "record type", rt)
      }
    }
    case ERecLiteral(fields) => {
      val ftyps = fields map { case (n, e) => (n, checkExpression(e, tenv))}
      TRecType(Id("anon"), ftyps) //TODO maybe just remove these
    }
    case EMemAccess(mem, index) => {
      val memt = tenv(mem)
      val idxt = checkExpression(index, tenv)
      (memt, idxt) match {
        case (TMemType(e, s), TSizedInt(l, true)) if l == s => e
        case _ => throw UnexpectedType(e.pos, "memory access", "mismatched types", memt)
      }
    }
    case EBitExtract(num, start, end) => {
      val ntyp = checkExpression(num, tenv)
      val bitsLeft = math.abs(end - start)
      ntyp.matchOrError(e.pos, "bit extract", "sized number") {
        case TSizedInt(l, u) if l >= bitsLeft => TSizedInt(bitsLeft, u)
        case _ => throw UnexpectedType(e.pos, "bit extract", "sized number larger than extract range", ntyp)
      }
    }
    case ETernary(cond, tval, fval) => {
      val ctyp = checkExpression(cond, tenv)
      ctyp.matchOrError(cond.pos, "ternary condition", "boolean") { case _: TBool => () }
      val ttyp = checkExpression(tval, tenv)
      val ftyp = checkExpression(fval, tenv)
      if (isEquiv(ttyp, ftyp)) ttyp
      else throw UnexpectedType(e.pos, "ternary", "both conditions must match type", ftyp)
    }
    case EApp(func, args) => {
      val ftyp = tenv(func)
      ftyp match {
        case TFun(targs, tret) => {
          if (targs.length != args.length) {
            throw ArgLengthMismatch(e.pos, targs.length, args.length)
          }
          targs.zip(args).foreach(t => t match {
            case (expectedT, a) =>
                  val atyp = checkExpression(a, tenv)
                  if (!isSubtype(atyp, expectedT)) {
                    throw UnexpectedSubtype(e.pos, a.toString(), expectedT, atyp)
                  }
          })
          tret
        }
        case _ => throw UnexpectedType(func.pos, "function call", "function type", ftyp)
      }
    }
    case EVar(id) => id.typ match {
      case Some(t) if !tenv.get(id).isDefined => t
      case Some(t) => if (isEquiv(t,tenv(id))) {
        t
      } else {
        throw UnexpectedType(id.pos, "variable", "variable type set to new conflicting type", t)
      }
      case None => tenv(id)
    }
  }
}
