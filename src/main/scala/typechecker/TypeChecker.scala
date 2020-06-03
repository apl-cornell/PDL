package pipedsl.typechecker

import pipedsl.common.Errors._
import pipedsl.common.Syntax._
import Environments.TypeEnvironment
import Subtypes._
import pipedsl.common.Syntax


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


  def checkFuncDef(f: FuncDef, tenv: TypeEnvironment): TypeEnvironment = {
    val typList = f.args.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ }) //TODO disallow memories as params
    val fenv = f.args.foldLeft[TypeEnvironment](tenv)((env, p) => { env.add(p.name, p.typ)})
    val ftyp = TFun(typList, f.ret)
    val e1 = checkCommand(f.body, fenv)
    val rt = checkFuncWellFormed(f.body, e1)
    if (!rt.isDefined) {
      throw MalformedFunction(f.pos, "Missing return statement")
    } else if (!isEquiv(ftyp, rt.get)) {
      throw UnexpectedType(f.pos, s"${f.name} return type", ftyp.toString(), rt.get)
    }
    tenv.add(f.name, ftyp)
  }

  def checkFuncWellFormed(c: Command, tenv: TypeEnvironment): Option[Type] = c match {
    case CSeq(c1, c2) => {
      val r1 = checkFuncWellFormed(c1, tenv)
      if (r1.isDefined) { throw MalformedFunction(c2.pos, "Unexpected command following return") }
      checkFuncWellFormed(c2, tenv)
    }
    case CTBar(c1, c2) => {
      throw MalformedFunction(c.pos, "Time steps are not allowed in combinational functions")
    }
    case CIf(cond, cons, alt) => {
      val rt = checkFuncWellFormed(cons, tenv)
      val rf = checkFuncWellFormed(alt, tenv)
      (rt, rf) match {
        case (Some(t1), Some(t2)) if isEquiv(t1, t2) => rt
        case (Some(t1), Some(t2)) => throw MalformedFunction(c.pos, s"Mismatched return types ${t1.toString()}, and ${t2.toString()}")
        case (None, None) => None
        case _ => throw MalformedFunction(c.pos, "Missing return in branch of if")
      }
    }
    case CReturn(exp) => exp.typ
    case _ => None
  }

  def checkModuleDef(m: ModuleDef, tenv: TypeEnvironment) = {
    val inputTyps = m.inputs.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ }) //TODO disallow memories
    val modTyps = m.modules.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ}) //TODO require memory or module types
    val inEnv = m.inputs.foldLeft[TypeEnvironment](tenv)((env, p) => { env.add(p.name, p.typ) })
    val pipeEnv = m.modules.foldLeft[TypeEnvironment](inEnv)((env, p) => { env.add(p.name, p.typ) })
    checkCommand(m.body, pipeEnv)
    val modTyp = TModType(inputTyps, modTyps)
    tenv.add(m.name, modTyp)
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
      val (condTyp, cenv) = checkExpression(cond, tenv)
      condTyp.matchOrError(cond.pos, "if condition", "boolean") { case _: TBool => () }
      val etrue = checkCommand(cons, cenv)
      val efalse = checkCommand(alt, cenv)
      etrue.intersect(efalse)
    }
    case CAssign(lhs, rhs) => {
      val (rTyp, renv) = checkExpression(rhs, tenv)
      val (lTyp, lenv) = checkExpression(lhs, renv)
      if (isSubtype(rTyp, lTyp)) lenv
      else throw UnexpectedSubtype(rhs.pos, "assignment", lTyp, rTyp)
    }
    case CRecv(lhs, rhs) => {
      val (rTyp, renv) = checkExpression(rhs, tenv)
      val (lTyp, lenv) = checkExpression(lhs, renv)
      if (isSubtype(rTyp, lTyp)) lenv
      else throw UnexpectedSubtype(rhs.pos, "recv", lTyp, rTyp)
    }
    case CCall(id, args) => tenv//TODO need module types first
    case COutput(exp) => {
      checkExpression(exp, tenv)
      tenv
    }
    case CReturn(exp) =>
      checkExpression(exp, tenv)
      tenv
    case CExpr(exp) =>{
      checkExpression(exp, tenv)
      tenv
    }
    case CEmpty => tenv
  }

  def checkExpression(e: Expr, tenv: TypeEnvironment): (Type, TypeEnvironment) = {
    val (typ, nenv) = _checkE(e, tenv);
    if (e.typ.isDefined) {
      if (e.isLVal) {
        if (!isEquiv(e.typ.get, typ)) throw UnexpectedType(e.pos, e.toString(), typ.toString(), e.typ.get)
      } else {
        throw AlreadyBoundType(e.pos, e.toString(), e.typ.get, typ)
      }
    }
    e.typ = Some(typ)
    (typ, nenv)
  }

  def _checkE(e: Expr, tenv: TypeEnvironment): (Type, TypeEnvironment) = e match {
    case EInt(v, base, bits) => (TSizedInt(bits, true), tenv)
    case EBool(v) => (TBool(), tenv)
    case EBinop(op, e1, e2) => {
      val (t1, env1) = checkExpression(e1, tenv)
      val (t2, env2) = checkExpression(e2, env1)
      //TODO this is overly pessimistic, can concat different size integers
      if (!isEquiv(t1, t2)) {
        throw UnexpectedType(e2.pos, e2.toString(), t1.toString(), t2)
      }
      op match {
        case EqOp(_) => (TBool(), env2)
        case CmpOp(_) => (TBool(), env2)
        case BoolOp(_, _) => t1.matchOrError(e1.pos, "boolean op", "boolean") { case _: TBool => (TBool(), env2) }
        case NumOp(_, _) => t1.matchOrError(e1.pos, "number op", "number") { case t: TSizedInt => (t, env2) }
        case BitOp(n, _) => n match {
          case "++" => (t1, t2) match {
            case (TSizedInt(l1, u1), TSizedInt(l2, u2)) if u1 == u2 => (TSizedInt(l1 + l2, u1), env2)
            case (_, _) => throw UnexpectedType(e.pos, "concat", "sized number", t1)
          }
          case _ => t1.matchOrError(e1.pos, s"$n op", "sized number") { case t: TSizedInt => (t, env2) }
        }
      }
    }
    case ERecAccess(rec, fieldName) => {
      val (rt, renv) = checkExpression(rec, tenv)
      rt match {
        case TRecType(n, fs) => fs.get(fieldName) match {
          case Some(t) => (t, renv)
          case _ => throw MissingType(fieldName.pos, s"Field $n. ${fieldName.v}")
        }
        case _ => throw UnexpectedType(e.pos, "record access", "record type", rt)
      }
    }
    case ERecLiteral(fields) => {
      val ftyps = fields map { case (n, e) => (n, checkExpression(e, tenv)._1) }
      (TRecType(Id("anon"), ftyps) , tenv)//TODO these are wrong, maybe just remove these
    }
    case EMemAccess(mem, index) => {
      val memt = tenv(mem)
      val (idxt, env1) = checkExpression(index, tenv)
      (memt, idxt) match {
        case (TMemType(e, s), TSizedInt(l, true)) if l == s => (e, env1)
        case _ => throw UnexpectedType(e.pos, "memory access", "mismatched types", memt)
      }
    }
    case EBitExtract(num, start, end) => {
      val (ntyp, nenv) = checkExpression(num, tenv)
      val bitsLeft = math.abs(end - start)
      ntyp.matchOrError(e.pos, "bit extract", "sized number") {
        case TSizedInt(l, u) if l >= bitsLeft => (TSizedInt(bitsLeft, u), nenv)
        case _ => throw UnexpectedType(e.pos, "bit extract", "sized number larger than extract range", ntyp)
      }
    }
    case ETernary(cond, tval, fval) => {
      val (ctyp, cenv) = checkExpression(cond, tenv)
      ctyp.matchOrError(cond.pos, "ternary condition", "boolean") { case _: TBool => () }
      val (ttyp, trenv) = checkExpression(tval, cenv)
      val (ftyp, fenv) = checkExpression(fval, cenv)
      if (isEquiv(ttyp, ftyp)) (ttyp, trenv.intersect(fenv))
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
                  val (atyp, aenv) = checkExpression(a, tenv)
                  if (!isSubtype(atyp, expectedT)) {
                    throw UnexpectedSubtype(e.pos, a.toString(), expectedT, atyp)
                  }
          })
          (tret, tenv) //TODO check return env
        }
        case _ => throw UnexpectedType(func.pos, "function call", "function type", ftyp)
      }
    }
    case EVar(id) => e.typ match {
      case Some(t) if !tenv.get(id).isDefined => (t, tenv.add(id, t))
      case Some(t) => if (isEquiv(t,tenv(id))) {
        (t, tenv)
      } else {
        throw UnexpectedType(id.pos, "variable", "variable type set to new conflicting type", t)
      }
      case None => (tenv(id), tenv)
    }
  }
}
