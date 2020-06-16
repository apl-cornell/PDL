package pipedsl.typechecker

import pipedsl.common.Errors._
import pipedsl.common.Syntax._
import Subtypes._
import TypeChecker.TypeChecks
import Environments.Environment
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
object BaseTypeChecker extends TypeChecks[Type] {

  override def emptyEnv(): Environment[Type] = Environments.EmptyTypeEnv
  
  def checkFunc(f: FuncDef, tenv: Environment[Type]): Environment[Type] = {
    val typList = f.args.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ }) //TODO disallow memories as params
    val fenv = f.args.foldLeft[Environment[Type]](tenv)((env, p) => { env.add(p.name, p.typ)})
    val ftyp = TFun(typList, f.ret)
    val e1 = checkCommand(f.body, fenv)
    val rt = checkFuncWellFormed(f.body, e1)
    if (rt.isEmpty) {
      throw MalformedFunction(f.pos, "Missing return statement")
    } else if (!areEqual(ftyp.ret, rt.get)) {
      throw UnexpectedType(f.pos, s"${f.name} return type", ftyp.toString(), rt.get)
    }
    tenv.add(f.name, ftyp)
  }

  private def checkFuncWellFormed(c: Command, tenv: Environment[Type]): Option[Type] = c match {
    case CSeq(c1, c2) => {
      val r1 = checkFuncWellFormed(c1, tenv)
      if (r1.isDefined) { throw MalformedFunction(c2.pos, "Unexpected command following return") }
      checkFuncWellFormed(c2, tenv)
    }
    case _: CTBar | _: CSpeculate | _: CResolve | _: CCheck | _:COutput => {
      throw MalformedFunction(c.pos, "Command not supported in combinational functions")
    }
    case CIf(_, cons, alt) => {
      val rt = checkFuncWellFormed(cons, tenv)
      val rf = checkFuncWellFormed(alt, tenv)
      (rt, rf) match {
        case (Some(t1), Some(t2)) if areEqual(t1, t2) => rt
        case (Some(t1), Some(t2)) => throw MalformedFunction(c.pos, s"Mismatched return types ${t1.toString()}, and ${t2.toString()}")
        case (None, None) => None
        case _ => throw MalformedFunction(c.pos, "Missing return in branch of if")
      }
    }
    case CReturn(exp) => exp.typ
    case _ => None
  }

  def checkModule(m: ModuleDef, tenv: Environment[Type]): Environment[Type] = {
    val inputTyps = m.inputs.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ }) //TODO disallow memories
    val modTyps = m.modules.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ}) //TODO require memory or module types
    val inEnv = m.inputs.foldLeft[Environment[Type]](tenv)((env, p) => { env.add(p.name, p.typ) })
    val pipeEnv = m.modules.foldLeft[Environment[Type]](inEnv)((env, p) => { env.add(p.name, p.typ) })
    val specVars = getSpeculativeVariables(m.body)
    val modTyp = TModType(inputTyps, modTyps, specVars)
    val finalEnv = pipeEnv.add(m.name, modTyp)
    checkModuleBodyWellFormed(m.body, hascall = false, Set())
    checkCommand(m.body, finalEnv)
    finalEnv
  }
  /**
   * - Checks that all paths have at most 1 call statement
   * - Checks that every variable is *assigned* exactly once in each path.
   *   Receives may be conditional (and therefore occur only in some paths)
   * @param c
   * @param hascall
   * @return
   */
  def checkModuleBodyWellFormed(c: Command, hascall: Boolean, assignees: Set[Id]): (Boolean, Set[Id]) = c match {
    case CSeq(c1, c2) => {
      val (hc2, as2) = checkModuleBodyWellFormed(c1, hascall, assignees)
      checkModuleBodyWellFormed(c2, hc2, as2)
    }
    case CTBar(c1, c2) => {
      val (hc2, as2) = checkModuleBodyWellFormed(c1, hascall, assignees)
      checkModuleBodyWellFormed(c2, hc2, as2)
    }
    case CIf(_, cons, alt) => {
      val (hct, ast) = checkModuleBodyWellFormed(cons, hascall, assignees)
      val (hcf, asf) = checkModuleBodyWellFormed(alt, hascall, assignees)
      if (!asf.equals(ast)) {
        throw MismatchedAssigns(c.pos, ast.diff(asf))
      }
      (hct || hcf, asf)
    }
    case CCall(_, _) => if (hascall) {
      //throw UnexpectedCall(c.pos)
      //TODO maybe we don't bother with this. trust programmer to not call multiple times
      (hascall, assignees)
      } else { (true, assignees) }
    case CAssign(lhs@EVar(id), _) => {
        if (assignees(id)) { throw UnexpectedAssignment(lhs.pos, id) } else {
          (hascall, assignees + id)
      }
    }
    case CSpeculate(_, _, body) => checkModuleBodyWellFormed(body, hascall, assignees)
    case CReturn(_) => throw UnexpectedReturn(c.pos)
    case _ => (hascall, assignees)
  }

  def checkCircuit(c: Circuit, tenv: Environment[Type]): Environment[Type] = c match {
    case CirSeq(c1, c2) => {
      val e1 = checkCircuit(c1, tenv)
      checkCircuit(c2, e1)
    }
    case CirConnect(name, c) => {
      val (t, env2) = checkCirExpr(c, tenv)
      env2.add(name, t)
    }
  }

  private def checkCirExpr(c: CirExpr, tenv: Environment[Type]): (Type, Environment[Type]) = c match {
    case CirMem(elemTyp, addrSize) => {
      val mtyp = TMemType(elemTyp, addrSize)
      c.typ = Some(mtyp)
      (mtyp, tenv)
    }
    case CirNew(mod, inits, mods) => {
      val mtyp = tenv(mod)
      mtyp match {
        case TModType(ityps, refs, _) => {
          if(ityps.length != inits.length) {
            throw ArgLengthMismatch(c.pos, ityps.length, inits.length)
          }
          ityps.zip(inits).foreach( t => t match {
            case (expectedT, arg) => {
              val (atyp, aenv) = checkExpression(arg, tenv)
              if (!isSubtype(atyp, expectedT)) {
                throw UnexpectedSubtype(arg.pos, arg.toString, expectedT, atyp)
              }
            }
          })
          refs.zip(mods).foreach( t => t match {
            case (reftyp, mname) => {
              if (!(isSubtype(tenv(mname), reftyp))) {
                throw UnexpectedSubtype(mname.pos, mname.toString, reftyp, tenv(mname))
              }
            }
          })
          (mtyp, tenv)
        }
        case x => throw UnexpectedType(c.pos, c.toString, "Module Type", x)
      }
    }
  }

  def checkCommand(c: Command, tenv: Environment[Type]): Environment[Type] = c match {
    case CSeq(c1, c2) => {
      val e2 = checkCommand(c1, tenv)
      checkCommand(c2, e2)
    }
    case CTBar(c1, c2) => {
      val e2 = checkCommand(c1, tenv)
      checkCommand(c2, e2)
    }
    case CDecl(id, typ,_) => typ match {
      case TMemType(_,_) | TModType(_,_,_) => throw UnexpectedType(id.pos, id.toString, "Non memory/module type", typ)
      case _ => tenv.add(id, typ)
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
    case CLockOp(mem, _) => {
      tenv(mem).matchOrError(mem.pos, "lock operation", "Memory or Module Type")
      { case _: TModType => tenv
        case _: TMemType => tenv
      }
    }
    case CSpeculate(nvar, predval, body) => {
      val (predtyp, env1) = checkExpression(predval, tenv)
      val ltyp = nvar.typ.get //parser ensures type is defined
      val nenv = env1.add(nvar.id, ltyp)
      if (isSubtype(predtyp, ltyp)) checkCommand(body, nenv)
      else throw UnexpectedSubtype(predval.pos, "speculate", ltyp, predtyp)
    }
    case CCheck(predVar, realVal) => {
      val predtyp = tenv(predVar)
      val (rtyp, nenv) = checkExpression(realVal, tenv)
      if (isSubtype(rtyp, predtyp)) nenv
      else throw UnexpectedSubtype(realVal.pos, "check speculation", predtyp, rtyp)
    }
    case CResolve(predVar) => {
      tenv(predVar)
      tenv
    }
    case CCall(id, args) => {
      tenv(id) match {
        case TModType(ityps, _, _) => {
          if (args.length != ityps.length) {
            throw ArgLengthMismatch(c.pos, args.length, ityps.length)
          }
          ityps.zip(args).foreach {
            case (expectedT, a) =>
              val (atyp, _) = checkExpression(a, tenv)
              if (!isSubtype(atyp, expectedT)) {
                throw UnexpectedSubtype(a.pos, a.toString, expectedT, atyp)
              }
          }
          tenv
        }
        case _ => throw UnexpectedType(c.pos, id.toString, "Module type", tenv(id))
      }
    }
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

  def checkExpression(e: Expr, tenv: Environment[Type]): (Type, Environment[Type]) = {
    val (typ, nenv) = _checkE(e, tenv);
    if (e.typ.isDefined) {
      if (e.isLVal) {
        if (!areEqual(e.typ.get, typ)) throw UnexpectedType(e.pos, e.toString, typ.toString(), e.typ.get)
      } else {
        throw AlreadyBoundType(e.pos, e.toString, e.typ.get, typ)
      }
    }
    e.typ = Some(typ)
    (typ, nenv)
  }

  private def _checkE(e: Expr, tenv: Environment[Type]): (Type, Environment[Type]) = e match {
    case EInt(v, base, bits) => (TSizedInt(bits, unsigned = true), tenv)
    case EBool(v) => (TBool(), tenv)
    case EUop(op, e) => {
      val (t1, env1) = checkExpression(e, tenv)
      op match {
        case BoolUOp(op) => t1.matchOrError(e.pos, "boolean op", "boolean") { case _: TBool => (TBool(), env1) }
        case NumUOp(op) => t1.matchOrError(e.pos, "number op", "number") { case t: TSizedInt => (t, env1) }
        case BitUOp(op) => t1.matchOrError(e.pos, "bit op", "sized integer") { case t: TSizedInt => (t, env1) }
      }
    }
    case EBinop(op, e1, e2) => {
      val (t1, env1) = checkExpression(e1, tenv)
      val (t2, env2) = checkExpression(e2, env1)
      op match {
        case BitOp("++", _) => (t1, t2) match {
          //handle concatenation separately since it doesn't require equal operands
          case (TSizedInt(l1, u1), TSizedInt(l2, u2)) if u1 == u2 => (TSizedInt(l1 + l2, u1), env2)
          case (_, _) => throw UnexpectedType(e.pos, "concat", "sized number", t1)
        }
        case _ => if (!areEqual(t1, t2)) { throw UnexpectedType(e2.pos, e2.toString, t1.toString(), t2) } else {
          op match {
            case EqOp(_) => (TBool(), env2)
            case CmpOp(_) => (TBool(), env2)
            case BoolOp(_, _) => t1.matchOrError(e1.pos, "boolean op", "boolean") { case _: TBool => (TBool(), env2) }
            case NumOp(_, _) => t1.matchOrError(e1.pos, "number op", "number") { case t: TSizedInt => (t, env2) }
            case BitOp(_, _) => t1.matchOrError(e1.pos, "bit op", "sized integer") { case t: TSizedInt => (t, env2) }
          }
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
      val bitsLeft = math.abs(end - start) + 1
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
      if (areEqual(ttyp, ftyp)) (ttyp, trenv.intersect(fenv))
      else throw UnexpectedType(e.pos, "ternary", s"false condition must match ${ttyp.toString}", ftyp)
    }
    case EApp(func, args) => {
      val ftyp = tenv(func)
      ftyp match {
        case TFun(targs, tret) => {
          if (targs.length != args.length) {
            throw ArgLengthMismatch(e.pos, targs.length, args.length)
          }
          targs.zip(args).foreach {
            case (expectedT, a) =>
              val (atyp, aenv) = checkExpression(a, tenv)
              if (!isSubtype(atyp, expectedT)) {
                throw UnexpectedSubtype(e.pos, a.toString, expectedT, atyp)
              }
          }
          (tret, tenv) //TODO check return env
        }
        case _ => throw UnexpectedType(func.pos, "function call", "function type", ftyp)
      }
    }
    case EVar(id) => e.typ match {
      case Some(t) if tenv.get(id).isEmpty => (t, tenv.add(id, t))
      case Some(t) => if (areEqual(t,tenv(id))) {
        (t, tenv)
      } else {
        throw UnexpectedType(id.pos, "variable", "variable type set to new conflicting type", t)
      }
      case None => (tenv(id), tenv)
    }
    //TODO some other rules for casting
    case ECast(ctyp, exp) =>
      val (etyp, tenv2) = checkExpression(exp, tenv)
      (ctyp, etyp) match {
        case (t1, t2) if !areEqual(t1, t2) => throw IllegalCast(e.pos, t1, t2)
        case _ => ()
      }
      (ctyp, tenv2)
  }

  //Returns all variables which could cause the first pipeline stage
  //to start speculatively
  def getSpeculativeVariables(c: Command): Set[Id] = c match {
    case CSeq(c1, c2) => getSpeculativeVariables(c1) ++ getSpeculativeVariables(c2)
    case CTBar(c1, c2) => getSpeculativeVariables(c1) ++ getSpeculativeVariables(c2)
    case CIf(_, cons, alt) => getSpeculativeVariables(cons) ++ getSpeculativeVariables(alt)
    case CSpeculate(predVar, _, _) => Set(predVar.id)
    case _ => Set()
  }
}
