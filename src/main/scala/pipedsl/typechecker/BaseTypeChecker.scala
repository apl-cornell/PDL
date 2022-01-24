package pipedsl.typechecker

import pipedsl.common.Errors._
import pipedsl.common.Syntax._
import Subtypes._
import TypeChecker.TypeChecks
import Environments.Environment
import pipedsl.common.LockImplementation
import pipedsl.common.Syntax.Latency.{Asynchronous, Combinational, Latency, Sequential}
import pipedsl.common.Utilities.{defaultReadPorts, defaultWritePorts}


//  This checks the 'Normal stuff' with base types.
//  Assignment
//  Function calls and returns
//  Module Instantiations
object BaseTypeChecker extends TypeChecks[Id, Type] {
  
  override def emptyEnv(): Environment[Id,Type] = Environments.EmptyTypeEnv

  //Add these named types to the env
  override def checkExt(e: ExternDef, env: Environment[Id, Type]): Environment[Id, Type] = {
    val ftyps = e.methods.foldLeft(Map[Id, (TFun, Latency)]())((ms, m) => {
      val typList = m.args.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ })
      val ftyp = TFun(typList, m.ret)
      ms + (m.name -> (ftyp, m.lat))
    })
    val typ = TObject(e.name, e.typParams, ftyps)
    e.typ = Some(typ)
    env.add(e.name, typ)
  }

  /**
   * This does the base type checking and well-fomedness checking for a given function with
   * an environment (that may have other function types defined already).
   * @param f - The function definition to check.
   * @param tenv - The current type environment (which only includes already defined functions)
   * @return - tenv plus a new mapping from f's name to f's type
   */
  override def checkFunc(f: FuncDef, tenv: Environment[Id, Type]): Environment[Id, Type] = {
    val typList = f.args.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ }) //TODO disallow memories as params
    val fenv = f.args.foldLeft[Environment[Id, Type]](tenv)((env, p) => { env.add(p.name, p.typ)})
    val ftyp = TFun(typList, f.ret)
    val e1 = checkCommand(f.name, f.body, fenv)
    val rt = checkFuncWellFormed(f.body, e1)
    if (rt.isEmpty) {
      throw MalformedFunction(f.pos, "Missing return statement")
    } else if (!areEqual(ftyp.ret, rt.get)) {
      throw UnexpectedType(f.pos, s"${f.name} return type", ftyp.toString(), rt.get)
    }
    tenv.add(f.name, ftyp)
  }

  /**
   * This checks that the function doesn't include any disallowed expressions
   * or commands (such as Calls) and checks that it does not have code following
   * its return statement. This function is recursively defined and should be
   * called on the function's body (which is a single command)
   * @param c The command to check for well-formedness
   * @param tenv The current type environment (mapping from names to types)
   * @return Some(return type) if the analyzed command returns a value or None other wise
   */
  private def checkFuncWellFormed(c: Command, tenv: Environment[Id, Type]): Option[Type] = c match {
    case CSeq(c1, c2) => {
      val r1 = checkFuncWellFormed(c1, tenv)
      val r2 = checkFuncWellFormed(c2, tenv)
      (r1, r2) match {
        case (Some(_), Some(_)) => throw MalformedFunction(c.pos, "Multiple return statements in execution")
        case (Some(_), _) => r1
        case (_, Some(_)) => r2
        case (None, None) => None
      }
    }
    case _: CTBar | _: CSplit | _:COutput =>
      throw MalformedFunction(c.pos, "Command not supported in combinational functions")
    case CIf(_, cons, alt) =>
      val rt = checkFuncWellFormed(cons, tenv)
      val rf = checkFuncWellFormed(alt, tenv)
      (rt, rf) match {
        case (Some(t1), Some(t2)) if areEqual(t1, t2) => rt
        case (Some(t1), Some(t2)) => throw MalformedFunction(c.pos, s"Mismatched return types ${t1.toString()}, and ${t2.toString()}")
        case (None, None) => None
        case _ => throw MalformedFunction(c.pos, "Missing return in branch of if")
      }
    case CReturn(exp) => exp.typ
    case _ => None
  }

  override def checkModule(m: ModuleDef, tenv: Environment[Id, Type]): Environment[Id, Type] = {
    //TODO disallow memories
    val inputTyps = m.inputs.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ })
    //TODO require memory or module types
    val modTyps = m.modules.foldLeft[List[Type]](List())((l, p) => { l :+ replaceNamedType(p.typ, tenv) })
    val inEnv = m.inputs.foldLeft[Environment[Id, Type]](tenv)((env, p) => { env.add(p.name, p.typ) })
    val pipeEnv = m.modules.foldLeft[Environment[Id, Type]](inEnv)((env, p) =>
      { env.add(p.name, replaceNamedType(p.typ, env)) })
    val modTyp = TModType(inputTyps, modTyps, m.ret, Some(m.name))
    val bodyEnv = pipeEnv.add(m.name, modTyp)
    val outEnv = tenv.add(m.name, modTyp)
    checkModuleBodyWellFormed(m.body, Set())
    checkCommand(m.name, m.body, bodyEnv)
    outEnv
  }

  //Module parameters can't be given a proper type during parsing if they refer to another module
  //This uses module names in the type environment to lookup their already checked types
  private def replaceNamedType(t: Type, tenv: Environment[Id, Type]): Type = t match {
    case TNamedType(name) => tenv(name)
    case _ => t
  }

  /**
   * Checks that no variable is assigned multiple times in the command.
   * @param c The command to check.
   * @return The set of variables that get assigned in this command.
   */
  private def checkModuleBodyWellFormed(c: Command, assignees: Set[Id]): Set[Id] = c match {
    case CSeq(c1, c2) =>
      val a2 = checkModuleBodyWellFormed(c1, assignees)
      checkModuleBodyWellFormed(c2,a2)
    case CTBar(c1, c2) =>
      val a2 = checkModuleBodyWellFormed(c1, assignees)
      checkModuleBodyWellFormed(c2, a2)
    case CSplit(cs, d) =>
      val branches = cs.map(c => c.body) :+ d
      //check all branches in same context
      branches.foldLeft(assignees)((res, c) => {
        val cassgns = checkModuleBodyWellFormed(c, assignees)
        res ++ cassgns
      })
    case CIf(_, cons, alt) =>
      val ast = checkModuleBodyWellFormed(cons, assignees)
      val asf = checkModuleBodyWellFormed(alt, assignees)
      ast ++ asf
    case CRecv(lhs@EVar(id), _) =>
      if (assignees(id)) { throw UnexpectedAssignment(lhs.pos, id) } else {
        assignees + id
      }
    case CAssign(lhs@EVar(id), _) =>
      if (assignees(id)) { throw UnexpectedAssignment(lhs.pos, id) } else {
        assignees + id
    }
    //returns are only for function calls
    case CReturn(_) => throw UnexpectedReturn(c.pos)
    case _ => assignees
  }
  
  override def checkCircuit(c: Circuit, tenv: Environment[Id, Type]): Environment[Id, Type] = c match {
    case CirSeq(c1, c2) => {
      val e1 = checkCircuit(c1, tenv)
      checkCircuit(c2, e1)
    }
    case CirConnect(name, c) =>
      val (t, env2) = checkCirExpr(c, tenv)
      name.typ = Some(t)
      env2.add(name, t)
    case CirExprStmt(ce) => checkCirExpr(ce, tenv)._2
  }

  private def checkCirExpr(c: CirExpr, tenv: Environment[Id, Type]): (Type, Environment[Id, Type]) = c match {
    case CirMem(elemTyp, addrSize, numPorts) => {
      if(numPorts > 2) throw TooManyPorts(c.pos, 2)
      val mtyp = TMemType(elemTyp, addrSize, Asynchronous, Asynchronous, numPorts, numPorts)
      c.typ = Some(mtyp)
      (mtyp, tenv)
    }
    case CirLockMem(elemTyp, addrSize, limpl, _, numPorts) => {
      val mtyp = TMemType(elemTyp, addrSize, Asynchronous, Asynchronous, numPorts, numPorts)
      val ltyp = TLockedMemType(mtyp, None, limpl)
      c.typ = Some(ltyp)
      (ltyp, tenv)
    }
    case CirLock(mem, lockimpl, idsz) => {
      val mtyp: TMemType = tenv(mem).
        matchOrError(mem.pos, "lock instantiation", "memory") { case c: TMemType => c }
      mem.typ = Some(mtyp)
      val newtyp = TLockedMemType(mtyp, idsz.headOption, lockimpl)
      c.typ = Some(newtyp)
      (newtyp, tenv)
    }
    case CirRegister(elemTyp, _) => {
      val mtyp = TMemType(elemTyp, 0, Combinational, Sequential, 0, 0)
      c.typ = Some(mtyp)
      (mtyp, tenv)
    }
    case CirRegFile(elemTyp, addrSize) => {
      val mtyp = TMemType(elemTyp, addrSize, Combinational, Sequential, defaultReadPorts, defaultWritePorts)
      c.typ = Some(mtyp)
      (mtyp, tenv)
    }
    case CirLockRegFile(elemTyp, addrSize, lockimpl, params) => {
      val mtyp = TMemType(elemTyp, addrSize, Combinational, Sequential, defaultReadPorts, defaultWritePorts)
      val idsz = params.headOption
      val ltyp = TLockedMemType(mtyp, idsz, lockimpl)
      c.typ = Some(ltyp)
      (ltyp, tenv)
    }
    case CirNew(mod, mods, _) => {
      val mtyp = tenv(mod)
      mtyp match {
        case TModType(_, refs, _, _) => {
          if(refs.length != mods.length) {
            throw ArgLengthMismatch(c.pos, mods.length, refs.length)
          }
          refs.zip(mods).foreach {
            case (reftyp, mname) => {
              if (!(isSubtype(tenv(mname), reftyp))) {
                throw UnexpectedSubtype(mname.pos, mname.toString, reftyp, tenv(mname))
              }
            }
          }
          (mtyp, tenv)
        }
        case TObject(_, _, _) => (mtyp, tenv)
        case x => throw UnexpectedType(c.pos, c.toString, "Module Type", x)
      }
    }
    case CirCall(mod, inits) => {
      val mtyp = tenv(mod)
      mtyp match {
        case TModType(ityps, _, _, _) => {
          if(ityps.length != inits.length) {
            throw ArgLengthMismatch(c.pos, inits.length, ityps.length)
          }
          ityps.zip(inits).foreach {
            case (expectedT, arg) => {
              val (atyp, aenv) = checkExpression(arg, tenv, None)
              if (!isSubtype(atyp, expectedT)) {
                throw UnexpectedSubtype(arg.pos, arg.toString, expectedT, atyp)
              }
            }
          }
          (mtyp, tenv)
        }
        case x => throw UnexpectedType(c.pos, c.toString, "Module Type", x)
      }
    }
  }

  /**
   *
   * @param c
   * @param tenv
   * @return
   */
  def checkCommand(modId: Id, c: Command, tenv: Environment[Id, Type]): Environment[Id, Type] = c match {
    case CSeq(c1, c2) => {
      val e2 = checkCommand(modId, c1, tenv)
      checkCommand(modId, c2, e2)
    }
    case CTBar(c1, c2) => {
      val e2 = checkCommand(modId, c1, tenv)
      checkCommand(modId, c2, e2)
    }
    case CSplit(cases, default) => {
      var endEnv = checkCommand(modId, default, tenv)
      for (c <- cases) {
        val (condTyp, cenv) = checkExpression(c.cond, tenv, None)
        condTyp.matchOrError(c.cond.pos, "case condition", "boolean") { case _: TBool => () }
        val benv = checkCommand(modId, c.body, cenv)
        endEnv = endEnv.intersect(benv)
      }
      endEnv
    }
    case CIf(cond, cons, alt) => {
      val (condTyp, cenv) = checkExpression(cond, tenv, None)
      condTyp.matchOrError(cond.pos, "if condition", "boolean") { case _: TBool => () }
      val etrue = checkCommand(modId, cons, cenv)
      val efalse = checkCommand(modId, alt, cenv)
      etrue.intersect(efalse)
    }
    case CAssign(lhs, rhs) => {
      val (rTyp, renv) = checkExpression(rhs, tenv, None)
      val (lTyp, lenv) = checkExpression(lhs, renv, Some(rTyp))
      if (isSubtype(rTyp, lTyp)) lenv
      else throw UnexpectedSubtype(rhs.pos, "assignment", lTyp, rTyp)
    }
    case CRecv(lhs, rhs) => {
      val (rTyp, renv) = checkExpression(rhs, tenv, None)
      val (lTyp, lenv) = checkExpression(lhs, renv, Some(rTyp))
      if (isSubtype(rTyp, lTyp)) lenv
      else throw UnexpectedSubtype(rhs.pos, "recv", lTyp, rTyp)
    }
    case CSpecCall(h, mod, args) => {
      val mtyp = tenv(mod)
      mod.typ = Some(mtyp)
      //Check that args to recursive 'call' are correct
      mtyp match {
        case TModType(inputs, _, _, _) => {
          if (inputs.length != args.length) {
            throw ArgLengthMismatch(c.pos, inputs.length, args.length)
          }
          inputs.zip(args).foreach {
            case (expectedT, a) =>
              val (atyp, _) = checkExpression(a, tenv, None)
              if (!isSubtype(atyp, expectedT)) {
                throw UnexpectedSubtype(c.pos, a.toString, expectedT, atyp)
              }
          }
        }
        case _ => throw UnexpectedType(c.pos, "SpecCall Mod", "Module Type", mtyp)
      }
      //add spec handle type to env
      tenv.add(h.id, h.typ.get)
    }
    case CLockStart(mod) => tenv(mod).matchOrError(mod.pos, "lock reservation start", "Locked Memory or Module Type")
      {
        case _: TMemType => tenv
        case _: TLockedMemType => tenv
        case _: TModType => tenv
      }
    case CLockEnd(mod) => tenv(mod).matchOrError(mod.pos, "lock reservation start", "Locked Memory or Module Type")
      {
        case _: TMemType => tenv
        case _: TLockedMemType => tenv
        case _: TModType => tenv
      }
    case CLockOp(mem, _, _, _, _) =>
      tenv(mem.id).matchOrError(mem.pos, "lock operation", "Locked Memory or Module Type")
      {
        case t: TLockedMemType =>
          val memt = t.mem
          mem.id.typ = Some(t)
          if(mem.evar.isEmpty) tenv
          else {
            val (idxt, _) =  checkExpression(mem.evar.get, tenv, None)
            idxt match {
              case TSizedInt(l, TUnsigned()) if l.getLen == memt.addrSize => tenv
              case _ => throw UnexpectedType(mem.pos, s"lock operation $c", "ubit<" + memt.addrSize + ">", idxt)
            }
          }
        case t: TModType =>
          mem.id.typ = Some(t)
          if(mem.id == modId){
            throw RecursiveCallLockAcquisition(mem.pos)
          }
          tenv
      }
    case CVerify(handle, args, preds, upd, cHandles) =>
      //if there's an update clause check that stuff:
      if (upd.isDefined) {
        checkExpression(upd.get, tenv, None)
      }
      cHandles.foreach(c => {
        val (ctyp, _) = checkExpression(c, tenv, None)
        ctyp.matchOrError(handle.pos, "Spec Verify Op", "Checkpiont Handle") {
          case TRequestHandle(_, RequestType.Checkpoint) => ()
        }
      })
      //check that handle has been created via speccall and that arg types line up
      val (htyp, _) = checkExpression(handle, tenv, None)
      htyp.matchOrError(handle.pos, "Spec Verify Op", "Speculation Handle") {
        case TRequestHandle(mod, RequestType.Speculation) =>
          val mtyp = tenv(mod)
          mtyp match {
            case TModType(inputs, _, _, _) =>
              if (inputs.length != args.length) {
                throw ArgLengthMismatch(c.pos, inputs.length, args.length)
              }
              if (inputs.length != preds.length) {
                throw ArgLengthMismatch(c.pos, inputs.length, preds.length)
              }
              inputs.zip(args).foreach {
                case (expectedT, a) =>
                  val (atyp, _) = checkExpression(a, tenv, None)
                  if (!isSubtype(atyp, expectedT)) {
                    throw UnexpectedSubtype(c.pos, a.toString, expectedT, atyp)
                  }
              }
              inputs.zip(preds).foreach {
                case (expectedT, p) =>
                  val (atyp, _) = checkExpression(p, tenv, None)
                  if (!isSubtype(atyp, expectedT)) {
                    throw UnexpectedSubtype(c.pos, p.toString, expectedT, atyp)
                  }
              }
            case _ =>  throw UnexpectedType(c.pos, "Spec Verify", "Module Type", mtyp)
          }
          tenv
      }
    case CUpdate(newHandle, handle, args, preds, cHandles) =>
      //TODO solve the fact that this is just verify copypasta-ed
      cHandles.foreach(c => {
        val (ctyp, _) = checkExpression(c, tenv, None)
        ctyp.matchOrError(handle.pos, "Spec Verify Op", "Checkpiont Handle") {
          case TRequestHandle(_, RequestType.Checkpoint) => ()
        }
      })
      //check that handle has been created via speccall and that arg types line up
      val (htyp, _) = checkExpression(handle, tenv, None)
      htyp.matchOrError(handle.pos, "Spec Verify Op", "Speculation Handle") {
        case TRequestHandle(mod, RequestType.Speculation) =>
          val mtyp = tenv(mod)
          mtyp match {
            case TModType(inputs, _, _, _) =>
              if (inputs.length != args.length) {
                throw ArgLengthMismatch(c.pos, inputs.length, args.length)
              }
              if (inputs.length != preds.length) {
                throw ArgLengthMismatch(c.pos, inputs.length, preds.length)
              }
              inputs.zip(args).foreach {
                case (expectedT, a) =>
                  val (atyp, _) = checkExpression(a, tenv, None)
                  if (!isSubtype(atyp, expectedT)) {
                    throw UnexpectedSubtype(c.pos, a.toString, expectedT, atyp)
                  }
              }
              inputs.zip(preds).foreach {
                case (expectedT, p) =>
                  val (atyp, _) = checkExpression(p, tenv, None)
                  if (!isSubtype(atyp, expectedT)) {
                    throw UnexpectedSubtype(c.pos, p.toString, expectedT, atyp)
                  }
              }
            case _ =>  throw UnexpectedType(c.pos, "Spec Update", "Module Type", mtyp)
          }
          newHandle.typ = Some(htyp)
          newHandle.id.typ = Some(htyp)
          tenv.add(newHandle.id, htyp)
      }
    case CInvalidate(handle, cHandles) =>
      val (htyp, _) = checkExpression(handle, tenv, None)
      htyp.matchOrError(handle.pos, "Spec Verify Op", "Speculation Handle") {
        case TRequestHandle(_, RequestType.Speculation) => ()
      }
      cHandles.foreach(c => {
        val (ctyp, _) = checkExpression(c, tenv, None)
        ctyp.matchOrError(handle.pos, "Spec Verify Op", "Checkpiont Handle") {
          case TRequestHandle(_, RequestType.Checkpoint) => ()
        }
      })
      tenv
    case CCheckSpec(_) => tenv
    case CCheckpoint(handle, mod) => tenv(mod).matchOrError(mod.pos, "lock checkpoint",
      "Locked Memory or Module Type that supports Checkpoint Functionality ")
      {
        case t: TLockedMemType if LockImplementation.getCheckpoint(t.limpl).isDefined &&
          LockImplementation.getRollback(t.limpl).isDefined =>
          mod.typ = Some(t)
      }
      tenv.add(handle.id, handle.typ.get)
    case COutput(exp) => {
      checkExpression(exp, tenv, None)
      tenv
    }
    case CReturn(exp) =>
      checkExpression(exp, tenv, None)
      tenv
    case CExpr(exp) =>{
      checkExpression(exp, tenv, None)
      tenv
    }
    case CPrint(args) =>
      args.foreach(a => {
        val (t, _) = checkExpression(a, tenv, None)
        t match {
          case TSizedInt(_, _) => tenv
          case TString() => tenv
          case TBool() => tenv
          case _ => throw UnexpectedType(a.pos, a.toString, "Need a printable type", t)
        }
      })
      tenv
    case CEmpty() => tenv
    case _ => throw UnexpectedCommand(c)
  }

  def checkExpression(e: Expr, tenv: Environment[Id, Type],
    defaultType: Option[Type]): (Type, Environment[Id, Type]) = {
    val (typ, nenv) = _checkE(e, tenv, defaultType);
    if (e.typ.isDefined) {
      if (!areEqual(e.typ.get, typ)) throw UnexpectedType(e.pos, e.toString, typ.toString(), e.typ.get)
    }
    e.typ = Some(typ)
    (typ, nenv)
  }

  private def _checkE(e: Expr, tenv: Environment[Id, Type], defaultType: Option[Type]): (Type, Environment[Id, Type] ) = e match {
    case e1@EInt(_, _, bits) =>
      if (e1.typ.isDefined) { (e1.typ.get, tenv) }
      else { (TSizedInt(TBitWidthLen(bits), TSigned()/*unsigned = false*/), tenv) }
    case EBool(v) => (TBool(), tenv)
    case EString(v) => (TString(), tenv)
    case EUop(op, e) => {
      val (t1, env1) = checkExpression(e, tenv, None)
      op match {
        case BoolUOp(op) => t1.matchOrError(e.pos, "boolean op", "boolean") { case _: TBool => (TBool(), env1) }
        case NumUOp(op) => t1.matchOrError(e.pos, "number op", "number") { case t: TSizedInt => (t, env1) }
        case BitUOp(op) => t1.matchOrError(e.pos, "bit op", "sized integer") { case t: TSizedInt => (t, env1) }
      }
    }
    case EBinop(op, e1, e2) => {
      val (t1, env1) = checkExpression(e1, tenv, None)
      val (t2, env2) = checkExpression(e2, env1, None)
      op match {
        case BitOp("++", _) => (t1, t2) match {
          case (TSizedInt(l1, u1), TSizedInt(l2, u2)) if u1 == u2 =>
            (TSizedInt(TBitWidthLen(l1.getLen + l2.getLen), u1), env2)
          case (_, _) => throw UnexpectedType(e.pos, "concat", "sized number", t1)
        }
        case BitOp("<<", _) => (t1, t2) match {
          case (TSizedInt(l1, u1), TSizedInt(_, _)) => (TSizedInt(l1, u1), env2)
          case (_, _) => throw UnexpectedType(e.pos, "shift left", "sized number", t1)
        }
        case BitOp(">>", _) => (t1, t2) match {
          case (TSizedInt(l1, u1), TSizedInt(_, _)) => (TSizedInt(l1, u1), env2)
          case (_, _) => throw UnexpectedType(e.pos, "shift right", "sized number", t1)
        }
        case NumOp("*", _) => (t1, t2) match {
          case (TSizedInt(l1, u1), TSizedInt(l2, u2)) if u1 == u2 =>
            (TSizedInt(TBitWidthLen(l1.getLen + l2.getLen), u1), env2)
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
      val (rt, renv) = checkExpression(rec, tenv, None)
      rt match {
        case TRecType(n, fs) => fs.get(fieldName) match {
          case Some(t) => (t, renv)
          case _ => throw MissingType(fieldName.pos, s"Field $n. ${fieldName.v}")
        }
        case _ => throw UnexpectedType(e.pos, "record access", "record type", rt)
      }
    }
    case ERecLiteral(fields) => {
      val ftyps = fields map { case (n, e) => (n, checkExpression(e, tenv, None)._1) }
      (TRecType(Id("anon"), ftyps) , tenv)//TODO these are wrong, maybe just remove these
    }
    case EMemAccess(mem, index, wm, _, _, _) =>
      val memt = tenv(mem)
      mem.typ = Some(memt)
      val (idxt, env1) = checkExpression(index, tenv, None)
      (memt, idxt) match {
        case (TMemType(e, s, _, _, _, _), TSizedInt(l, TUnsigned())) if l.getLen == s =>
          if (wm.isDefined) {
            val (wmt, _) = checkExpression(wm.get, tenv, None)
            wmt match {
              //TODO check that the mask size is correct (i.e., length of elemtype / 8)
              case TSizedInt(lm, TUnsigned()/*true*/) => ()
              case _ => throw UnexpectedType(wm.get.pos, "Write Mask", "Mask must be unsigned and has length equal" +
                " to the number of bytes in the element type", wmt)
            }
          }
          (e, env1)
        case (TLockedMemType(TMemType(e, s, _, _, _, _),_,_), TSizedInt(l, TUnsigned())) if l.getLen == s =>
          if (wm.isDefined) {
            val (wmt, _) = checkExpression(wm.get, tenv, None)
            wmt match {
              //TODO check that the mask size is correct (i.e., length of elemtype / 8)
              case TSizedInt(lm, TUnsigned()/*true*/) => ()
              case _ => throw UnexpectedType(wm.get.pos, "Write Mask", "Mask must be unsigned and has length equal" +
                " to the number of bytes in the element type", wmt)
            }
          }
          (e, env1)
        case _ => throw UnexpectedType(e.pos, "memory access", "mismatched types", memt)
      }
    case EBitExtract(num, start, end) => {
      val (ntyp, nenv) = checkExpression(num, tenv, None)
      val bitsLeft = math.abs(end - start) + 1
      ntyp.matchOrError(e.pos, "bit extract", "sized number") {
        case TSizedInt(l, u) if l.getLen >= bitsLeft => (TSizedInt(TBitWidthLen(bitsLeft), u), nenv)
        case _ => throw UnexpectedType(e.pos, "bit extract", "sized number larger than extract range", ntyp)
      }
    }
    case ETernary(cond, tval, fval) => {
      val (ctyp, cenv) = checkExpression(cond, tenv, None)
      ctyp.matchOrError(cond.pos, "ternary condition", "boolean") { case _: TBool => () }
      val (ttyp, trenv) = checkExpression(tval, cenv, None)
      val (ftyp, fenv) = checkExpression(fval, cenv, None)
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
              val (atyp, aenv) = checkExpression(a, tenv, None)
              if (!isSubtype(atyp, expectedT)) {
                throw UnexpectedSubtype(e.pos, a.toString, expectedT, atyp)
              }
          }
          (tret, tenv)
        }
        case _ => throw UnexpectedType(func.pos, "function call", "function type", ftyp)
      }
    }
    case ECall(mod, name, args, isAtomic) => {
      val mtyp = tenv(mod)
      mod.typ = Some(mtyp)
      mtyp match {
        case TModType(inputs, _, retType, _) => {
          if (inputs.length != args.length) {
            throw ArgLengthMismatch(e.pos, inputs.length, args.length)
          }
          inputs.zip(args).foreach {
            case (expectedT, a) =>
              val (atyp, aenv) = checkExpression(a, tenv, None)
              if (!isSubtype(atyp, expectedT)) {
                throw UnexpectedSubtype(e.pos, a.toString, expectedT, atyp)
              }
          }
          (if (retType.isDefined) retType.get else TVoid(), tenv)
        }
        case TObject(_, _, methods) if name.isDefined && methods.contains(name.get) => {
          val mtyp = methods(name.get)._1
          val inputs = mtyp.args
          val retType = mtyp.ret
          //TODO refactor and pull into function since it is same as above
          if (inputs.length != args.length) {
            throw ArgLengthMismatch(e.pos, inputs.length, args.length)
          }
          inputs.zip(args).foreach {
            case (expectedT, a) =>
              val (atyp, aenv) = checkExpression(a, tenv, None)
              if (!isSubtype(atyp, expectedT)) {
                throw UnexpectedSubtype(e.pos, a.toString, expectedT, atyp)
              }
          }
          (retType, tenv)
        }
        case _ => throw UnexpectedType(mod.pos, "module name", "module type", mtyp)
      }
    }
    case EVar(id) => e.typ match {
      case Some(t) if tenv.get(id).isEmpty => id.typ = Some(t); (t, tenv.add(id, t))
      case Some(t) => if (areEqual(t,tenv(id))) {
        id.typ = Some(t)
        (t, tenv)
      } else {
        throw UnexpectedType(id.pos, "variable", s"variable type set to new conflicting type : ${tenv(id)}", t)
      }
      case None if (tenv.get(id).isDefined || defaultType.isEmpty) => id.typ = Some(tenv(id)); (tenv(id), tenv)
      case None => id.typ = defaultType; (defaultType.get, tenv.add(id, defaultType.get))
    }
    case ECast(totyp, exp) =>
      val (etyp, tenv2) = checkExpression(exp, tenv, None)
       if (!canCast(etyp, totyp)) throw IllegalCast(e.pos, totyp, etyp)
      (totyp, tenv2)
    case _ => throw UnexpectedCase(e.pos)
  }

}
