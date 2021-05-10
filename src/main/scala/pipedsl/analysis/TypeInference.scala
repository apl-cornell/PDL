package pipedsl.analysis

import pipedsl.common.Errors.{ArgLengthMismatch, MalformedLockTypes, UnexpectedReturn, UnexpectedSubtype, UnexpectedType, UnificationError}
import pipedsl.common.Syntax._
import pipedsl.typechecker.Subtypes.{areEqual, isSubtype}
import pipedsl.common.Errors
import pipedsl.common.Syntax.Latency.{Asynchronous, Combinational, Sequential}
import pipedsl.typechecker.Environments.{Environment, TypeEnv}

object TypeInference {
  
  type Subst = List[(Id, Type)]
  private var currentDef: Id = Id("-invalid-")
  private var counter = 0

  def checkProgram(p: Prog): Environment[Id, Type] = {
    val funcEnvs = p.fdefs.foldLeft[Environment[Id, Type]](TypeEnv())(
      (env, f) => {
        currentDef = f.name 
        checkFunc(f, env.asInstanceOf[TypeEnv])
      })
    val modEnvs = p.moddefs.foldLeft[Environment[Id, Type]](funcEnvs)(
      (env, m) => {
        currentDef = m.name
        checkModule(m, env.asInstanceOf[TypeEnv])
      })
    checkCircuit(p.circ, modEnvs)
  }

  def checkCircuit(c: Circuit, tenv: Environment[Id, Type]): Environment[Id, Type] = c match {
    case CirSeq(c1, c2) => {
      val e1 = checkCircuit(c1, tenv)
      checkCircuit(c2, e1)
    }
    case CirConnect(name, c) =>
      val (t, env2) = checkCirExpr(c, tenv)
      env2.add(name, t)
    case CirExprStmt(ce) => checkCirExpr(ce, tenv)._2
  }

  private def checkCirExpr(c: CirExpr, tenv: Environment[Id, Type]): (Type, Environment[Id, Type]) = c match {
    case CirMem(elemTyp, addrSize) => {
      val mtyp = TMemType(elemTyp, addrSize, Asynchronous, Asynchronous)
      (mtyp, tenv)
    }
    case CirRegFile(elemTyp, addrSize) => {
      val mtyp = TMemType(elemTyp, addrSize, Combinational, Sequential)
      (mtyp, tenv)
    }
    case CirNew(mod, mods) => {
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
              val (subst, atyp, aenv) = infer(tenv.asInstanceOf[TypeEnv], arg)
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
  
  def checkModule(m: ModuleDef, env: TypeEnv): Environment[Id, Type] = {
    val inputTypes = m.inputs.map(p => p.typ)
    val modTypes = m.modules.map(m => replaceNamedType(m.typ, env))
    val modEnv = env.add(m.name, TModType(inputTypes, modTypes, m.ret, Some(m.name)))
    val inEnv = m.inputs.foldLeft[Environment[Id, Type]](modEnv)((env, p) => env.add(p.name, p.typ))
    val pipeEnv = m.modules.zip(modTypes).foldLeft[Environment[Id, Type]](inEnv)((env, m) => env.add(m._1.name, m._2))
    checkCommand(m.body, pipeEnv.asInstanceOf[TypeEnv], List())
    modEnv
  }
  
  def checkFunc(f: FuncDef, env: TypeEnv): Environment[Id, Type] = {
    val inputTypes = f.args.map(a => a.typ)
    val funType = TFun(inputTypes, f.ret)
    val funEnv = env.add(f.name, funType)
    val inEnv = f.args.foldLeft[Environment[Id, Type]](funEnv)((env, a) => env.add(a.name, a.typ))
    checkCommand(f.body, inEnv.asInstanceOf[TypeEnv], List())
    funEnv
  }

  private def replaceNamedType(t: Type, tenv: TypeEnv): Type = t match {
    case TNamedType(name) => tenv(name)
    case _ => t
  }
//INVARIANTS
//Transforms the argument sub by composing any additional substitution
//Transforms the argument env by subbing in the returned substitution and adding any relevatn variables
  def checkCommand(c: Command, env: TypeEnv, sub: Subst): (TypeEnv, Subst) =  c match {
    case CLockOp(mem, op) =>  //test basic first
      env(mem.id) match {
        case t@TMemType(elem, addrSize, readLatency, writeLatency) => mem.evar match {
          case Some(value) => 
            val (s, t, e) = infer(env, value)
            val tempSub = compose_subst(sub, s)
            val tNew = apply_subst_typ(tempSub, t)
            val newSub = compose_subst(tempSub, unify(tNew, TSizedInt(addrSize, true)))
            (e.apply_subst_typeenv(newSub), newSub)
          case None => (env, sub)
        }
        case TModType(inputs, refs, retType, name) => 
          if (mem.evar.isDefined) throw MalformedLockTypes("Pipeline modules can not have specific locks")
          (env, sub)
        case b  => throw UnexpectedType(mem.id.pos, c.toString, "Memory or Module Type", b)
      }
    case CEmpty() => (env, sub)
    case CReturn(exp) => 
      val (s, t, e) = infer(env, exp)
      val tempSub = compose_subst(sub, s)
      val tNew = apply_subst_typ(tempSub, t)
      val funT = env(currentDef)
      funT match {
        case TFun(args, ret) => 
          val retSub = compose_subst(tempSub, unify(tNew, ret))
          (e.apply_subst_typeenv(retSub), retSub)
        case b => throw UnexpectedType(c.pos, c.toString, funT.toString, b)
      }
    case CLockStart(mod) => 
      if(!(env(mod).isInstanceOf[TMemType] || env(mod).isInstanceOf[TModType])) {
        throw UnexpectedType(mod.pos, c.toString, "Memory or Module Type", env(mod))
      }
      (env, sub)
    case CIf(cond, cons, alt) =>
      val (condS, condT, env1) = infer(env, cond)
      val tempSub = compose_subst(sub, condS)
      val condTyp = apply_subst_typ(tempSub, condT)
      val newSub = compose_subst(tempSub, unify(condTyp, TBool()))
      val newEnv = env1.apply_subst_typeenv(newSub)
      val (consEnv, consSub) = checkCommand(cons, newEnv, newSub)
      val newEnv2 = newEnv.apply_subst_typeenv(consSub)
      val (altEnv, altSub) = checkCommand(alt, newEnv2, consSub)
      //TODO: Intersection of both envs?
      (consEnv.apply_subst_typeenv(altSub).intersect(altEnv).asInstanceOf[TypeEnv], altSub)
    case CLockEnd(mod) =>       
      if(!(env(mod).isInstanceOf[TMemType] || env(mod).isInstanceOf[TModType])) {
        throw UnexpectedType(mod.pos, c.toString, "Memory or Module Type", env(mod))
      }
      (env, sub)
    case CSplit(cases, default) => //TODO 
      var (runningEnv, runningSub) = checkCommand(default, env, sub)
      for (c <- cases) {
        val (condS, condT, env1) = infer(env, c.cond)
        val tempSub = compose_subst(runningSub, condS)
        val condTyp = apply_subst_typ(tempSub, condT)
        val newSub = compose_subst(tempSub, unify(condTyp, TBool()))
        //apply substitution to original environmnet, which you will use to check the body
        val newEnv = env1.apply_subst_typeenv(newSub)
        val (caseEnv, caseSub) = checkCommand(c.body, newEnv, newSub)
        runningSub = caseSub
        runningEnv = runningEnv.apply_subst_typeenv(runningSub).intersect(caseEnv).asInstanceOf[TypeEnv]
      }
      (runningEnv, runningSub)
    case CExpr(exp) => 
      val (s, t, e) = infer(env, exp)
      val retS = compose_subst(sub, s)
      (e.apply_subst_typeenv(retS), retS) //TODO
    case CCheck(predVar) => (env, sub)
    case CTBar(c1, c2) =>
      val (e, s) = checkCommand(c1, env, sub)
      val (e2, s2) = checkCommand(c2, e, s)
      (e2, s2)
    case CPrint(evar) => (env, sub)
    //how to unify or conditions> This I guess is an polymorphic function with restrictions
    case CSpeculate(predVar, predVal, verify, body) => (env, sub)
    case COutput(exp) =>
      val (s, t, e) = infer(env, exp)
      val tempSub = compose_subst(sub, s)
      val tNew = apply_subst_typ(tempSub, t)
      val modT = env(currentDef)
      modT match {
        case TModType(inputs, refs, retType, name) =>
          retType match {
            case Some(value) =>
              val retSub = compose_subst(tempSub, unify(tNew, value))
              (e.apply_subst_typeenv(retSub), retSub)
            case None => (e.apply_subst_typeenv(tempSub), tempSub)
          }
        case b => throw UnexpectedType(c.pos, c.toString, modT.toString, b)
      }
      //How to check wellformedness with the module body
    case CRecv(lhs, rhs, typ) =>
      val (slhs, tlhs, lhsEnv) = lhs match {
        case EVar(id) => (List(), typ.getOrElse(generateTypeVar()), env)
        case _ => infer(env, lhs)
      }
      val (srhs, trhs, rhsEnv) = infer(lhsEnv, rhs)
      val tempSub = compose_many_subst(sub, slhs, srhs)
      val lhstyp = apply_subst_typ(tempSub, tlhs)
      val rhstyp = apply_subst_typ(tempSub, trhs)
      val s1 = unify(lhstyp, rhstyp)
      val sret = compose_many_subst(tempSub, s1, typ match {
        case Some(value) => compose_subst(unify(lhstyp, value), unify(rhstyp, value))
        case None => List()
      })
      val newEnv = lhs match {
        case EVar(id) => rhsEnv.add(id, tlhs)
        case _ => rhsEnv
      }
      (newEnv.asInstanceOf[TypeEnv].apply_subst_typeenv(sret), sret)
    case CAssign(lhs, rhs, typ) =>
      val (slhs, tlhs, lhsEnv) = (List(), typ.getOrElse(generateTypeVar()), env)
      val (srhs, trhs, rhsEnv) = infer(lhsEnv, rhs)
      val tempSub = compose_many_subst(sub, slhs, srhs)
      val lhstyp = apply_subst_typ(tempSub, tlhs)
      val rhstyp = apply_subst_typ(tempSub, trhs)
      val s1 = unify(lhstyp, rhstyp)
      val sret = compose_many_subst(tempSub, s1, typ match {
        case Some(value) => compose_subst(unify(lhstyp, value), unify(rhstyp, value))
        case None => List()
      })
      val newEnv = lhs match {
        case EVar(id) => rhsEnv.add(id, tlhs)
        case _ => rhsEnv
      }
      (newEnv.asInstanceOf[TypeEnv].apply_subst_typeenv(sret), sret)
    case CSeq(c1, c2) => 
      val (e1, s) = checkCommand(c1, env, sub)
      val (e2, s2) = checkCommand(c2, e1, s)
      (e2, s2)
}

  private def generateTypeVar(): TNamedType = {
    counter += 1
    TNamedType(Id("__TYPE__" + counter))
  }
  private def occursIn(name: Id, b: Type): Boolean = b match {
    case TSizedInt(len, unsigned) => false
    case TString() => false
    case TVoid() => false
    case TBool() => false
    case TFun(args, ret) => args.foldLeft[Boolean](false)((b, t) => b || occursIn(name, t)) || occursIn(name, ret)
    case TRecType(name, fields) =>false
    case TMemType(elem, addrSize, readLatency, writeLatency) => false
    case TModType(inputs, refs, retType, name) => false
    case TNamedType(name) => false
  }

  private def subst_into_type(typevar: Id, toType: Type, inType: Type): Type = inType match {
    case TRecType(name, fields) => inType //TODO 
    case t@TMemType(elem, addrSize, readLatency, writeLatency) => t.copy(elem=subst_into_type(typevar, toType, elem))
    case TSizedInt(len, unsigned) => inType
    case TString() => inType
    case TBool() => inType
    case TVoid() => inType 
    case TFun(args, ret) => TFun(args.map(a=>subst_into_type(typevar, toType, a)), subst_into_type(typevar, toType, ret))
    case TNamedType(name) => if(name == typevar) toType else inType
    case TModType(inputs, refs, retType, name) => 
      TModType(
        inputs.map(i => subst_into_type(typevar, toType, i)), 
        refs.map(r => subst_into_type(typevar, toType, r)), 
        retType match {
          case Some(value) => Some(subst_into_type(typevar, toType, value))
          case None => None
        }, name)
  }

  def apply_subst_typ(subst: Subst, t: Type): Type = subst.foldLeft[Type](t)((t1, s) => subst_into_type(s._1, s._2, t1))
  
  private def apply_subst_substs(subst: Subst, inSubst: Subst): Subst = 
    inSubst.foldLeft[Subst](List())((s, c) => s :+ ((c._1, apply_subst_typ(subst, c._2))))

  private def compose_subst(sub1: Subst, sub2: Subst): Subst = 
    sub1 ++ apply_subst_substs(sub1, sub2)

  private def compose_many_subst(subs: Subst*): Subst = 
    subs.foldRight[Subst](List())((s1, s2) => compose_subst(s1, s2))

  
  private def unify(a: Type, b: Type): Subst = (a,b) match {
    case (t1: TNamedType, t2) => if (!occursIn(t1.name, t2)) List((t1.name, t2)) else List()
    case (t1, t2: TNamedType) => if (!occursIn(t2.name, t1)) List((t2.name, t1)) else List()
    case (_:TString, _:TString) => List()
    case (_: TBool, _:TBool) => List()
    case (_: TVoid, _:TVoid) => List()
    case (_: TSizedInt, _: TSizedInt) => List()
      //TODO: TSIZEDINT
    case (TFun(args1, ret1), TFun(args2, ret2)) if args1.length == args2.length =>  
      compose_subst(args1.zip(args2).foldLeft[Subst](List())((s, t) => s ++ unify(t._1, t._2)), unify(ret1, ret2))
    case (TModType(input1, refs1, retType1, name1), TModType(input2, refs2, retType2, name2)) => 
    //TODO: Name?\
      if (name1 != name2) throw UnificationError(a,b)
      compose_subst(
        input1.zip(input2).foldLeft[Subst](List())((s, t) => s ++ unify(t._1, t._2)), 
        compose_subst(
          refs1.zip(refs2).foldLeft[Subst](List())((s,t) => s ++ unify(t._1, t._2)),
          ((retType1, retType2) match {
            case (Some(t1:Type), Some(t2:Type)) => unify(t1, t2)
            case (None, None) => List()
            case _ => throw UnificationError(a,b)
          })))
    case (TMemType(elem1, addrSize1, readLatency1, writeLatency1), TMemType(elem2, addrSize2, readLatency2, writeLatency2)) =>
      if (addrSize1 != addrSize2 || readLatency1 != readLatency2 || writeLatency1 != writeLatency2) throw UnificationError(a, b)
      unify(elem1, elem2)
    case _ => throw UnificationError(a, b)
  }
  
  //Updating the type environment with the new substitution whenever you generate one allows errors to be found :D
  //The environment returned is guaratneed to already have been substituted into with the returned substitution
  private def infer(env: TypeEnv, e: Expr): (Subst, Type, TypeEnv) = e match {
    case EInt(v, base, bits) => (List(), TSizedInt(bits, true), env)
    case EString(v) => (List(), TString(), env)
    case EBool(v) => (List(), TBool(), env)
    case EUop(op, ex) => 
      val (s, t, env1) = infer(env, ex)
      val retType = generateTypeVar()
      val tNew = apply_subst_typ(s, t)
      val subst = unify(TFun(List(tNew), retType), uOpExpectedType(op))
      val retSubst = compose_subst(s, subst)
      val retTyp = apply_subst_typ(retSubst, retType)
      (retSubst, retTyp, env1.apply_subst_typeenv(retSubst))
    case EBinop(op, e1, e2) =>
      val (s1, t1, env1) = infer(env, e1)
      val (s2, t2, env2) = infer(env1, e2)
      val retType = generateTypeVar()
      val subTemp = compose_subst(s1, s2)
      val t1New = apply_subst_typ(subTemp, t1)
      val t2New = apply_subst_typ(subTemp, t2)
      val argSub = unify(t1New, t2New)
      val subst = unify(TFun(List(t2New,t1New), retType), binOpExpectedType(op))
      val retSubst = compose_many_subst(subTemp, argSub, subst)
      val retTyp = apply_subst_typ(retSubst, retType)
      (retSubst, retTyp, env2.apply_subst_typeenv(retSubst))
    case EMemAccess(mem, index) => 
      if(!env(mem).isInstanceOf[TMemType]) throw UnexpectedType(e.pos, "Memory Access", "TMemtype", env(mem))
      val retType = generateTypeVar()
      val (s, t, env1) = infer(env, index)
      val tTemp = apply_subst_typ(s, t)
      val subst = unify(TFun(List(tTemp), retType), getMemAccessType(env1(mem).asInstanceOf[TMemType]))
      val retSubst = compose_subst(s,subst)
      val retTyp = apply_subst_typ(retSubst, tTemp)
      (retSubst, retTyp, env1.apply_subst_typeenv(retSubst))
    case EBitExtract(num, start, end) =>  
      val (s, t, e) = infer(env, num)
      t match {
        case TSizedInt(len, unsigned) if len >= (math.abs(end - start) + 1) => (s, t, e)
        case b => throw UnificationError(b, TSizedInt(32, true))
      }
      
      //TODO
    case ETernary(cond, tval, fval) =>  
      val (sc, tc, env1) = infer(env, cond)
      val (st, tt, env2) = infer(env1, tval)
      val (sf, tf, env3) = infer(env2, fval)
      val substSoFar =  compose_many_subst(sc, st, sf)
      val tcNew = apply_subst_typ(substSoFar, tc)
      val ttNew = apply_subst_typ(substSoFar, tt)
      val tfNew = apply_subst_typ(substSoFar, tf)
      val substc = unify(tcNew, TBool())
      val subst = unify(ttNew, tfNew)
      val retSubst = compose_many_subst(sc, st, sf, substc, subst)
      val retType = apply_subst_typ(retSubst, ttNew)
      (retSubst, retType, env3.apply_subst_typeenv(retSubst))
    case EApp(func, args) => 
      val expectedType = env(func)
      val retType = generateTypeVar()
      var runningEnv: TypeEnv = env
      var runningSubst: Subst = List()
      var typeList: List[Type] = List()
      for (a <- args) {
        val (sub, typ, env1) = infer(runningEnv, a) 
        runningSubst = compose_subst(runningSubst, sub)
        typeList = typeList :+ typ
        runningEnv = env1
      }
      typeList = typeList.map(t=> apply_subst_typ(runningSubst, t))
      val subst = unify(TFun(typeList, retType), expectedType)
      val retSubst = compose_subst(runningSubst, subst)
      val retEnv = runningEnv.apply_subst_typeenv(retSubst)
      val retTyp = apply_subst_typ(retSubst, retType)
      (retSubst, retTyp, retEnv)
    case ECall(mod, args) => 
      if (!env(mod).isInstanceOf[TModType]) throw UnexpectedType(e.pos, "Module Call", "TModType", env(mod))
      val expectedType = getArrowModType(env(mod).asInstanceOf[TModType])
      val retType = generateTypeVar()
      var runningEnv: TypeEnv = env
      var runningSubst: Subst = List()
      var typeList: List[Type] = List()
      for (a <- args) {
        val (sub, typ, env1) = infer(runningEnv, a)
        runningSubst = compose_subst(runningSubst, sub)
        typeList = typeList :+ typ
        runningEnv = env1
      }
      typeList = typeList.map(t=> apply_subst_typ(runningSubst, t))
      val subst = unify(TFun(typeList, retType), expectedType)
      val retSubst = compose_subst(runningSubst, subst)
      val retEnv = runningEnv.apply_subst_typeenv(retSubst)
      val retTyp = apply_subst_typ(retSubst, retType)
      (retSubst, retTyp, retEnv)
    case EVar(id) => (List(), env(id), env)
    case ECast(ctyp, exp) => 
      //TODO this is wrong probably
      val (s, t, env1) = infer(env, exp) 
      val newT = apply_subst_typ(s, t)
      if (!areEqual(ctyp, newT)) throw Errors.IllegalCast(e.pos, ctyp, newT)
      (s, ctyp, env1)
  }
  
  private def binOpExpectedType(b: BOp): Type = b match {
    case EqOp(op) => 
      val t = generateTypeVar() // TODO: This can be anything?
      TFun(List(t, t), TBool())
    case CmpOp(op) => TFun(List(TSizedInt(32, true), TSizedInt(32, true)), TBool())//TODO: TSizedInt?
    case BoolOp(op, fun) => TFun(List(TBool(), TBool()), TBool())
    case NumOp(op, fun) => TFun(List(TSizedInt(32, true), TSizedInt(32, true)), TSizedInt(32, true))
    case BitOp(op, fun) => TFun(List(TSizedInt(32, true), TSizedInt(32, true)), TSizedInt(32, true))
  }

  private def uOpExpectedType(u: UOp): Type = u match {
      case BitUOp(op) => TFun(List(TSizedInt(32, true)), TSizedInt(32, true))
      case BoolUOp(op) => TFun(List(TBool()), TBool())
      case NumUOp(op) => TFun(List(TSizedInt(32, true)), TSizedInt(32, true))
  }

  private def getArrowModType(t: TModType): TFun = {
    TFun(t.inputs, t.retType match {
      case None => TVoid()
      case Some(value) => value
    })
  }

  private def getMemAccessType(t: TMemType): TFun = {
    TFun(List(TSizedInt(t.addrSize, unsigned=true)), t.elem)
  }
    
}
