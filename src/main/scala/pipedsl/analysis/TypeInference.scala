package pipedsl.analysis

import pipedsl.common.Errors.{UnexpectedType, UnificationError}
import pipedsl.common.Syntax._
import pipedsl.typechecker.Subtypes.areEqual
import pipedsl.common.Errors
import java.util.function.ToIntFunction
import pipedsl.typechecker.Environments.TypeEnv
import pipedsl.typechecker.Environments

object TypeInference {
  
  type Subst = List[(Id, Type)]
  
  var counter = 0

  def checkProgram(p: Prog) = {
    var runningEnv: TypeEnv = TypeEnv()
    for (f <- p.fdefs) {
      runningEnv = runningEnv.add(f.name, TFun(f.args.map(p => p.typ), f.ret))
      checkCommand(f.body, runningEnv.asInstanceOf[TypeEnv], List())
    }
    for (m <- p.moddefs) {
      val inputTypes = m.inputs.map(p => p.typ)
      val modTypes = m.modules.map(m => replaceNamedType(m.typ, runningEnv))
      runningEnv = runningEnv.add(m.name, TModType(inputTypes, modTypes, m.ret, Some(m.name)))
      checkCommand(m.body, runningEnv.asInstanceOf[TypeEnv], List())
    }
  }

  def checkModule(m: ModuleDef, env: TypeEnv) = {
    val inputTypes = m.inputs.map(p => p.typ)
    val modTypes = m.modules.map(m => replaceNamedType(m.typ, env))
    val modEnv = env.add(m.name, TModType(inputTypes, modTypes, m.ret, Some(m.name)))
    for ()
  }

  private def replaceNamedType(t: Type, tenv: TypeEnv): Type = t match {
    case TNamedType(name) => tenv(name)
    case _ => t
  }

  def checkCommand(c: Command, env: TypeEnv, sub: Subst): (TypeEnv, Subst) = c match {
    case CLockOp(mem, op) =>  //test basic first
      (env, sub)
    case CEmpty() => (env, sub)
    case CReturn(exp) => (env, sub)
    case CLockStart(mod) => throw new RuntimeException()//how to unify or condition?
    case CIf(cond, cons, alt) =>
      //TODO: Might need to do some special handling with composing substitutions in this case
      val (condS, condT) = infer(env, cond)
      val newSub = compose_subst(sub, condS)
      val newEnv = env.apply_subst_typeenv(condS)
      val (consEnv, consSub) = checkCommand(cons, newEnv, newSub)
      val newEnv2 = newEnv.apply_subst_typeenv(consSub)
      val (altEnv, altSub) = checkCommand(alt, newEnv2, consSub)
      //TODO: Intersection of both envs?
      ((consEnv.apply_subst_typeenv(altSub).intersect(altEnv.apply_subst_typeenv(altSub)).asInstanceOf[TypeEnv]), altSub)
    case CLockEnd(mod) => (env, sub) //how to unify an or condition?
    case CSplit(cases, default) => //TODO 
      var (runningEnv, runningSub) = checkCommand(default, env, sub)
      runningEnv = runningEnv.apply_subst_typeenv(runningSub)
      for (c <- cases) {
        val (condS, condT) = infer(env, c.cond)
        //construct substitution you will use to check command
        val newSub = compose_subst(runningSub, condS)
        //apply substitution to original environmnet, which you will use to check the body
        val newEnv = env.apply_subst_typeenv(newSub)
        val (caseEnv, caseSub) = checkCommand(c.body, newEnv, newSub)
        runningSub = caseSub
        runningEnv = runningEnv.apply_subst_typeenv(runningSub).intersect(caseEnv.apply_subst_typeenv(runningSub))
      }
      (runningEnv, runningSub)
    case CExpr(exp) => (env, sub) //TODO
    case CCheck(predVar) => (env, sub)
    case CTBar(c1, c2) =>
      val (e, s) = checkCommand(c1, env, sub)
      val e1 = e.apply_subst_typeenv(s)
      val (e2, s2) = checkCommand(c2, e1, s)
      (e2.apply_subst_typeenv(s2), s2)
    case CPrint(evar) => (env, sub)
    //how to unify or conditions> This I guess is an polymorphic function with restrictions
    case CSpeculate(predVar, predVal, verify, body) => (env, sub)
    case COutput(exp) =>
      (env, sub)
      //How to check wellformedness with the module body
    case CRecv(lhs, rhs, typ) =>
      val (slhs, tlhs) = infer(env, lhs)
      val (srhs, trhs) = infer(env, rhs)
      val s1 = unify(tlhs, trhs)
      val sret = compose_many_subst(sub, slhs, srhs, s1, typ match {
        case Some(value) => compose_subst(unify(tlhs, value), unify(trhs, value))
        case None => List()
      })
      val newEnv = lhs match {
        case EVar(id) => env.add(id, tlhs)
        case _ => env
      }
      (newEnv.asInstanceOf[TypeEnv].apply_subst_typeenv(sret), sret)
    case CAssign(lhs, rhs, typ) =>
      val (slhs, tlhs) = infer(env, lhs)
      val (srhs, trhs) = infer(env, rhs)
      val s1 = unify(tlhs, trhs)
      val sret = compose_many_subst(sub, slhs, srhs, s1, typ match {
        case Some(value) => compose_subst(unify(tlhs, value), unify(trhs, value))
        case None => List()
      })
      val newEnv = lhs match {
        case EVar(id) => env.add(id, tlhs)
        case _ => env
      }
      (newEnv.asInstanceOf[TypeEnv].apply_subst_typeenv(sret), sret)
    case CSeq(c1, c2) => 
      val (e, s) = checkCommand(c1, env, sub)
      val e1 = e.apply_subst_typeenv(s)
      val (e2, s2) = checkCommand(c2, e1, s)
      (e2.apply_subst_typeenv(s2), s2)
  }

  private def generateTypeVar(): TNamedType = TNamedType(Id("__TYPE__" + counter)); counter += 1
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
  
  //Updating the type environment with the new subtitution whenever you generate one allows errors to be found :D
  private def infer(env: TypeEnv, e: Expr): (Subst, Type) = e match {
    case EInt(v, base, bits) => (List(), TSizedInt(bits, true))
    case EString(v) => (List(), TString())
    case EBool(v) => (List(), TBool())
    case EUop(op, ex) => 
      val (s, t) = infer(env, ex)
      val retType = generateTypeVar()
      val subst = unify(TFun(List(t), retType), uOpExpectedType(op))
      (compose_subst(s, subst), retType)
    case EBinop(op, e1, e2) =>
      val (s1, t1) = infer(env, e1) 
      val newEnv = env.apply_subst_typeenv(s1)
      val (s2, t2) = infer(newEnv, e2)
      val retType = generateTypeVar()
      val subst = unify(TFun(List(t2,t1), retType), binOpExpectedType(op))
      (compose_many_subst(s1, s2, subst), retType)
    case EMemAccess(mem, index) => 
      if(!env(mem).isInstanceOf[TMemType]) throw UnexpectedType(e.pos, "Memory Access", "TMemtype", env(mem))
      val retTyp = generateTypeVar()
      val (s, t) = infer(env, index)
      val subst = unify(TFun(List(t), retTyp), getMemAccessType(env(mem).asInstanceOf[TMemType]))
      (compose_subst(s,subst), retTyp)
    case EBitExtract(num, start, end) => throw new RuntimeException() //TODO
    case ETernary(cond, tval, fval) =>  
      val (sc, tc) = infer(env, cond)
      val env1 = env.apply_subst_typeenv(sc)
      val (st, tt) = infer(env1, tval)
      val env2 = env1.apply_subst_typeenv(st)
      val (sf, tf) = infer(env, fval)
      val substc = unify(tc, TBool())
      val subst = unify(tt, tf)
      (compose_many_subst(sc, st, sf, substc, subst), tt)
    case EApp(func, args) => 
      val expectedType = env(func)
      val retType = generateTypeVar()
      val (subList, typeList) = args.map(a => infer(env, a)).unzip
      val subst = unify(TFun(typeList, retType), expectedType)
      (compose_many_subst((subList :+ subst).toSeq:_*), retType)
    case ECall(mod, args) => 
      if (!env(mod).isInstanceOf[TModType]) throw UnexpectedType(e.pos, "Module Call", "TModType", env(mod))
      val expectedType = getArrowModType(env(mod).asInstanceOf[TModType])
      val retType = generateTypeVar()
      val (subList, typeList) = args.map(a => infer(env, a)).unzip
      val subst = unify(TFun(typeList, retType), expectedType)
      (compose_many_subst((subList :+ subst).toSeq:_*), retType)
    case EVar(id) => (List(), env(id))
    case ECast(ctyp, exp) => 
      //TODO this is wrong probably
      val (s, t) = infer(env, exp) 
      val newT = apply_subst_typ(s, t)
      if (!areEqual(ctyp, newT)) throw Errors.IllegalCast(e.pos, ctyp, newT)
      (s, ctyp)
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
