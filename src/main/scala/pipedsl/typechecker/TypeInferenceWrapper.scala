package pipedsl.typechecker

import pipedsl.common.{Errors, Syntax}
import pipedsl.common.Errors._
import pipedsl.common.Syntax.Latency.{Asynchronous, Combinational, Latency, Sequential}
import pipedsl.common.Syntax._
import pipedsl.common.Constraints._
import pipedsl.common.Constraints.ImplicitConstraints._
import pipedsl.common.Utilities.{defaultReadPorts, defaultWritePorts, degenerify, fopt_func, is_generic, is_my_generic, specialize, typeMap, typeMapFunc, typeMapModule, without_prefix}
import pipedsl.typechecker.Environments.{EmptyTypeEnv, Environment, TypeEnv}
import pipedsl.typechecker.Subtypes.{canCast, isSubtype}
import com.microsoft.z3.{Status, AST => Z3AST, ArithExpr => Z3ArithExpr, BoolExpr => Z3BoolExpr, Context => Z3Context, IntExpr => Z3IntExpr, Solver => Z3Solver}
import TBitWidthImplicits._
import pipedsl.codegen.bsv.ConstraintsToBluespec.to_provisos

import scala.collection.mutable
import scala.language.implicitConversions

object TypeInferenceWrapper
 {
  type Subst = List[(Id, Type)]
  type bool = Boolean


  def apply_subst_typ(subst: Subst, t: Type): Type = subst.foldLeft[Type](t)((t1, s) => subst_into_type(s._1, s._2, t1))

  private implicit class PipelineContainer[F](val value: F) {
   def |>[G] (f: F => G) :G = f(value)
  }


  private def to_width(tp : Type): TBitWidth = tp.matchOrError(tp.pos, "width", "TBitWidth")
   {case w : TBitWidth => w}

  private def to_sign(tp : Type): TSignedNess = tp.matchOrError(tp.pos, "width", "TBitWidth")
  {case s : TSignedNess => s}

  private def to_len(tp :Type) :Int = tp.matchOrError(tp.pos, "len", "TBitWidthLen")
  {case l : TBitWidthLen => l.len}

  private def subst_into_type(typevar: Id, toType: Type, inType: Type): Type = inType match
  {
   case t: TMemType => t.copy(elem = subst_into_type(typevar, toType, t.elem)).setPos(t.pos)
   case t1@TLockedMemType(t2: TMemType, _, _) => t1.copy(t2.copy(elem = subst_into_type(typevar, toType, t2.elem))).setPos(t1.pos)
   case TSizedInt(len, signedness) =>
    val width = subst_into_type(typevar, toType, len) |> to_width
    val sign = subst_into_type(typevar, toType, signedness) |> to_sign
    TSizedInt(width, sign).setPos(inType.pos)
   case TInteger() => inType
   case TString() => inType
   case TBool() => inType
   case TVoid() => inType
   case TSigned() => inType
   case TUnsigned() => inType
   case TMaybe(btyp) => TMaybe(subst_into_type(typevar, toType, btyp))
   case TRecType(_,_) => inType //TODO
   case TFun(args, ret) => TFun(args.map(a => subst_into_type(typevar, toType, a)), subst_into_type(typevar, toType, ret)).setPos(inType.pos)
   case TNamedType(name) => if (name == typevar) toType else inType
   case TSignVar(name) => if (name == typevar) toType else inType
   case TModType(inputs, refs, retType, name) => TModType(inputs.map(i => subst_into_type(typevar, toType, i)), refs.map(r => subst_into_type(typevar, toType, r)), retType match
   { case Some(value) => Some(subst_into_type(typevar, toType, value))
    case None => None
   }, name).setPos(inType.pos)
   case TObject(_, _, _) => inType //TODO support these properly once we add polymorphism
   case TRequestHandle(_, _) => inType //TODO do we ever need to sub into this?
   case t: TBitWidth => t match
   {
    case TBitWidthVar(name) =>
     if(name == typevar && is_my_generic(name) && toType != inType)
      throw NotSoGenericAreWe(name, toType)
     if (name == typevar) toType else inType
    case TBitWidthLen(_) => inType
    case TBitWidthAdd(b1, b2) =>
     val w1 = subst_into_type(typevar, toType, b1) |> to_width
     val w2 = subst_into_type(typevar, toType, b2) |> to_width
     TBitWidthAdd(w1, w2).setPos(inType.pos)
    case TBitWidthSub(b1, b2) =>
     val w1 = subst_into_type(typevar, toType, b1) |> to_width
     val w2 = subst_into_type(typevar, toType, b2) |> to_width
     TBitWidthSub(w1, w2).setPos(inType.pos)
    case TBitWidthMax(b1, b2) =>
     val t1 = TBitWidthMax(subst_into_type(typevar, toType, b1) |> to_width,
      subst_into_type(typevar, toType, b2) |> to_width)
     t1 match
     {
      case t1: TBitWidthMax =>
       (t1.b1, t1.b2) match
       {
        case (TBitWidthLen(len), _: TBitWidthVar) => TBitWidthLen(len).setPos(inType.pos)
        case (_: TBitWidthVar, TBitWidthLen(len)) => TBitWidthLen(len).setPos(inType.pos)
        case (TBitWidthVar(v1), TBitWidthVar(v2)) if v1.v == v2.v => TBitWidthVar(v1)
        case _ => t1.setPos(inType.pos)
       }
     }
   }
  }

  class TypeInference(autocast: bool)
   {
    private var currentDef: Id = Id("-invalid-")
    private var counter = 0

    private val add_map = mutable.HashMap[(String, String), Id]()
    private val min_map = mutable.HashMap[String, Int]()
    private var context = new Z3Context()
    private var solver = context.mkSolver()


    private var constraints = List[Constraint]()
    //we'll keep a local list of the statements, and check it at the end w/ z3

    private def type_subst_map_fopt(t :Type, tp_mp: mutable.HashMap[Id, Type], templated :mutable.HashSet[Id] = mutable.HashSet.empty) :Option[Type] = try
     {
      Some(type_subst_map(t, tp_mp, templated))
     } catch
     {
      case IntWidthNotSpecified() => None
     }

    private def type_subst_map(t: Type, tp_mp: mutable.HashMap[Id, Type], templated :mutable.HashSet[Id]): Type =
     {
      t match
      {
       case TSignVar(nm) => tp_mp.get(nm) match
       {
        case Some(value) => type_subst_map(value, tp_mp, templated)
       }
       case sz@TSizedInt(len, sign) =>
        val width = type_subst_map(len, tp_mp, templated).copyMeta(sz) |> to_width
        val sn = type_subst_map(sign, tp_mp, templated).copyMeta(sign) |> to_sign
        sz.copy(len = width, sign = sn)
       case f@TFun(args, ret) => f.copy(args = args.map(type_subst_map(_, tp_mp, templated)), ret = type_subst_map(ret, tp_mp, templated)).copyMeta(f)
       case r@TRecType(_, fields) => r.copy(fields = fields.map(idtp => (idtp._1, type_subst_map(idtp._2, tp_mp, templated)))).copyMeta(r)
       case m: TMemType => m.copy(elem = type_subst_map(m.elem, tp_mp, templated)).copyMeta(m)
       case m@TModType(inputs, refs, _, _) => m.copy(inputs = inputs.map(type_subst_map(_, tp_mp, templated)), refs = refs.map(type_subst_map(_, tp_mp, templated))).copyMeta(m)
       case l@TLockedMemType(mem, _, _) => l.copy(mem = type_subst_map(mem, tp_mp, templated).asInstanceOf[TMemType]).copyMeta(l)
       case TNamedType(name) if /*templated.contains(name)*/ is_my_generic(name) => t
       case TNamedType(name)  => tp_mp.get(name) match
       {
        case Some(value) => type_subst_map(value, tp_mp, templated)
        case None => throw IntWidthNotSpecified()
       }
       case m@TMaybe(btyp) => m.copy(btyp = type_subst_map(btyp, tp_mp, templated))
       case tba@TBitWidthAdd(b1, b2) =>
        val len1 = type_subst_map(b1, tp_mp, templated)
        val len2 = type_subst_map(b2, tp_mp, templated)
        (len1, len2) match {
         case (TBitWidthLen(l1), TBitWidthLen(l2)) => TBitWidthLen(l1 + l2)
         case (l1:TBitWidth, l2:TBitWidth) =>
           val id = Id(tba.stringRep())
           //val id = Id("_A" + lst.head + "_ADD_" + lst(1))
           //add_map.addOne((lst.head, lst(1)), id)
           constraints = constraints.prepended(
            ReEq(IntAdd(IntVar(Id(l1.stringRep())), IntVar(Id(l2.stringRep()))), IntVar(id)))
           TBitWidthVar(id)
        }
       case tbs@TBitWidthSub(b1, b2) =>
        val len1 = type_subst_map(b1, tp_mp, templated)
        val len2 = type_subst_map(b2, tp_mp, templated)
        (len1, len2) match {
         case (TBitWidthLen(l1), TBitWidthLen(l2)) => TBitWidthLen(l1 + l2)
         case (l1 :TBitWidth, l2 :TBitWidth) =>
          val id = Id(tbs.stringRep())
          constraints = constraints.prepended(
           ReEq(IntSub(IntVar(Id(l1.stringRep())), IntVar(Id(l2.stringRep()))), IntVar(id)))
           TBitWidthVar(id)
        }
       case TBitWidthMax(b1, b2) =>
        val len1 = type_subst_map(b1, tp_mp, templated) |> to_len
        val len2 = type_subst_map(b2, tp_mp, templated) |> to_len
        TBitWidthLen(Math.max(len1, len2))
       case TBitWidthVar(name) if /*templated.contains(name)*/ is_my_generic(name) => t
       case TBitWidthVar(name) => tp_mp.get(name) match
       {
        case Some(value) => type_subst_map(value, tp_mp, templated)
        case None =>
         throw IntWidthNotSpecified()
       }
       case o@TObject(name, typParams, methods) =>
        val tmp = o.copy(typParams = typParams.map(type_subst_map(_, tp_mp, templated)),
         methods = methods.map(id_funlat =>
          (id_funlat._1, (type_subst_map(id_funlat._2._1, tp_mp, templated).asInstanceOf[TFun], id_funlat._2._2))))
          .copyMeta(o)
        tmp
       case _ => t
      }
     }



    def checkProgram(p: Prog): Prog =
     {
      val extEnv = p.exts.foldLeft[Environment[Id, Type]](TypeEnv())((env, ext) => {
       val ftyps = ext.methods.foldLeft(Map[Id, (TFun, Latency)]())((ms, m) => {
        val typList = m.args.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ })
        val ftyp = TFun(typList, m.ret)
        ms + (m.name -> (ftyp, m.lat))
       })
       val typ = TObject(ext.name, ext.typParams, ftyps)
       ext.typ = Some(typ)
       env.add(ext.name, typ)
      })
      val (funcEnvs, newFuncs) = p.fdefs.foldLeft[(Environment[Id, Type], List[FuncDef])]((extEnv, List.empty[FuncDef]))((envNlst: (Environment[Id, Type], List[FuncDef]), f) =>
       {
        val env = envNlst._1
        val lst = envNlst._2
        currentDef = f.name
        val (nenv, nfunc) = checkFunc(f, env.asInstanceOf[TypeEnv])
        (nenv, lst.prepended(nfunc))
       })
      val (modEnvs, newMods) = p.moddefs.foldLeft[(Environment[Id, Type], List[ModuleDef])]((funcEnvs, List.empty[ModuleDef]))((envNlst, m) =>
       {
        val env = envNlst._1
        val lst = envNlst._2
        currentDef = m.name
        val (nenv, nmod) = checkModule(m, env.asInstanceOf[TypeEnv])
        (nenv, lst.prepended(nmod))
       })
      val (_, newCirc) = checkCircuit(p.circ, modEnvs)
      p.copy(fdefs = newFuncs.reverse, moddefs = newMods.reverse, circ = newCirc)
     }

    def checkCircuit(c: Circuit, tenv: Environment[Id, Type]): (Environment[Id, Type], Circuit) = c match
    {
     case cs@CirSeq(c1, c2) => val (e1, nc1) = checkCircuit(c1, tenv)
      val (e2, nc2) = checkCircuit(c2, e1)
      (e2, cs.copy(c1 = nc1, c2 = nc2).setPos(cs.pos))
     case cc@CirConnect(name, ce) => val (t, env2, nce) = checkCirExpr(ce, tenv)
      (env2.add(name, t), cc.copy(c = nce).setPos(cc.pos))
     case ces@CirExprStmt(ce) => val (_, nv, nce) = checkCirExpr(ce, tenv)
      (nv, ces.copy(ce = nce).setPos(ces.pos))
    }

    def checkModule(m: ModuleDef, env: TypeEnv): (Environment[Id, Type], ModuleDef) =
     {
      val inputTypes = m.inputs.map(p => p.typ)
      val modTypes = m.modules.map(m => replaceNamedType(m.typ, env))
      val mod_subs = m.modules.zip(modTypes).foldLeft(List() :Subst)((subst, types) =>
      compose_subst(subst, unify(types._1.typ, types._2)._1))
      val modEnv = env.add(m.name, TModType(inputTypes, modTypes, m.ret, Some(m.name)))
      val inEnv = m.inputs.foldLeft[Environment[Id, Type]](modEnv)((env, p) => env.add(p.name, p.typ))
      val pipeEnv = m.modules.zip(modTypes).foldLeft[Environment[Id, Type]](inEnv)((env, m) => env.add(m._1.name, m._2))
      val (fixed_cmd, _, subst) = checkCommand(m.body, pipeEnv.asInstanceOf[TypeEnv], mod_subs)
      val hash = mutable.HashMap.from(subst)
      val newMod = typeMapModule(m.copy(body = fixed_cmd).copyMeta(m), fopt_func(type_subst_map_fopt(_, hash)))

      val new_input = newMod.inputs.map(p => p.typ)
      val new_mod_tps = newMod.modules.map(m => replaceNamedType(m.typ, env))

      val fin_env = modEnv.remove(m.name)
        .add(newMod.name, TModType(new_input, new_mod_tps, newMod.ret, Some(newMod.name)))
      (fin_env, newMod)
     }

    def checkFunc(f: FuncDef, env: TypeEnv): (Environment[Id, Type], FuncDef) =
     {
      add_map.clear()
      min_map.clear()
      context = new Z3Context()
      solver = context.mkSolver()
      constraints = List()
      val inputTypes = f.args.map(a => a.typ)
      val funType = TFun(inputTypes, f.ret)
      val funEnv = env.add(f.name, funType)
      val templated = mutable.HashSet.from(f.templateTypes)
      val template_vals_env = templated.foldLeft(funEnv)((env, a) => env.add(without_prefix(a), TInteger()))
      val inEnv = f.args.foldLeft[Environment[Id, Type]](template_vals_env)((env, a) => env.add(a.name, a.typ))
      val (fixed_cmd, _, subst) = checkCommand(f.body, inEnv.asInstanceOf[TypeEnv], List())
      constraints = reduce_constraint_list(constraints)
      solver.push()
      constraints.foreach(c => solver.add(to_z3(context, c)))
      val stat = solver.check()
      solver.pop()
      stat match
      {
       case Status.UNSATISFIABLE |
            Status.UNKNOWN => throw new RuntimeException("Too many constraints!!")//todo throw too many constraints!!
       case Status.SATISFIABLE =>
      }
      val hash = mutable.HashMap.from(subst)
      val newFunc = typeMapFunc(f.copy(body = fixed_cmd,
       args = f.args.map(p => p.copy(typ = type_subst_map(p.typ, hash, templated))),
       ret = type_subst_map(f.ret, hash, templated)).setPos(f.pos), fopt_func(type_subst_map_fopt(_, hash, templated)))
      /*TODO: add another pass over the types to make substitutions for bit expressions.
              Collect them as provisos*/
      newFunc.adds.addAll(add_map)
      newFunc.constraints = newFunc.constraints ++ constraints
      newFunc.mins.addAll(min_map)
      (funEnv, newFunc)
     }

    private var unique_count = 0
    private def uniquify_gen(value: Type) :Type = value match {
     case TNamedType(name) if is_generic(name) =>  TNamedType(Id(name + unique_count.toString + "*")).copyMeta(value)
     case TBitWidthVar(name) if is_generic(name) => TBitWidthVar(Id(name + unique_count.toString + "*")).copyMeta(value)
     case s@TSizedInt(len, _) => s.copy(len = uniquify_gen(len).asInstanceOf[TBitWidth]).copyMeta(value)
     case _ => value
    }

    private def uniquify_fun(t : TFun) :TFun =
     {
      unique_count += 1
      TFun(ret = uniquify_gen(t.ret), args = t.args.map(uniquify_gen))
     }

    private def uniquify_obj(t : TObject) :TObject =
     {
      unique_count += 1
      val methods = t.methods.map(idfunlat =>
       {
        unique_count -= 1;
        (idfunlat._1, (idfunlat._2._1 |> uniquify_fun, idfunlat._2._2))
       })
      val types = t.typParams.map(uniquify_gen)
      t.copy(methods = methods, typParams = types).copyMeta(t).asInstanceOf[TObject]
     }

    /** INVARIANTS
     * Transforms the argument sub by composing any additional substitution
     * Transforms the argument env by subbing in the returned substitution and adding any relevant variables */
    def checkCommand(c: Command, env: TypeEnv, sub: Subst): (Command, TypeEnv, Subst) = c match
    {
     case CLockOp(mem, _, _, _, _) => env(mem.id) match
     {
      case tm: TMemType => mem.evar match
      {
       case Some(value) => val (s, t, e, _) = infer(env, value)
        val tempSub = compose_subst(sub, s)
        val tNew = apply_subst_typ(tempSub, t)
        val newSub = compose_subst(tempSub, unify(tNew, TSizedInt(TBitWidthLen(tm.addrSize), TUnsigned()))._1)
        (c, e.apply_subst_typeenv(newSub), newSub)
       case None => (c, env, sub)
      }
      case TLockedMemType(tm: TMemType, _, _) => mem.evar match
      {
       case Some(value) => val (s, t, e, _) = infer(env, value)
        val tempSub = compose_subst(sub, s)
        val tNew = apply_subst_typ(tempSub, t)
        val newSub = compose_subst(tempSub, unify(tNew, TSizedInt(TBitWidthLen(tm.addrSize), TUnsigned()))._1)
        (c, e.apply_subst_typeenv(newSub), newSub)
       case None => (c, env, sub)
      }
      case _: TModType => if (mem.evar.isDefined) throw MalformedLockTypes("Pipeline modules can not have specific locks")
       (c, env, sub)
      case b => throw UnexpectedType(mem.id.pos, c.toString, "Memory or Module Type", b)
     }
     case CEmpty() => (c, env, sub)
     case cr@CReturn(exp) =>
      val (s, t, e, fixed) = infer(env, exp)
      val tempSub = compose_subst(sub, s)
      val tNew = apply_subst_typ(tempSub, t)
      val funT = env(currentDef)
      funT match
      {
       case TFun(_, ret) => val (subst, cast) = unify(tNew, ret)
        val more_fixed = if (cast)
         {
          val tmp = ECast(ret, fixed)
          tmp.typ = Some(tmp.ctyp)
          tmp
         } else
         {
          fixed.typ = Some(tNew)
          fixed
         }
        val retSub = compose_subst(tempSub, subst)
        (cr.copy(exp = more_fixed).copyMeta(cr), e.apply_subst_typeenv(retSub), retSub)
       case b => throw UnexpectedType(c.pos, c.toString, funT.toString, b)
      }
     case CLockStart(mod) => if (!(env(mod).isInstanceOf[TMemType] || env(mod).isInstanceOf[TModType] || env(mod).isInstanceOf[TLockedMemType]))
      {
       throw UnexpectedType(mod.pos, c.toString, "Memory or Module Type", env(mod))
      }
      (c, env, sub)
     case i@CIf(cond, cons, alt) => val (condS, condT, env1, fixed_cond) = infer(env, cond)
      val tempSub = compose_subst(sub, condS)
      val condTyp = apply_subst_typ(tempSub, condT)
      val newSub = compose_subst(tempSub, unify(condTyp, TBool())._1)
      val newEnv = env1.apply_subst_typeenv(newSub)
      val (fixed_cons, consEnv, consSub) = checkCommand(cons, newEnv, newSub)
      val newEnv2 = newEnv.apply_subst_typeenv(consSub)
      val (fixed_alt, altEnv, altSub) = checkCommand(alt, newEnv2, consSub)
      (i.copy(cond = fixed_cond, cons = fixed_cons, alt = fixed_alt).copyMeta(i), consEnv.apply_subst_typeenv(altSub).intersect(altEnv).asInstanceOf[TypeEnv], altSub)
     case CLockEnd(mod) => if (!(env(mod).isInstanceOf[TMemType] || env(mod).isInstanceOf[TModType] || env(mod).isInstanceOf[TLockedMemType]))
      {
       throw UnexpectedType(mod.pos, c.toString, "Memory or Module Type", env(mod))
      }
      (c, env, sub)
     case cs@CSplit(cases, default) => var (fixed_def, runningEnv, runningSub) = checkCommand(default, env, sub)
      var fixed_cases: List[CaseObj] = List()
      for (c <- cases)
       {
        val (condS, condT, env1, fixed_cond) = infer(env, c.cond)
        val tempSub = compose_subst(runningSub, condS)
        val condTyp = apply_subst_typ(tempSub, condT)
        val newSub = compose_subst(tempSub, unify(condTyp, TBool())._1)
        val newEnv = env1.apply_subst_typeenv(newSub)
        val (fixed_bod, caseEnv, caseSub) = checkCommand(c.body, newEnv, newSub)
        fixed_cases = fixed_cases :+ c.copy(cond = fixed_cond, body = fixed_bod).setPos(c.pos)
        runningSub = caseSub
        runningEnv = runningEnv.apply_subst_typeenv(runningSub).intersect(caseEnv).asInstanceOf[TypeEnv]
       }
      (cs.copy(cases = fixed_cases, default = fixed_def).copyMeta(cs), runningEnv, runningSub)
     case ce@CExpr(exp) => val (s, _, e, fixed) = infer(env, exp)
      val retS = compose_subst(sub, s)
      (ce.copy(exp = fixed).copyMeta(ce), e.apply_subst_typeenv(retS), retS)
     case CCheckSpec(_) => (c, env, sub)
     case c@CVerify(handle, args, preds, update) =>
      update.map(c =>
      {
       infer(env, c)

      })
      val (s, a) = args.foldLeft(sub, List[Expr]())((sublst, exp) => {
       val (s, t, e, fixed) = infer(env, exp)
       val tempSub = compose_subst(sublst._1, s)
       val tNew = apply_subst_typ(tempSub, t)
       (tempSub, fixed::sublst._2)
      })
      (c.copy(args = a.reverse), env, s)
     case c@CUpdate(_, _, args, _) =>
      val (s, a) = args.foldLeft(sub, List[Expr]())((sublst, exp) => {
       val (s, t, e, fixed) = infer(env, exp)
       val tempSub = compose_subst(sublst._1, s)
       val tNew = apply_subst_typ(tempSub, t)
       (tempSub, fixed::sublst._2)
      })
      (c.copy(args = a.reverse), env, s)
     case CInvalidate(_) => (c, env, sub)
     case ct@CTBar(c1, c2) => val (fixed1, e, s) = checkCommand(c1, env, sub)
      val (fixed2, e2, s2) = checkCommand(c2, e, s)
      (ct.copy(c1 = fixed1, c2 = fixed2).copyMeta(ct), e2, s2)
     case CPrint(_) => (c, env, sub)
     case CSpecCall(h,_,_) => //TODO maybe wrong?
      //add spec handle type to env
      (c, env.add(h.id, h.typ.get).asInstanceOf[TypeEnv], sub)
     case CCheckpoint(handle, lock) =>
      //add checkpoint handle type to env
      (c, env.add(handle.id, handle.typ.get).asInstanceOf[TypeEnv], sub)
     case co@COutput(exp) =>
      val (s, t, e, fixed) = infer(env, exp)
      val tempSub = compose_subst(sub, s)
      val tNew = apply_subst_typ(tempSub, t)
      val modT = env(currentDef)
      modT match
      {
       case tm: TModType => tm.retType match
       {
        case Some(value) => val (subst, cast) = unify(tNew, value)
         val fixed1 = if (cast) ECast(value, fixed) else fixed
         val retSub = compose_subst(tempSub, subst)
         (co.copy(exp = fixed1).copyMeta(co), e.apply_subst_typeenv(retSub), retSub)
        case None => (co.copy(exp = fixed).copyMeta(co), e.apply_subst_typeenv(tempSub), tempSub)
       }
       case b => throw UnexpectedType(c.pos, c.toString, modT.toString, b)
      }
     case cr@CRecv(lhs, rhs) =>
      val typ = lhs.typ
      val (slhs, tlhs, lhsEnv, lhsFixed) = lhs match
     {
      case EVar(_) => (List(), typ.getOrElse(generateTypeVar()), env, lhs)
      case _ => infer(env, lhs)
     }
      val (srhs, trhs, rhsEnv, rhsFixed) = infer(lhsEnv, rhs)
      val tempSub = compose_many_subst(sub, slhs, srhs)
      val lhstyp = apply_subst_typ(tempSub, tlhs)
      val rhstyp = apply_subst_typ(tempSub, trhs)

      val (s1, cast) = unify(rhstyp, lhstyp)
      val rhsFixed1 = if (cast) ECast(lhstyp, rhsFixed) else rhsFixed
      val sret = compose_many_subst(tempSub, s1, typ match
      { case Some(value) => val (s2, _) = unify(lhstyp, value)
        val (s3, _) = unify(rhstyp, value)
        compose_subst(s2, s3)
       case None => List()
      })

      lhs.typ = Some(apply_subst_typ(s1, lhstyp))
      rhs.typ = Some(apply_subst_typ(s1, rhstyp))
      val newEnv = lhs match
      {
       case EVar(id) => rhsEnv.add(id, tlhs)
       case _ => rhsEnv
      }
      (cr.copy(lhs = lhsFixed, rhs = rhsFixed1).copyMeta(cr), newEnv.asInstanceOf[TypeEnv].apply_subst_typeenv(sret), sret)
     case ca@CAssign(lhs, rhs) =>
      val typ = lhs.typ
      val (slhs, tlhs, lhsEnv) = (List(), typ.getOrElse(generateTypeVar()), env)
      val (srhs, trhs, rhsEnv, rhsFixed) = infer(lhsEnv, rhs)
      val tempSub = compose_many_subst(sub, slhs, srhs)
      val lhstyp = apply_subst_typ(tempSub, tlhs)
      val rhstyp = apply_subst_typ(tempSub, trhs)
      val (s1, cast) = unify(rhstyp, lhstyp)
      val rhsFixed1 = if (cast) ECast(tlhs, rhsFixed) else rhsFixed
      val sret = compose_many_subst(tempSub, s1, typ match
      { case Some(value) => val (s2, _) = unify(lhstyp, value)
        val (s3, _) = unify(rhstyp, value)
        compose_subst(s2, s3)
       case None => List()
      })
      val newEnv = lhs match
      {
       case EVar(id) => rhsEnv.remove(id).add(id, tlhs)
       case _ => rhsEnv
      }
      lhs.typ = Some(apply_subst_typ(s1, lhstyp))
      lhs.id.typ = lhs.typ
      rhs.typ = Some(apply_subst_typ(s1, rhstyp))
      (ca.copy(rhs = rhsFixed1).copyMeta(ca), newEnv.asInstanceOf[TypeEnv].apply_subst_typeenv(sret), sret)
     case cs@CSeq(c1, c2) => val (fixed1, e1, s) = checkCommand(c1, env, sub)
      val (fixed2, e2, s2) = checkCommand(c2, e1, s)
      (cs.copy(c1 = fixed1, c2 = fixed2).copyMeta(cs), e2, s2)
     case _: InternalCommand => (c, env, sub)
    }

    /** for subtyping bit widths, t1 is the subtype, t2 is the supertype. so t2 is the expected private */
    def unify(a: Type, b: Type, binop: bool = false): (Subst, bool) =
     {
      val ret = (a, b) match
      {
       case (t1: TNamedType, t2) => if (!occursIn(t1.name, t2)) (List((t1.name, t2)), false) else (List(), false)
       case (t1, t2: TNamedType) => if (!occursIn(t2.name, t1)) (List((t2.name, t1)), false) else (List(), false)
       case (t1: TSignVar, t2: TSignedNess) => if (!occursIn(t1.id, t2)) (List((t1.id, t2)), false) else (List(), false)
       case (t1: TSignedNess, t2: TSignVar) => if (!occursIn(t2.id, t1)) (List((t2.id, t1)), false) else (List(), false)
       case (_: TString, _: TString) => (List(), false)
       case (_: TBool, _: TBool) => (List(), false)
       case (_: TVoid, _: TVoid) => (List(), false)
       case (_: TSigned, _: TSigned) => (List(), false)
       case (_: TUnsigned, _: TUnsigned) => (List(), false)
       case (_: TObject, _: TObject) => (List(), false) //TODO change once we support polymorphism
       case (TBool(), TSizedInt(len, u)) if len.getLen == 1 && u.unsigned() => (List(), false)
       case (TSizedInt(len, u), TBool()) if len.getLen == 1 && u.unsigned() => (List(), false)
       case (TSizedInt(len1, signed1), TSizedInt(len2, signed2)) => val (s1, c1) = unify(len1, len2, binop)
        val (s2, c2) = unify(signed1, signed2, binop)
        (compose_subst(s1, s2), c1 || c2)
       case (TFun(args1, ret1), TFun(args2, ret2)) if args1.length == args2.length => val (s1, c1) = args1.zip(args2).foldLeft[(Subst, bool)]((List(), false))((sc, t) =>
        {
         val (unif_s, unif_c) = unify(apply_subst_typ(sc._1, t._1), apply_subst_typ(sc._1, t._2), binop)
         (compose_subst(sc._1, unif_s), unif_c || sc._2)
        })
        val (s2, c2) = unify(apply_subst_typ(s1, ret1), apply_subst_typ(s1, ret2), binop)
        (compose_subst(s1, s2), c1 || c2)
       case (TModType(input1, refs1, retType1, name1), TModType(input2, refs2, retType2, name2)) => //TODO: Name?\ if (name1 != name2) throw UnificationError(a, b)
        if (name1 != name2) throw UnificationError(a, b)
        val (s1, c1) = input1.zip(input2).foldLeft[(Subst, bool)]((List(), false))((sc, t) =>
         {
          val (unif_s, unif_c) = unify(apply_subst_typ(sc._1, t._1), apply_subst_typ(sc._1, t._2))
          (compose_subst(sc._1, unif_s), unif_c || sc._2)
         })
        val (s2, c2) = refs1.zip(refs2).foldLeft[(Subst, bool)](s1, c1)((sc, t) =>
         {
          val (unif_s, unif_c) = unify(apply_subst_typ(sc._1, t._1), apply_subst_typ(sc._1, t._2))
          (compose_subst(sc._1, unif_s), sc._2 || unif_c)
         })
        val (s3, c3) = (retType1, retType2) match
        {
         case (Some(t1: Type), Some(t2: Type)) => unify(apply_subst_typ(s2, t1), apply_subst_typ(s2, t2))
         case (None, None) => (List(), false)
         case _ => throw UnificationError(a, b)
        }
        (compose_subst(s2, s3), c2 || c3)
       case (TMemType(elem1, addr1, rl1, wl1, rp1, wp1), TMemType(elem2, addr2, rl2, wl2, rp2, wp2)) => if (addr1 != addr2 || rl1 != rl2 || wl1 != wl2 || rp1 < rp2 || wp1 < wp2) throw UnificationError(a, b)
        unify(elem1, elem2)
       case (t1 :TBitWidthVar, t2 :TBitWidthVar) if t1.name == t2.name =>
        (List(), false)
       case (t1 :TBitWidthVar, t2 :TBitWidthVar) if is_my_generic(t2) => if (!occursIn(t1.name, t2)) (List((t1.name, t2)), false) else (List(), false)
       case (t1 :TBitWidthVar, t2 :TBitWidthVar) if is_my_generic(t1) => if (!occursIn(t2.name, t1)) (List((t2.name, t1)), false) else (List(), false)
       case (t1 :TBitWidthVar, t2 :TBitWidthVar) if is_generic(t2) => if (!occursIn(t1.name, t2)) (List((t1.name, t2)), false) else (List(), false)
       case (t1 :TBitWidthVar, t2 :TBitWidthVar) if is_generic(t1) => if (!occursIn(t2.name, t1)) (List((t2.name, t1)), false) else (List(), false)

       case (TBitWidthAdd(a1: TBitWidthLen, a2), TBitWidthLen(len)) => unify(a2, TBitWidthLen(len - a1.len), binop)
       case (TBitWidthAdd(a2, a1: TBitWidthLen), TBitWidthLen(len)) => unify(a2, TBitWidthLen(len - a1.len), binop)
       case (TBitWidthLen(len), TBitWidthAdd(a1: TBitWidthLen, a2)) => unify(a2, TBitWidthLen(len - a1.len), binop)
       case (TBitWidthLen(len), TBitWidthAdd(a2, a1: TBitWidthLen)) => unify(a2, TBitWidthLen(len - a1.len), binop)

       case (t1: TBitWidthVar, t2: TBitWidth) => if (!occursIn(t1.name, t2)) (List((t1.name, t2)), false) else (List(), false)
       case (t1: TBitWidth, t2: TBitWidthVar) => if (!occursIn(t2.name, t1)) (List((t2.name, t1)), false) else (List(), false)
//       case (TBitWidthAdd(a1: TBitWidthVar, a2: TBitWidthVar), TBitWidthLen(len)) => if (len % 2 != 0) throw new RuntimeException(s"result of bitwidthadd should be even. Consider multiply. Found $len")
//        val (s1, c1) = unify(a1, TBitWidthLen(len / 2), binop)
//        val (s2, c2) = unify(a2, TBitWidthLen(len / 2), binop)
//        (compose_subst(s1, s2), c1 || c2)
//       case (TBitWidthLen(len), TBitWidthAdd(a1: TBitWidthVar, a2: TBitWidthVar)) => if (len % 2 != 0) throw new RuntimeException(s"result of bitwidthadd should be even. Consider multiply. Found $len")
//        val (s1, c1) = unify(a1, TBitWidthLen(len / 2), binop)
//        val (s2, c2) = unify(a2, TBitWidthLen(len / 2), binop)
//        (compose_subst(s1, s2), c1 || c2)


       case (t1: TBitWidthLen, t2: TBitWidthLen) => if (autocast)
        {
         if (binop) (List(), t1.len != t2.len) else if (t2.len < t1.len) throw UnificationError(t1, t2) else (List(), t1.len != t2.len)
        } else
        {
         if (t2.len != t1.len) throw UnificationError(t1, t2) else (List(), false)
        }
       case (t1 :TBitWidth, t2 :TBitWidth) =>
        constraints = constraints.prepended(ReEq(t1, t2))
        (List(), binop)
       case (other1, other2) if other1 == other2 =>
        (List(), false)
       case _ => throw UnificationError(a, b)
      }
      ret
     }

    private def checkCirExpr(c: CirExpr, tenv: Environment[Id, Type]): (Type, Environment[Id, Type], CirExpr) = c match
    {
     case CirMem(elemTyp, addrSize, numPorts) => if (numPorts > 2) throw TooManyPorts(c.pos, 2)
      val mtyp = TMemType(elemTyp, addrSize, Asynchronous, Asynchronous, numPorts, numPorts)
      c.typ = Some(mtyp)
      (mtyp, tenv, c)
     case CirLock(mem, impl, _) => val mtyp: TMemType = tenv(mem).matchOrError(mem.pos, "lock instantiation", "memory")
     { case c: TMemType => c }
      mem.typ = Some(mtyp)
      val newtyp = TLockedMemType(mtyp, None, impl)
      c.typ = Some(newtyp)
      (newtyp, tenv, c)
     case CirLockMem(elemTyp, addrSize, impl, _, numPorts) => val mtyp = TMemType(elemTyp, addrSize, Asynchronous, Asynchronous, numPorts, numPorts)
      val ltyp = TLockedMemType(mtyp, None, impl)
      c.typ = Some(ltyp)
      (ltyp, tenv, c)
     case CirRegister(elemTyp, _) => val mtyp = TMemType(elemTyp, 0, Combinational, Sequential, 0, 0)
      c.typ = Some(mtyp)
      (mtyp, tenv, c)
     case CirRegFile(elemTyp, addrSize) => val mtyp = TMemType(elemTyp, addrSize, Combinational, Sequential, defaultReadPorts, defaultWritePorts)
      c.typ = Some(mtyp)
      (mtyp, tenv, c)
     case CirLockRegFile(elemTyp, addrSize, impl, szParams) => val mtyp = TMemType(elemTyp, addrSize, Combinational, Sequential, defaultReadPorts, defaultWritePorts)
      val idsz = szParams.headOption
      val ltyp = TLockedMemType(mtyp, idsz, impl)
      c.typ = Some(ltyp)
      (ltyp, tenv, c)
     case CirNew(mod, specialized, mods, _) => val mtyp = specialize(tenv(mod), specialized)
      mtyp match
      {
       case TModType(_, refs, _, _) => if (refs.length != mods.length) throw ArgLengthMismatch(c.pos, mods.length, refs.length)
        refs.zip(mods).foreach
        { case (reftyp, mname) => if (!isSubtype(tenv(mname), reftyp)) throw UnexpectedSubtype(mname.pos, mname.toString, reftyp, tenv(mname)) }
        (mtyp, tenv, c)
       case _: TObject => (mtyp, tenv, c)
       case x => throw UnexpectedType(c.pos, c.toString, "Module Type", x)
      }
     case cc@CirCall(mod, inits) => val mtyp = tenv(mod)
      mtyp match
      {
       case TModType(ityps, _, _, _) => if (ityps.length != inits.length) throw ArgLengthMismatch(c.pos, inits.length, ityps.length)
        val fixed_args = ityps.zip(inits).map
        { case (expectedT, arg) => val (_, atyp, _, a_fixed) = infer(tenv.asInstanceOf[TypeEnv], arg)
         if (!isSubtype(atyp, expectedT)) throw UnexpectedSubtype(arg.pos, arg.toString, expectedT, atyp)
         a_fixed
        }
        (mtyp, tenv, cc.copy(args = fixed_args))
       case x => throw UnexpectedType(c.pos, c.toString, "Module Type", x)
      }
    }

    private def replaceNamedType(t: Type, tenv: TypeEnv): Type = t match
    {
     case TNamedType(name) => tenv(name) match {
      case t :TObject => uniquify_obj(t)
      case other => other
     }
     case _ => t
    }

    private def occursIn(name: Id, b: Type): bool = b match
    {
     case TSizedInt(len, _) => occursIn(name, len)
     case TString() => false
     case TVoid() => false
     case TBool() => false
     case TMaybe(t) => occursIn(name, t)
     case TFun(args, ret) => args.foldLeft[bool](false)((b, t) => b || occursIn(name, t)) || occursIn(name, ret)
     case _: TRecType => false
     case _: TMemType => false
     case _: TModType => false
     case TNamedType(name1) => name1 == name
     case TSignVar(name1) => name1 == name
     case TBitWidthVar(name1) => name1 == name
     case TBitWidthAdd(b1, b2) => occursIn(name, b1) || occursIn(name, b2)
     case TBitWidthSub(b1, b2) => occursIn(name, b1) || occursIn(name, b2)
     case TBitWidthMax(b1, b2) => occursIn(name, b1) || occursIn(name, b2)
     case TBitWidthLen(_) => false
     case _: TSignedNess => false
     case _: TObject => false //TODO need?
     case _: TRequestHandle => false //TODO need?
     case TLockedMemType(_, _, _) => false
    }

    private def apply_subst_substs(subst: Subst, inSubst: Subst): Subst = inSubst.foldLeft[Subst](List())((s, c) => s :+ ((c._1, apply_subst_typ(subst, c._2))))

    private def compose_subst(sub1: Subst, sub2: Subst): Subst = sub1 ++ apply_subst_substs(sub1, sub2)

    private def compose_many_subst(subs: Subst*): Subst = subs.foldRight[Subst](List())((s1, s2) => compose_subst(s1, s2))

    private def binOpExpectedType(b: BOp): Type =
     {
      val tmp = b match
      {
       case EqOp(_) => val t = generateTypeVar() // TODO: This can be anything?
        TFun(List(t, t), TBool())
       case CmpOp(_) => val t = generateTypeVar() // TODO: This can be anything?
        TFun(List(t, t), TBool())
       case _: BoolOp => TFun(List(TBool(), TBool()), TBool())
       case NumOp(op, _) => val b1 = generateBitWidthTypeVar()
        val b2 = generateBitWidthTypeVar()
        val s = generateSignTypeVar()
        op match
        {
         case "/" => TFun(List(TSizedInt(b1, s), TSizedInt(b2, s)), TSizedInt(b1, s))
         case "*" => TFun(List(TSizedInt(b1, s), TSizedInt(b2, s)), TSizedInt(TBitWidthAdd(b1, b2), s))
         case "$*" => TFun(List(TSizedInt(b1, s), TSizedInt(b1, s)), TSizedInt(b1, s))
         case "+" => TFun(List(TSizedInt(b1, s), TSizedInt(b1, s)), TSizedInt(b1, s))
         case "-" => TFun(List(TSizedInt(b1, s), TSizedInt(b1, s)), TSizedInt(b1, s))
         case "%" => TFun(List(TSizedInt(b1, s), TSizedInt(b2, s)), TSizedInt(b1, s))
        }
       case BitOp(op, _) => val b1 = generateBitWidthTypeVar()
        val b2 = generateBitWidthTypeVar()
        val s = generateSignTypeVar()
        op match
        {
         case "++" => TFun(List(TSizedInt(b1, s), TSizedInt(b2, s)), TSizedInt(TBitWidthAdd(b1, b2), s))
         case _ => TFun(List(TSizedInt(b1, s), TSizedInt(b2, generateSignTypeVar())), TSizedInt(b1, s))
        }
      }
      tmp.setPos(b.pos)
     }

    private def generateTypeVar(): TNamedType =
     {
      counter += 1
      TNamedType(Id("__TYPE__" + counter))
     }

    private def generateBitWidthTypeVar(): TBitWidthVar =
     {
      counter += 1
      TBitWidthVar(Id("__BITWIDTH__" + counter))
     }


    private def z3_of_index(index: EIndex) :Z3ArithExpr = index match
    {
     case EIndConst(v) => context.mkInt(v)
     case EIndAdd(l, r) => context.mkAdd(z3_of_index(l), z3_of_index(r))
     case EIndSub(l, r) => context.mkSub(z3_of_index(l), z3_of_index(r))
     case EIndVar(id) => context.mkIntConst(id.v)
    }

    private def z3_of_width(width: TBitWidth) :Z3ArithExpr = width match
    {
     case TBitWidthVar(name) => context.mkIntConst(name.v)
     case TBitWidthLen(len) => context.mkInt(len)
     case TBitWidthAdd(b1, b2) => context.mkAdd(z3_of_width(b1), z3_of_width(b2))
     case TBitWidthMax(b1, b2) => ???
    }

    /** Updating the type environment with the new substitution whenever you generate one allows errors to be found :D
     * The environment returned is guaratneed to already have been substituted into with the returned substitution */
    private def infer(env: TypeEnv, e: Expr): (Subst, Type, TypeEnv, Expr) =
     {
      val ret : (Subst, Type, TypeEnv, Expr) = e match
      {
       case _: EInt => val newvar = generateTypeVar()
        if (e.typ.isEmpty) e.typ = Some(newvar)
        (List(), e.typ.getOrElse(generateTypeVar()), env, e)
       case EString(_) => (List(), TString(), env, e)
       case EBool(_) => (List(), TBool(), env, e)
       case u@EUop(op, ex) => val (s, t, env1, fixed) = infer(env, ex)
        val retType = generateTypeVar()
        val tNew = apply_subst_typ(s, t)
        val (subst, cast) = unify(TFun(List(tNew), retType), uOpExpectedType(op))
        val retSubst = compose_subst(s, subst)
        val retTyp = apply_subst_typ(retSubst, retType)
        val fixed1 = if (cast) ECast(retTyp, fixed) else fixed
        (retSubst, retTyp, env1.apply_subst_typeenv(retSubst), u.copy(ex = fixed1).copyMeta(u))
       case b@EBinop(op, e1, e2) =>
        val (s1, t1, env1, fixed1) = infer(env, e1)
        val (s2, t2, env2, fixed2) = infer(env1, e2)
        val retType = generateTypeVar()
        val subTemp = compose_subst(s1, s2)
        val t1New = apply_subst_typ(subTemp, t1)
        val t2New = apply_subst_typ(subTemp, t2)
        val (subst, _) = unify(TFun(List(t1New, t2New), retType), binOpExpectedType(op), binop = true)
        val retSubst = compose_many_subst(subTemp, subst)
        val retTyp = apply_subst_typ(retSubst, retType)
        val t1VNew = apply_subst_typ(retSubst, t1New)
        val t2VNew = apply_subst_typ(retSubst, t2New)
        val (castL, castR) = binOpTypesFromRet(op, retTyp, t1VNew, t2VNew)
        val moreFixed1 = castL match
        {
         case Some(tp) => assert(autocast)
          ECast(tp, fixed1).copyMeta(fixed1)
         case _ => fixed1
        }
        val moreFixed2 = castR match
        {
         case Some(tp) => assert(autocast)
          ECast(tp, fixed2).copyMeta(fixed2)
         case _ => fixed2
        }
        moreFixed1.typ = Some(castL.getOrElse(t1VNew))
        moreFixed2.typ = Some(castR.getOrElse(t2VNew))
        val finalRetType = generateTypeVar()
        val (finSubst, _) = unify(TFun(List(moreFixed1.typ.get, moreFixed2.typ.get), finalRetType), binOpExpectedType(op), binop = true)
        val finalRetSubst = compose_many_subst(subTemp, finSubst)
        val finalRetTyp = apply_subst_typ(finalRetSubst, finalRetType)
        val bFixed = b.copy(e1 = moreFixed1, e2 = moreFixed2).copyMeta(b)
        bFixed.typ = Some(finalRetTyp)
        (finalRetSubst, finalRetTyp, env2.apply_subst_typeenv(finalRetSubst), bFixed)
       case m@EMemAccess(mem, index, _, _, _, _) => if (!(env(mem).isInstanceOf[TMemType] || env(mem).isInstanceOf[TLockedMemType])) throw UnexpectedType(e.pos, "Memory Access", "TMemtype", env(mem))
        val retType = generateTypeVar()
        val (s, t, env1, fixed_idx) = infer(env, index)
        val tTemp = apply_subst_typ(s, t)
        val memt = env1(mem) match
        {
         case t@TMemType(_, _, _, _, _, _) => t
         case TLockedMemType(t, _, _) => t
         case _ => throw UnexpectedType(e.pos, "Memory Access", "TMemtype", env1(mem))
        }
        val (subst, _) = unify(TFun(List(tTemp), retType), getMemAccessType(memt))
        val retSubst = compose_subst(s, subst)
        val retTyp = apply_subst_typ(retSubst, retType)
        (retSubst, retTyp, env1.apply_subst_typeenv(retSubst), m.copy(index = fixed_idx).copyMeta(m))
       case b@EBitExtract(num, start, end) => val (s, t, en, fixed_num) = infer(env, num)
        t match
        {
         case TSizedInt(bitwidth, signedness) =>
          //constraints = constraints.prepended(ReGe(toConstraint(end), toConstraint(0)))
          constraints = constraints.prepended(ReGe(toConstraint(start), toConstraint(0)))
          constraints = constraints.prepended(ReGe(toConstraint(end), toConstraint(start)))
          constraints = constraints.prepended(ReGe(toConstraint(bitwidth), toConstraint(end)))
//          constraints = constraints.prepended(RelLt(toConstraint(end), toConstraint(bitwidth)))
//          constraints = constraints.prepended(RelLt(toConstraint(start), toConstraint(bitwidth)))
//          constraints = constraints.prepended(RelLt(IntSub(toConstraint(end), toConstraint(start)), toConstraint(bitwidth)))

          (s, TSizedInt(TBitWidthAdd(TBitWidthSub(end,start), 1), signedness), en, b.copy(num = fixed_num).copyMeta(b))
         case b => throw UnificationError(b, TSizedInt(TBitWidthLen(32), TUnsigned())) //TODO Add better error message
        } //TODO
       case trn@ETernary(cond, tval, fval) => val (sc, tc, env1, fixed_cond) = infer(env, cond)
        val (st, tt, env2, fixed_tval) = infer(env1, tval)
        val (sf, tf, env3, fixed_fval) = infer(env2, fval)
        val substSoFar = compose_many_subst(sc, st, sf)
        val tcNew = apply_subst_typ(substSoFar, tc)
        val ttNew = apply_subst_typ(substSoFar, tt)
        val tfNew = apply_subst_typ(substSoFar, tf)
        val (substc, _) = unify(tcNew, TBool())
        val (subst, cast) = unify(ttNew, tfNew)
        val (fixed_tval1, fixed_fval1) = if (cast) if (ttNew <<= tfNew) (ECast(tfNew, fixed_tval), fixed_fval) else (fixed_tval, ECast(ttNew, fixed_fval)) else (fixed_tval, fixed_fval)
        val retSubst = compose_many_subst(sc, st, sf, substc, subst)
        val retType = apply_subst_typ(retSubst, ttNew)
        (retSubst, retType, env3.apply_subst_typeenv(retSubst), trn.copy(cond = fixed_cond, tval = fixed_tval1, fval = fixed_fval1).copyMeta(trn))
       case ap@EApp(func, args) =>
        val expectedType = uniquify_fun(env(func).asInstanceOf[TFun])
        val retType = generateTypeVar()
        var runningEnv: TypeEnv = env
        var runningSubst: Subst = List()
        var typeList: List[Type] = List()
        var argList: List[Expr] = List()
        for (a <- args)
         {
          val (sub, typ, env1, fixed_a) = infer(runningEnv, a)
          runningSubst = compose_subst(runningSubst, sub)
          typeList = typeList :+ typ
          argList = argList :+ fixed_a
          runningEnv = env1
         }
        typeList = typeList.map(t => apply_subst_typ(runningSubst, t))
        val (subst, cast) = unify(TFun(typeList, retType), expectedType)
        val fixed_arg_list = if (cast)
         {
          argList.zip(expectedType.args).map(argtp => ECast(argtp._2, argtp._1))
         } else argList
        val retSubst = compose_subst(runningSubst, subst)
        val retEnv = runningEnv.apply_subst_typeenv(retSubst)
        val retTyp = apply_subst_typ(retSubst, retType)
        ap.typ = Some(retTyp)
        (retSubst, retTyp, retEnv, ap.copy(args = fixed_arg_list).copyMeta(ap))
       case ca@ECall(mod, name, args, isAtomic) if name.isEmpty =>
        if (!env(mod).isInstanceOf[TModType]) throw UnexpectedType(e.pos, "Module Call", "TModType", env(mod))
        val expectedType = getArrowModType(env(mod).asInstanceOf[TModType])
        substIntoCall(expectedType, ca, args, env)
       case ca@ECall(mod, method, args, isAtomic) if method.isDefined =>
        if (!env(mod).isInstanceOf[TObject]) throw UnexpectedType(e.pos, "Object Call", "TObject", env(mod))
        val expectedType = env(mod).asInstanceOf[TObject].methods(method.get)._1
        substIntoCall(expectedType, ca, args, env)
       case EVar(id) =>
        id.typ = Some(env(id))
        (List(), env(id), env, e)
       case ECast(ctyp, exp) =>
        val (s, t, env1, _) = infer(env, exp)
        val newT = apply_subst_typ(s, t)
        if (!canCast(ctyp, newT)) throw Errors.IllegalCast(e.pos, ctyp, newT)
        (s, ctyp, env1, e)
      }
      e.typ = Some (ret._2)
      ret
     }

    private def generateSignTypeVar(): TSignedNess =
     {
      counter += 1
      TSignVar(Id("__SIGN__" + counter))
     }

    private def binOpTypesFromRet(b: BOp, retType: Type, t1: Type, t2: Type): (Option[Type], Option[Type]) =
     {
      val tmp = b match
      {
       case EqOp(_) => val meet = t1 ⊓ t2
        (if (meet ==== t1) None else Some(meet), if (meet ==== t2) None else Some(meet))
       case CmpOp(_) => val meet = t1 ⊓ t2
        (if (meet ==== t1) None else Some(meet), if (meet ==== t2) None else Some(meet))
       case _: BoolOp => (None, None)
       case NumOp(op, _) => op match
       {
        case "/" | "%" => val meet = retType ⊓ t1
         if (meet ==== t1) (None, None) else (Some(meet), None)
        case "*" => (None, None)
        case "+" | "-" | "$*" => val meet = t1 ⊓ t2 ⊓ retType
         (if (meet ==== t1) None else Some(meet), if (meet ==== t2) None else Some(meet))
       }
       case BitOp(op, _) => op match
       {
        case "++" => (None, None)
        case _ => val meet = t1 ⊓ retType
         if (meet ==== t1) (None, None) else (Some(meet), None)
       }
      }
      tmp match
      {
       case (Some(x), Some(y)) => (Some(x.setPos(b.pos)), Some(y.setPos(b.pos)))
       case (Some(x), None) => (Some(x.setPos(b.pos)), None)
       case (None, Some(y)) => (None, Some(y.setPos(b.pos)))
       case _ => tmp
      }
     }

    private def uOpExpectedType(u: UOp): Type = u match
    {
     case BitUOp(_) => val b1 = generateBitWidthTypeVar() //TODO: Fix this
      val s = generateSignTypeVar()
      TFun(List(TSizedInt(b1, s)), TSizedInt(b1, s))
     case BoolUOp(_) => TFun(List(TBool()), TBool())
     case NumUOp(_) => val b1 = generateBitWidthTypeVar()
      val s = generateSignTypeVar()
      TFun(List(TSizedInt(b1, s)), TSizedInt(b1, s))
    }

    private def getArrowModType(t: TModType): TFun =
     {
      TFun(t.inputs, t.retType match
      { case None => TVoid()
       case Some(value) => value
      })
     }

    private def getMemAccessType(t: TMemType): TFun =
     {
      TFun(List(TSizedInt(TBitWidthLen(t.addrSize), sign = TUnsigned())), t.elem)
     }

    private def substIntoCall(expectedType: TFun, ca: ECall, args: List[Expr], env: TypeEnv) = {
     val retType = generateTypeVar()
     var runningEnv: TypeEnv = env
     var runningSubst: Subst = List()
     var typeList: List[Type] = List()
     var argList: List[Expr] = List()
     for (a <- args) {
      val (sub, typ, env1, fixed_a) = infer(runningEnv, a)
      runningSubst = compose_subst(runningSubst, sub)
      typeList = typeList :+ typ
      argList = argList :+ fixed_a
      runningEnv = env1
     }
     typeList = typeList.map(t => apply_subst_typ(runningSubst, t))
     val (subst, cast) = unify(TFun(typeList, retType), expectedType)
     val fixed_arg_list = if (cast) {
      argList.zip(expectedType.args).map(argtp => ECast(argtp._2, argtp._1))
     } else argList
     val retSubst = compose_subst(runningSubst, subst)
     val retEnv = runningEnv.apply_subst_typeenv(retSubst)
     val retTyp = apply_subst_typ(retSubst, retType)
     ca.typ = Some(retTyp)
     val mtyp = env(ca.mod)
     ca.mod.typ = Some(mtyp)
   (retSubst, retTyp, retEnv, ca.copy(args = fixed_arg_list).copyMeta(ca))
    }
   }
 }
