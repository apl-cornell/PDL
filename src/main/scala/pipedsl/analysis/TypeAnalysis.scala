package pipedsl.analysis

import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.relation.Tree
import pipedsl.common.Errors.{ArgLengthMismatch, IllegalCast, MalformedFunction, MissingType, UnexpectedAssignment, UnexpectedCase, UnexpectedCommand, UnexpectedReturn, UnexpectedSubtype, UnexpectedType}
import pipedsl.common.Syntax
import pipedsl.common.Syntax.Latency.{Asynchronous, Combinational, Sequential}
import pipedsl.common.Syntax.{BitOp, BitUOp, BoolOp, BoolUOp, CAssign, CCheck, CIf, CLockEnd, CLockOp, CLockStart, COutput, CRecv, CReturn, CSeq, CSpeculate, CSplit, CTBar, CirCall, CirConnect, CirExpr, CirExprStmt, CirMem, CirNew, CirRegFile, CirSeq, Circuit, CmpOp, Command, EVar, EqOp, Expr, FuncDef, Id, ModuleDef, NumOp, NumUOp, Prog, ProgramNode, TBool, TFun, TMemType, TModType, TNamedType, TRecType, TSizedInt, TString, TVoid, Type}
import pipedsl.typechecker.Environments.{EmptyTypeEnv, Environment, TypeEnv}
import pipedsl.typechecker.Subtypes.{areEqual, isSubtype}

class TypeAnalysis(program: Tree[ProgramNode, Prog]) extends Attribution {
  
  def checkProg(): Unit = {
    val prog = program.root
    prog.fdefs.foreach(f => {
      checkCommand(f.body)
      val rt = checkFuncWellFormed(f.body)
      if (rt.isEmpty) {
        throw MalformedFunction(f.pos, "Missing return statement")
      } else if (!areEqual(f.ret, rt.get)) {
        throw UnexpectedType(f.pos, s"${f.name} return type", f.ret.toString(), rt.get)
      }
    })
    prog.moddefs.foreach(m => {
      checkCommand(m.body) 
      checkModuleBodyWellFormed(m.body, Set())
    })
    checkCircuit(prog.circ)
  }
  //Make this only a TOP LEVEL COMMAND (i.e. not the commands inside if statements or otherwise) Essentially, only
  //traverse CSEQ or CTBAR
  val rightMostLeaf: Command => Command =
    attr {
      case c@CSeq(c1, c2) => rightMostLeaf(c2)
      case c@CTBar(c1, c2) => rightMostLeaf(c2)
      case c => c
    }
  
  /**
   * This checks that the function doesn't include any disallowed expressions
   * or commands (such as Calls) and checks that it does not have code following
   * its return statement. This function is recursively defined and should be
   * called on the function's body (which is a single command)
   * @param c The command to check for well-formedness
   * @return Some(return type) if the analyzed command returns a value or None other wise
   */
  private def checkFuncWellFormed(c: Command): Option[Type] = c match {
    case CSeq(c1, c2) => {
      val r1 = checkFuncWellFormed(c1)
      val r2 = checkFuncWellFormed(c2)
      (r1, r2) match {
        case (Some(_), Some(_)) => throw MalformedFunction(c.pos, "Multiple return statements in execution")
        case (Some(_), _) => r1
        case (_, Some(_)) => r2
        case (None, None) => None
      }
    }
    case _: CTBar | _: CSplit | _: CSpeculate | _: CCheck | _:COutput =>
      throw MalformedFunction(c.pos, "Command not supported in combinational functions")
    case CIf(_, cons, alt) =>
      val rt = checkFuncWellFormed(cons)
      val rf = checkFuncWellFormed(alt)
      (rt, rf) match {
        case (Some(t1), Some(t2)) if areEqual(t1, t2) => rt
        case (Some(t1), Some(t2)) => throw MalformedFunction(c.pos, s"Mismatched return types ${t1.toString()}, and ${t2.toString()}")
        case (None, None) => None
        case _ => throw MalformedFunction(c.pos, "Missing return in branch of if")
      }
    case CReturn(exp) => Some(typeCheck(exp))
    case _ => None
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
    case CRecv(lhs@EVar(id), _, _) =>
      if (assignees(id)) { throw UnexpectedAssignment(lhs.pos, id) } else {
        assignees + id
      }
    case CAssign(lhs@EVar(id), _, _) =>
      if (assignees(id)) { throw UnexpectedAssignment(lhs.pos, id) } else {
        assignees + id
      }
    //returns are only for function calls
    case CReturn(_) => throw UnexpectedReturn(c.pos)
    case _ => assignees
  }
  
  val contextAfterNode: ProgramNode => Environment[Id, Type] = {
    attr {
      case c@CAssign(lhs@EVar(id), rhs, typ) => context(c).add(id, typ.get)
      case c@CRecv(lhs@EVar(id), rhs, typ) => context(c).add(id, typ.get)
      case p => context(p)
    }
  }

  val contextAfterCommand: Environment[Id, Type] => Command => Environment[Id, Type] = {
    paramAttr {
      env => {
        case c@CSeq(c1, c2) => val e1 = contextAfterCommand(env)(c1); contextAfterCommand(e1)(c2)
        case c@CTBar(c1, c2) => val e1 = contextAfterCommand(env)(c1); contextAfterCommand(e1)(c2)
        case c@CIf(cond, cons, alt) => contextAfterCommand(env)(cons).intersect(contextAfterCommand(env)(alt))
        case c@CSplit(cases, default) =>
          var runningEnv = contextAfterCommand(env)(default)
          for (c <- cases) {
            runningEnv = runningEnv.intersect(contextAfterCommand(env)(c.body))
          }
          runningEnv
        case c@CAssign(lhs@EVar(id), rhs, typ) => env.add(id, typ.get)
        case c@CRecv(lhs@EVar(id), rhs, typ) => env.add(id, typ.get)
        case _ => env
      }
    }
  }
  //Need some thing that gives the context after a thingy

  val context: ProgramNode => Environment[Id, Type] = {
    attr {
      //Context is prev OR parent 
      //Need to match prev with CAssign, CIf,or CSplit, or CRecV
      case program.prev(program.parent(c@CSeq(c1, c2))) => context(c1).union(contextAfterCommand(EmptyTypeEnv)(c1))
      case program.prev(program.parent(c@CTBar(c1, c2))) => context(c1).union(contextAfterCommand(EmptyTypeEnv)(c1))
      case program.parent(m@ModuleDef(name,inputs,modules,ret,body)) =>
        val tenv = context(m)
        val inEnv = inputs.foldLeft[Environment[Id, Type]](tenv)((env, p) => { env.add(p.name, p.typ) })
        val pipeEnv = modules.foldLeft[Environment[Id, Type]](inEnv)((env, p) =>
        { env.add(p.name, replaceNamedType(p.typ, m)) })
        pipeEnv.add(m.name, typeCheck(m))
      case program.parent(f@FuncDef(name, args, ret, body)) =>
        val tenv = context(f)
        f.args.foldLeft[Environment[Id, Type]](tenv)((env, p) => { env.add(p.name, p.typ)})
      case program.prev(f@FuncDef(name, args, ret, body))=>
        context(f).add(f.name, typeCheck(f))
      case program.prev(m@ModuleDef(name, inputs, modules, ret, body)) =>
        context(m).add(m.name, typeCheck(m))
      case program.prev(cc@CirConnect(name, c)) => context(cc).add(name, checkCirExpr(c))
      case program.parent(p) => context(p)
      case program.root => TypeEnv()
    }
  }

  //Module parameters can't be given a proper type during parsing if they refer to another module
  //This uses module names in the type environment to lookup their already checked types
  private def replaceNamedType(t: Type, mod: ModuleDef): Type = t match {
    case TNamedType(name) => context(mod)(name)
    case _ => t
  }

  def checkCommand(c: Command): Unit = {
    val tenv = context(c)
    c match {
      case CSeq(c1, c2) => checkCommand(c1); checkCommand(c2);
      case CTBar(c1, c2) => checkCommand(c1); checkCommand(c2);
      case CIf(cond, cons, alt) =>
        typeCheck(cond).matchOrError(cond.pos, "if condition", "boolean") { case _: TBool => () }
        checkCommand(cons)
        checkCommand(alt)
      case CAssign(lhs, rhs, typ) =>
        val lTyp = typeCheck(lhs)
        val rTyp = typeCheck(rhs)
        if (isSubtype(rTyp, lTyp)) TVoid() else throw UnexpectedSubtype(rhs.pos, "assignment", lTyp, rTyp)
      case CRecv(lhs, rhs, typ) =>
        val lTyp = typeCheck(lhs)
        val rTyp = typeCheck(rhs)
        if (isSubtype(rTyp, lTyp)) TVoid() else throw UnexpectedSubtype(rhs.pos, "assignment", lTyp, rTyp)
      case Syntax.CPrint(evar) =>
        val t = typeCheck(evar)
        if (! (t.isInstanceOf[TSizedInt] || t.isInstanceOf[TString] || t.isInstanceOf[TBool])) {
          throw UnexpectedType(evar.pos, evar.toString, "Need a printable type", t)
        }
      case Syntax.COutput(exp) => typeCheck(exp)
      case Syntax.CReturn(exp) => typeCheck(exp)
      case Syntax.CExpr(exp) => typeCheck(exp)
      case CLockStart(mod) => tenv(mod).matchOrError(mod.pos, "lock reservation start", "Memory or Module Type")
      {
        case _: TModType => ()
        case _: TMemType => ()
      }
      case CLockEnd(mod) => tenv(mod).matchOrError(mod.pos, "lock reservation start", "Memory or Module Type")
      {
        case _: TModType => ()
        case _: TMemType => ()
      }
      case CLockOp(mem, _) => {
        tenv(mem.id).matchOrError(mem.pos, "lock operation", "Memory or Module Type")
        {
          case t: TModType =>
            if (mem.evar.isDefined) throw UnexpectedType(t.pos, "address lock operation", "Memory Type", t)
          case memt: TMemType => {
            if(!mem.evar.isEmpty) {
              val idxt = typeCheck(mem.evar.get)
              idxt match {
                case TSizedInt(l, true) if l == memt.addrSize => ()
                case _ => throw UnexpectedType(mem.pos, "lock operation", "ubit<" + memt.addrSize + ">", idxt)
              }
            }
          }
        }
      }
      case CSplit(cases, default) =>
        for (c <- cases) {
          val condTyp = typeCheck(c.cond)
          condTyp.matchOrError(c.cond.pos, "case condition", "boolean") { case _: TBool => () }
          checkCommand(c.body)
        }
        checkCommand(default)
      case Syntax.CSpeculate(predVar, predVal, verify, body) =>
      case Syntax.CCheck(predVar) =>
      case Syntax.CEmpty() =>
      case _ => throw UnexpectedCommand(c)
    }
  }
  
  def checkCircuit(c: Circuit): Unit = {
      c match {
      case CirSeq(c1, c2) => {
        checkCircuit(c1)
        checkCircuit(c2)
      }
      case CirConnect(name, c) =>
        val t = typeCheck(c)
      case CirExprStmt(ce) => typeCheck(ce)
    }
  }

  val typeCheck: ProgramNode => Type = {
    attr {
      //used to get the defined type of a node 
      case program.next(program.parent(p@CAssign(lhs@EVar(id), rhs, typ))) => typ.get
      case program.next(program.parent(p@CRecv(lhs@EVar(id), rhs, typ))) => typ.get
      case program.parent.pair(id: Id, evar: EVar) => typeCheck(evar)
      case program.parent.pair(id: Id, mod: ModuleDef) => typeCheck(mod)
      case program.parent.pair(id: Id, func: FuncDef) => typeCheck(func)
      case i:Id => context(i)(i)
      case ce: CirExpr => checkCirExpr(ce)
      case e: Syntax.Expr => checkExpr(e)
      case m: ModuleDef => //TODO disallow memories
        val inputTyps = m.inputs.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ })
        //TODO require memory or module types
        val modTyps = m.modules.foldLeft[List[Type]](List())((l, p) => { l :+ replaceNamedType(p.typ, m) })
        TModType(inputTyps, modTyps, m.ret, Some(m.name))
      case f: FuncDef =>         
        val typList = f.args.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ }) //TODO disallow memories as params
        TFun(typList, f.ret)
    
    }
  }
  
  def checkExpr(e: Expr): Type = {
    val tenv = context(e)
    e match {
      case Syntax.EInt(v, base, bits) => TSizedInt(bits, unsigned = true)
      case Syntax.EString(v) =>TString()
      case Syntax.EBool(v) =>TBool()
      case Syntax.EUop(op, ex) =>
        val t1= typeCheck(ex)
        op match {
          case BoolUOp(op) => t1.matchOrError(e.pos, "boolean op", "boolean") { case _: TBool => TBool() }
          case NumUOp(op) => t1.matchOrError(e.pos, "number op", "number") { case t: TSizedInt => t }
          case BitUOp(op) => t1.matchOrError(e.pos, "bit op", "sized integer") { case t: TSizedInt => t }
        }
      case Syntax.EBinop(op, e1, e2) =>
        val t1 = typeCheck(e1)
        val t2 = typeCheck(e2)
        op match {
          case BitOp("++", _) => (t1, t2) match {
            case (TSizedInt(l1, u1), TSizedInt(l2, u2)) if u1 == u2 => TSizedInt(l1 + l2, u1)
            case (_, _) => throw UnexpectedType(e.pos, "concat", "sized number", t1)
          }
          case BitOp("<<", _) => (t1, t2) match {
            case (TSizedInt(l1, u1), TSizedInt(_, _)) => TSizedInt(l1, u1)
          }
          case BitOp(">>", _) => (t1, t2) match {
            case (TSizedInt(l1, u1), TSizedInt(_, _)) => TSizedInt(l1, u1)
          }
          case NumOp("*", _) => (t1, t2) match {
            case (TSizedInt(l1, u1), TSizedInt(l2, u2)) if u1 == u2 => TSizedInt(l1 + l2, u1)
            case (_, _) => throw UnexpectedType(e.pos, "concat", "sized number", t1)
          }
          case _ => if (!areEqual(t1, t2)) { throw UnexpectedType(e2.pos, e2.toString, t1.toString(), t2) } else {
            op match {
              case EqOp(_) => TBool()
              case CmpOp(_) => TBool()
              case BoolOp(_, _) => t1.matchOrError(e1.pos, "boolean op", "boolean") { case _: TBool => TBool() }
              case NumOp(_, _) => t1.matchOrError(e1.pos, "number op", "number") { case t: TSizedInt => t}
              case BitOp(_, _) => t1.matchOrError(e1.pos, "bit op", "sized integer") { case t: TSizedInt => t }
            }
          }
        }
      case Syntax.ERecAccess(rec, fieldName) =>
        val rt = typeCheck(rec)
        rt match {
          case TRecType(n, fs) => fs.get(fieldName) match {
            case Some(t) => t
            case _ => throw MissingType(fieldName.pos, s"Field $n. ${fieldName.v}")
          }
          case _ => throw UnexpectedType(e.pos, "record access", "record type", rt)
        }
      case Syntax.ERecLiteral(fields) =>
        val ftyps = fields map { case (n, e) => (n, typeCheck(e)) }
        TRecType(Id("anon"), ftyps)//TODO these are wrong, maybe just remove these
      case Syntax.EMemAccess(mem, index) =>
        val memt = tenv(mem)
        val idxt= typeCheck(index)
        (memt, idxt) match {
          case (TMemType(e, s, _, _), TSizedInt(l, true)) if l == s => e
          case _ => throw UnexpectedType(e.pos, "memory access", "mismatched types", memt)
        }
      case Syntax.EBitExtract(num, start, end) =>
        val ntyp = typeCheck(num)
        val bitsLeft = math.abs(end - start) + 1
        ntyp.matchOrError(e.pos, "bit extract", "sized number") {
          case TSizedInt(l, u) if l >= bitsLeft => TSizedInt(bitsLeft, u)
          case _ => throw UnexpectedType(e.pos, "bit extract", "sized number larger than extract range", ntyp)
        }
      case Syntax.ETernary(cond, tval, fval) =>
        val ctyp = typeCheck(cond)
        ctyp.matchOrError(cond.pos, "ternary condition", "boolean") { case _: TBool => () }
        val ttyp = typeCheck(tval)
        val ftyp = typeCheck(fval)
        if (areEqual(ttyp, ftyp)) ttyp
        else throw UnexpectedType(e.pos, "ternary", s"false condition must match ${ttyp.toString}", ftyp)
      case Syntax.EApp(func, args) =>
        val ftyp = tenv(func)
        ftyp match {
          case TFun(targs, tret) => {
            if (targs.length != args.length) {
              throw ArgLengthMismatch(e.pos, targs.length, args.length)
            }
            targs.zip(args).foreach {
              case (expectedT, a) =>
                val atyp = typeCheck(a)
                if (!isSubtype(atyp, expectedT)) {
                  throw UnexpectedSubtype(e.pos, a.toString, expectedT, atyp)
                }
            }
            tret
          }
          case _ => throw UnexpectedType(func.pos, "function call", "function type", ftyp)
        }
      case Syntax.ECall(mod, args) =>
        val mtyp = tenv(mod)
        mtyp match {
          case TModType(inputs, _, retType, _) => {
            if (inputs.length != args.length) {
              throw ArgLengthMismatch(e.pos, inputs.length, args.length)
            }
            inputs.zip(args).foreach {
              case (expectedT, a) =>
                val atyp = typeCheck(a)
                if (!isSubtype(atyp, expectedT)) {
                  throw UnexpectedSubtype(e.pos, a.toString, expectedT, atyp)
                }
            }
            if (retType.isDefined) retType.get else TVoid()
          }
          case _ => throw UnexpectedType(mod.pos, "module name", "module type", mtyp)
        }
      case EVar(id) => tenv(id)
      case Syntax.ECast(ctyp, exp) =>
        val etyp = typeCheck(exp)
        (ctyp, etyp) match {
          case (t1, t2) if !areEqual(t1, t2) => throw IllegalCast(e.pos, t1, t2)
          case _ => ()
        }
        ctyp
      case _ => print(e); throw UnexpectedCase(e.pos)
    }
  }

  def checkCirExpr(c: CirExpr): Type = {
    val tenv = context(c)
    c match {
      case CirMem(elemTyp, addrSize) => {
        val mtyp = TMemType(elemTyp, addrSize, Asynchronous, Asynchronous)
        mtyp
      }
      case CirRegFile(elemTyp, addrSize) => {
        val mtyp = TMemType(elemTyp, addrSize, Combinational, Sequential)
        mtyp
      }
      case CirNew(mod, mods) => {
        val mtyp = context(c)(mod)
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
            mtyp
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
                val atyp = typeCheck(arg)
                if (!isSubtype(atyp, expectedT)) {
                  throw UnexpectedSubtype(arg.pos, arg.toString, expectedT, atyp)
                }
              }
            }
            mtyp
          }
          case x => throw UnexpectedType(c.pos, c.toString, "Module Type", x)
        }
      }
    }
  }
}

object TypeAnalysis extends Attribution with AnalysisProvider[TypeAnalysis] {
  val instance: Prog => TypeAnalysis =
    attr {
      case p => new TypeAnalysis(new Tree[ProgramNode, Prog](p))
    }
  override def get(program: Prog): TypeAnalysis = instance(program)
}
