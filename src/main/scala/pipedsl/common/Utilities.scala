package pipedsl.common

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context}
import com.sun.org.apache.xpath.internal.Expression
import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Errors.UnexpectedCommand
import pipedsl.common.Syntax._

import scala.annotation.tailrec


object Utilities {

  def log2(x: Int): Int = {
    var y = x
    if (x < 0) { y = -y }
    var bits = 1
    while (y > 1) {
      y = y >> 1
      bits += 1
    }
    bits
  }

  def exp2(x: Int): Int = {
    1 << x
  }

  def freshVar(baseName: String, usedNames:Set[Id], counter: Int): (Id, Set[Id], Int) = {
    var n = baseName
    var newcount = counter
    while (usedNames(Id(n))) {
      n = baseName + "_" + newcount
      newcount += 1
    }
    val res = Id(n)
    (res, usedNames + res, newcount)
  }

  /**
   * Return every Id that is used for a variable name somewhere.
   * This is primarily used to generate fresh names by gathering the
   * set of used ones.
   * @param c The command to check
   * @return The set of used identifiers in c.
   */
  def getAllVarNames(c: Command): Set[Id] = c match {
    case CSeq(c1, c2) => getAllVarNames(c1) ++ getAllVarNames(c2)
    case CTBar(c1, c2) => getAllVarNames(c1) ++ getAllVarNames(c2)
    case CSplit(cases, default) =>
      val names = getAllVarNames(default)
      cases.foldLeft(names)((v, c) => {
        v ++ getUsedVars(c.cond) ++ getAllVarNames(c.body)
      })
    case CIf(cond, cons, alt) => getUsedVars(cond) ++ getAllVarNames(cons) ++ getAllVarNames(alt)
    case CAssign(lhs, rhs, _) => getUsedVars(lhs) ++ getUsedVars(rhs)
    case CRecv(lhs, rhs, _) => getUsedVars(lhs) ++ getUsedVars(rhs)
    case CLockStart(mod) => Set(mod)
    case CLockEnd(mod) => Set(mod)
    case CLockOp(mem, _, _) => if (mem.evar.isDefined) Set(mem.id, mem.evar.get.id) else Set(mem.id)
    case CSpecCall(handle, pipe, args) => args.foldLeft(Set(pipe, handle.id))((s, a) => s ++ getUsedVars(a))
    case CVerify(handle, args, preds) => args.foldLeft(Set[Id]())((s, a) => s ++ getUsedVars(a)) ++
        preds.foldLeft(Set[Id]())((s, p) => s ++ getUsedVars(p)) + handle.id
    case CInvalidate(handle) => Set(handle.id)
    case CCheckSpec(_) => Set()
    case COutput(exp) => getUsedVars(exp)
    case CReturn(exp) => getUsedVars(exp)
    case CExpr(exp) => getUsedVars(exp)
    case CPrint(args) => args.foldLeft(Set[Id]())((s, a) => s ++ getUsedVars(a))
    case ICondCommand(cond, cs) => getUsedVars(cond) ++ cs.foldLeft(Set[Id]())((s, c) => getAllVarNames(c) ++ s)
    case IUpdate(specId,value,originalSpec) => getUsedVars(value) ++ getUsedVars(originalSpec) + specId
    case Syntax.CEmpty() => Set()
    case _ => throw UnexpectedCommand(c)
  }

  def getWrittenVars(c: Command): Set[Id] = c match {
    case CSeq(c1, c2) => getWrittenVars(c1) ++ getWrittenVars(c2)
    case CTBar(c1, c2) => getWrittenVars(c1) ++ getWrittenVars(c2)
    case CSplit(cases, default) =>
      cases.foldLeft(getWrittenVars(default))((v, c) => {
        v ++ getWrittenVars(c.body)
      })
    case CIf(_, cons, alt) => getWrittenVars(cons) ++ getWrittenVars(alt)
    case CAssign(lhs, _, _) => lhs match { case EVar(id) => Set(id) ; case _ => Set() }
    case CRecv(lhs, _, _) => lhs match { case EVar(id) => Set(id) ; case _ => Set() }
    case ICondCommand(_, c2) => getWrittenVars(c2)
    case IMemRecv(_, _, data) => if (data.isDefined) Set(data.get.id) else Set()
    case IMemSend(handle, _, _, _, _) => Set(handle.id)
    case ISend(handle, _, _) => Set(handle.id)
    case IRecv(_, _, out) => Set(out.id)
    case IReserveLock(handle, _) => Set(handle.id)
    case IAssignLock(handle, _, _) => Set(handle.id)
    case CSpecCall(handle, _, _) => Set(handle.id)
    case _ => Set()
  }

  def getWrittenVars(cs: List[Command]): Set[Id] = {
    cs.foldLeft(Set[Id]())((s, c) => s ++ getWrittenVars(c))
  }

  /**
   * This gets all of the variables which are referenced
   * (not defined) in the given command. This also skips external
   * references like module names since those are not normal variables.
   * @param c The command to check
   * @return The set of variable identifiers which are used in c.
   */
  def getUsedVars(c: Command): Set[Id] = c match {
    case CSeq(c1, c2) => getUsedVars(c1) ++ getUsedVars(c2)
    case CTBar(c1, c2) => getUsedVars(c1) ++ getUsedVars(c2)
    case CSplit(cases, default) =>
      cases.foldLeft(getUsedVars(default))((v, c) => {
        v ++ getUsedVars(c.cond) ++ getUsedVars(c.body)
      })
    case CPrint(args) => args.foldLeft(Set[Id]())((s, a) => s ++ getUsedVars(a))
    case CIf(cond, cons, alt) => getUsedVars(cond) ++ getUsedVars(cons) ++ getUsedVars(alt)
    case CAssign(_, rhs, _) => getUsedVars(rhs)
    case CRecv(lhs, rhs, _) => getUsedVars(rhs) ++ (lhs match {
      case e:EMemAccess => getUsedVars(e)
      case _ => Set()
    })
    case CLockOp(mem, _, _) => if (mem.evar.isDefined) Set(mem.evar.get.id) else Set()
    case COutput(exp) => getUsedVars(exp)
    case CReturn(exp) => getUsedVars(exp)
    case CExpr(exp) => getUsedVars(exp)
    case ICondCommand(cond, c2) => getUsedVars(cond) ++ getUsedVars(c2)
    case IUpdate(specId, value, originalSpec) => getUsedVars(value) + specId ++ getUsedVars(originalSpec)
    case ICheck(specId, value) => getUsedVars(value) + specId
    case IMemSend(_, writeMask, _, data, addr) =>
      val dataSet = if (data.isDefined) {
        Set(data.get.id, addr.id)
      } else Set(addr.id)
      dataSet ++ (if (writeMask.isDefined) { getUsedVars(writeMask.get) } else { Set() })
    case IMemRecv(_, handle, _) => Set(handle.id)
    case IMemWrite(_, addr, data) => Set(addr.id, data.id)
    case IRecv(handle, _, _) => Set(handle.id)
    case ISend(_, _, args) => args.map(a => a.id).toSet
    case IReserveLock(_, larg) => larg.evar match {
      case Some(value) => Set(value.id)
      case None => Set()
    }
    case IAssignLock(_, src, default) => getUsedVars(src) ++
      (if (default.isDefined) getUsedVars(default.get) else Set())
    case ICheckLockOwned(larg, handle) => Set(handle.id) ++ (larg.evar match {
      case Some(value) => Set(value.id)
      case None => Set()
    })
    case IReleaseLock(larg, handle) => Set(handle.id) ++ (larg.evar match {
      case Some(value) => Set(value.id)
      case None => Set()
    })
    case ILockNoOp(_) => Set()
    case ICheckLockFree(_) => Set()
    case CLockStart(_) => Set()
    case CLockEnd(_) => Set()
    case CSpecCall(handle, _, args) => args.foldLeft(Set(handle.id))((s, a) => s ++ getUsedVars(a))
    case CVerify(handle, args, preds) => args.foldLeft(Set[Id]())((s, a) => s ++ getUsedVars(a)) ++
      preds.foldLeft(Set[Id]())((s, p) => s ++ getUsedVars(p)) + handle.id
    case CInvalidate(handle) => Set(handle.id)
    case CCheckSpec(_) => Set()
    case CEmpty() => Set()
  }

  /**
   * This gets all of the variables which are referenced
   * (not defined) in the given command. This also skips external
   * references like module names since those are not normal variables.
   * @param e The expression to check
   * @return The set of variable identifiers which are used in e.
   */
  def getUsedVars(e: Expr): Set[Id] = e match {
    case EUop(_, ex) => getUsedVars(ex)
    case EBinop(_, e1, e2) => getUsedVars(e1) ++ getUsedVars(e2)
    case ERecAccess(rec, _) => getUsedVars(rec)
    case EMemAccess(_, index, mask) => (if (mask.isDefined) getUsedVars(mask.get) else Set()) ++
      getUsedVars(index) //memories aren't variables, they're externally defined
    case EBitExtract(num, _, _) => getUsedVars(num)
    case ETernary(cond, tval, fval) => getUsedVars(cond) ++ getUsedVars(tval) ++ getUsedVars(fval)
    case EApp(_, args) => args.foldLeft[Set[Id]](Set())((s, a) => { s ++ getUsedVars(a) })
      //functions are also externally defined
    case ECall(id, args) => args.foldLeft[Set[Id]](Set(id))((s, a) => { s ++ getUsedVars(a) })
    case EVar(id) => id.typ = e.typ; Set(id)
    case ECast(_, exp) => getUsedVars(exp)
    case EFromMaybe(ex) => getUsedVars(ex)
    case EToMaybe(ex) => getUsedVars(ex)
    case EIsValid(ex) => getUsedVars(ex)
    case _ => Set()
  }

  /**
   * A convenience method for called the getUsedVars(c: Command)
   * function on a collection of commands and unioning the results.
   * @param cs The collection to check
   * @return The set of variable identifiers used in any of the commands in cs.
   */
  def getUsedVars(cs: Iterable[Command]): Set[Id] = {
    cs.foldLeft(Set[Id]())((s,c) => s ++ getUsedVars(c))
  }

  /**
   * This does a graph traversal to find all stages which
   * are reachable from the supplied stage and returns them
   * in the order they were found (therefore it will always start
   * with stg)
   * @param stg The stage to start checking from
   * @return The list of stages reachable from stg.
   */
  def getReachableStages(stg: PStage): List[PStage] = {
    visit[List[PStage]](stg, List[PStage](), (s, stgs) => stgs :+ s)
  }

  /**
   * A generic iterative graph visitor algorithm
   * that uses the succs method on stages to find successors.
   * This executes the traversal in a BFS order.
   * @param stg The starting stage to visit
   * @param start The initial result holder (result after visiting 0 stages)
   * @param visitor The visitor function which takes a stage and a current result and accumulates a new result.
   * @tparam T The type of the result that visitor produces
   * @return The results of applying the visitor to all reachable stages in a BFS order.
   */
  def visit[T](stg: PStage, start: T, visitor: (PStage, T) => T): T = {
    var result = visitor(stg, start)
    var visited = Set(stg)
    var fringe: List[PStage] = stg.succs.toList
    while (fringe.nonEmpty) {
      val next = fringe.head
      fringe = fringe.tail
      result = visitor(next, result)
      visited = visited + next
      val newvisits = next.succs.diff(visited)
      fringe = fringe ++ newvisits.toList
    }
    result
  }

  def flattenStageList(stgs: List[PStage]): List[PStage] = {
    stgs.foldLeft(List[PStage]())((l, stg) => stg match {
      case s: DAGSyntax.IfStage => (l :+ s) ++ s.condStages.flatMap(stg => flattenStageList(stg)) ++ flattenStageList(s.defaultStages)
      case _ => l :+ stg
    })
  }
  def flattenIfStages(stgs: List[PStage]): List[PStage] = {
    stgs.foldLeft(List[PStage]())((l, stg) => stg match {
      case s: DAGSyntax.IfStage => (l :+ s) ++ s.condStages.flatMap(stg => flattenStageList(stg)) ++ flattenStageList(s.defaultStages)
      case _ => l :+ stg
    })
  }

  /**
   * Utility function for finding _internal commands_ that correspond
   * to receive statements (i.e., those that wait for an asynchronous response from some
   * other hardware)
   * @param c The command to check
   * @return True if c is such a receiving command, else false
   */
  def isReceivingCmd(c: Command): Boolean = c match {
    case _: IRecv | _:IMemRecv => true
    case _ => false
  }

  /**
   * Produces a new Expression object that
   * represents the conjunction of the arguments.
   * If either argument is missing then the result
   * is just the other argument. If neither are provided, the result
   * is of None type.
   * @param condL The left expression
   * @param condR The right expression
   * @return Some(The conjunction of left and right) or None if neither are provided
   */
  def andExpr(condL: Option[Expr], condR: Option[Expr]): Option[Expr] = (condL, condR) match {
    case (None, None) => None
    case (Some(e1), Some(e2)) => Some(AndOp(e1, e2))
    case (Some(_), None) => condL
    case (None, Some(_)) => condR
  }

  def updateSetMap[K, V](m: Map[K, Set[V]], k: K, v: V): Map[K, Set[V]] = {
    if (m.contains(k)) {
      m.updated(k, m(k) + v)
    } else {
      m.updated(k, Set(v))
    }
  }

  def updateListMap[K,V](m: Map[K,List[V]], k: K, v: V): Map[K, List[V]] = {
    if (m.contains(k)) {
      m.updated(k, m(k) :+ v)
    } else {
      m.updated(k, List(v))
    }
  }

  def updateListMap[K,V](m: Map[K,List[V]], k: K, vs: List[V]): Map[K, List[V]] = {
    if (m.contains(k)) {
      m.updated(k, m(k) ++ vs)
    } else {
      m.updated(k, vs)
    }
  }

  implicit class RichOption[A](opt: Option[A]) {
    def getOrThrow[T <: Throwable](except: T): A = opt match {
      case Some(v) => v
      case None => throw except
    }
  }


  def opt_func[A, B](f :A => B) : Option[A] => Option[B] =
    {
      case Some(value) => Some(f(value))
      case None => None
    }

  private def typeMapExpr(e :Expr, f_opt : Option[Type] => Option[Type]) : Unit =
    {
      println(s"setting ${e.typ} to ${f_opt(e.typ)}")
      e.typ = f_opt(e.typ)
      e match
      {
        case EIsValid(ex) => typeMapExpr(ex, f_opt)
        case EFromMaybe(ex) => typeMapExpr(ex, f_opt)
        case EToMaybe(ex) => typeMapExpr(ex, f_opt)
        case EUop(_, ex) => typeMapExpr(ex, f_opt)
        case EBinop(_, e1, e2) => typeMapExpr(e1, f_opt); typeMapExpr(e2, f_opt)
        case ERecAccess(rec, fieldName) => typeMapId(fieldName, f_opt); typeMapExpr(rec, f_opt);
        case ERecLiteral(fields) =>
          fields.foreach(idex => {typeMapId(idex._1, f_opt); typeMapExpr(idex._2, f_opt)})
        case EMemAccess(mem, index, wmask) =>
          typeMapId(mem, f_opt); typeMapExpr(index, f_opt)
          wmask.fold(())(typeMapExpr(_, f_opt))
        case EBitExtract(num, _, _) => typeMapExpr(num, f_opt)
        case ETernary(cond, tval, fval) =>
          typeMapExpr(cond, f_opt); typeMapExpr(tval, f_opt); typeMapExpr(fval, f_opt)
        case EApp(func, args) =>
          typeMapId(func, f_opt); args.foreach(typeMapExpr(_, f_opt))
        case ECall(mod, args) =>
          typeMapId(mod, f_opt); args.foreach(typeMapExpr(_, f_opt))
        case EVar(id) => typeMapId(id, f_opt)
        case ECast(_, exp) => typeMapExpr(exp, f_opt)
        case expr: CirExpr => expr match
        {
          case CirLock(mem, _, _) => typeMapId(mem, f_opt)
          case CirNew(mod, mods) => typeMapId(mod, f_opt)
            mods.foreach((i: Id) => typeMapId(i, f_opt))
          case CirCall(mod, args) => typeMapId(mod, f_opt)
            args.foreach(typeMapExpr(_, f_opt))
          case _ => ()
        }
        case _ => ()
      }
    }
  private def typeMapCmd(c :Command, f_opt :Option[Type] => Option[Type]) :Unit = c match
  {
    case CSeq(c1, c2) => typeMapCmd(c1, f_opt); typeMapCmd(c2, f_opt)
    case CTBar(c1, c2) => typeMapCmd(c1, f_opt); typeMapCmd(c2, f_opt)
    case CIf(cond, cons, alt) => typeMapExpr(cond, f_opt); typeMapCmd(cons, f_opt); typeMapCmd(alt, f_opt)
    case CAssign(lhs, rhs, _) => typeMapExpr(lhs, f_opt); typeMapExpr(rhs, f_opt)
    case CRecv(lhs, rhs, _) => typeMapExpr(lhs, f_opt); typeMapExpr(rhs, f_opt)
    case CSpecCall(handle, pipe, args) =>
      typeMapExpr(handle, f_opt); typeMapId(pipe, f_opt); args.foreach(typeMapExpr(_, f_opt))
    case CVerify(handle, args, preds) =>
      typeMapExpr(handle, f_opt); args.foreach(typeMapExpr(_, f_opt)); preds.foreach(typeMapExpr(_, f_opt))
    case CInvalidate(handle) => typeMapExpr(handle, f_opt)
    case CPrint(args) => args.foreach(typeMapExpr(_, f_opt))
    case COutput(exp) => typeMapExpr(exp, f_opt)
    case CReturn(exp) => typeMapExpr(exp, f_opt)
    case CExpr(exp) => typeMapExpr(exp, f_opt)
    case CLockStart(mod) => typeMapId(mod, f_opt)
    case CLockEnd(mod) => typeMapId(mod, f_opt)
    case CLockOp(mem, _, _) =>
      typeMapId(mem.id, f_opt);
      mem.evar match
      {case Some(value) => typeMapExpr(value, f_opt)
      case None => ()}
    case CSplit(cases, default) =>
      cases.foreach(cs => {typeMapExpr(cs.cond, f_opt); typeMapCmd(cs.body, f_opt)})
      typeMapCmd(default, f_opt)
    case _ => ()
  }

  private def typeMapId(i: Id, f_opt: Option[Type] => Option[Type]):Unit =
    {
      i.typ = f_opt(i.typ)
    }



  def typeMapFunc(fun :FuncDef, f_opt :Option[Type] => Option[Type]) :Unit = typeMapCmd(fun.body, f_opt)
  def typeMapModule(mod :ModuleDef, f_opt :Option[Type] => Option[Type]) :Unit = typeMapCmd(mod.body, f_opt)

  def typeMap(p: Prog, f: Type => Type) :Unit=
    {
      val f_opt = opt_func(f)
      p.fdefs.foreach(typeMapFunc(_, f_opt))
      p.moddefs.foreach(typeMapModule(_, f_opt))
    }



  /** Like [[Z3Context.mkAnd]], but automatically casts inputs to [[Z3BoolExpr]]s. */
  def mkAnd(ctx: Z3Context, expressions: Z3AST *): Z3BoolExpr =
    ctx.mkAnd(expressions.map(ast => ast.asInstanceOf[Z3BoolExpr]):_*)

  /** Like [[Z3Context.mkOr]], but automatically casts inputs to
   * [[Z3BoolExpr]]s. */
  def mkOr(ctx : Z3Context, expressions: Z3AST *): Z3BoolExpr =
    ctx.mkOr(expressions.map(ast => ast.asInstanceOf[Z3BoolExpr]):_*)

  /** Like [[Z3Context.mkImplies]], but automatically casts inputs to [[Z3BoolExpr]]s. */
  def mkImplies(ctx: Z3Context, t1: Z3AST, t2: Z3AST): Z3BoolExpr =
    ctx.mkImplies(t1.asInstanceOf[Z3BoolExpr], t2.asInstanceOf[Z3BoolExpr])

  val (defaultReadPorts, defaultWritePorts) = (5, 2)

}
