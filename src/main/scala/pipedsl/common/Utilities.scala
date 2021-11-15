package pipedsl.common

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context}
import pipedsl.common.DAGSyntax.PStage
import pipedsl.common.Errors.{LackOfConstraints, UnexpectedCommand}
import pipedsl.common.Syntax._

import scala.collection.mutable



object Utilities {

  def log2(x: Int): Int = {
    var y = x
    if (x < 0) {
      y = -y
    }
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

  def freshVar(baseName: String, usedNames: Set[Id], counter: Int): (Id, Set[Id], Int) = {
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
   *
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
    case CAssign(lhs, rhs) => getUsedVars(lhs) ++ getUsedVars(rhs)
    case CRecv(lhs, rhs) => getUsedVars(lhs) ++ getUsedVars(rhs)
    case CLockStart(mod) => Set(mod)
    case CLockEnd(mod) => Set(mod)
    case CLockOp(mem, _, _, args, ret) =>
      val mem_vars = if (mem.evar.isDefined) Set(mem.id, mem.evar.get.id) else Set(mem.id)
      val arg_vars = args.foldLeft(Set[Id]())((set, arg) => getUsedVars(arg) ++ set)
      val ret_vars = ret match { case Some(value) => Set(value.id) case None => Set() }
      mem_vars ++ arg_vars ++ ret_vars
    case CSpecCall(handle, pipe, args) => args.foldLeft(Set(pipe, handle.id))((s, a) => s ++ getUsedVars(a))
    case CVerify(handle, args, preds, upd) => (if (upd.isDefined) getUsedVars(upd.get) else Set()) ++
      args.foldLeft(Set[Id]())((s, a) => s ++ getUsedVars(a)) ++
      preds.foldLeft(Set[Id]())((s, p) => s ++ getUsedVars(p)) + handle.id
    case CUpdate(nh, handle, args, preds) =>
      args.foldLeft(Set[Id]())((s, a) => s ++ getUsedVars(a)) ++
        preds.foldLeft(Set[Id]())((s, p) => s ++ getUsedVars(p)) + handle.id + nh.id
    case CInvalidate(handle) => Set(handle.id)
    case CCheckSpec(_) => Set()
    case COutput(exp) => getUsedVars(exp)
    case CReturn(exp) => getUsedVars(exp)
    case CExpr(exp) => getUsedVars(exp)
    case CPrint(args) => args.foldLeft(Set[Id]())((s, a) => s ++ getUsedVars(a))
    case ICondCommand(cond, cs) => getUsedVars(cond) ++ cs.foldLeft(Set[Id]())((s, c) => getAllVarNames(c) ++ s)
    case IUpdate(specId, value, originalSpec) => getUsedVars(value) ++ getUsedVars(originalSpec) + specId
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
    case CAssign(lhs, rhs) =>
      val lhsid: Set[Id] = lhs match {
        case EVar(id) => Set(id);
        case _ => Set()
      }
      getMemReads(rhs).foldLeft(lhsid)((s, m) => {
        if (m.outHandle.isDefined) s + m.outHandle.get.id
        else s
      })
    case CRecv(lhs, rhs) =>
      val lhsid: Set[Id] = lhs match {
        case EVar(id) => Set(id);
        case _ => Set()
      }
      getMemReads(rhs).foldLeft(lhsid)((s, m) => {
        if (m.outHandle.isDefined) s + m.outHandle.get.id
        else s
      })
    case ICondCommand(_, c2) => getWrittenVars(c2)
    case IMemRecv(_, _, data) => if (data.isDefined) Set(data.get.id) else Set()
    case IMemSend(handle, _, _, _, _, _, outHandle, _) => outHandle match
    {
      case Some(vl) => Set(handle.id, vl.id)
      case None => Set(handle.id)
    }
    case i :IMemWrite => i.outHandle.map(vl => Set(vl.id)).getOrElse(Set())
    case ISend(handle, _, _) => Set(handle.id)
    case IRecv(_, _, out) => Set(out.id)
    case IReserveLock(handle, _) => Set(handle.id)
    case IAssignLock(handle, _, _) => Set(handle.id)
    case ICheckLockOwned(_, _, outHandle) => Set(outHandle.id)
    case CSpecCall(handle, _, _) => Set(handle.id)
    case CUpdate(newHandle, _, _, _) => Set(newHandle.id)
    case _ => Set()
  }

  def getWrittenVars(cs: List[Command]): Set[Id] = {
    cs.foldLeft(Set[Id]())((s, c) => s ++ getWrittenVars(c))
  }

  /**
   * This gets all of the variables which are referenced
   * (not defined) in the given command. This also skips external
   * references like module names since those are not normal variables.
   *
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
    case CAssign(_, rhs) => getUsedVars(rhs)
    case CRecv(lhs, rhs) => getUsedVars(rhs) ++ (lhs match {
      case e: EMemAccess => getUsedVars(e)
      case _ => Set()
    })
    case CLockOp(mem, _, _, args, _) =>
      val mem_vars = if (mem.evar.isDefined) Set(mem.evar.get.id) else Set()
      val arg_vars = args.foldLeft(Set[Id]())((set, arg) => set ++ getUsedVars(arg))
      arg_vars ++ mem_vars
    case COutput(exp) => getUsedVars(exp)
    case CReturn(exp) => getUsedVars(exp)
    case CExpr(exp) => getUsedVars(exp)
    case ICondCommand(cond, c2) => getUsedVars(cond) ++ getUsedVars(c2)
    case IUpdate(specId, value, originalSpec) => getUsedVars(value) + specId ++ getUsedVars(originalSpec)
    case ICheck(specId, value) => getUsedVars(value) + specId
    case IMemSend(_, writeMask, _, data, addr, inHandle, _, _) =>
      val dataSet = if (data.isDefined) {
        Set(data.get.id, addr.id).union(inHandle.map(h => Set(h.id)).getOrElse(Set()))
      } else Set(addr.id).union(inHandle.map(h => Set(h.id)).getOrElse(Set()))
      dataSet ++ (if (writeMask.isDefined) {
        getUsedVars(writeMask.get)
      } else {
        Set()
      })
    case IMemRecv(_, handle, _) => Set(handle.id)
    case IMemWrite(_, addr, data, inHandle, _, _) =>
      Set(addr.id, data.id).union(inHandle.map(h => Set(h.id)).getOrElse(Set()))
    case IRecv(handle, _, _) => Set(handle.id)
    case ISend(_, _, args) => args.map(a => a.id).toSet
    case IReserveLock(_, larg) => larg.evar match {
      case Some(value) => Set(value.id)
      case None => Set()
    }
    case IAssignLock(_, src, default) => getUsedVars(src) ++
      (if (default.isDefined) getUsedVars(default.get) else Set())
    case ICheckLockOwned(larg, inHandle, _) => Set(inHandle.id) ++ (larg.evar match {
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
    case CVerify(handle, args, preds, upd) => (if (upd.isDefined) getUsedVars(upd.get) else Set()) ++
      args.foldLeft(Set[Id]())((s, a) => s ++ getUsedVars(a)) ++
      preds.foldLeft(Set[Id]())((s, p) => s ++ getUsedVars(p)) + handle.id
    case CUpdate(nh, handle, args, preds) =>
      args.foldLeft(Set[Id]())((s, a) => s ++ getUsedVars(a)) ++
        preds.foldLeft(Set[Id]())((s, p) => s ++ getUsedVars(p)) + handle.id
    case CInvalidate(handle) => Set(handle.id)
    case CCheckSpec(_) => Set()
    case CEmpty() => Set()
  }

  /**
   * This gets all of the variables which are referenced
   * (not defined) in the given command. This also skips external
   * references like module names since those are not normal variables.
   *
   * @param e The expression to check
   * @return The set of variable identifiers which are used in e.
   */
  def getUsedVars(e: Expr): Set[Id] = e match {
    case EUop(_, ex) => getUsedVars(ex)
    case EBinop(_, e1, e2) => getUsedVars(e1) ++ getUsedVars(e2)
    case ERecAccess(rec, _) => getUsedVars(rec)
    case EMemAccess(_, index, mask, inHandle, _, _) =>
      getUsedVars(index)  ++ //memories aren't variables, they're externally defined
      (inHandle match {case Some(e) => getUsedVars(e) case None => Set()}) ++
      (if (mask.isDefined) getUsedVars(mask.get) else Set())
    case EBitExtract(num, _, _) => getUsedVars(num)
    case ETernary(cond, tval, fval) => getUsedVars(cond) ++ getUsedVars(tval) ++ getUsedVars(fval)
    case EApp(_, args) => args.foldLeft[Set[Id]](Set())((s, a) => {
      s ++ getUsedVars(a)
    })
    //functions are also externally defined
    case ECall(id, _, args) => args.foldLeft[Set[Id]](Set(id))((s, a) => {
      s ++ getUsedVars(a)
    })
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
   *
   * @param cs The collection to check
   * @return The set of variable identifiers used in any of the commands in cs.
   */
  def getUsedVars(cs: Iterable[Command]): Set[Id] = {
    cs.foldLeft(Set[Id]())((s, c) => s ++ getUsedVars(c))
  }

  /**
   * This does a graph traversal to find all stages which
   * are reachable from the supplied stage and returns them
   * in the order they were found (therefore it will always start
   * with stg)
   *
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
   *
   * @param stg     The starting stage to visit
   * @param start   The initial result holder (result after visiting 0 stages)
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

  def depthFirstTrav[T](stg: PStage,
                        start: T,
                        visitor: (PStage, T) => T,
                        succs: PStage => Iterable[PStage],
                        visited: mutable.HashSet[PStage] = mutable.HashSet.empty[PStage]): T = {
    if (visited.contains(stg)) return start
    val result = visitor(stg, start)
    visited.add(stg)
    val fringe = succs(stg)
    fringe.foldLeft(result)((acc, st) => depthFirstTrav(st, acc, visitor, succs, visited))
  }

  def annotateSpecTimings(ends: Iterable[PStage]): Map[PStage, Option[Int]] = {
    def hasSpecCmd(c: Command): Boolean = c match {
      case CSeq(c1, c2) => hasSpecCmd(c1) || hasSpecCmd(c2)
      case CTBar(c1, c2) => hasSpecCmd(c1) || hasSpecCmd(c2)
      case CIf(_, cons, alt) => hasSpecCmd(cons) || hasSpecCmd(alt)
      case CSpecCall(_, _, _) => true
      case CCheckSpec(_) => true
      case CVerify(_, _, _, _) => true
      case CUpdate(_, _, _, _) => true
      case CInvalidate(_) => true
      case CSplit(cases, default) => cases.exists(c => hasSpecCmd(c.body)) || hasSpecCmd(default)
      case ICondCommand(_, cs) => cs.exists(hasSpecCmd)
      case _: IUpdate => true
      case _: ICheck => true
      case _ => false
    }

    val hasSpecStg: (PStage) => Boolean = stg => {
      stg.getCmds.exists(c => hasSpecCmd(c))
    }
    val initMap = (Map.empty[PStage, Option[Int]], 0)
    val visited = mutable.HashSet.empty[PStage]

    def update(stg: PStage, t: Tuple2[Map[PStage, Option[Int]], Int]): Tuple2[Map[PStage, Option[Int]], Int] = {
      val map = t._1
      val num = t._2
      if (hasSpecStg(stg)) (map + (stg -> Some(num)), num + 1)
      else (map + (stg -> None), num)
    }

    def pred(stg: PStage): Iterable[PStage] = {
      stg.preds
    }

    ends.foldLeft(initMap)((m, s) => {
      depthFirstTrav(s, m, update, pred, visited)
    })._1
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
   *
   * @param c The command to check
   * @return True if c is such a receiving command, else false
   */
  def isReceivingCmd(c: Command): Boolean = c match {
    case _: IRecv | _: IMemRecv => true
    case _ => false
  }

  /**
   * Produces a new Expression object that
   * represents the conjunction of the arguments.
   * If either argument is missing then the result
   * is just the other argument. If neither are provided, the result
   * is of None type.
   *
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

  def updateListMap[K, V](m: Map[K, List[V]], k: K, v: V): Map[K, List[V]] = {
    if (m.contains(k)) {
      m.updated(k, m(k) :+ v)
    } else {
      m.updated(k, List(v))
    }
  }

  def updateListMap[K, V](m: Map[K, List[V]], k: K, vs: List[V]): Map[K, List[V]] = {
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


  def opt_func[A, B](f: A => B): Option[A] => Option[B] = {
    case Some(value) => Some(f(value))
    case None => None
  }

  def fopt_func[A, B](f: A => Option[B]): Option[A] => FOption[B] = {
    case Some(value) => f(value) match {
      case Some(value) => FSome(value)
      case None => FError
    }
    case None => FNone
  }

  sealed abstract class FOption[+A]() {
    def get: A

    def getOrElse[B >: A](default: => B): B

    def toOptionOrThrow(x: Exception): Option[A]

    def toOptionUnsafe = toOptionOrThrow(new RuntimeException("you brought this upon yourself"))

    def isEmpty: Boolean

    def isError: Boolean
  }

  final case class FSome[+A](value: A) extends FOption[A] {
    override def get: A = value

    override def toOptionOrThrow(x: Exception): Option[A] = Some(value)

    override def isError: Boolean = false

    override def isEmpty: Boolean = false

    override def getOrElse[B >: A](default: => B): B = value
  }

  case object FNone extends FOption[Nothing] {
    override def get: Nothing = throw new RuntimeException("empty")

    override def toOptionOrThrow(x: Exception): Option[Nothing] = None

    override def isEmpty: Boolean = true

    override def isError: Boolean = false

    override def getOrElse[B >: Nothing](default: => B): B = default
  }

  case object FError extends FOption[Nothing] {
    override def get: Nothing = throw new RuntimeException("error")

    override def toOptionOrThrow(x: Exception): Option[Nothing] = throw x

    override def isEmpty: Boolean = true

    override def isError: Boolean = true

    override def getOrElse[B >: Nothing](default: => B): B = default
  }

  private def typeMapVar(v: EVar, f_opt: Option[Type] => FOption[Type]): EVar = typeMapExpr(v, f_opt) match {
      case v2@EVar(_) => v2
      case _ => throw new RuntimeException("Unreachable!")
  }

  /**
   * Maps the function f_opt over the types of e1.
   * MODIFIES THE TYPE OF e1
   * @param e1 the expression to map over
   * @param f_opt the function to apply to the types
   * @return the expression with new types
   */
  private def typeMapExpr(e1 :Expr, f_opt : Option[Type] => FOption[Type]) : Expr =
    {
      f_opt(e1.typ) match
      {
        case FError => e1 match
        {
          case EInt(v, _, _) => val sign: TSignedNess =
            e1.typ match {
              case Some(TSizedInt(_, sign)) => sign match
              {
                case TSignVar(_) => TSigned()
                case defined => defined
              }
              case Some(_) => TSigned()
            }
            e1.typ = Some(TSizedInt(TBitWidthLen(log2(v)), sign))
          case t =>
            println(s"bad: ${t.typ}")
            throw LackOfConstraints(e1)
        }
        case t => e1.typ = t.toOptionUnsafe
      }
      e1 match
      {
        case e@EInt(v, _, _) =>
          if(e.typ.isEmpty)
            e.typ = Some(TSizedInt(TBitWidthLen(log2(v)), TSigned()))
          e.typ.get.matchOrError(e.pos, "Int", "TSizedInt")
          {
            case t: TSizedInt => t.len.matchOrError(e.pos, "TSizedInt", "len or var")
            {
              case TBitWidthLen(l) => e.copy(bits = l).copyMeta(e)
              case TBitWidthVar(v) if is_generic(v) => e
            }
          }
        case e@EIsValid(ex) => e.copy(ex = typeMapExpr(ex, f_opt)).copyMeta(e)
        case e@EFromMaybe(ex) => e.copy(ex = typeMapExpr(ex, f_opt)).copyMeta(e)
        case e@EToMaybe(ex) => e.copy(ex = typeMapExpr(ex, f_opt)).copyMeta(e)
        case e@EUop(_, ex) =>  e.copy(ex = typeMapExpr(ex, f_opt)).copyMeta(e)
        case e@EBinop(_, e1, e2) =>
          e.copy(e1 = typeMapExpr(e1, f_opt),
            e2 = typeMapExpr(e2, f_opt)).copyMeta(e)
        case e@ERecAccess(rec, fieldName) =>
          e.copy(fieldName = typeMapId(fieldName, f_opt), rec = typeMapExpr(rec, f_opt)).copyMeta(e)
        case e@ERecLiteral(fields) =>
          e.copy(fields = fields.map(idex => typeMapId(idex._1, f_opt) -> typeMapExpr(idex._2, f_opt))).copyMeta(e)
        case e@EMemAccess(mem, index, wmask, inHandle, outHandle, isAtomic) =>
          e.copy(mem = typeMapId(mem, f_opt), index = typeMapExpr(index, f_opt),
            wmask = opt_func(typeMapExpr(_, f_opt))(wmask), inHandle = inHandle.map(typeMapEVar(_, f_opt)),
            outHandle = outHandle.map(typeMapEVar(_, f_opt)), isAtomic).copyMeta(e)
        case e@EBitExtract(num, _, _) => e.copy(num = typeMapExpr(num, f_opt)).copyMeta(e)
        case e@ETernary(cond, tval, fval) =>
          e.copy(cond = typeMapExpr(cond, f_opt),
            tval = typeMapExpr(tval, f_opt),
            fval = typeMapExpr(fval, f_opt)).copyMeta(e)
        case e@EApp(func, args) =>
          e.copy(func = typeMapId(func, f_opt), args = args.map(typeMapExpr(_, f_opt))).copyMeta(e)
        case e@ECall(mod, _, args) =>
          e.copy(mod = typeMapId(mod, f_opt), args = args.map(typeMapExpr(_, f_opt))).copyMeta(e)
        case e@EVar(id) =>
          e.copy(id = typeMapId(id, f_opt)).copyMeta(e)
        case e@ECast(tp, exp) =>
          val ntp = f_opt(Some(tp)).get
          val tmp = e.copy(ctyp = ntp, exp = typeMapExpr(exp, f_opt)).copyMeta(e)
          tmp.typ = Some(ntp)
          tmp
        case expr: CirExpr => expr match
        {
          case e@CirLock(mem, _, _) => e.copy(mem = typeMapId(mem, f_opt)).copyMeta(e)
          case e@CirNew(mod, specialized, mods, _) => e.copy(mod = typeMapId(mod, f_opt),
            mods = mods.map((i: Id) => typeMapId(i, f_opt))).copyMeta(e)
          case e@CirCall(mod, args) => e.copy(mod = typeMapId(mod, f_opt),
            args = args.map(typeMapExpr(_, f_opt))).copyMeta(e)
          case _ => e1
        }
        case _ => e1
      }
    }

  private def typeMapEVar(eVar: Syntax.EVar, f_opt: Option[Syntax.Type] => Utilities.FOption[Syntax.Type]) :EVar =
    {
      typeMapExpr(eVar, f_opt) match
      {
        case e :EVar => e
      }
    }


  /**
   * maps a function over the types of a command
   * CHANGED THE TYPES OF THE ORIGINAL COMMAND
   * @param c1 the command to map over
   * @param f_opt the function from types to types
   * @return a new command with the types mapped
   */
  private def typeMapCmd(c1 :Command, f_opt :Option[Type] => FOption[Type]) :Command =
    {
      c1 match
      {
        case c@CSeq(c1, c2) => c.copy(c1 = typeMapCmd(c1, f_opt), c2 = typeMapCmd(c2, f_opt)).copyMeta(c)
        case c@CTBar(c1, c2) => c.copy(c1 = typeMapCmd(c1, f_opt), c2 = typeMapCmd(c2, f_opt)).copyMeta(c)
        case c@CIf(cond, cons, alt) =>
          c.copy(cond = typeMapExpr(cond, f_opt),
            cons = typeMapCmd(cons, f_opt),
            alt = typeMapCmd(alt, f_opt)).copyMeta(c)
        case c@CAssign(lhs, rhs) =>
          c.copy(lhs = typeMapEVar(lhs, f_opt), rhs = typeMapExpr(rhs, f_opt)).copyMeta(c)
        case c@CRecv(lhs, rhs) =>
          c.copy(lhs = typeMapExpr(lhs, f_opt), rhs = typeMapExpr(rhs, f_opt)).copyMeta(c)
        case c@CSpecCall(handle, pipe, args) =>
          c.copy(handle = typeMapEVar(handle, f_opt),
            pipe = typeMapId(pipe, f_opt),
            args = args.map(typeMapExpr(_, f_opt))).copyMeta(c)
        case c@CVerify(handle, args, preds, _) =>
          c.copy(handle = typeMapEVar(handle, f_opt),
            args = args.map(typeMapExpr(_, f_opt)),
            preds = preds.map(typeMapVar(_, f_opt))).copyMeta(c)
        case c@CInvalidate(handle) => c.copy(typeMapEVar(handle, f_opt)).copyMeta(c)
        case c@CPrint(args) => c.copy(args = args.map(typeMapExpr(_, f_opt))).copyMeta(c)
        case c@COutput(exp) => c.copy(exp = typeMapExpr(exp, f_opt)).copyMeta(c)
        case c@CReturn(exp) => c.copy(exp = typeMapExpr(exp, f_opt)).copyMeta(c)
        case c@CExpr(exp) => c.copy(exp = typeMapExpr(exp, f_opt)).copyMeta(c)
        case c@CLockStart(mod) => c.copy(mod = typeMapId(mod, f_opt)).copyMeta(c)
        case c@CLockEnd(mod) => c.copy(mod = typeMapId(mod, f_opt)).copyMeta(c)
        case c@CLockOp(mem@LockArg(id, evar), _, _, args, ret) =>
          c.copy(mem = mem.copy(id = typeMapId(id, f_opt), evar = evar match {
            case Some(e) => Some(typeMapEVar(e, f_opt))
            case None => None
          }), args = args.map(typeMapExpr(_, f_opt)),
            ret = ret.map(typeMapEVar(_, f_opt))).copyMeta(c)
        case c@CSplit(cases, default) =>
          c.copy(cases = cases.map(cs => cs.copy(cond = typeMapExpr(cs.cond, f_opt), body = typeMapCmd(cs.body, f_opt))),
            default = typeMapCmd(default, f_opt)).copyMeta(c)
        case _ => c1
      }
    }

  /**
   * Maps a function over the type of an Id
   * @param i the id to map over
   * @param f_opt the function to apply to i.typ
   * @return a COPY of i with a (potentially) new type
   */
  private def typeMapId(i: Id, f_opt: Option[Type] => FOption[Type]) :Id =
    {
      val ni = i.copy()
      ni.typ = f_opt(i.typ).toOptionOrThrow(LackOfConstraints(i))
      ni
    }



  def typeMapFunc(fun :FuncDef, f_opt :Option[Type] => FOption[Type]) :FuncDef =
    fun.copy(body = typeMapCmd(fun.body, f_opt))
  def typeMapModule(mod :ModuleDef, f_opt :Option[Type] => FOption[Type]) :ModuleDef =
    mod.copy(body = typeMapCmd(mod.body, f_opt),
      modules = mod.modules.map(p =>
        p.copy(typ = f_opt(Some(p.typ)).getOrElse(p.typ))
      )).copyMeta(mod)

  def typeMap(p: Prog, f: Type => Option[Type]) :Unit=
    {
      val f_opt = fopt_func(f)
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


  def getMemReads(e: Expr): List[EMemAccess] = e match {
    case EIsValid(ex) => getMemReads(ex)
    case EFromMaybe(ex) => getMemReads(ex)
    case EToMaybe(ex) => getMemReads(ex)
    case EUop(_, ex) => getMemReads(ex)
    case EBinop(_, e1, e2) => getMemReads(e1) ++ getMemReads(e2)
    case em@EMemAccess(_, _, _, _, _, _) => List(em)
    case EBitExtract(num, _, _) => getMemReads(num)
    case ETernary(cond, tval, fval) => getMemReads(cond) ++ getMemReads(tval) ++ getMemReads(fval)
    case EApp(_, args) => args.foldLeft(List[EMemAccess]())((l, a) => l ++ getMemReads(a))
    case ECall(_, _, args) => args.foldLeft(List[EMemAccess]())((l, a) => l ++ getMemReads(a))
    case ECast(_, exp) => getMemReads(exp)
    case _ => List()
  }

  val (defaultReadPorts, defaultWritePorts) = (5, 2)

  val lock_handle_prefix = "_lock_id_"
  val is_handle_var :Id => Boolean =
    { id: Id => id.v.startsWith(lock_handle_prefix) }

  val generic_type_prefix = "__SUPERSPECIALGENERIC_"
  def is_generic(t :Any) :Boolean = t match {
    case TNamedType(name) => name.v.startsWith(generic_type_prefix)
    case TSizedInt(l, _) => is_generic(l)
    case TBitWidthVar(name) => name.v.startsWith(generic_type_prefix)
    case _:Type => false
    case id:Id => id.v.startsWith(generic_type_prefix)
    case s:String => s.startsWith(generic_type_prefix)
  }

  val not_gen_pref = "__NOT_GEN"
  def degenerify(t :Type) :Type =
    {
      def _degenerify(t :Type) :Type = t match
      {
        case TSignVar(nm) if (is_generic(nm)) =>
          TSignVar(Id(not_gen_pref + nm.v).copyMeta(nm))
        case TSizedInt(len, sign) =>
          TSizedInt(degenerify(len).asInstanceOf[TBitWidth], degenerify(sign).asInstanceOf[TSignedNess])
        case TFun(args, ret) =>
          TFun(args.map(degenerify), degenerify(ret))
        case TRecType(name, fields) =>
          TRecType(name, fields.map((idtp) => (idtp._1, degenerify(idtp._2))))
        case TModType(inputs, refs, retType, name) =>
          TModType(inputs.map(degenerify), refs.map(degenerify), retType.map(degenerify), name)
        case TReqHandle(tp, rtyp) => TReqHandle(degenerify(tp), rtyp)
        case TNamedType(name) if is_generic(name) =>
          TNamedType(Id(not_gen_pref + name.v).copyMeta(name))
        case TMaybe(btyp) =>
          TMaybe(degenerify(btyp))
        case TBitWidthVar(nm) if is_generic(nm) =>
          TBitWidthVar(Id(not_gen_pref + nm.v).copyMeta(nm))
        case TObject(name, typParams, methods) =>
          TObject(name, typParams.map(degenerify), methods.map(idtp => (idtp._1, (degenerify(idtp._2._1).asInstanceOf[TFun], idtp._2._2))))
        case other => other
      }
      _degenerify(t).copyMeta(t)
    }

  def specialise(t :Type, s :List[Int]) :Type = t match {
    case t :TObject =>
    val new_types = s.map(l => TBitWidthLen(l))
    t.copy(typParams = new_types,
      methods = t.methods.map((id_funlat) => {
        val old_fun = id_funlat._2._1
        val assoc_list = t.typParams.map({
          case TBitWidthVar(v) => v.v
          case TNamedType(v) => v.v
          case _ => ""
        }).zip(new_types)
        val map = assoc_list.toMap
        val new_args = old_fun.args.map
        { case ts@TSizedInt(len@TBitWidthVar(name), sign) => TSizedInt(map.getOrElse(name.v, len).copyMeta(len).asInstanceOf[TBitWidth], sign).copyMeta(ts)
        case other => other }
        val new_ret = old_fun.ret match {
          case ts@TSizedInt(len@TBitWidthVar(name), sign) => TSizedInt(map.getOrElse(name.v, len).copyMeta(len).asInstanceOf[TBitWidth], sign).copyMeta(ts)
          case other => other
        }
        (id_funlat._1, (TFun(new_args, new_ret).copyMeta(old_fun).asInstanceOf[TFun], id_funlat._2._2))
      })).copyMeta(t)
    case _ => t
  }
}
