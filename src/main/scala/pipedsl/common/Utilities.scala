package pipedsl.common

import Syntax._
import pipedsl.common.DAGSyntax.PStage

object Utilities {

  def log2(x: Int): Int = {
    var y = x
    if (x < 0) { y = -y }
    var bits = 1;
    while (y > 1) {
      y = y >> 1
      bits += 1
    }
    bits
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
   *
   * @param c
   * @return
   */
  def getAllVarNames(c: Command): Set[Id] = c match {
    case CSeq(c1, c2) => getAllVarNames(c1) ++ getAllVarNames(c2)
    case CTBar(c1, c2) => getAllVarNames(c1) ++ getAllVarNames(c2)
    case CSplit(cases, default) => {
      val names = getAllVarNames(default)
      cases.foldLeft(names)((v, c) => {
        v ++ getUsedVars(c.cond) ++ getAllVarNames(c.body)
      })
    }
    case CIf(cond, cons, alt) => getUsedVars(cond) ++ getAllVarNames(cons) ++ getAllVarNames(alt)
    case CAssign(lhs, rhs) => getUsedVars(lhs) ++ getUsedVars(rhs)
    case CRecv(lhs, rhs) => getUsedVars(lhs) ++ getUsedVars(rhs)
    case CLockOp(mem, _) => Set(mem)
    case CSpeculate(predVar, predVal, verify, body) =>
     getUsedVars(predVal) ++ getAllVarNames(verify) ++ getAllVarNames(body) + predVar.id
    case CCheck(predVar) => Set(predVar)
    case CCall(id, args) => args.foldLeft[Set[Id]](Set(id))((s, a) => { s ++ getUsedVars(a) })
    case COutput(exp) => getUsedVars(exp)
    case CReturn(exp) => getUsedVars(exp)
    case CExpr(exp) => getUsedVars(exp)
    case ICondCommand(cond, c2) => getUsedVars(cond) ++ getAllVarNames(c2)
    case ISpeculate(specId, specVar, value) => getUsedVars(value) + specId ++ getUsedVars(specVar)
    case IUpdate(specId,value,originalSpec) => getUsedVars(value) ++ getUsedVars(originalSpec) + specId
    case Syntax.CEmpty => Set()
  }

  //Get variables written this cycle combinationally
  def getWrittenVars(c: Command): Set[Id] = c match {
    case CSeq(c1, c2) => getWrittenVars(c1) ++ getWrittenVars(c2)
    case CTBar(c1, c2) => getWrittenVars(c1) ++ getWrittenVars(c2)
    case CSplit(cases, default) =>
      cases.foldLeft(getWrittenVars(default))((v, c) => {
        v ++ getWrittenVars(c.body)
      })
    case CIf(_, cons, alt) => getWrittenVars(cons) ++ getWrittenVars(alt)
    case CAssign(lhs, _) => lhs match { case EVar(id) => Set(id) ; case _ => Set() }
    case CRecv(lhs, _) => lhs match { case EVar(id) => Set(id) ; case _ => Set() }
    case CSpeculate(_, _, verify, body) => getWrittenVars(verify) ++ getWrittenVars(body)
    case ICondCommand(_, c2) => getWrittenVars(c2)
    case ISpeculate(s, svar, _) => Set(s, svar.id)
    case IMemRecv(_, _, data) => if (data.isDefined) Set(data.get.id) else Set()
    case IMemSend(_, handle, _, _, _) => Set(handle.id)
    case _ => Set()
  }

  def getWrittenVars(cs: List[Command]): Set[Id] = {
    cs.foldLeft(Set[Id]())((s, c) => s ++ getWrittenVars(c))
  }

  /**
   *
   * @param c
   * @return
   */
  def getUsedVars(c: Command): Set[Id] = c match {
    case CSeq(c1, c2) => getUsedVars(c1) ++ getUsedVars(c2)
    case CTBar(c1, c2) => getUsedVars(c1) ++ getUsedVars(c2)
    case CSplit(cases, default) =>
      cases.foldLeft(getUsedVars(default))((v, c) => {
        v ++ getUsedVars(c.cond) ++ getUsedVars(c.body)
      })
    case CIf(cond, cons, alt) => getUsedVars(cond) ++ getUsedVars(cons) ++ getUsedVars(alt)
    case CAssign(_, rhs) => getUsedVars(rhs)
    case CRecv(lhs, rhs) => getUsedVars(rhs) ++ (lhs match {
      case e:EMemAccess => getUsedVars(e)
      case _ => Set()
    })
    case CCall(_, args) => args.foldLeft[Set[Id]](Set())( (s, a) => s ++ getUsedVars(a) )
    case COutput(exp) => getUsedVars(exp)
    case CReturn(exp) => getUsedVars(exp)
    case CExpr(exp) => getUsedVars(exp)
    case CSpeculate(_, predVal, verify, body) => getUsedVars(predVal) ++ getUsedVars(verify) ++ getUsedVars(body)
    case CCheck(predVar) => Set(predVar)
    case ICondCommand(cond, c2) => getUsedVars(cond) ++ getUsedVars(c2)
    case IUpdate(specId, value, originalSpec) => getUsedVars(value) + specId ++ getUsedVars(originalSpec)
    case ICheck(specId, value) => getUsedVars(value) + specId
    case ISpeculate(_,_, value) => getUsedVars(value)
    case IMemRecv(handle, _, _) => Set(handle.id)
    case IMemSend(_, _, _, data, addr) =>
      if (data.isDefined) {
        Set(data.get.id, addr.id)
      } else Set(addr.id)
    case _ => Set()
  }

  /**
   *
   * @param e
   * @return
   */
  def getUsedVars(e: Expr): Set[Id] = e match {
    case EUop(_, ex) => getUsedVars(ex)
    case EBinop(_, e1, e2) => getUsedVars(e1) ++ getUsedVars(e2)
    case ERecAccess(rec, _) => getUsedVars(rec)
    case EMemAccess(_, index) => getUsedVars(index) //memories aren't variables, they're externally defined
    case EBitExtract(num, _, _) => getUsedVars(num)
    case ETernary(cond, tval, fval) => getUsedVars(cond) ++ getUsedVars(tval) ++ getUsedVars(fval)
    case EApp(_, args) => args.foldLeft[Set[Id]](Set())((s, a) => { s ++ getUsedVars(a) })
      //functions are also externally defined
    case EVar(id) => id.typ = e.typ; Set(id)
    case ECast(_, exp) => getUsedVars(exp)
    case _ => Set()
  }

  /**
   *
   * @param cs
   * @return
   */
  def getUsedVars(cs: List[Command]): Set[Id] = {
    cs.foldLeft(Set[Id]())((s,c) => s ++ getUsedVars(c))
  }

  /**
   *
   * @param stg
   * @return
   */
  def getReachableStages(stg: PStage): Set[PStage] = {
    visit[Set[PStage]](stg, Set(stg), (s, stgs) => stgs + s);
  }

  /**
   *
   * @param stg
   * @param start
   * @param visitor
   * @tparam T
   * @return
   */
  def visit[T](stg: PStage, start: T, visitor: (PStage, T) => T): T = {
    var result = visitor(stg, start)
    var visited = Set(stg);
    var fringe: Set[PStage] = stg.succs
    while (fringe.nonEmpty) {
      val next = fringe.head
      fringe = fringe.tail
      if (!visited.contains(next)) {
        result = visitor(next, result)
        visited = visited + next
        fringe = fringe ++ next.succs
      }
    }
    result
  }

  def flattenStageList(stgs: List[PStage]): List[PStage] = {
    stgs.foldLeft(List[PStage]())((l, stg) => stg match {
      case s: DAGSyntax.IfStage => (l :+ s) ++ flattenStageList(s.trueStages) ++ flattenStageList(s.falseStages)
      case s: DAGSyntax.SpecStage => (l :+ s) ++ flattenStageList(s.verifyStages) ++ flattenStageList(s.specStages)
      case _ => l :+ stg
    })
  }
  def flattenIfStages(stgs: List[PStage]): List[PStage] = {
    stgs.foldLeft(List[PStage]())((l, stg) => stg match {
      case s: DAGSyntax.IfStage => (l :+ s) ++ flattenStageList(s.trueStages) ++ flattenStageList(s.falseStages)
      case _ => l :+ stg
    })
  }
  /**
   *
   * @param condL
   * @param condR
   * @return
   */
  def andExpr(condL: Option[Expr], condR: Option[Expr]): Option[Expr] = (condL, condR) match {
    case (None, None) => None
    case (Some(e1), Some(e2)) => Some(AndOp(e1, e2))
    case (Some(_), None) => condL
    case (None, Some(_)) => condR
  }

  implicit class RichOption[A](opt: Option[A]) {
    def getOrThrow[T <: Throwable](except: T) = opt match {
      case Some(v) => v
      case None => throw except
    }
  }


}
