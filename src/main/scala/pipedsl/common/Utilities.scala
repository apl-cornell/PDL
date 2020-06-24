package pipedsl.common

import Syntax._

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
    case CSpeculate(predVar, predVal, body) =>
     getUsedVars(predVal) ++ getAllVarNames(body) + predVar.id
    case CCheck(predVar, realVal) => getUsedVars(realVal) + predVar
    case CResolve(predVar) => Set(predVar)
    case CCall(id, args) => args.foldLeft[Set[Id]](Set(id))((s, a) => { s ++ getUsedVars(a) })
    case COutput(exp) => getUsedVars(exp)
    case CReturn(exp) => getUsedVars(exp)
    case CExpr(exp) => getUsedVars(exp)
    case CDecl(id, _, _) => Set(id)
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
    case CSpeculate(_, _, body) => getWrittenVars(body)
    case _ => Set()
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
    case CAssign(lhs, rhs) => getUsedVars(rhs)
    case CRecv(lhs, rhs) => getUsedVars(rhs)
    case CCall(id, args) => args.foldLeft[Set[Id]](Set())( (s, a) => s ++ getUsedVars(a) )
    case COutput(exp) => getUsedVars(exp)
    case CReturn(exp) => getUsedVars(exp)
    case CExpr(exp) => getUsedVars(exp)
    case CSpeculate(predVar, predVal, body) => getUsedVars(predVal) ++ getUsedVars(body)
    case CCheck(predVar, realVal) => getUsedVars(realVal) + predVar
    case CResolve(predVar) => Set(predVar)
    case _ => Set()
  }

  /**
   *
   * @param e
   * @return
   */
  def getUsedVars(e: Expr): Set[Id] = e match {
    case EUop(op, ex) => getUsedVars(ex)
    case EBinop(op, e1, e2) => getUsedVars(e1) ++ getUsedVars(e2)
    case ERecAccess(rec, fieldName) => getUsedVars(rec)
    case EMemAccess(mem, index) => getUsedVars(index) //memories aren't variables, they're externally defined
    case EBitExtract(num, start, end) => getUsedVars(num)
    case ETernary(cond, tval, fval) => getUsedVars(cond) ++ getUsedVars(tval) ++ getUsedVars(fval)
    case EApp(func, args) => args.foldLeft[Set[Id]](Set())((s, a) => { s ++ getUsedVars(a) }) //functions are also externally defined
    case EVar(id) => Set(id)
    case ECast(_, exp) => getUsedVars(exp)
    case _ => Set()
  }

  implicit class RichOption[A](opt: Option[A]) {
    def getOrThrow[T <: Throwable](except: T) = opt match {
      case Some(v) => v
      case None => throw except
    }
  }

}
