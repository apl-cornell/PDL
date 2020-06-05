package pipedsl

import pipedsl.common.DAGSyntax._
import pipedsl.common.Errors.UnexpectedType
import pipedsl.common.Syntax
import pipedsl.common.Syntax._

object PipeCompiler {

  var counter = 0

  def nextStageId(): Id = {
    val s = s"Stage__$counter"
    counter += 1
    Id(s)
  }

  def compileToDag(m: ModuleDef): PStage = {
    val extMems: List[Process] = m.modules.foldLeft[List[Process]](List()) ((l, m) => {
      l :+ (m.typ match {
        case memt: TMemType => new PMemory(m.name, memt)
        case modt: TModType => new PBlackBox(m.name, modt)
        case _ => throw UnexpectedType(m.pos, m.name.toString, "Module or Memory", m.typ)
      })
    })
    convertToStage(m.body, extMems)
  }

  def convertToStage(c: Command, ext: List[Process]): PStage = c match {
    case CTBar(c1, c2) => {
      val stg1 = convertToStage(c1, ext)
      val stg2 = convertToStage(c2, ext)
      val recvars = getReadVarsC(stg2.body)
      val s1Tos2 = new Channel(stg1, stg2)
      //TODO get rid of the recvars that come from receive statments (i.e. memory reads)
      val s = new Send(None, recvars.toList, s1Tos2)
      val r = new Receive(None, recvars.toList, s1Tos2)
      stg1.sends = List(s)
      stg2.recvs = List(r)
      stg1
    }
    case _ => {
      new PStage(nextStageId(), List(), c, List())
    }
  }

  //All variables that are read from prior stage
  def getReadVarsC(c: Command): Set[EVar] = c match {
    case CSeq(c1, c2) => {
      getReadVarsC(c1) ++ (getReadVarsC(c2) -- getWriteVarsC(c1))
    }
    case CIf(cond, cons, alt) => {
      getReadVarsE(cond) ++ getReadVarsC(cons) ++ getReadVarsC(alt)
    }
    case CAssign(lhs, rhs) => (lhs match {
      case EMemAccess(_, index) => getReadVarsE(index)
      case _ => Set()
    }) ++ getReadVarsE(rhs)
    case CRecv(lhs, rhs) => (lhs match {
      case EMemAccess(_, index) => getReadVarsE(index)
      case _ => Set()
    }) ++ getReadVarsE(rhs)
    case CCall(_, args) => args.foldLeft[Set[EVar]](Set())((l, a) => {
      l ++ getReadVarsE(a)
    })
    case COutput(exp) => getReadVarsE(exp)
    case CReturn(exp) => getReadVarsE(exp)
    case CExpr(exp) => getReadVarsE(exp)
    case Syntax.CEmpty => Set()
  }

  def getWriteVarsC(c: Command): Set[EVar] = c match {
    case CSeq(c1, c2) => getWriteVarsC(c1) ++ getWriteVarsC(c2)
    case CIf(_, cons, alt) => getWriteVarsC(cons) ++ getWriteVarsC(alt)
    case CAssign(lhs, _) => getWriteVarsE(lhs)
    case CRecv(lhs, _) => getWriteVarsE(lhs) //TODO include this?
    case _ => Set()
  }

  def getReadVarsE(e: Expr): Set[EVar] = e match {
    case EBinop(_, e1, e2) => getReadVarsE(e1) ++ getReadVarsE(e2)
    case ERecAccess(rec, _) => getReadVarsE(rec)
    case EBitExtract(num, _, _) => getReadVarsE(num)
    case ETernary(cond, tval, fval) => getReadVarsE(cond) ++ getReadVarsE(tval) ++ getReadVarsE(fval)
    case EApp(_, args) => args.foldLeft[Set[EVar]](Set())( (s, a) => { s ++ getReadVarsE(a) })
    case EMemAccess(_, index) => getReadVarsE(index)
    case ev: EVar => Set(ev)
    case _ => Set()
  }

  def getWriteVarsE(e: Expr): Set[EVar] = e match {
    case ev: EVar => Set(ev)
    case _ => Set()
  }
}
