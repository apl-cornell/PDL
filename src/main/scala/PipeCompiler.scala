package pipedsl

import pipedsl.common.DAGSyntax._
import pipedsl.common.Errors.{UnexpectedExpr, UnexpectedType}
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
    val extMems: Map[Id, Process] = m.modules.foldLeft[Map[Id,Process]](Map()) ((l, m) => {
      l + (m.name -> (m.typ match {
        case memt: TMemType => new PMemory(m.name, memt)
        case modt: TModType => new PBlackBox(m.name, modt)
        case _ => throw UnexpectedType(m.pos, m.name.toString, "Module or Memory", m.typ)
      }))
    })
    convertToStage(m.body, extMems)
  }

  def convertToStage(c: Command, ext: Map[Id, Process]): PStage = c match {
    case CTBar(c1, c2) => {
      val stg1 = convertToStage(c1, ext)
      val stg2 = convertToStage(c2, ext)
      val extSends = getExtSends(stg1.body)
      //TODO for each different receiver, create a channel
      //TODO then create send and recv objects to represent communication on those channels
      val recvars = getReadVarsC(stg2.body)
      // vars sent from s1 to s2 directly are all that are read,
      // but not sent via an external module/memory read
      val s1tos2Sends = recvars -- extSends.foldLeft[Set[EVar]](Set())((s, exts) => { s + exts._1 })
      val s1Tos2 = new Channel(stg1, stg2)
      val s = new Send(None, s1tos2Sends.toList, s1Tos2)
      val r = new Receive(None, s1tos2Sends.toList, s1Tos2)
      stg1.sends = List(s)
      stg2.recvs = List(r)
      stg1
    }
    case _ => {
      new PStage(nextStageId(), List(), c, List())
    }
  }

  //Get the variable we're sending and the variable we expect
  //To receive into
  //Returns (RecVar, SendVar, ExtMod)
  def getExtSends(c: Command): List[(EVar, EVar, Id)] = c match {
    case CSeq(c1, c2) => getExtSends(c1) ++ getExtSends(c2)
    case CIf(_, cons, alt) => getExtSends(cons) ++ getExtSends(alt)
    case CAssign(_, _) => List()
    case CRecv(lhs, rhs) =>(lhs, rhs) match {
      case (e:EVar, EMemAccess(mem, idx@EVar(_))) => {
        List((e, idx, mem))
      }
      case (EMemAccess(_,_), _) => throw UnexpectedExpr(lhs)
      case _ => List()
    }
    case _ => List()
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

  //All variables written by this stage
  def getWriteVarsC(c: Command): Set[EVar] = c match {
    case CSeq(c1, c2) => getWriteVarsC(c1) ++ getWriteVarsC(c2)
    case CIf(_, cons, alt) => getWriteVarsC(cons) ++ getWriteVarsC(alt)
    case CAssign(lhs, _) => getWriteVarsE(lhs)
    case CRecv(lhs, _) => getWriteVarsE(lhs) //TODO include this?
    case _ => Set()
  }

  //All variables referenced in these expressions
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

  //We only record writes to variables, not memories here
  def getWriteVarsE(e: Expr): Set[EVar] = e match {
    case ev: EVar => Set(ev)
    case _ => Set()
  }
}
