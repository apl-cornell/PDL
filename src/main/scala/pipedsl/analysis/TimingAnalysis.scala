package pipedsl.analysis

import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.relation.Tree
import pipedsl.common.Errors.{UnavailableArgUse, UnexpectedAsyncReference, UnexpectedType}
import pipedsl.common.Syntax.Latency.{Asynchronous, Combinational, Latency}
import pipedsl.common.Syntax.{CAssign, CCheck, CEmpty, CExpr, CIf, CLockEnd, CLockOp, CLockStart, COutput, CPrint, CRecv, CReturn, CSeq, CSpeculate, CSplit, CTBar, Command, EApp, EBinop, EBitExtract, ECall, ECast, EMemAccess, ERecAccess, ETernary, EUop, EVar, Id, Latency, ModuleDef, Prog, ProgramNode, TMemType}

class TimingAnalysis(program: Tree[ProgramNode, Prog], typeAnalysis: TypeAnalysis) extends Attribution{

  type Available = Set[Id]
  val NoneAvailable: Available = Set[Id]()

  def checkCommand(c: Command): Unit = c match {
    case CSeq(c1, c2) => checkCommand(c1); checkCommand(c2)
    case CTBar(c1, c2) => checkCommand(c2); checkCommand(c2)
    case CIf(cond, cons, alt) => 
      if (latency(cond) == Latency.Combinational) {
        throw UnexpectedAsyncReference(cond.pos, cond.toString)
      }
      checkCommand(cons)
      checkCommand(alt)
    case CAssign(lhs, rhs, typ) =>       
      if (latency(rhs) != Latency.Combinational) {
        throw UnexpectedAsyncReference(rhs.pos, rhs.toString)
      }
    case CRecv(lhs, rhs, typ) => 
      latency(lhs)
      latency(rhs)
    case CPrint(evar) => latency(evar)
    case COutput(exp) =>      
      if (latency(exp) != Combinational) {
        throw UnexpectedAsyncReference(exp.pos, exp.toString)
      }
    case CReturn(exp) =>
      if (latency(exp) != Combinational) {
        throw UnexpectedAsyncReference(exp.pos, exp.toString)
      }
    case CExpr(exp) => latency(exp)
    case CSpeculate(predVar, predVal, verify, body) =>
    case CCheck(predVar) =>
    case CSplit(cases, default) =>
      checkCommand(default)
      for (c <- cases) {
        if(latency(c.cond) != Latency.Combinational) {
          throw UnexpectedAsyncReference(c.cond.pos, c.cond.toString)
        }
        checkCommand(c.body)
      }
    case CLockOp(mem, op) =>
      if (mem.evar.isDefined) {
        latency(mem.evar.get)
      }
    case CLockStart(mod) =>
    case CLockEnd(mod) =>
    case CEmpty =>
    case _ =>
  }
  
  val availableNow: ProgramNode => Available = {
    attr {
        case program.prev(program.parent(c@CSeq(c1, c2))) => availableNow(c1)
        case program.prev(program.parent(c@CTBar(c1, c2))) => availableNow(c1) ++ availableNext(c1)
        case program.prev(c@CAssign(lhs@EVar(id), rhs, typ)) =>
          availableNow(c) + id
        case program.prev(c@CIf(cond, cons, alt)) => 
          availableNowAfterNode(typeAnalysis.rightMostLeaf(cons))
            .intersect(availableNowAfterNode(typeAnalysis.rightMostLeaf(alt)))
        case program.prev(c@CSplit(cases, default)) =>
          cases.foldLeft[Available](availableNowAfterNode(typeAnalysis.rightMostLeaf(default)))(
            (s,c) => s.intersect(availableNow(typeAnalysis.rightMostLeaf(c.body)))
          )
        case program.parent(m@ModuleDef(name, inputs, modules, ret, body)) =>
          val inputs = m.inputs.foldLeft[Available](NoneAvailable)((av,p) => {
            av + p.name
          })
          val allAvailable = m.modules.foldLeft[Available](inputs)((av, m) => {
            av + m.name
          })
          allAvailable
        case program.parent(p) => availableNow(p)
    }
  }
  
  /** 
   * Attribute that gives the available variable after we are are at a node; note that "after" 
   * is in terms of the sequential view of the program, rather than the recursive AST view
   */
  val availableNowAfterNode:ProgramNode => Available = {
    attr {
      case c@CAssign(lhs@EVar(id), rhs, typ) => availableNow(c) + id
      case p => availableNow(p)
    }
  }
  
  val availableNext: ProgramNode => Available = 
    attr {
      case program.prev(program.parent(c@CSeq(c1, c2))) => availableNext(c1)
      case program.prev(program.parent(c@CTBar(c1, c2))) => NoneAvailable
      case program.prev(c@CRecv(lhs, rhs, typ)) => 
        (lhs, rhs) match {
          case (EVar(id), _) => availableNext(c) + id
          case (EMemAccess(_,_), EMemAccess(_,_)) => throw UnexpectedAsyncReference(lhs.pos, "Both sides of <- cannot be memory or modules references")
          case _ => availableNext(c)
        }
      case program.prev(c@CIf(cond, cons, alt)) =>
        availableNext(typeAnalysis.rightMostLeaf(cons)).intersect(availableNext(typeAnalysis.rightMostLeaf(alt)))
      case program.prev(c@CSplit(cases, default)) =>
        cases.foldLeft[Available](availableNext(typeAnalysis.rightMostLeaf(default)))(
          (s,c) => s.intersect(availableNext(typeAnalysis.rightMostLeaf(c.body)))
        )
      case program.parent(m@ModuleDef(name, inputs, modules, ret, body)) => NoneAvailable
      case program.parent(p) => availableNext(p)
  }
  
  val availableNextAfterNode: ProgramNode => Available = {
    attr {
      case c@CRecv(lhs@EVar(id), rhs, typ) => availableNext(c) + id
      case p => availableNext(p)
    }
  }
  
  val latency: ProgramNode => Latency = {
    case EUop(_, e) => latency(e) match {
      case Combinational => Combinational
      case _ => throw UnexpectedAsyncReference(e.pos, e.toString)
    }
    case EBinop(_, e1, e2) =>
      (latency(e1), latency(e2)) match {
        case (Combinational, Combinational) => Combinational
        case (Combinational, _) => throw UnexpectedAsyncReference(e2.pos, e2.toString)
        case (_, Combinational) => throw UnexpectedAsyncReference(e1.pos, e1.toString)
      }
    case ERecAccess(rec, _) => latency(rec) match {
      case Combinational => Combinational
      case _ => throw UnexpectedAsyncReference(rec.pos, rec.toString)
    }
    case e@EMemAccess(m, index) => typeAnalysis.typeCheck(m) match {
      case TMemType(_, _, rLat, wLat) =>
        val memLat = if (isRhs(e)) { rLat } else { wLat }
        val indexExpr = latency(index)
        indexExpr match {
          case Combinational => memLat
          case _ => throw UnexpectedAsyncReference(index.pos, index.toString)
        }
      case _ => throw UnexpectedType(m.pos, m.v, "Mem Type",typeAnalysis.typeCheck(m))
    }
    case EBitExtract(num, _, _) => latency(num) match {
      case Combinational => Combinational
      case _ => throw UnexpectedAsyncReference(num.pos, num.toString)
    }
    case ETernary(cond, tval, fval) =>
      (latency(cond), latency(tval), latency(fval)) match {
        case (Combinational, Combinational, Combinational) => Combinational
        case (_, Combinational, Combinational) => throw UnexpectedAsyncReference(cond.pos, cond.toString)
        case (_, _, Combinational) => throw UnexpectedAsyncReference(tval.pos, tval.toString)
        case _ => throw UnexpectedAsyncReference(fval.pos, fval.toString)
      }
    case EApp(_, args) =>
      args.foreach(a => if(latency(a) != Combinational) {
        throw UnexpectedAsyncReference(a.pos, a.toString)
      })
      Combinational
    case ECall(_, args) =>
      args.foreach(a => if(latency(a) != Combinational) {
        throw UnexpectedAsyncReference(a.pos, a.toString)
      })
      Asynchronous
    case e@EVar(id) => if(!availableNow(e)(id) && isRhs(e)) { throw UnavailableArgUse(e.pos, id.toString)} else { Combinational }
    case ECast(_, exp) => latency(exp)
    case _ => Combinational
  }
  
  val isRhs: ProgramNode => Boolean = {
    attr {
      case program.prev(program.parent(_:CAssign)) => true  
      case program.prev(program.parent(_:CRecv)) => true
      case _ => false  
    }
  }
}
