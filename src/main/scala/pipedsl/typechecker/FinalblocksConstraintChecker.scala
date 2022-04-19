package pipedsl.typechecker

import org.apache.commons.io.filefilter.FalseFileFilter
import pipedsl.common.Syntax._
import pipedsl.typechecker.Environments._
import pipedsl.common.Errors._

class FinalblocksConstraintChecker {
  private var currentMod: Id = Id("-invalid-")
  private var ifPostCall: Boolean = false
  private var hasCommitBlk: Boolean = false

  def check(p:Prog) : Unit = {
    val Prog(_, _, moddefs, _) = p
    moddefs.foreach(m => checkModule(m))
  }

  private def checkModule(moduleDef: ModuleDef): Unit = {
    currentMod = moduleDef.name
    ifPostCall = false
    hasCommitBlk = false

    moduleDef.commit_blk match {
      case Some(c) =>
        hasCommitBlk = true
        checkCommand(c)
      case _ => ()
    }

    moduleDef.except_blk match {
      case ExceptEmpty() => ()
      case ExceptFull(args, c) if (hasCommitBlk == true) => checkCommand(c)
      case ExceptFull(args, c) if (hasCommitBlk == false) =>
        throw MalformedExceptBlock(c.pos)
    }
  }

  private def checkCommand(command: Command): Unit = command match {
    case CSeq(c1, c2) => val s1 = checkCommand(c1); checkCommand(c2)
    case CTBar(c1, c2) => val s1 = checkCommand(c1); checkCommand(c2)
    case CIf(_, cons, alt) => val s1 = checkCommand(cons); checkCommand(alt)
    case CSplit(cases, default) =>
      checkCommand(default)
    case CLockOp(mem, _, _, _, _) =>
      if (ifPostCall == true) throw MalformedLockTypes("Cannot Reserve any locks after a call")
    case CAssign(lhs, rhs) => checkExpr(lhs); checkExpr(rhs)
    case CRecv(lhs, rhs) => checkExpr(lhs); checkExpr(rhs)
    case CExcept(args) => throw IllegalThrowPlacement(command.pos)
    case _ => ()
  }

  private def checkExpr(e: Expr): Unit = e match {
    case EIsValid(ex) => checkExpr(ex)
    case EFromMaybe(ex) => checkExpr(ex)
    case EToMaybe(ex) => checkExpr(ex)
    case EUop(_, ex) => checkExpr(ex)
    case EBinop(_, e1, e2) => checkExpr(e1); checkExpr(e2)
    case ETernary(cond, tval, fval) => checkExpr(cond); checkExpr(tval); checkExpr(fval)
    case EApp(_, args) => ifPostCall = true
    case ECall(_, _, _, _) => ifPostCall = true
  }
}
