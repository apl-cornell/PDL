package pipedsl.typechecker

import pipedsl.common.Errors.{MissingType, UnexpectedType}
import pipedsl.common.Syntax
import pipedsl.common.Syntax._
import pipedsl.typechecker.Environments.TypeEnvironment

//TODO kinds of typechecking we need to do:

//Normal stuff
//  Assignment
//  Function calls and returns
//  Module Instantiations
//
//Pipeline Stuff
//  Call statment exactly once in each path w/ typed args
//  Dynamic Lock Checking
//  More complex lock types (indexed / dependent)
//  Speculation Restrictions
//
//Security Types
//  Normal IFC
//  How to Check "call"s
//  Speculation Labels
object TypeChecker {

  def typeCheck(p: Prog): Unit = {
    val Prog(fdefs, mdefs, cir) = p
  }


  def checkFuncDef(f: FuncDef) = {

  }

  def checkModuleDef(m: ModuleDef) = {

  }

  def checkCommand(c: Command, tenv: TypeEnvironment): TypeEnvironment = c match {
    case CSeq(c1, c2) => {
      val e2 = checkCommand(c1, tenv)
      checkCommand(c2, e2)
    }
    case CTBar(c1, c2) => {
      val e2 = checkCommand(c1, tenv)
      checkCommand(c2, e2)
    }
    case CIf(cond, cons, alt) => {
      val condTyp = checkExpression(cond, tenv)
      condTyp.matchOrError(cond.pos, "if condition", "boolean") { case _: TBool => () }
      val etrue = checkCommand(cons, tenv)
      val efalse = checkCommand(alt, tenv)
      etrue.intersect(efalse)
    }
    case CAssign(lhs, rhs) => {
      val lTyp = checkExpression(lhs, tenv)
      val rTyp = checkExpression(rhs, tenv)
    }
    case CRecv(lhs, rhs) =>
    case CCall(id, args) =>
    case COutput(exp) =>
    case CReturn(exp) =>
    case CExpr(exp) =>
    case Syntax.CEmpty =>
  }

  def checkExpression(e: Expr, tenv: TypeEnvironment): Type = {
    TVoid()
  }
}
