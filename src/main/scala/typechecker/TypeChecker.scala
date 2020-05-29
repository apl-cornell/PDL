package pipedsl.typechecker

import pipedsl.common.Errors._
import pipedsl.common.Syntax._
import Environments.TypeEnvironment
import Subtypes._


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
      if (isSubtype(rTyp, lTyp)) tenv
      else throw UnexpectedSubtype(rhs.pos, "assignment", lTyp, rTyp)
    }
    case CRecv(lhs, rhs) => {
      val lTyp = checkExpression(lhs, tenv)
      val rTyp = checkExpression(rhs, tenv)
      if (isSubtype(rTyp, lTyp)) tenv
      else throw UnexpectedSubtype(rhs.pos, "recv", lTyp, rTyp)
    }
    case CCall(id, args) => //TODO need module types first
    case COutput(exp) => {
      checkExpression(exp, tenv)
      tenv
    }
    case CReturn(exp) => //TODO need context for being in a function definition
    case CExpr(exp) =>{
      checkExpression(exp, tenv)
      tenv
    }
    case CEmpty => tenv
  }

  def checkExpression(e: Expr, tenv: TypeEnvironment): Type = e match {
    case EInt(v, base, bits) =>
    case EBool(v) =>
    case EBinop(op, e1, e2) =>
    case ERecAccess(rec, fieldName) =>
    case ERecLiteral(fields) =>
    case EMemAccess(mem, index) =>
    case EBitExtract(num, start, end) =>
    case ETernary(cond, tval, fval) =>
    case EApp(func, args) =>
    case EVar(id) =>
  }
}
