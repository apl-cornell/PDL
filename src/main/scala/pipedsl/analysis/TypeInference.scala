package pipedsl.analysis

import pipedsl.common.Errors.{UnexpectedType, UnificationError}
import pipedsl.common.Syntax
import pipedsl.common.Syntax.{BOp, EBinop, Expr, Id, TBool, TFun, TNamedType, TSizedInt, TString, TVoid, Type}

class TypeInference {
  
  type Env = Map[Id, Type]
  type Subst = List[(Id, Type)]
  
  var counter = 0
    
  private def generateTypeVar(): TNamedType = TNamedType(Id("__TYPE__" + counter)); counter += 1
  private def occursIn(name: Id, b: Type): Boolean = b match {
    case Syntax.TSizedInt(len, unsigned) => false
    case Syntax.TString() => false
    case Syntax.TVoid() => false
    case Syntax.TBool() => false
    case Syntax.TFun(args, ret) => args.foldLeft[Boolean](false)((b, t) => b || occursIn(name, t)) || occursIn(name, ret)
    case Syntax.TRecType(name, fields) =>false
    case Syntax.TMemType(elem, addrSize, readLatency, writeLatency) => false
    case Syntax.TModType(inputs, refs, retType, name) => false
    case Syntax.TNamedType(name) => false
  }
  
  private def unify(a: Type, b: Type): Subst = (a,b) match {
    case (t1: TNamedType, t2) => if (!occursIn(t1.name, t2)) List((t1.name, t2)) else List()
    case (t1, t2: TNamedType) => if (!occursIn(t2.name, t1)) List((t2.name, t1)) else List()
    case (_:TString, _:TString) => List()
    case (_: TBool, _:TBool) => List()
    case (_: TVoid, _:TVoid) => List()
    case (_: TSizedInt, _: TSizedInt) => List()
      //TODO: TSIZEDINT
    case (TFun(args1, ret1), TFun(args2, ret2)) if args1.length == args2.length =>  
      args1.zip(args2).foldLeft[Subst](List())((s, t) => s ++ unify(t._1, t._2)) ++ unify(ret1, ret2)
    case _ => throw UnificationError(a, b)
  }
  
  private def infer(env: Env, e: Expr): (Subst, Type) = e match {
    case Syntax.EInvalid => 
    case Syntax.EIsValid(ex) =>
    case Syntax.EFromMaybe(ex) =>
    case Syntax.EInt(v, base, bits) => (List(), TSizedInt(bits, true))
    case Syntax.EString(v) => (List(), TString())
    case Syntax.EBool(v) => (List(), TBool())
    case Syntax.EUop(op, ex) => op match {
      case Syntax.BoolUOp(op) => 
      case _ => 
    }
    case Syntax.EBinop(op, e1, e2) =>
      val (s1, t1) = infer(env, e1)
      //TODO substitute into env, use new env
      val (s2, t2) = infer(env, e2)
    case Syntax.ERecAccess(rec, fieldName) =>
    case Syntax.ERecLiteral(fields) =>
    case Syntax.EMemAccess(mem, index) =>
    case Syntax.EBitExtract(num, start, end) =>
    case Syntax.ETernary(cond, tval, fval) => 
    case Syntax.EApp(func, args) =>
    case Syntax.ECall(mod, args) =>
    case Syntax.EVar(id) =>
    case Syntax.ECast(ctyp, exp) =>
    case expr: Syntax.CirExpr =>
  }
  
  private def binOpExpectedType(b: BOp): Type = b match {
    case Syntax.EqOp(op) => 
      val t = generateTypeVar() // TODO: This can be anything?
      TFun(List(t, t), TBool())
    case Syntax.CmpOp(op) => TFun(List(TSizedInt(32, true), TSizedInt(32, true)), TBool())//TODO: TSizedInt?
    case Syntax.BoolOp(op, fun) => TFun(List(TBool(), TBool()), TBool())
    case Syntax.NumOp(op, fun) => TFun(List(TSizedInt(32, true), TSizedInt(32, true)), TSizedInt(32, true))
    case Syntax.BitOp(op, fun) => TFun(List(TSizedInt(32, true), TSizedInt(32, true)), TSizedInt(32, true))
  }
    
}
