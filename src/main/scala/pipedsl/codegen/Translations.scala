package pipedsl.codegen

import pipedsl.common.Syntax._

object Translations {

  trait Translator[T,E,V,F] {

    def toType(t: Type): T

    def toType(t: Option[Type]): Option[T] = t match {
      case Some(value) => Some(toType(value))
      case None => None
    }

    def toExpr(e: Expr): E

    def toExpr(e: Option[Expr]): Option[E] = e match {
      case Some(value) => Some(toExpr(value))
      case None => None
    }

    def toVar(i: Id): V

    def toVar(v: EVar): V

    def toVar(v: Option[EVar]): Option[V] = v match {
      case Some(value) => Some(toVar(value))
      case None => None
    }

    def toFunc(f: FuncDef): F
  }

}
