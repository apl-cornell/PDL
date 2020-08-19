package pipedsl.common

import pipedsl.common.BSVSyntax.MethodType.MethodType
import pipedsl.common.Syntax.Type

object BSVSyntax {

  object MethodType extends Enumeration {
    type MethodType = Value
    val Action, Val, ActionVal = Value
  }

  sealed trait BSVType

  case class BParam(name: String, typ: BSVType)

  case class BStruct(name: String, fields: List[BParam]) extends BSVType
  case class BInterface(name: String) extends BSVType
  case class BSizedInt(unsigned: Boolean, size: Int) extends BSVType
  case class BBool() extends BSVType

  def toBSVType(t: Type): BSVType = t match {
    case Syntax.TSizedInt(len, unsigned) => BSizedInt(unsigned, len)
    case Syntax.TBool() => BBool()
      //TODO others
  }

  sealed trait BExpr

  case class BVar(name: String, typ: BSVType) extends BExpr
  case class BBOp(op: String, lhs: BExpr, rh: BExpr) extends BExpr
  case class BUOp(op: String, expr: BExpr) extends BExpr
  case class BModule(name: String, args: List[BExpr] = List()) extends BExpr

  sealed trait BStatement

  case class BModAssign(lhs: BVar, rhs: BModule) extends BStatement
  case class BAssign(lhs: BVar, rhs: BExpr) extends BStatement
  case class BMethodInvoke(mod: BVar, method: String, args: List[BExpr]) extends BStatement
  case class BIf(cond: BExpr, trueBranch: List[BStatement], falseBranch: List[BStatement]) extends BStatement

  case class BStructDef(typ: BSVType, derives: List[String])
  case class BRuleDef(name: String, cond: Option[BExpr] = None, body: List[BStatement])
  case class BMethodSig(name: String, typ: MethodType, params: List[BParam])
  case class BMethodDef(sig: BMethodSig, body: List[BStatement])
  case class BModuleDef(name: String, typ: BInterface,
    params: List[BParam], rules: List[BRuleDef], methods: List[BMethodDef])
  case class BInterfaceDef(name: String, methods: List[BMethodSig])
  case class BImport(name: String)
  case class BProgram(topModule: String, imports: List[BImport], structs: List[BStructDef],
    interfaces: List[BInterfaceDef], modules: List[BModuleDef])
}
