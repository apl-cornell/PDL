package pipedsl.common

object BSVSyntax {

  sealed trait BSVType

  case class BInterface(name: String) extends BSVType
  case class BSizedInt(unsigned: Boolean, size: Int) extends BSVType
  case class BBool() extends BSVType

  case class BParam(name: String, typ: BSVType)

  sealed trait BExpr
  sealed trait BStatement

  case class BModInst(name: String, mtyp: BSVType, instType: BSVType) extends BStatement
  case class BRegAssign(lhs: BExpr, rhs: BExpr) extends BStatement
  case class BAssign(lhs: BExpr, rhs: BExpr) extends BStatement


  case class BModuleDef(name: String, typ: BInterface, params: List[BParam]) extends BStatement
}
