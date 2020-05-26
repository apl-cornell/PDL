package pipedsl
import scala.util.parsing.combinator._
import common.Syntax._

class Parser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  // General parser combinators
  def braces[T](parser: P[T]): P[T] = "{" ~> parser <~ "}"
  def brackets[T](parser: P[T]): P[T] = "[" ~> parser <~ "]"
  def parens[T](parser: P[T]): P[T] = "(" ~> parser <~ ")"
  def angular[T](parser: P[T]): P[T] = "<" ~> parser <~ ">"

  // General syntax components
  lazy val iden: P[Id] = positioned {
    "" ~> "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { v => Id(v) }
  }
  lazy val posint = "[0-9]+".r ^^ { n => n.toInt } | err("Expected positive number")

  lazy val stringVal: P[String] =
    "\"" ~> "[^\"]*".r <~ "\""

  // Atoms
  lazy val uInt: P[Expr] = "(-)?[0-9]+".r ^^ { n => EInt(n.toInt) }
  lazy val hex = "0x[0-9a-fA-F]+".r ^^ { n => Integer.parseInt(n.substring(2), 16) }
  lazy val octal = "0[0-7]+".r ^^ { n => Integer.parseInt(n.substring(1), 8) }
  lazy val boolean = "true" ^^ { _ => true } | "false" ^^ { _ => false }

  lazy val simpleAtom: P[Expr] = positioned {
      hex ^^ { case h => EInt(h, 16) } |
      octal ^^ { case o => EInt(o, 8) } |
      uInt |
      boolean ^^ { case b => EBool(b) } |
      iden ~ parens(repsep(expr, ",")) ^^ { case f ~ args => EApp(f, args) } |
      iden ^^ { case id => EVar(id) } |
      parens(expr)
  }

  // Binops. Need to parse them seperately from EBinop to get positions.
  import pipedsl.common.Syntax.{OpConstructor => OC}
  lazy val mulOps: P[BOp] = positioned {
    "/" ^^ { _ => NumOp("/", OC.div) } |
      "*" ^^ { _ => NumOp("*", OC.mul) } |
      "%" ^^ { _ => NumOp("%", OC.mod) }
  }
  lazy val addOps: P[BOp] = positioned {
    "+" ^^ { _ => NumOp("+", OC.add) } |
      "-" ^^ { _ => NumOp("-", OC.sub) }
  }
  lazy val eqOps: P[BOp] = positioned {
    ("==" | "!=") ^^ { op => EqOp(op) } |
      (">=" | "<=" | ">" | "<") ^^ { op => CmpOp(op) }
  }
  lazy val shOps: P[BOp] = positioned {
    ">>" ^^ { op => BitOp(op, OC.sr) } |
       "<<" ^^ { op => BitOp(op, OC.sl) }
  }
  lazy val bAnd: P[BOp] = positioned("&" ^^ { op => BitOp(op, OC.band) })
  lazy val bOr: P[BOp] = positioned("|" ^^  { op => BitOp(op, OC.bor) })
  lazy val bXor: P[BOp] = positioned("^" ^^ { op => BitOp(op, OC.bxor) })

  lazy val and: P[BOp] = positioned("&&" ^^ { op => BoolOp(op) })
  lazy val or: P[BOp] = positioned("||" ^^  { op => BoolOp(op) })

  /** Expressions
   * The bin* parsers implement the precedence order of operators described
   * for C/C++: https://en.cppreference.com/w/c/language/operator_precedence
   * The tower-like structure is required to implement precedence correctly.
   */
  def parseOp(base: P[Expr], op: P[BOp]): P[Expr] = positioned {
    chainl1[Expr](base, op ^^ { case op => EBinop(op, _, _)})
  }
  lazy val binMul = parseOp(recAccess, mulOps)
  lazy val binAdd = parseOp(binMul, addOps)
  lazy val binEq = parseOp(binAdd, eqOps)
  lazy val binSh = parseOp(binEq, shOps)
  lazy val binBAnd = parseOp(binSh, bAnd)
  lazy val binBXor = parseOp(binBAnd, bXor)
  lazy val binBOr = parseOp(binBXor, bOr)
  lazy val binAnd = parseOp(binBOr, and)
  lazy val binOr = parseOp(binAnd, or)
  lazy val expr = positioned (binOr)

  lazy val recAccess: P[Expr] = positioned {
    recAccess ~ "." ~ iden ^^ { case rec ~ _ ~ f => ERecAccess(rec, f) } |
      simpleAtom
  }

  lazy val simpleCmd: P[Command] = positioned {
      "return" ~> expr ^^ { case e => CReturn(e) } |
      expr ~ ":=" ~ expr ^^ { case l ~ _ ~ r => CAssign(l, r) } |
      expr ^^ { case e => CExpr(e) }
  }

  lazy val blockCmd: P[Command] = positioned {
    block | conditional
  }

  lazy val block: P[Command] = {
    braces(cmd.?) ^^ { case c => c.getOrElse(CEmpty) }
  }

  lazy val conditional: P[Command] = positioned {
    "if" ~> parens(expr) ~ block ~ ("else" ~> block).?  ^^ {
      case cond ~ cons ~ alt => CIf(cond, cons, if (alt.isDefined) alt.get else CEmpty )
    }
  }

  lazy val seqCmd: P[Command] = {
    simpleCmd ~ ";" ~ seqCmd ^^ { case c1 ~ _ ~ c2 => CSeq(c1, c2) } |
      blockCmd ~ seqCmd ^^ { case c1 ~ c2 => CSeq(c1, c2) } |
      simpleCmd <~ ";" | blockCmd | simpleCmd
  }

  lazy val cmd: P[Command] = positioned {
    seqCmd ~ "---" ~ cmd ^^ { case c1 ~ _ ~ c2 => CTBar(c1, c2) } |
      seqCmd
  }
}
