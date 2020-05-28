package pipedsl
import scala.util.parsing.combinator._
import common.Syntax._
import common.Utilities._
class Parser extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  // General parser combinators
  def braces[T](parser: P[T]): P[T] = "{" ~> parser <~ "}"

  def brackets[T](parser: P[T]): P[T] = "[" ~> parser <~ "]"

  def parens[T](parser: P[T]): P[T] = "(" ~> parser <~ ")"

  def angular[T](parser: P[T]): P[T] = "<" ~> parser <~ ">"

  override protected val whiteSpace = """(\s|\/\/.*|(/\*((\*[^/])|[^*])*\*/))+""".r

  // General syntax components
  lazy val iden: P[Id] = positioned {
    "" ~> "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { v => Id(v) }
  }
  lazy val posint = "[0-9]+".r ^^ { n => n.toInt } | err("Expected positive number")

  lazy val stringVal: P[String] =
    "\"" ~> "[^\"]*".r <~ "\""

  // Atoms
  lazy val uInt: P[Expr] = "(-)?[0-9]+".r ^^ { n => EInt(n.toInt, 10, log2(n.toInt)) }
  lazy val hex = "0x[0-9a-fA-F]+".r ^^ {
    n => EInt(Integer.parseInt(n.substring(2), 16), 16, 4 * n.length())
  }
  lazy val octal = "0[0-7]+".r ^^ {
    n => EInt(Integer.parseInt(n.substring(1), 8), 8, 3 * n.length())
  }
  lazy val binary = "0b[0-1]+".r ^^ {
    n => EInt(Integer.parseInt(n.substring(2), 2), 2, n.length())
  }
  lazy val boolean = "true" ^^ { _ => true } | "false" ^^ { _ => false }

  lazy val recLiteralField: P[(Id, Expr)] = iden ~ ("=" ~> expr) ^^ { case i ~ e => (i, e) }
  lazy val recLiteral = positioned {
    braces(repsep(recLiteralField, ",")) ^^ { case fieldList => ERecLiteral(fieldList.toMap) }
  }

  lazy val variable: P[Expr] = positioned {
    iden ^^ { case id => EVar(id) }
  }

  lazy val recAccess: P[Expr] = positioned {
    recAccess ~ "." ~ iden ^^ { case rec ~ _ ~ f => ERecAccess(rec, f) } |
      expr
  }

  lazy val memAccess: P[Expr] = positioned {
    expr ~ brackets(expr) ^^ { case m ~ i => EMemAccess(m, i) }
  }

  lazy val bitAccess: P[Expr] = positioned {
    expr ~ braces(expr ~ ":" ~ expr) ^^ { case n ~ (e ~ _ ~ s) => EBitExtract(n, s, e) }
  }

  lazy val ternary: P[Expr] = positioned {
    parens(expr) ~ "?" ~ expr ~ ":" ~ expr ^^ { case c ~ _ ~ t ~ _ ~ v => ETernary(c, t, v) }
  }

  lazy val simpleAtom: P[Expr] = positioned {
    memAccess |
      bitAccess |
      recAccess |
      ternary |
      recLiteral |
      hex |
      octal |
      binary |
      uInt |
      boolean ^^ { case b => EBool(b) } |
      iden ~ parens(repsep(expr, ",")) ^^ { case f ~ args => EApp(f, args) } |
      variable |
      parens(expr)
  }

  // Binops. Need to parse them separately from EBinop to get positions.

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
  lazy val bOr: P[BOp] = positioned("|" ^^ { op => BitOp(op, OC.bor) })
  lazy val bXor: P[BOp] = positioned("^" ^^ { op => BitOp(op, OC.bxor) })

  lazy val and: P[BOp] = positioned("&&" ^^ { op => BoolOp(op, OC.and) })
  lazy val or: P[BOp] = positioned("||" ^^ { op => BoolOp(op, OC.or) })

  /** Expressions
   * The bin* parsers implement the precedence order of operators described
   * for C/C++: https://en.cppreference.com/w/c/language/operator_precedence
   * The tower-like structure is required to implement precedence correctly.
   */
  def parseOp(base: P[Expr], op: P[BOp]): P[Expr] = positioned {
    chainl1[Expr](base, op ^^ { case op => EBinop(op, _, _) })
  }

  lazy val binMul = parseOp(simpleAtom, mulOps)
  lazy val binAdd = parseOp(binMul, addOps)
  lazy val binEq = parseOp(binAdd, eqOps)
  lazy val binSh = parseOp(binEq, shOps)
  lazy val binBAnd = parseOp(binSh, bAnd)
  lazy val binBXor = parseOp(binBAnd, bXor)
  lazy val binBOr = parseOp(binBXor, bOr)
  lazy val binAnd = parseOp(binBOr, and)
  lazy val binOr = parseOp(binAnd, or)
  lazy val expr = positioned(binOr)

  lazy val simpleCmd: P[Command] = positioned {
    "return" ~> expr ^^ { case e => CReturn(e) } |
    "output" ~> expr ^^ { case e => COutput(e) } |
      "call" ~> iden ~ parens(repsep(expr, ",")) ^^ { case i ~ args => CCall(i, args) } |
      expr ~ "=" ~ expr ^^ { case l ~ _ ~ r => CAssign(l, r) } |
      expr ~ "<-" ~ expr ^^ { case l ~ _ ~ r => CRecv(l, r) } |
      expr ^^ { case e => CExpr(e) }
  }

  lazy val blockCmd: P[Command] = positioned {
    block | conditional
  }

  lazy val block: P[Command] = {
    braces(cmd.?) ^^ { case c => c.getOrElse(CEmpty) }
  }

  lazy val conditional: P[Command] = positioned {
    "if" ~> parens(expr) ~ block ~ ("else" ~> blockCmd).? ^^ {
      case cond ~ cons ~ alt => CIf(cond, cons, if (alt.isDefined) alt.get else CEmpty)
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

  lazy val typ: P[Type] = iden.? ^^ { _ => TVoid() } //TODO fill in type parsing

  lazy val param: P[Param] = iden ~ ":" ~ typ ^^ { case i ~ _ ~ t => Param(i, t) }

  lazy val fdef: P[FuncDef] = positioned {
    "def" ~> iden ~ parens(repsep(param, ",")) ~ ":" ~ typ ~ braces(cmd) ^^ {
      case i ~ ps ~ _ ~ t ~ c => FuncDef(i, ps, t, c)
    }
  }

  //TODO fill in module parsing
  lazy val pipedef: P[PipeDef] = positioned {
    "pipe" ~> iden ~ parens(repsep(param, ",")) ~ brackets(repsep(iden, ",")) ~ braces(cmd) ^^ {
      case i ~ ps ~ mods ~ c => PipeDef(i, ps, mods, c)
    }
  }

  lazy val cnew: P[CirNew] = positioned {
    "new" ~ iden ~ parens(repsep(expr,",")) ~ brackets(repsep(iden,",")).? ^^ {
      case _ ~ i ~ args ~ mods => CirNew(i, args, if (mods.isDefined) mods.get else List[Id]())
    }
  }

  lazy val cmem: P[CirMem] = positioned {
    "memory" ~> parens(posint ~ typ) ^^ { case s ~ t => CirMem(s, t) }
  }
  lazy val cname: P[Circuit] = positioned {
    iden ~ "=" ~ (cnew | cmem) ^^ { case i ~ _ ~ n => CirName(i, n)}
  }

  lazy val cseq: P[Circuit] = positioned {
    cname ~ ";" ~ cseq ^^ { case n ~ _ ~ c => CirSeq(n, c) } |
    cname <~ ";" | cname
  }

  lazy val circuit: P[Circuit] = positioned {
    "circuit" ~> braces(cseq)
  }

  lazy val prog: P[Prog] = positioned {
    fdef.+ ~ pipedef.+ ~ circuit ^^ {
      case f ~ p ~ c => Prog(f, p, c)
    }
  }
}