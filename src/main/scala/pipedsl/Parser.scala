package pipedsl
import scala.util.parsing.combinator._
import common.Syntax._
import common.Utilities._
import common.Locks._

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
  lazy val uInt: P[Expr] = "[0-9]+".r ~ angular(posint).? ^^ { case n ~ bits => EInt(n.toInt, 10, if (bits.isDefined) bits.get else log2(n.toInt)) }
  lazy val hex = "0x[0-9a-fA-F]+".r ~ angular(posint).? ^^ { case (n ~ bits) => EInt(Integer.parseInt(n.substring(2), 16), 16,
    if (bits.isDefined) bits.get else 4 * n.substring(2).length()) }
  lazy val octal = "0[0-7]+".r ~ angular(posint).? ^^ {
    case (n ~ bits) => EInt(Integer.parseInt(n.substring(1), 8), 8,
      if (bits.isDefined) bits.get else 3 * n.substring(1).length()) }
  lazy val binary = "0b[0-1]+".r ~ angular(posint).? ^^ {
    case (n ~ bits)  => EInt(Integer.parseInt(n.substring(2), 2), 2,
      if (bits.isDefined) bits.get else n.substring(2).length()) }
  lazy val boolean = "true" ^^ { _ => true } | "false" ^^ { _ => false }

  lazy val recLiteralField: P[(Id, Expr)] = iden ~ ("=" ~> expr) ^^ { case i ~ e => (i, e) }
  lazy val recLiteral = positioned {
    braces(repsep(recLiteralField, ",")) ^^ (fieldList => ERecLiteral(fieldList.toMap))
  }

  lazy val variable: P[EVar] = positioned {
    iden ^^ (id => EVar(id))
  }

  lazy val recAccess: P[Expr] = positioned {
    recAccess ~ "." ~ iden ^^ { case rec ~ _ ~ f => ERecAccess(rec, f) } |
      expr
  }

  lazy val memAccess: P[Expr] = positioned {
    iden ~ brackets(expr) ^^ { case m ~ i => EMemAccess(m, i) }
  }

  lazy val bitAccess: P[Expr] = positioned {
    expr ~ braces(posint ~ ":" ~ posint) ^^ { case n ~ (e ~ _ ~ s) => EBitExtract(n, s, e) }
  }

  lazy val ternary: P[Expr] = positioned {
    parens(expr) ~ "?" ~ expr ~ ":" ~ expr ^^ { case c ~ _ ~ t ~ _ ~ v => ETernary(c, t, v) }
  }

  lazy val cast: P[Expr] = positioned {
    "cast" ~> parens(expr ~ "," ~ typ) ^^ {  case e ~ _ ~ t => ECast(t, e) }
  }
  //UOps
  lazy val not: P[UOp] = positioned("!" ^^ { _ => NotOp() })

  lazy val simpleAtom: P[Expr] = positioned {
    not ~ expr ^^ { case n ~ e => EUop(n, e) } |
    cast |
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

  lazy val concat: P[BOp] = positioned("++" ^^ { op => BitOp(op, OC.concat) })


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
  lazy val binConcat = parseOp(binOr, concat)
  lazy val expr = positioned(binConcat)

  lazy val simpleCmd: P[Command] = positioned {
    check |
    "acquire" ~> parens(iden) ^^ { i => CLockOp(i, LockState.Acquired)} |
    "reserve" ~> parens(iden) ^^ { i => CLockOp(i, LockState.Reserved)} |
      "release" ~> parens(iden) ^^ { i => CLockOp(i, LockState.Released)} |
    "return" ~> expr ^^ { case e => CReturn(e) } |
    "output" ~> expr ^^ { case e => COutput(e) } |
      "call" ~> iden ~ parens(repsep(expr, ",")) ^^ { case i ~ args => CCall(i, args) } |
      typ.? ~ variable ~ "=" ~ expr ^^ { case t ~ n ~ _ ~ r => n.typ = t
        CAssign(n, r) } |
      typ.? ~ expr ~ "<-" ~ expr ^^ { case t ~ l ~ _ ~ r => l.typ = t
        CRecv(l, r) } |
       "next".? ~ typ ~ iden ^^ { case n ~ t ~ i => CDecl(i, t, n.isDefined) } |
      expr ^^ { case e => CExpr(e) }
  }

  lazy val blockCmd: P[Command] = positioned {
    block | conditional | speculate
  }

  lazy val speculate: P[Command] = positioned {
    "speculate" ~> parens(typ ~ variable ~ "=" ~ expr ~ "," ~ block ~ "," ~ block) ^^ {
      case t ~ v ~ _ ~ ev ~ _ ~ cv ~ _ ~ cs => {
        v.typ = Some(t)
        CSpeculate(v, ev, cv, cs)
    }}
  }
  lazy val check: P[Command] = positioned {
    "check" ~> parens(iden) ^^ { id => CCheck(id) }
  }

  lazy val casestmt: P[CaseObj] = positioned {
    "case:" ~> expr ~ block ^^ { case e ~ b => CaseObj(e, b) }
  }
  lazy val defaultcase: P[Command] = positioned {
    "default:" ~> block
  }
  lazy val split: P[Command] = positioned {
    "split" ~> braces(rep(casestmt) ~ defaultcase.?) ^^ { case cl ~ dc => CSplit(cl, if (dc.isDefined) dc.get else CEmpty) }
  }

  lazy val block: P[Command] = {
    braces(cmd.?) ^^ (c => c.getOrElse(CEmpty))
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
    "---" ~> cmd ^^ { c => CTBar(CEmpty, c)} | seqCmd
  }

  lazy val sizedInt: P[Type] = "int" ~> angular(posint) ^^ { bits => TSizedInt(bits, unsigned = true) }
  lazy val memory: P[Type] = sizedInt ~ brackets(posint) ^^ { case elem ~ size => TMemType(elem, size) }
  lazy val bool: P[Type] = "bool".r ^^ { _ => TBool() }
  lazy val baseTyp: P[Type] = memory | sizedInt | bool

  lazy val typ: P[Type] = "spec" ~> angular(baseTyp) ^^ { t => t.maybeSpec = true; t } |
    baseTyp

  lazy val param: P[Param] = iden ~ ":" ~ typ ^^ { case i ~ _ ~ t => Param(i, t) }

  lazy val fdef: P[FuncDef] = positioned {
    "def" ~> iden ~ parens(repsep(param, ",")) ~ ":" ~ typ ~ braces(cmd) ^^ {
      case i ~ ps ~ _ ~ t ~ c => FuncDef(i, ps, t, c)
    }
  }

  lazy val moddef: P[ModuleDef] = positioned {
    "pipe" ~> iden ~ parens(repsep(param, ",")) ~ brackets(repsep(param, ",")) ~ braces(cmd) ^^ {
      case i ~ ps ~ mods ~ c => ModuleDef(i, ps, mods, c)
    }
  }

  lazy val cnew: P[CirExpr] = positioned {
    "new" ~ iden ~ parens(repsep(expr,",")) ~ brackets(repsep(iden,",")).? ^^ {
      case _ ~ i ~ args ~ mods => CirNew(i, args, if (mods.isDefined) mods.get else List[Id]())
    }
  }
  lazy val cmem: P[CirExpr] = positioned {
    "memory" ~> parens(sizedInt ~ "," ~ posint) ^^ { case elem ~ _ ~ addr => CirMem(elem, addr) }
  }

  lazy val cconn: P[Circuit] = positioned {
    iden ~ "=" ~ (cnew | cmem) ^^ { case i ~ _ ~ n => CirConnect(i, n)}
  }

  lazy val cseq: P[Circuit] = positioned {
    cconn ~ ";" ~ cseq ^^ { case n ~ _ ~ c => CirSeq(n, c) } |
      cconn <~ ";" | cconn
  }

  lazy val circuit: P[Circuit] = positioned {
    "circuit" ~> braces(cseq)
  }

  lazy val prog: P[Prog] = positioned {
    fdef.+ ~ moddef.+ ~ circuit ^^ {
      case f ~ p ~ c => Prog(f, p, c)
    }
  }
}