package pipedsl
import scala.util.parsing.combinator._
import common.Syntax._
import common.Utilities._
import common.Locks._
import pipedsl.common.LockImplementation

import scala.util.matching.Regex

class Parser(rflockImpl: String) extends RegexParsers with PackratParsers {
  type P[T] = PackratParser[T]

  // General parser combinators
  def braces[T](parser: P[T]): P[T] = "{" ~> parser <~ "}"

  def brackets[T](parser: P[T]): P[T] = "[" ~> parser <~ "]"

  def parens[T](parser: P[T]): P[T] = "(" ~> parser <~ ")"

  def angular[T](parser: P[T]): P[T] = "<" ~> parser <~ ">"

  override protected val whiteSpace: Regex = """(\s|\/\/.*|(/\*((\*[^/])|[^*])*\*/))+""".r

  // General syntax components
  lazy val iden: P[Id] = positioned {
    "" ~> "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { v => Id(v) }
  }

  lazy val posint: Parser[Int] = "[0-9]+".r ^^ { n => n.toInt } | err("Expected positive number")

  lazy val stringVal: P[EString] =
    "\"" ~> "[^\"]*".r <~ "\"" ^^ {n => EString(n)}

  private def toInt(n: Int, base: Int, bits: Option[Int], isUnsigned: Boolean): EInt = {
    val e = EInt(n, base, if (bits.isDefined) bits.get else log2(n))
    e.typ = Some(TSizedInt(e.bits, unsigned = isUnsigned))
    e
  }

  // Atoms
  lazy val dec: P[EInt] = "u".? ~ "-?[0-9]+".r ~ angular(posint).? ^^ {
    case u ~ n ~ bits => toInt(n.toInt, 10, bits, u.isDefined)
  }
  lazy val hex: P[EInt] = "u".? ~ "0x-?[0-9a-fA-F]+".r ~ angular(posint).? ^^ {
    case u ~ n ~ bits => toInt(Integer.parseInt(n.substring(2), 16), 16, bits, u.isDefined)
  }
  lazy val octal: P[EInt] = "u".? ~ "0-?[0-7]+".r ~ angular(posint).? ^^ {
    case u ~ n ~ bits => toInt(Integer.parseInt(n.substring(1), 8), 8, bits, u.isDefined)
  }
  lazy val binary: P[EInt] = "u".? ~ "0b-?[0-1]+".r ~ angular(posint).? ^^ {
    case u ~ n ~ bits => toInt(Integer.parseInt(n.substring(2), 2), 2, bits, u.isDefined)
  }

  lazy val num: P[EInt] = dec | hex | octal | binary

  lazy val boolean: P[Boolean] = "true" ^^ { _ => true } | "false" ^^ { _ => false }

  lazy val recLiteralField: P[(Id, Expr)] = iden ~ ("=" ~> expr) ^^ { case i ~ e => (i, e) }
  lazy val recLiteral: Parser[ERecLiteral] = positioned {
    braces(repsep(recLiteralField, ",")) ^^ (fieldList => ERecLiteral(fieldList.toMap))
  }

  lazy val variable: P[EVar] = positioned {
    iden ^^ (id => EVar(id))
  }

  lazy val recAccess: P[Expr] = positioned {
    recAccess ~ "." ~ iden ^^ { case rec ~ _ ~ f => ERecAccess(rec, f) } |
      expr
  }

  //Mask write:
  // int<32> w;
  // m[a, 0b1100] <- w;
  lazy val memAccess: P[Expr] = positioned {
    iden ~ brackets(expr ~ ("," ~> expr).?) ^^ { case m ~ (i ~ n) => EMemAccess(m, i, n) }
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

  lazy val mag: P[Expr] = positioned {
    "mag" ~> parens(expr) ^^ (e => EUop(MagOp(), e))
  }
  lazy val sign: P[Expr] = positioned {
    "sign" ~> parens(expr) ^^ (e => EUop(SignOp(), e))
  }

  lazy val neg: P[Expr] = positioned {
    "-" ~> parens(expr) ^^ (e => EUop(NegOp(), e))
  }

  lazy val methodCall: P[ECall] = positioned {
    iden ~ "." ~ iden ~ parens(repsep(expr, ",")) ^^ { case i ~ _  ~ n ~ args => ECall(i, Some(n), args) }
  }

  lazy val simpleAtom: P[Expr] = positioned {
    "call" ~> iden ~ parens(repsep(expr, ",")) ^^ { case i ~ args => ECall(i, None, args) } |
      not ~ expr ^^ { case n ~ e => EUop(n, e) } |
      neg |
      cast |
      mag |
      sign |
      memAccess |
      bitAccess |
      recAccess |
      ternary |
      recLiteral |
      hex |
      octal |
      binary |
      dec |
      stringVal | 
      boolean ^^ (b => EBool(b)) |
      iden ~ parens(repsep(expr, ",")) ^^ { case f ~ args => EApp(f, args) } |
      variable |
      parens(expr)
  }

  // Binops. Need to parse them separately from EBinop to get positions.

  import pipedsl.common.Syntax.{OpConstructor => OC}

  lazy val mulOps: P[BOp] = positioned {
    "/" ^^ { _ => NumOp("/", OC.div) } |
    "*" ^^ { _ => NumOp("*", OC.mul) } |
    "%" ^^ { _ => NumOp("%", OC.mod) } |
    "$*" ^^ { _ => NumOp("$*", OC.mul) }
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
    chainl1[Expr](base, op ^^ (op => EBinop(op, _, _)))
  }

  lazy val binMul: P[Expr] = parseOp(simpleAtom, mulOps)
  lazy val binAdd: P[Expr] = parseOp(binMul, addOps)
  lazy val binEq: P[Expr] = parseOp(binAdd, eqOps)
  lazy val binSh: P[Expr] = parseOp(binEq, shOps)
  lazy val binBAnd: P[Expr] = parseOp(binSh, bAnd)
  lazy val binBXor: P[Expr] = parseOp(binBAnd, bXor)
  lazy val binBOr: P[Expr] = parseOp(binBXor, bOr)
  lazy val binAnd: P[Expr] = parseOp(binBOr, and)
  lazy val binOr: P[Expr] = parseOp(binAnd, or)
  lazy val binConcat: P[Expr] = parseOp(binOr, concat)
  lazy val expr: Parser[Expr] = positioned(binConcat)
  lazy val lhs: Parser[Expr] = memAccess | variable

  lazy val simpleCmd: P[Command] = positioned {
    speccall |
    typ.? ~ variable ~ "=" ~ expr ^^ { case t ~ n ~ _ ~ r => n.typ = t; CAssign(n, r) } |
      typ.? ~ lhs ~ "<-" ~ expr ^^ { case t ~ l ~ _ ~ r => l.typ = t
        CRecv(l, r)
      } |
      check |
      resolveSpec |
      "start" ~> parens(iden) ^^ { i => CLockStart(i) } |
      "end" ~> parens(iden) ^^ { i => CLockEnd(i) } |
      "acquire" ~> parens(lockArg ~ ("," ~> lockType).?) ^^ { case i ~ t => CSeq(CLockOp(i, Reserved, t), CLockOp(i, Acquired, t)) } |
      "reserve" ~> parens(lockArg ~ ("," ~> lockType).?) ^^ { case i ~ t => CLockOp(i, Reserved, t)} |
      "block" ~> parens(lockArg) ^^ { i => CLockOp(i, Acquired, None) } |
      "release" ~> parens(lockArg) ^^ { i => CLockOp(i, Released, None)} |
      "print" ~> parens(repsep(expr, ",")) ^^ (e => CPrint(e)) |
      "return" ~> expr ^^ (e => CReturn(e)) |
      "output" ~> expr ^^ (e => COutput(e)) |
      expr ^^ (e => CExpr(e)) 
  }
  
  lazy val lockArg: P[LockArg] = positioned { 
    iden ~ brackets(variable).? ^^ {case i ~ v => LockArg(i, v)}
  }

  lazy val lockType: P[LockType] = positioned {
    "R" ^^ {_ => LockRead} |
    "r" ^^ {_ => LockRead} |
    "W" ^^ {_ => LockWrite} |
    "w" ^^ {_ => LockWrite}
  }

  lazy val blockCmd: P[Command] = positioned {
    block | conditional | split
  }

  //TODO better syntax for these
  lazy val check: P[Command] = positioned {
    "spec_barrier()" ^^ { _ => CCheckSpec(true) } |
    "spec_check()" ^^ { _ => CCheckSpec(false) }
  }

  lazy val speccall: P[Command] = positioned {
    iden ~ "<-" ~ "speccall" ~ iden ~ parens(repsep(methodCall | expr, ",")) ^^ {
      case h ~ _ ~ _ ~ i ~ args =>
        val sv = EVar(h)
        sv.typ = Some(TRequestHandle(i, RequestType.Speculation))
        h.typ = sv.typ
        CSpecCall(sv, i, args)
    } |
    iden ~ "<-" ~ "update" ~ parens(variable ~ "," ~ repsep(methodCall | expr, ",")) ^^ {
      case ni ~ _ ~ _ ~ (oi ~ _ ~ e) => CUpdate(EVar(ni), oi, e, List()) }
  }

  lazy val resolveSpec: P[Command] = positioned {
    "verify" ~> parens(variable ~ "," ~ repsep(expr,",")) ~ braces(methodCall).? ^^ {
      case i ~ _ ~ e ~ u => CVerify(i, e, List(), u) } |
    "invalidate" ~> parens(variable) ^^ (i => CInvalidate(i))
  }

  lazy val casestmt: P[CaseObj] = positioned {
    "case:" ~> expr ~ block ^^ { case e ~ b => CaseObj(e, b) }
  }
  lazy val defaultcase: P[Command] = positioned {
    "default:" ~> block
  }
  lazy val split: P[Command] = positioned {
    "split" ~> braces(rep(casestmt) ~ defaultcase.?) ^^ { case cl ~ dc => CSplit(cl, if (dc.isDefined) dc.get else CEmpty()) }
  }

  lazy val block: P[Command] = {
    braces(cmd.?) ^^ (c => c.getOrElse(CEmpty()))
  }

  lazy val conditional: P[Command] = positioned {
    "if" ~> parens(expr) ~ block ~ ("else" ~> blockCmd).? ^^ {
      case cond ~ cons ~ alt => CIf(cond, cons, if (alt.isDefined) alt.get else CEmpty())
    }
  }

  lazy val seqCmd: P[Command] = {
    simpleCmd ~ ";" ~ seqCmd ^^ { case c1 ~ _ ~ c2 => CSeq(c1, c2) } |
      blockCmd ~ seqCmd ^^ { case c1 ~ c2 => CSeq(c1, c2) } |
      simpleCmd <~ ";" | blockCmd | "" ^^ { _ => CEmpty() }
  }

  lazy val cmd: P[Command] = positioned {
    seqCmd ~ "---" ~ cmd ^^ { case c1 ~ _ ~ c2 => CTBar(c1, c2) } |
      seqCmd
  }

  lazy val sizedInt: P[Type] = "int" ~> angular(posint) ^^ { bits => TSizedInt(bits, unsigned = false) } |
  "uint" ~> angular(posint) ^^ { bits => TSizedInt(bits, unsigned = true) }

  lazy val latency: P[Latency.Latency] =
    "c" ^^ { _ => Latency.Combinational } |
    "s" ^^ { _ => Latency.Sequential }    |
    "a" ^^ { _ => Latency.Asynchronous }

  lazy val lockedMemory: P[Type] = sizedInt ~ brackets(posint) ~ (angular(latency ~ ("," ~> latency)) ~ parens(iden).?).?  ^^ {
    case elem ~ size ~ lats =>
      if (lats.isDefined) {
        val rlat = lats.get._1._1
        val wlat = lats.get._1._2
        val lock = lats.get._2
        val mtyp = TMemType(elem, size, rlat, wlat)
        if (lock.isDefined)
          TLockedMemType(mtyp, None, LockImplementation.getLockImpl(lock.get))
        else
          TLockedMemType(mtyp, None, LockImplementation.getDefaultLockImpl)
      } else {
        val mtyp = TMemType(elem, size,  Latency.Asynchronous, Latency.Asynchronous)
        TLockedMemType(mtyp, None, LockImplementation.getDefaultLockImpl)
      }
  }

  lazy val void: P[Type] = "()" ^^ { _ => TVoid() }
  lazy val bool: P[Type] = "bool".r ^^ { _ => TBool() }
  lazy val string: P[Type] = "String".r ^^ {_ => TString() }
  lazy val baseTyp: P[Type] = lockedMemory | sizedInt | bool | string | void

  lazy val typ: P[Type] = "spec" ~> angular(baseTyp) ^^ { t => t.maybeSpec = true; t } |
    baseTyp

  lazy val typeName: P[Type] = iden ^^ { i => TNamedType(i) }

  lazy val param: P[Param] = iden ~ ":" ~ (typ | typeName) ^^ { case i ~ _ ~ t =>
    i.typ = Some(t)
    Param(i, t)
  }

  lazy val methodDef: P[FuncDef] = positioned {
    "method" ~> iden ~ parens(repsep(param, ",")) ~ ":" ~ typ <~ ";" ^^ {
      case i ~ ps ~ _ ~ t => FuncDef(i, ps, t, CEmpty())
    }
  }

  lazy val fdef: P[FuncDef] = positioned {
    "def" ~> iden ~ parens(repsep(param, ",")) ~ ":" ~ typ ~ braces(cmd) ^^ {
      case i ~ ps ~ _ ~ t ~ c => FuncDef(i, ps, t, c)
    }
  }

  lazy val moddef: P[ModuleDef] = positioned {
    "pipe" ~> iden ~ parens(repsep(param, ",")) ~ brackets(repsep(param, ",")) ~ (":" ~> typ).? ~ braces(cmd) ^^ {
      case i ~ ps ~ mods ~ rt ~ c => ModuleDef(i, ps, mods, rt, c)
    }
  }

  lazy val ccall: P[CirCall] = positioned {
    "call" ~ iden ~ parens(repsep(expr, ",")) ^^ {
      case _ ~ i ~ inits => CirCall(i, inits)
    }
  }

  lazy val cnew: P[CirExpr] = positioned {
    "new" ~ iden ~ brackets(repsep(iden,",")).? ~ parens(repsep(num,",")).? ^^ {
      case _ ~ i ~ mods ~ params => CirNew(i, if (mods.isDefined) mods.get else List(),
        if (params.isDefined) params.get else List())
    }
  }
  lazy val cmem: P[CirExpr] = positioned {
    "memory" ~> parens(sizedInt ~ "," ~ posint) ^^ { case elem ~ _ ~ addr => CirMem(elem, addr) }
  }

  lazy val crf: P[CirExpr] = positioned {
    "regfile" ~> parens(sizedInt ~ "," ~ posint) ^^ { case elem ~ _ ~ addr => CirRegFile(elem, addr) }
  }

  lazy val clockrf: P[CirExpr] = positioned {
    ("rflock" ~> iden.?) ~ parens(sizedInt ~ "," ~ posint ~ ("," ~> repsep(posint,",")).?) ^^ {
      case i ~ (elem ~ _ ~ addr ~ szs) =>
        CirLockRegFile(elem, addr, LockImplementation.getLockImpl(i.getOrElse(Id(rflockImpl))), szs.getOrElse(List()))
    }
  }

  lazy val clock: P[CirExpr] = positioned {
    iden ~ parens(iden) ~ angular(repsep(posint,",")).? ^^ {
      case lid ~ mem ~ szs => CirLock(mem, LockImplementation.getLockImpl(lid), szs.getOrElse(List()))
    }
  }

  lazy val cconn: P[Circuit] = positioned {
    iden ~ "=" ~ (cnew | cmem | crf | clockrf | clock | ccall) ^^ { case i ~ _ ~ n => CirConnect(i, n)}
  }

  lazy val cexpr: P[Circuit] = positioned {
    ccall ^^ (c => CirExprStmt(c))
  }

  lazy val cseq: P[Circuit] = positioned {
    cconn ~ ";" ~ cseq ^^ { case n ~ _ ~ c => CirSeq(n, c) } |
      cconn <~ ";" | cexpr <~ ";"
  }

  lazy val circuit: P[Circuit] = positioned {
    "circuit" ~> braces(cseq)
  }

  lazy val extern: P[ExternDef] = positioned {
    "extern" ~> iden ~ angular(repsep(typ, ",")).? ~ braces(methodDef.*) ^^ {
      case i ~ t ~ f => ExternDef(i, if (t.isDefined) t.get else List(), f) }
  }

  lazy val prog: P[Prog] = positioned {
    extern.* ~ fdef.* ~ moddef.* ~ circuit ^^ {
      case e ~ f ~ p ~ c => Prog(e, f, p, c)
    }
  }
}
