package pipedsl.codegen.bsv

import pipedsl.codegen.Translations.Translator
import pipedsl.codegen.bsv.ConstraintsToBluespec.to_provisos
import pipedsl.common.Errors.{MissingType, UnexpectedBSVType, UnexpectedCommand, UnexpectedExpr, UnexpectedType}
import pipedsl.common.LockImplementation
import pipedsl.common.LockImplementation.{LockInterface, getDefaultLockImpl, supportsCheckpoint}
import pipedsl.common.Syntax.Latency.{Asynchronous, Combinational}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities.generic_type_prefix

object BSVSyntax {

  sealed trait MethodType
  case object Action extends MethodType
  case class Value(rtyp: BSVType) extends MethodType
  case class ActionValue(rtyp: BSVType) extends MethodType

  sealed trait Proviso
  case class PBits(szName: String) extends Proviso
  case class PAdd(num1 :String, num2 :String, sum :String) extends Proviso
  case class PMin(name :String, min :Int) extends Proviso
  case class PMax(num1 :String, num2 :String, max :String) extends Proviso
  case class PEq(num1 :String, num2 :String) extends Proviso

  sealed trait BSVType
  case class BNumericType(sz: Int) extends BSVType
  case class BCombMemType(elem: BSVType, addrSize: Int) extends BSVType
  case class BAsyncMemType(elem: BSVType, addrSize: Int) extends BSVType
  case class BStruct(name: String, fields: List[BVar]) extends BSVType
  case class BInterface(name: String, tparams: List[BVar] = List()) extends BSVType
  case class BSizedType(name: String, sizeParams: List[Integer] = List()) extends BSVType
  case class BSizedInt(unsigned: Boolean, size: Int) extends BSVType
  case class BInteger() extends  BSVType
  case class BVarSizedInt(unsigned :Boolean, size :String) extends BSVType
  case class BTypeParam(name: String, provisos: List[Proviso]) extends BSVType
  case object BBool extends BSVType
  case object BString extends BSVType
  case object BVoid extends BSVType
  case object BEmptyModule extends BSVType

  def BMaybe(t: BSVType): BInterface = BInterface("Maybe", List(BVar("basetype", t)))

  class BSVTranslator(val bsints: BluespecInterfaces, val modmap: Map[Id, BSVType] = Map(),
    val handleMap: Map[BSVType, BSVType] = Map()) extends Translator[BSVType, BExpr, BVar, BFuncDef] {

    private var variablePrefix = ""

    def setVariablePrefix(p: String): Unit = variablePrefix = p

    //rename the type variable so that lockId and chkpointId can be independent
    //otherwise leave it the same
    private def lockIdToCheckId(l: BSVType): BSVType = l match {
      case BTypeParam(name, provisos) => BTypeParam("chk" + name, provisos.map {
        case PBits(szName) => PBits("chk" + szName)
        case b => b
      })
      case _ => l
    }
    //TODO it would be nice not to need two type translation methods
    def toTypeForMod(t: Type, n: Id): BSVType = t match {
      case TLockedMemType(mem, idsz, limpl) =>
        val lidtyp = if (idsz.isDefined) BSizedInt(unsigned = true, idsz.get) else modmap(n)
        val cidtyp = if (LockImplementation.supportsCheckpoint(limpl)) {
          Some(if (idsz.isDefined) {
            BSizedInt(unsigned = true, limpl.getChkIdSize(idsz.get))
          } else {
            lockIdToCheckId(modmap(n))
          })
        } else None
        val elemTyp = toType(mem.elem)
        val isAsync = mem.readLatency != Combinational
        val mtyp = bsints.getBaseMemType(isAsync,
          getTypeSize(elemTyp), BSizedInt(unsigned = true, mem.addrSize), elemTyp,
          if (isAsync) Math.max(mem.readPorts, mem.writePorts) else 0)
        //TODO pass checkpoint ID
        getLockedMemType(mem, mtyp, lidtyp, cidtyp, limpl, useTypeVars = true, Some(n))
      case _ => toType(t)
    }

    def getTypeSize(b: BSVType): Int = b match {
      case BSizedInt(_, size) => size
      case _ => throw UnexpectedBSVType("The size of the given BSV Type cannot be determined")
    }

    def toClientType(t: Type): BSVType = t match {
      case TMemType(elem, addrSize, rlat, wlat, _, _) if rlat == wlat && rlat == Asynchronous =>
        val elemTyp = toType(elem)
        bsints.getClientType(getTypeSize(elemTyp), BSizedInt(unsigned = true, addrSize), elemTyp)
      case _ => throw new RuntimeException
    }

    def toType(t: Type): BSVType = t match {
      case TObject(name, tparams, _) =>
        BInterface(name.toString, tparams.map(t => BVar( "nocare", toType(t))))
      case TMemType(elem, addrSize, rlat, _, readPorts, _) =>
        val elemTyp = toType(elem)
        bsints.getBaseMemType(isAsync = rlat != Combinational,
          getTypeSize(elemTyp), BSizedInt(unsigned = true, addrSize), elemTyp, readPorts)
      case TLockedMemType(mem, idsz, limpl) =>
        val mtyp = toType(mem).matchOrError() { case c: BInterface => c }
        val lidtyp = if (limpl.useUniqueLockId()) {
          if (idsz.isDefined) {
            bsints.getLockHandleType(idsz.get)
          } else bsints.getDefaultLockHandleType
        } else {
          //re-use the rid from the memory
          mtyp.tparams.find(bv => bv.name == bsints.reqIdName).get.typ
        }
        val cidtyp = if (LockImplementation.supportsCheckpoint(limpl)) {
          Some(if (idsz.isDefined) {
            bsints.getChkHandleType(limpl.getChkIdSize(idsz.get))
          } else bsints.getDefaultChkHandleType)
        } else { None }
        //TODO pass checkpoint ID
        getLockedMemType(mem, mtyp, lidtyp, cidtyp, limpl, useTypeVars = false, None)
      case TSizedInt(TBitWidthVar(name), sign) => BVarSizedInt(sign.unsigned(), name.v)
      case TSizedInt(len :TBitWidthLen, sign) => BSizedInt(sign.unsigned(), len.getLen)
      case TInteger() => BInteger();
      case TBitWidthLen(len) => BNumericType(len)        
      case TBool() => BBool
      case TString() => BString
      case TModType(_, _, _, Some(n)) => modmap(n)
      case TModType(_, _, _, None) => throw UnexpectedType(t.pos, "Module type", "A Some(mod name) typ", t)
      case TMaybe(btyp) => BMaybe(toType(btyp))
      case TRequestHandle(n, rtyp) => rtyp match {
        case pipedsl.common.Syntax.RequestType.Checkpoint =>
          lockIdToCheckId(modmap(n))
        //TODO allow this to be specified somewhere
        case pipedsl.common.Syntax.RequestType.Speculation => bsints.getDefaultSpecHandleType
        case pipedsl.common.Syntax.RequestType.Lock => {
          //if its a type variable it'll be in the modmap, else use default lockid size
          if (modmap.contains(n)) { modmap(n) } else {
            bsints.getLockHandleType(getDefaultLockImpl.getLockIdSize)
          }
        }
        case pipedsl.common.Syntax.RequestType.Module => //pipedsl.common.Syntax.RequestType.Module =>
          val modtyp = toType(n.typ.get)
          if (handleMap.contains(modtyp)) {
            handleMap(modtyp)
          } else {
            //if not in the handle map, use the appropriate default handle size. If the
            //handle is for a normal module then there is no default
            n.typ.get match {
              case l: TLockedMemType => if(l.limpl.useUniqueLockId()) bsints.getDefaultMemHandleType else modmap(n)
              case _: TMemType => bsints.getDefaultMemHandleType
              case _ => throw UnexpectedType(n.pos, "Module request handle", "A defined module req type", n.typ.get)
            }
          }
      }
      case TVoid() => BVoid
      case TNamedType(name) => BTypeParam(name.v, List(PBits("_sz" + name.v)))
      //TODO implement function type translation
      case TFun(_, _) => throw new RuntimeException
      //TODO better error
      case TRecType(_, _) => throw new RuntimeException
      case _ => throw UnexpectedType(t.pos, "type annotation", "Not Supported in BSV", t)
    }

    def toBSVVar(v: BVar): BVar = {
      BVar(variablePrefix + v.name, v.typ)
    }

    def toVar(i: Id): BVar = {
      if (i.typ.isEmpty)
        {
          println(s"$i at ${i.pos}")
        }
      BVar(variablePrefix + i.v, toType(i.typ.get))
    }

    def toVar(v: EVar): BVar = {
      toVar(v.id)
    }

    def toBSVIndex(index: EIndex) :BIndex = index match
    {
      case EIndConst(v) => BIndConst(v)
      case EIndAdd(l, r) => BIndAdd(toBSVIndex(l), toBSVIndex(r))
      case EIndSub(l, r) => BIndSub(toBSVIndex(l), toBSVIndex(r))
      case EIndVar(id) => BIndVar("val" + id.v)
    }

    def toExpr(e: Expr): BExpr = e match {
      case EInt(v, base, bits) => BIntLit(v, base, bits)
      case EBool(v) => BBoolLit(v)
      case EString(v) => BStringLit(v)
      case eu@EUop(_, _) => translateUOp(eu)
      case eb@EBinop(_, _, _) => toBSVBop(eb)
      case EBitExtract(num, start, end) =>
          val bnum = toExpr(num)
          //remove nested pack/unpacks
          bnum match {
            case BUnpack(e) => BUnpack(BBitExtract(e, toBSVIndex(start), toBSVIndex(end)))
            case e => BUnpack(BBitExtract(BPack(e), toBSVIndex(start), toBSVIndex(end)))
          }
      case ETernary(cond, tval, fval) => BTernaryExpr(toExpr(cond), toExpr(tval), toExpr(fval))
      case e@EVar(_) => toVar(e)
      case EApp(func, args) => BFuncCall(func.v, args.map(a => toExpr(a)))
      case ERecAccess(_, _) => throw UnexpectedExpr(e)
      case ERecLiteral(_) => throw UnexpectedExpr(e)
      case EMemAccess(mem, index, _, inHandle, _, isAtomic) =>
        val portNum = mem.typ.get match {
          case memType: TLockedMemType => if (memType.limpl.usesReadPortNum) e.portNum else None
          case _ => None
        }
        if (isLockedMemory(mem)) {
          //use the lock implementation's read method
          val mi = LockImplementation.getReadInfo(mem, index, inHandle, portNum, isAtomic).get
          BMethodInvoke(BVar(mem.v, toType(mem.typ.get)), mi.name, mi.usesArgs.map(a => toExpr(a)))
        } else {
          //use the unlocked method
          bsints.getUnlockedCombRead(BVar(mem.v, toType(mem.typ.get)), toExpr(index), portNum)
        }
      case ec@ECast(_, _) => translateCast(ec)
      case EIsValid(ex) => BIsValid(toExpr(ex))
      case EInvalid => BInvalid
      case EFromMaybe(ex) => BFromMaybe(BDontCare, toExpr(ex))
      case EToMaybe(ex) => BTaggedValid(toExpr(ex))
      case ECall(mod, method, args, isAtomic) if method.isDefined =>
        //type doesn't matter on the var
        BMethodInvoke(BVar(mod.v, BVoid), method.get.v, args.map(a => toExpr(a)))
      case _ => throw UnexpectedExpr(e)
    }

    private def translateUOp(e: EUop): BExpr = e.op match {
      case NumUOp(op) if op == "abs" || op == "signum" => BFuncCall(op, List(toExpr(e.ex)))
      case _ => BUOp(e.op.op, toExpr(e.ex))
    }
    //TODO handle casts better
    private def translateCast(e: ECast): BExpr = {
      e.ctyp match {
        case to@TSizedInt(_, _) => e.exp.typ match {
          case Some(from@TSizedInt(_, _)) => translateIntCast(from, to, e.exp)
          case _ => throw UnexpectedType(e.pos, "Couldn't translate BSV cast",
          "TSizedInt", e.ctyp)
        }
        case TBool() => toExpr(e.exp)
        case _ => throw UnexpectedType(e.pos, "Couldn't translate BSV cast",
          "TBool", e.ctyp)
      }
    }

    //This appropriately extends, truncates and packs/unpacks the
    //expression as appropriate to convert from one type to the other.
    //Extension must be made explicit since the pack/unpack operations will not
    //automatically determine an output type
    private def translateIntCast(from: TSizedInt, to:TSizedInt, e: Expr): BExpr = {
      val baseExpr = toExpr(e)
      //val needsExtend = from.len.getLen < to.len.getLen
      //val needsTruncate = to.len.getLen < from.len.getLen
      val needsExtend = (to.len, from.len) match {
        case (TBitWidthLen(lto), TBitWidthLen(lfrom)) => lfrom < lto
        case _ => false
      }
      val needsTruncate = (to.len, from.len) match {
        case (TBitWidthLen(lto), TBitWidthLen(lfrom)) => lto < lfrom
        case _ => false
      }


      val needsPack = (to, from) match {
        case (TSizedInt(_:TBitWidthLen, sto), TSizedInt(_:TBitWidthLen, sfrom)) =>
          sto != sfrom
        case _ => true
      }

        from.sign != to.sign
      val extended = if (needsExtend) {
        //If making a signed number, sign extend
        BExtend(baseExpr, useSign = to.sign.signed())
      } else if (needsTruncate) {
        BTruncate(baseExpr)
      } else {
        baseExpr
      }
      if (needsPack) {
        extended match {
            //don't nest unpacks
          case BUnpack(_) => extended
          case ex => BUnpack(BPack(ex))
        }
      } else {
        extended
      }
    }
    //TODO a better way to translate operators
    private def toBSVBop(b: EBinop): BExpr = b.op match {
      case BitOp("++", _) =>
        val left = toExpr(b.e1) match {
          case BUnpack(e) => e
          case e => BPack(e)
        }
        val right = toExpr(b.e2) match {
          case BUnpack(e) => e
          case e => BPack(e)
        }
        BUnpack(BConcat(left, List(right)))
      case NumOp("$*",_) =>
        BBOp("*",toExpr(b.e1), toExpr(b.e2))
      case NumOp("*",_) => b.typ match {
        case Some(TSizedInt(_, sign)) =>
          val op = if (sign.unsigned()) { "unsignedMul" } else { "signedMul" }
          BBOp(op, toExpr(b.e1), toExpr(b.e2), isInfix = false)
        case None => throw MissingType(b.pos, "Missing Type on Multiply BinOp")
        case _ => throw UnexpectedType(b.pos, "Mul Op", "Sized Integers", b.typ.get)
      }
      case _ => BBOp(b.op.op, toExpr(b.e1), toExpr(b.e2))
    }

    //TODO make these parameters passable and no just linked to the static lockimpl definition
    def getLockedModType(limpl: LockInterface): BInterface = {
      val lid = BVar("lidtyp", bsints.getLockHandleType(limpl.getLockIdSize))
      val chkid = BVar("chkidtyp", bsints.getChkHandleType(limpl.getChkIdSize(limpl.getLockIdSize)))
      BInterface(limpl.getModuleName(None), List(lid, chkid))
    }


    private def getLockedMemType(m: TMemType, mtyp: BInterface, lockIdTyp: BSVType, chkIdTyp: Option[BSVType],
                                 limpl: LockInterface, useTypeVars: Boolean = false, paramId: Option[Id]): BInterface = {
      val intName = limpl.getModuleName(m)
      //TODO allow these to come from somewhere - right now
      // the call to getTypeArgs just sets default values
      val lparams = limpl.getTypeArgs(List()).zipWithIndex.map(a => {
        val sz = a._1
        val idx = a._2
        if (useTypeVars) {
          //Don't use the Bits#() proviso for this type since it is a static integer not a wire type
          BVar("_unused_", BTypeParam("_szParam_" + idx + "_" + paramId.get, List()))
        } else {
          BVar("_unused_", BNumericType(sz))
       }})
      //replace tparam named 'ridtyp' w/ lockidtyp if not using a unique id
      val newLid = BVar("lidtyp", lockIdTyp)
      val tmpparams = if (limpl.useUniqueLockId()) {
        mtyp.tparams :+ newLid
      } else {
        mtyp.tparams.map(bv => {
          if (bv.name == bsints.reqIdName) { newLid } else { bv }
        })
      }
      //add checkpoint id if used
      val params = if (chkIdTyp.isDefined) {
        tmpparams :+ BVar("cidtyp", chkIdTyp.get)
      } else { tmpparams } ++ lparams
      BInterface(intName, params)
    }

    //This updates the translator's map of already defined functions
    //so that they can be used by later translation operations
    def toFunc(b: FuncDef): BFuncDef = {
      val rettype = toType(b.ret)
      val params = b.args.foldLeft(List[BVar]())((ps, arg) => {
        ps :+ BVar(arg.name.v, toType(arg.typ))
      })
      val fdef = BFuncDef(b.name.v, rettype, params,
        getFuncGenIntDecls(b) ++ translateFuncBody(b.body),
        getProvisos(b))
      fdef
    }

    private def getProvisos(b :FuncDef) :List[Proviso] =
    {
      val tmp = (b.adds.toList.map(pairid => PAdd(pairid._1._1, pairid._1._2, pairid._2.v)) ++
        b.mins.toList.map(pair => PMin(pair._1, pair._2))).distinct ++
        to_provisos(b.constraints)
      tmp
    }

    private def getFuncGenIntDecls(b :FuncDef): List[BStatement] =
    {
      b.templateTypes.map(id =>
        {
          val tmp = Id("val" + id.v)
          tmp.typ = Some(TInteger())
          BDecl(toVar(tmp),
            Some(BValueOf(id.v)))
        })
    }

    private def translateFuncBody(c: Command): List[BStatement] = c match {
      case CSeq(c1, c2) =>
        translateFuncBody(c1) ++ translateFuncBody(c2)
      case CIf(cond, cons, alt) =>
        List(BIf(toExpr(cond), translateFuncBody(cons), translateFuncBody(alt)))
      case CAssign(lhs, rhs) =>
        List(BDecl(toVar(lhs), Some(toExpr(rhs))))
      case CReturn(exp) =>
        List(BReturnStmt(toExpr(exp)))
      case CExpr(exp) =>
        List(BExprStmt(toExpr(exp)))
      //TODO case Syntax.CSplit(cases, default) =>
      case CEmpty() => List()
      case _ => throw UnexpectedCommand(c)
    }
  }


  /**
   * This creates a struct literal from the given struct type
   * and translator. This assumes that the arguments to struct creation
   * are variables with the same name as the fields (modulo any variable renaming
   * that translator may do).
   *
   * @param typ The BSV Struct Type
   * @param t   The translator object that will potentially rename the constructor arguments
   * @return The canonical struct literal for type typ.
   */
  def getCanonicalStruct(typ: BStruct, t: BSVTranslator): BStructLit = {
    val argmap = typ.fields.foldLeft(Map[BVar, BExpr]())((m, f) => {
      //don't translate field name, do translate RHS
      m + (BVar(f.name, f.typ) -> t.toBSVVar(f))
    })
    BStructLit(typ, argmap)
  }

  /**
   * This creates a struct literal from the given struct type
   * and specified arguments. This assumes that enough arguments
   * have been provided to set each field of the struct and that they
   * have been provided in the appropriate order (same as the order
   * of the fields in typ.
   *
   * @param typ  The BSV Struct Type
   * @param args The arguments to construct the struct with.
   * @return A Struct Literal created from args of type typ.
   */
  def getNamedStruct(typ: BStruct, args: Iterable[BExpr]): BStructLit = {
    BStructLit(typ, typ.fields.zip(args).toMap)
  }

  sealed trait BExpr

  case object BDontCare extends BExpr
  case object BZero extends BExpr
  case object BAllOnes extends BExpr
  case object BOne extends BExpr
  case object BTime extends BExpr
  case object BInvalid extends BExpr
  case class BTaggedValid(exp: BExpr) extends BExpr
  case class BFromMaybe(default: BExpr, exp: BExpr) extends BExpr
  case class BIsValid(exp: BExpr) extends BExpr
  case class BExtend(e: BExpr, useSign: Boolean) extends BExpr
  case class BTruncate(e: BExpr) extends BExpr
  case class BPack(e: BExpr) extends BExpr
  case class BUnpack(e: BExpr) extends BExpr
  case class BTernaryExpr(cond: BExpr, trueExpr: BExpr, falseExpr: BExpr) extends BExpr
  case class BBoolLit(v: Boolean) extends BExpr
  case class BUnsizedInt(v: Int) extends BExpr
  case class BIntLit(v: Int, base: Int, bits: Int) extends BExpr
  case class BStringLit(v: String) extends BExpr
  case class BStructLit(typ: BStruct, fields: Map[BVar, BExpr]) extends BExpr
  case class BStructAccess(rec: BExpr, field: BExpr) extends BExpr
  case class BVar(name: String, typ: BSVType) extends BExpr
  case class BBOp(op: String, lhs: BExpr, rhs: BExpr, isInfix: Boolean = true) extends BExpr
  case class BUOp(op: String, expr: BExpr) extends BExpr
  case class BBitExtract(expr: BExpr, start: BIndex, end: BIndex) extends BExpr
  case class BConcat(first: BExpr, rest: List[BExpr]) extends BExpr
  case class BModule(name: String, args: List[BExpr] = List()) extends BExpr
  case class BMethodInvoke(mod: BExpr, method: String, args: List[BExpr]) extends BExpr
  case class BFuncCall(func: String, args: List[BExpr]) extends BExpr
  case class BValueOf(s :String) extends BExpr

  sealed trait BIndex
  case class BIndConst(n :Int) extends BIndex
  case class BIndVar(v :String) extends BIndex
  case class BIndAdd(l :BIndex, r :BIndex) extends BIndex
  case class BIndSub(l :BIndex, r :BIndex) extends BIndex

  sealed trait BStatement {
    var useLet: Boolean = false
    def setUseLet(b: Boolean): BStatement = { this.useLet = b; this }
  }

  case class BStmtSeq(stmts: List[BStatement]) extends BStatement

  case class BExprStmt(expr: BExpr) extends BStatement

  case class BReturnStmt(expr: BExpr) extends BStatement

  case class BModInst(lhs: BVar, rhs: BModule) extends BStatement

  case class BModAssign(lhs: BVar, rhs: BExpr) extends BStatement

  case class BAssign(lhs: BVar, rhs: BExpr) extends BStatement

  case class BInvokeAssign(lhs: BVar, rhs: BExpr) extends BStatement

  case class BIntAssign(lhs: BVar, rhs: BVar) extends BStatement

  case class BDecl(lhs: BVar, rhs: Option[BExpr]) extends BStatement

  case class BIf(cond: BExpr, trueBranch: List[BStatement], falseBranch: List[BStatement]) extends BStatement

  case class BDisplay(fmt: Option[String], args: List[BExpr]) extends BStatement

  case object BFinish extends BStatement
  
  case class BDisplayVar(BVar: BVar) extends BStatement
  
  case object BEmpty extends BStatement

  case class BStructDef(typ: BStruct, derives: List[String])

  case class BRuleDef(name: String, conds: List[BExpr], body: List[BStatement])

  case class BMethodSig(name: String, typ: MethodType, params: List[BVar])

  case class BFuncDef(name: String, rettyp: BSVType,
                      params: List[BVar], body: List[BStatement],
                      provisos: List[Proviso])

  case class BMethodDef(sig: BMethodSig, cond: Option[BExpr] = None, body: List[BStatement])


  case class BModuleDef(name: String, typ: Option[BInterface],
    params: List[BVar], body: List[BStatement], rules: List[BRuleDef], methods: List[BMethodDef])

  case class BInterfaceDef(typ: BInterface, methods: List[BMethodSig], subints: List[BVar] = List())

  case class BImport(name: String)

  case class BExport(name: String, expFields: Boolean)

  case class BProgram(name: String, topModule: BModuleDef, imports: List[BImport], exports: List[BExport], structs: List[BStructDef],
    interfaces: List[BInterfaceDef], modules: List[BModuleDef])


  /**
   * Define common helper methods implicit classes.
   */
  implicit class RichType(typ: BSVType) {
    def matchOrError[A]()
      (andThen: PartialFunction[BSVType, A]): A = {
      val mismatchError: PartialFunction[BSVType, A] = {
        case _ => throw UnexpectedBSVType(s"Didn't expect BSV Type $typ")
      }
      andThen.orElse(mismatchError)(typ)
    }
  }
}
