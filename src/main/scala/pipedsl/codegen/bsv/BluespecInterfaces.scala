package pipedsl.codegen.bsv

import BSVSyntax._
import pipedsl.common.Errors.UnexpectedBSVType

class BluespecInterfaces(val addrlockmod: Option[String]) {

  val topModTyp: BInterface = BInterface("TopMod")

  def toIntVar(v: BVar): BVar = {
    BVar("_int" + v.name, v.typ)
  }
  def topModInterface(submods: Iterable[BVar]): BInterfaceDef = BInterfaceDef(
    typ = topModTyp,
    methods = List(),
    subints = submods.map(v => toIntVar(v)).toList
  )
  private val tbInitMethod = "init"
  val tbInit: BMethodSig = BMethodSig(
    name = tbInitMethod,
    typ = Action,
    params = List())

  def tbModule(modName: String, testMod: BModule, initStmts: List[BStatement], modDone: List[BExpr], doneRegs: List[BStatement],
    bsInts: BluespecInterfaces, debug: Boolean): BModuleDef = {
    val startedRegInst = BModInst(BVar("started", bsInts.getRegType(BBool)),
      bsInts.getReg(BBoolLit(false)))
    val startedReg = startedRegInst.lhs
    val initCond = BUOp("!", startedReg)
    val setStartReg = BModAssign(startedReg, BBoolLit(true))
    val debugStart = if (debug) { BDisplay(Some("Starting Pipeline %t"), List(BTime)) } else BEmpty
    val initRule = BRuleDef(
      name = "initTB",
      conds = List(initCond),
      body = initStmts :+ setStartReg :+ debugStart
    )
    val doneRule = BRuleDef(
      name = "stopTB",
      conds = modDone,
      body = List(BFinish)
    )
    BModuleDef(
      name = "mkTB",
      typ = None,
      params = List(),
      body = doneRegs ++ List(startedRegInst, BModInst(BVar(modName, topModTyp), testMod)),
      rules = List(initRule, doneRule),
      methods = List()
    )
  }

  private val requestMethodName = "req"
  private val responseMethodName = "resp"
  private val peekMethodName = "peek"
  private val checkHandleMethodName = "checkHandle"

  def getModRequest(mod: BVar, args: Iterable[BExpr]): BMethodInvoke = {
    BMethodInvoke(mod, requestMethodName, args.toList)
  }
  def getModPeek(mod: BVar): BMethodInvoke = {
    BMethodInvoke(mod, peekMethodName, List())
  }
  def getModCheckHandle(mod: BVar, handle: BExpr): BMethodInvoke = {
    BMethodInvoke(mod, checkHandleMethodName, List(handle))
  }
  def getModResponse(mod: BVar): BMethodInvoke = {
    BMethodInvoke(mod, responseMethodName, List())
  }

  private val regModuleName = "mkReg"
  private val regType = "Reg"

  private val fifoModuleName = "mkFIFOF"
  private val fifoNBModuleName = "mkNBFIFOF"
  private val fifoType = "FIFOF"
  private val fifoDequeuMethodName = "deq"
  private val fifoEnqueueMethodName = "enq"
  private val fifoFirstMethodName = "first"

  private val lockHandleName = "LockId"
  val defaultLockHandleSize = 4

  def getDefaultLockHandleType: BSizedType = getLockHandleType(defaultLockHandleSize)

  def getLockHandleType(sz: Integer): BSizedType = {
    BSizedType(lockHandleName, List(sz))
  }

  private val lockRegionType = "Reg"
  private val lockRegionModule = "mkReg"
  private val lockType = "Lock"
  private val lockModuleName = "mkLock"
  private val addrLockType = "AddrLock"
  private val addrLockModuleName = if (addrlockmod.isDefined) addrlockmod.get else "mkFAAddrLock"

  def getLockRegionType: BInterface = {
    BInterface(lockRegionType, List(BVar("busy", BBool)))
  }

  //lock regions are, by default, available
  def getLockRegionModule: BModule = {
    BModule(lockRegionModule, List(BBoolLit(true)))
  }

  def getLockType(ht: BSVType): BInterface = {
    BInterface(lockType, List(BVar("idsize", ht)))
  }

  def getAddrLockType(ht: BSVType, elemtyp: BSVType, sz: Int = 4): BInterface = {
    BInterface(addrLockType,
      List(BVar("idsize", ht), BVar("addrtyp", elemtyp), BVar("numentries", BNumericType(sz))))
  }

  /**
  def getLockModule(typ: BSVType): BModule = typ match {
    case BInterface(lt, _) if lt == lockType => BModule(lockModuleName, List())
    case BInterface(lt, _) if lt == addrLockType => BModule(addrLockModuleName, List())
    case BInterface(lt, _) => BModule(lt, List())
    case _ => throw UnexpectedBSVType("Expected a lock interface type")
  }
  **/

  def getStart(mod: BVar): BStatement = {
    BModAssign(mod, BBoolLit(false))
  }

  def getStop(mod: BVar): BStatement = {
    BModAssign(mod, BBoolLit(true))
  }

  //the lockstate is represented by a boolean:
  //true => available, false => busy
  def getCheckStart(mod: BVar): BExpr = {
    mod
  }

  private val memHandleName = "MemId"
  private val defaultMemHandleSize = 8
  def getDefaultMemHandleType: BSizedType = getMemHandleType(defaultMemHandleSize)
  def getMemHandleType(sz: Integer): BSizedType = {
    BSizedType(memHandleName, List(sz))
  }

  private val asyncMemType = "BramPort"
  private val asyncMemMod = "mkBramPort"
  private val combMemType = "RegFile"
  private val combMemMod = "mkRegFile"

  val reqIdName = "ridtyp"

  def getBaseMemType(isAsync: Boolean, elemSize: Int, addr: BSVType, data: BSVType): BInterface = {
    if (isAsync) {
      //TODO make this type parameterizable
      val reqTyp = getDefaultMemHandleType
      val maskSize = elemSize / 8
      BInterface(asyncMemType, List(BVar("addrtyp", addr), BVar("elemtyp", data),
        BVar(reqIdName, reqTyp), BVar("nsz", BNumericType(maskSize))))
    } else {
      BInterface(combMemType,  List(BVar("addrtyp", addr), BVar("elemtyp", data)))
    }
  }

  def getMem(memtyp: BInterface, initFile: Option[String]): BModule = {
    memtyp.name match {
      case `asyncMemType` => BModule(asyncMemMod, List(BBoolLit(initFile.isDefined), BStringLit(initFile.getOrElse(""))))
      case `combMemType` => BModule(combMemMod, List(BBoolLit(initFile.isDefined), BStringLit(initFile.getOrElse(""))))
      case _ => throw UnexpectedBSVType(s"${memtyp.name} is not a supported memory interface")
    }
  }


  private val memCombReadName = "read"
  private val memCombWriteName = "write"
  private val memAsyncPeekName = "mem.peekResp"
  private val memAsyncReqName = "mem.req"
  private val memAsyncRespName = "mem.resp"
  private val memAsyncCheckName = "mem.checkRespId"

  def toMask(isWrite: Boolean, m: Option[BExpr]): BExpr = {
    val default = if (isWrite) { BAllOnes } else { BZero }
    if (m.isDefined) {
      BPack(m.get) //make it bits
    } else {
      default
    }
  }

  def getMemPeek(mem: BVar, handle: BExpr): BMethodInvoke = {
    BMethodInvoke(mem, memAsyncPeekName, List(handle))
  }
  def getCombRead(mem: BVar, addr: BExpr): BMethodInvoke = {
    BMethodInvoke(mem, memCombReadName, List(addr))
  }
  def getCombWrite(mem: BVar, addr: BExpr, data: BExpr): BMethodInvoke = {
    BMethodInvoke(mem, memCombWriteName, List(addr, data))
  }
  def getMemReq(mem: BVar, writeMask: Option[BExpr], addr: BExpr, data: Option[BExpr]): BMethodInvoke = {
    val isWrite = data.isDefined
    val mask = toMask(isWrite, writeMask)
    BMethodInvoke(mem, memAsyncReqName, List(addr, if (data.isDefined) data.get else BDontCare, mask))
  }
  def getCheckMemResp(mem: BVar, handle: BExpr): BMethodInvoke = {
    BMethodInvoke(mem, memAsyncCheckName, List(handle))
  }
  def getMemResp(mem: BVar, handle: BExpr): BMethodInvoke = {
    BMethodInvoke(mem, memAsyncRespName, List(handle))
  }

  /**
   * Uses the configured fifo interface type and the provided
   * BSV type to make a paramterized fifo type.
   *
   * @param typ - The BSV type that describes the fifo's elements' types
   * @return - The new BSV type describing the parameterized fifo
   */
  def getFifoType(typ: BSVType): BInterface = {
    BInterface(fifoType, List(BVar("elemtyp", typ)))
  }

  def getFifo: BModule = BModule(fifoModuleName, List())
  def getNBFifo: BModule = BModule(fifoNBModuleName, List())
  def getFifoDeq(f: BVar): BMethodInvoke = {
    BMethodInvoke(f, fifoDequeuMethodName, List())
  }
  def getFifoEnq(f: BVar, data: BExpr): BMethodInvoke = {
    BMethodInvoke(f, fifoEnqueueMethodName, List(data))
  }
  def getFifoPeek(f: BVar): BMethodInvoke = {
    BMethodInvoke(f, fifoFirstMethodName, List())
  }

  private val specHandleName = "SpecId"
  private val defaultSpecHandleSize = 4
  private val specModuleName = "mkSpecTable"
  private val specModuleType = "SpecTable"
  private val specAllocName = "alloc"
  private val specFreeName = "free"
  private val specCheckName = "check"
  private val specValidateName = "validate"
  private val specInvalidateName = "invalidate"

  def getDefaultSpecHandleType: BSizedType = getSpecHandleType(defaultSpecHandleSize)
  def getSpecHandleType(i: Integer): BSizedType = {
    BSizedType(specHandleName, List(i))
  }

  def getSpecTable: BModule = BModule(specModuleName, List())

  def getSpecTableType(typ: BSVType): BInterface = {
    BInterface(specModuleType, List(BVar("sidTyp", typ)))
  }

  def getSpecAlloc(st: BExpr): BExpr = {
    BMethodInvoke(st, specAllocName, List())
  }

  def getSpecFree(st: BVar, h: BExpr): BExpr = {
    BMethodInvoke(st, specFreeName, List(h))
  }

  def getSpecCheck(st: BVar, h: BExpr, order: Int): BExpr = {
    BMethodInvoke(st, specCheckName, List(h, BUnsizedInt(order)))
  }

  def getSpecValidate(st: BVar, h: BExpr, order: Int): BExpr = {
    BMethodInvoke(st, specValidateName, List(h, BUnsizedInt(order)))
  }
  def getSpecInvalidate(st: BVar, h: BExpr, order: Int): BExpr = {
    BMethodInvoke(st, specInvalidateName, List(h, BUnsizedInt(order)))
  }

  /**
   * Uses the configured register interface type and the provided
   * BSV type to make a paramterized register type.
   *
   * @param typ - The BSV type that describes the register's element type
   * @return - The new BSV type describing the parameterized register
   */
  def getRegType(typ: BSVType): BInterface = {
    BInterface(regType, List(BVar("elemtyp", typ)))
  }

  def getReg(initVal: BExpr): BModule = {
    BModule(regModuleName, List(initVal))
  }


  def defineInterface(intName: String, inputs: List[BVar],
    handleTyp: BSVType, retTyp: Option[BSVType]): BInterfaceDef = {
    var methods: List[BMethodSig] =
      List( requestMethod(inputs, handleTyp),
        responseMethod,
        checkHandleMethod(handleTyp))
    if (retTyp.isDefined) {
      methods = methods :+ peekMethod(retTyp.get)
    }
    BInterfaceDef(BInterface(intName.capitalize), methods)
  }

  def getRequestMethod(bint: BInterfaceDef): BMethodSig = bint.methods.find(m => m.name == requestMethodName).get
  def getResponseMethod(bint: BInterfaceDef): BMethodSig = bint.methods.find(m => m.name == responseMethodName).get
  def getPeekMethod(bint: BInterfaceDef): BMethodSig = bint.methods.find(m => m.name == peekMethodName).get
  def getHandleMethod(bint: BInterfaceDef): BMethodSig = bint.methods.find(m => m.name == checkHandleMethodName).get

  private def requestMethod(args: List[BVar], handleTyp: BSVType): BMethodSig =
    BMethodSig(requestMethodName, ActionValue(handleTyp), args)
  private def responseMethod: BMethodSig = BMethodSig(responseMethodName, Action, List())
  private def peekMethod(rettype: BSVType): BMethodSig = BMethodSig(peekMethodName, Value(rettype), List())
  private def checkHandleMethod(handletyp: BSVType): BMethodSig =
    BMethodSig(checkHandleMethodName, Value(BBool), List(BVar("handle", handletyp)))


  def getModuleName(prog: BProgram): String = prog.topModule.name
  def getInterface(prog: BProgram): BSVType = prog.topModule.typ match {
    case Some(value) => value
    case None => BEmptyModule
  }

  def getHandleType(prog: BProgram): BSVType = {
    val inttyp = prog.topModule.typ.get
    val handlemethod = prog.interfaces.find(i => i.typ == inttyp).get.
      methods.find(m => m.name == checkHandleMethodName).get
    handlemethod.typ match {
      case Value(rtyp) => rtyp
        //others should be unreachable
      case _ => throw UnexpectedBSVType(s"Handle Method should be Value type! but was ${handlemethod.typ}")
    }
  }
}
