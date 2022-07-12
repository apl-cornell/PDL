package pipedsl.codegen.bsv

import BSVSyntax._
import pipedsl.common.Errors.UnexpectedBSVType
import pipedsl.common.LockImplementation

class BluespecInterfaces() {

  val topModTyp: BInterface = BInterface("TopMod")

  def toIntVar(v: BVar): BVar = {
    val sfx = if(v.name.last.isDigit) v.name.last else ""
    val dotIndex = v.name.indexOf('.')
    val vname = if (dotIndex == -1) v.name else v.name.substring(0, dotIndex) + sfx
    BVar("_int" + vname, v.typ)
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

  def tbModule(modName: String, testMod: BModule, initStmts: List[BStatement], modDone: List[BExpr], modInsts: List[BStatement],
               bsInts: BluespecInterfaces, debug: Boolean, printTimer: Boolean = false): BModuleDef = {
    val startedRegInst = BModInst(BVar("started", bsInts.getRegType(BBool)),
      bsInts.getReg(BBoolLit(false)))
    val startedReg = startedRegInst.lhs
    val timerRegInst = BModInst(BVar("timer", bsInts.getRegType(BSizedInt(unsigned = true, 32))),
      bsInts.getReg(BZero))
    val timerReg = timerRegInst.lhs
    val initCond = BUOp("!", startedReg)
    val setStartReg = BModAssign(startedReg, BBoolLit(true))
    val debugStart = if (debug) { BDisplay(Some("Starting Pipeline %t"), List(BTime)) } else BEmpty
    val initRule = BRuleDef(
      name = "initTB",
      conds = List(initCond),
      body = initStmts :+ setStartReg :+ debugStart
    )
    val timerRule = BRuleDef(
      name = "timerCount",
      conds = List(),
      body = List(BModAssign(timerReg, BBOp("+", timerReg, BOne)))
    )
    val timerDone = BBOp(">=", timerReg, BIntLit(1000000,10,32))
    val doneConds = if (modDone.isEmpty) {
      List(timerDone)
    } else {
      List(BBOp("||", timerDone, modDone.reduce((l, r) => {
        BBOp("&&", l, r)}
      )))
    }
    val doneRule = BRuleDef(
      name = "stopTB",
      conds = doneConds,
      body = List(if (printTimer) BDisplay(Some("TIME %t"), List(BTime)) else BEmpty, BFinish)
    )
    BModuleDef(
      name = "mkTB",
      typ = None,
      params = List(),
      body = List(startedRegInst, timerRegInst, BModInst(BVar(modName, topModTyp), testMod)) ++ modInsts,
      rules = List(initRule, timerRule, doneRule),
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
  private val chkpointHandleName = "LockId"

  def getDefaultLockHandleType: BSizedType = getLockHandleType(LockImplementation.defaultLockHandleSize)
  def getDefaultChkHandleType: BSizedType = getChkHandleType(LockImplementation.defaultChkHandleSize)

  def getLockHandleType(sz: Integer): BSizedType = {
    BSizedType(lockHandleName, List(sz))
  }
  def getChkHandleType(sz: Integer): BSizedType = {
    BSizedType(chkpointHandleName, List(sz))
  }

  private val lockRegionType = "Reg"
  private val lockRegionModule = "mkReg"

  def getLockRegionType: BInterface = {
    BInterface(lockRegionType, List(BVar("busy", BBool)))
  }
  //lock regions are, by default, available
  def getLockRegionModule: BModule = {
    BModule(lockRegionModule, List(BBoolLit(true)))
  }
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


  private val asyncMemPortType = "BramPort"
  private val asyncMemPortMod = "mkBramPort"
  private val asyncMemPortType2 = "BramPort2"
  private val asyncMemPortMod2 = "mkBramPort2"
  private val asyncMemType = "AsyncMem"
  private val asyncMemMod = "mkAsyncMem"
  private val asyncMemType2 = "AsyncMem2"
  private val asyncMemMod2 = "mkAsyncMem2"
  private val combMemType = "RegFile"
  private val combMemMod = "mkRegFile"

  val reqIdName = "ridtyp"

  private def getMaskSize(elemSize: Int): Int = elemSize / 8

  def getMemPort(elemSize: Int,
                 addr: BSVType,
                 data: BSVType,
                 portNum: Int): BInterface = {
    val reqTyp = getDefaultMemHandleType //TODO make parameterizable
    val maskSize = elemSize / 8
    val memType = portNum match {
      case 1 => asyncMemPortType
      case 2 => asyncMemPortType2
    }
    BInterface(memType, List(BVar("addrtyp", addr), BVar("elemtyp", data),
      BVar(reqIdName, reqTyp), BVar("nsz", BNumericType(maskSize))))
  }

  def getBaseMemType(isAsync: Boolean,
                     elemSize: Int,
                     addr: BSVType,
                     data: BSVType,
                     portNum: Int): BInterface = {
    if (isAsync) {
      //TODO make this type parameterizable
      val reqTyp = getDefaultMemHandleType
      val maskSize = elemSize / 8
      val memType = portNum match {
        case 1 => asyncMemType
        case 2 => asyncMemType2
      }
      BInterface(memType, List(BVar("addrtyp", addr), BVar("elemtyp", data),
        BVar(reqIdName, reqTyp), BVar("nsz", BNumericType(maskSize))))
    } else {
      BInterface(combMemType,  List(BVar("addrtyp", addr), BVar("elemtyp", data)))
    }
  }

  def getRegister(initVal: Int): BModule = {
    BModule("mkRegister", List(BUnsizedInt(initVal)))
  }

  def getMem(memtyp: BInterface, initFile: Option[String]): BModule = {
    memtyp.name match {
      case `asyncMemPortType` =>
        BModule(asyncMemPortMod, List(BBoolLit(initFile.isDefined), BStringLit(initFile.getOrElse(""))))
      case `asyncMemPortType2` =>
        BModule(asyncMemPortMod2, List(BBoolLit(initFile.isDefined), BStringLit(initFile.getOrElse(""))))
      case `asyncMemType` => BModule(asyncMemMod, List())
      case `asyncMemType2` => BModule(asyncMemMod2, List())
      case `combMemType` => BModule(combMemMod, List(BBoolLit(initFile.isDefined), BStringLit(initFile.getOrElse(""))))
      case _ => throw UnexpectedBSVType(s"${memtyp.name} is not a supported memory interface")
    }
  }

  def getClientType(elemSize: Int, addr: BSVType, data: BSVType): BSVType = {
    val mask = BVar("nsz", BSizedType("Bit", List(getMaskSize(elemSize))))
    val addrt = BVar("addr", addr)
    val datat = BVar("data", data)
    val reqType = BVar("req", BInterface("Tuple3", List(mask, addrt, datat)))
    val respType = BVar("resp", data)
    BInterface("Client", List(reqType, respType))
  }

  private val memCombReadNameUnlocked = "sub"
  private val memCombWriteNameUnlocked = "upd"

  private val memAsync = "mem."
  private val memAsyncPeekName = "peekResp"
  private val memAsyncReqName = "req"
  private val memAsyncRespName = "resp"
  private val memAsyncCheckName = "checkRespId"
  private val memAsyncClearName = "clear"

  //TODO refactor to reuse code better
  def toMask(isWrite: Boolean, m: Option[BExpr]): BExpr = {
    val default = if (isWrite) { BAllOnes } else { BZero }
    if (m.isDefined) {
      BPack(m.get) //make it bits
    } else {
      default
    }
  }

  def getUnlockedCombRead(mem: BVar, addr: BExpr, port: Option[Int]): BMethodInvoke = {
    val portString = if (port.isDefined) port.get.toString else ""
    BMethodInvoke(mem, memCombReadNameUnlocked + portString, List(addr))
  }
  def getUnlockedCombWrite(mem: BVar, addr: BExpr, data: BExpr, port: Option[Int]): BMethodInvoke = {
    val portString = if (port.isDefined) port.get.toString else ""
    BMethodInvoke(mem, memCombWriteNameUnlocked + portString, List(addr, data))
  }

  def getMemPeek(mem: BVar, handle: BExpr, port: Option[Int], isLocked: Boolean): BMethodInvoke = {
    val portString = if (port.isDefined) port.get.toString else ""
    BMethodInvoke(mem, (if (isLocked) memAsync else "") + memAsyncPeekName + portString, List(handle))
  }

  def getMemReq(mem: BVar, isWrite: Boolean, writeMask: Option[BExpr], data: Option[BExpr],
                addrArgs: List[BExpr], methodName: String, isAtomic: Boolean): BMethodInvoke = {
    val mask = toMask(isWrite, writeMask)
    val dataArg = data.getOrElse(BDontCare)
    val name = if (isAtomic) methodName else memAsync + methodName //atomics go through lock, not the lower mem interface
    BMethodInvoke(mem, name, addrArgs :+ dataArg :+ mask)
  }

  def getUnlockedMemReq(mem: BVar, writeMask: Option[BExpr],
                        addr: BExpr, data: Option[BExpr], port: Option[Int]): BMethodInvoke = {
    val isWrite = data.isDefined
    val mask = toMask(isWrite, writeMask)
    val portString = if (port.isDefined) port.get.toString else ""
    BMethodInvoke(mem, memAsyncReqName + portString, List(addr, data.getOrElse(BDontCare), mask))
  }

  def getCheckMemResp(mem: BVar, handle: BExpr, port: Option[Int], isLocked: Boolean): BMethodInvoke = {
    val portString = if (port.isDefined) port.get.toString else ""
    BMethodInvoke(mem, (if (isLocked) memAsync else "") + memAsyncCheckName + portString, List(handle))
  }

  def getMemResp(mem: BVar, handle: BExpr, port: Option[Int], isLocked: Boolean): BMethodInvoke = {
    val portString = if (port.isDefined) port.get.toString else ""
    BMethodInvoke(mem, (if (isLocked) memAsync else "") + memAsyncRespName + portString, List(handle))
  }

  def getMemClear(mem: BVar, isAsync: Boolean, isLocked: Boolean): Option[BMethodInvoke] = {
    if (isAsync) {
      Some(BMethodInvoke(mem, (if (isLocked) memAsync else "") + memAsyncClearName, List()))
    } else {
      None
    }
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
  def getOutputQType(ttyp: BSVType, dtyp: BSVType): BInterface = {
    BInterface("OutputQ", List(BVar("tagtyp", ttyp), BVar("datatyp", dtyp)));
  }
  def getFifo: BModule = BModule(fifoModuleName, List())
  def getNBFifo: BModule = BModule(fifoNBModuleName, List())
  def getBypassFifo: BModule = BModule("mkBypassFIFOF", List())
  def getOutputQ(init: BExpr): BModule = BModule("mkOutputFIFOF", List(init))
  def getFifoDeq(f: BVar): BMethodInvoke = {
    BMethodInvoke(f, fifoDequeuMethodName, List())
  }
  def getFifoEnq(f: BVar, data: BExpr): BMethodInvoke = {
    BMethodInvoke(f, fifoEnqueueMethodName, List(data))
  }
  def getFifoFirst(f: BVar): BMethodInvoke = {
    BMethodInvoke(f, fifoFirstMethodName, List())
  }
  def getOutCanWrite(q: BVar, tag: BExpr): BMethodInvoke = {
    BMethodInvoke(q, "canWrite", List(tag));
  }
  def getOutCanRead(q: BVar, tag: BExpr): BMethodInvoke = {
    BMethodInvoke(q, "canRead", List(tag));
  }

  private val specHandleName = "SpecId"
  private val defaultSpecHandleSize = 4
  private val specModuleName = "mkSpecTable"
  private val specModuleType = "SpecTable"
  private val specAllocName = "alloc"
  private val specFreeName = "free"
  private val specClearName = "clear"
  private val specCheckName = "check"
  private val specValidateName = "validate"
  private val specInvalidateName = "invalidate"

  def getDefaultSpecHandleType: BSizedType = getSpecHandleType(defaultSpecHandleSize)
  def getSpecHandleType(i: Integer): BSizedType = {
    BSizedType(specHandleName, List(i))
  }

  def getSpecTable: BModule = BModule(specModuleName, List())

  def getSpecTableType(typ: BSVType, sz:Int): BInterface = {
    BInterface(specModuleType, List(BVar("sidTyp", typ), BVar("bypassCnt", BNumericType(sz))))
  }

  def getSpecAlloc(st: BExpr): BExpr = {
    BMethodInvoke(st, specAllocName, List())
  }

  def getSpecFree(st: BVar, h: BExpr): BExpr = {
    BMethodInvoke(st, specFreeName, List(h))
  }

  def getSpecClear(st: BVar): BExpr = {
    BMethodInvoke(st, specClearName, List());
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

  //Client Server Constants
  private val bramClient = "bram_client"
  private val bramServer = "bram_server"
  private val mkConnName = "mkConnection"

  def getBramServerName = bramServer
  def getBramClientName = bramClient

  def makeConnection(client: BVar, server: BVar, port_num :Int): BStatement = {
    BExprStmt(BFuncCall(mkConnName,
      List(client, BMethodInvoke(server, getBramServerName + (if(port_num > 0) port_num else ""), List()))))
  }
}
