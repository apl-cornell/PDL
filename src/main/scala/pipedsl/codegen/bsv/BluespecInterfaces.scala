package pipedsl.codegen.bsv

import BSVSyntax._
import pipedsl.common.Errors.UnexpectedBSVType

object BluespecInterfaces {

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
  private val fifoType = "FIFOF"
  private val fifoDequeuMethodName = "deq"
  private val fifoEnqueueMethodName = "enq"
  private val fifoFirstMethodName = "first"

  private val lockHandleName = "LockId"
  private val defaultLockHandleSize = 4

  def getDefaultLockHandleType: BSizedType = getLockHandleType(defaultLockHandleSize)

  def getLockHandleType(sz: Integer): BSizedType = {
    BSizedType(lockHandleName, List(sz))
  }

  private val lockRegionType = "Reg"
  private val lockRegionModule = "mkReg"
  private val lockType = "Lock"
  private val lockModuleName = "mkLock"
  private val addrLockType = "AddrLock"
  private val addrLockModuleName = "mkFAAddrLock"

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

  def getLockModule(typ: BSVType): BModule = typ match {
    case BInterface(lt, _) if lt == lockType => BModule(lockModuleName, List())
    case BInterface(lt, _) if lt == addrLockType => BModule(addrLockModuleName, List())
    case _ => throw UnexpectedBSVType("Expected a lock interface type")
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

  private val lockEmptyName = "isEmpty"
  private val lockOwnsName = "owns"
  private val lockResName = "res"
  private val lockRelName = "rel"

  def getCheckEmpty(mod: BVar, addr: Option[BVar]): BMethodInvoke = {
    BMethodInvoke(mod, lockEmptyName, if (addr.isDefined) List(addr.get) else List())
  }
  def getCheckOwns(mod: BVar, handle: BExpr, addr: Option[BVar]): BMethodInvoke = {
    val args = if (addr.isDefined) List(BFromMaybe(BZero, handle), addr.get) else List(BFromMaybe(BZero, handle))
    BMethodInvoke(mod, lockOwnsName, args)
  }
  def getReserve(mod: BVar, addr: Option[BVar]): BMethodInvoke = {
    BMethodInvoke(mod, lockResName, if (addr.isDefined) List(addr.get) else List())
  }
  def getRelease(mod: BVar, handle: BExpr, addr: Option[BVar]): BMethodInvoke = {
    val args = if (addr.isDefined) List(BFromMaybe(BZero, handle), addr.get) else List(BFromMaybe(BZero, handle))
    BMethodInvoke(mod, lockRelName, args)
  }

  private val memHandleName = "MemId"
  private val defaultMemHandleSize = 2
  def getDefaultMemHandleType: BSizedType = getMemHandleType(defaultMemHandleSize)
  def getMemHandleType(sz: Integer): BSizedType = {
    BSizedType(memHandleName, List(sz))
  }

  private val asyncMemType = "AsyncMem"
  private val asyncMemMod = "mkAsyncMem"
  private val combMemName = "CombMem"
  private val combMemMod = "mkCombMem"

  def getIdParam(name: String): BTypeParam = BTypeParam(name + "Id")
  def getMemType(isAsync: Boolean, addr: BSVType, data: BSVType, id:Option[BSVType]): BInterface = {
    if (isAsync) {
      val idtyp = if (id.isDefined) {
        id.get
      } else {
        getIdParam("unkownMem")
      }
      BInterface(asyncMemType,
        List(BVar("elemtyp", data), BVar("addrtyp", addr), BVar("idtyp", idtyp)))
    } else {
      BInterface(combMemName,  List(BVar("elemtyp", data), BVar("addrtyp", addr)))
    }
  }

  def getMem(memtyp: BInterface): BModule = {
    memtyp.name match {
      case `asyncMemType` => BModule(asyncMemMod, List())
      case `combMemName` => BModule(combMemMod, List())
      case _ => throw UnexpectedBSVType(s"${memtyp.name} is not an supported memory interface")
    }
  }
  private val memCombReadName = "read"
  private val memCombWriteName = "write"

  private val memAsyncPeekName = "peekResp"
  private val memAsyncReqName = "req"
  private val memAsyncRespName = "resp"
  private val memAsyncCheckName = "checkRespId"

  def getMemPeek(mem: BVar): BMethodInvoke = {
    BMethodInvoke(mem, memAsyncPeekName, List())
  }
  def getCombRead(mem: BVar, addr: BExpr): BMethodInvoke = {
    BMethodInvoke(mem, memCombReadName, List(addr))
  }
  def getCombWrite(mem: BVar, addr: BExpr, data: BExpr): BMethodInvoke = {
    BMethodInvoke(mem, memCombWriteName, List(addr, data))
  }
  def getMemReq(mem: BVar, isWrite: Boolean, addr: BExpr, data: Option[BExpr]): BMethodInvoke = {
    BMethodInvoke(mem, memAsyncReqName, List(addr, if (data.isDefined) data.get else BDontCare, BBoolLit(isWrite)))
  }
  def getCheckMemResp(mem: BVar, handle: BExpr): BMethodInvoke = {
    BMethodInvoke(mem, memAsyncCheckName, List(handle))
  }
  def getMemResp(mem: BVar): BMethodInvoke = {
    BMethodInvoke(mem, memAsyncRespName, List())
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

  def getFifoDeq(f: BVar): BMethodInvoke = {
    BMethodInvoke(f, fifoDequeuMethodName, List())
  }
  def getFifoEnq(f: BVar, data: BExpr): BMethodInvoke = {
    BMethodInvoke(f, fifoEnqueueMethodName, List(data))
  }
  def getFifoPeek(f: BVar): BMethodInvoke = {
    BMethodInvoke(f, fifoFirstMethodName, List())
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
        BluespecInterfaces.responseMethod,
        BluespecInterfaces.checkHandleMethod(handleTyp))
    if (retTyp.isDefined) {
      methods = methods :+ BluespecInterfaces.peekMethod(retTyp.get)
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
