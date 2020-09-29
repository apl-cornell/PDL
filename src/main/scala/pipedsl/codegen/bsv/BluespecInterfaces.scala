package pipedsl.codegen.bsv

import pipedsl.common.BSVSyntax._
import pipedsl.common.Errors.UnexpectedBSVType

object BluespecInterfaces {

  val requestMethodName = "req"
  val responseMethodName = "resp"
  val peekMethodName = "peek"
  val checkHandleMethodName = "checkHandle"


  private val regModuleName = "mkReg"
  private val fifoModuleName = "mkFIFOF"
  private val regType = "Reg"
  private val fifoType = "FIFOF"
  private val fifoDequeuMethodName = "deq"
  private val fifoEnqueueMethodName = "enq"
  private val fifoFirstMethodName = "first"

  private val memCombReadName = "read"
  private val memWriteName = "write"
  private val memAsyncPeekName = "peekRead"
  private val memAsyncReadName = "readReq"
  private val memAsyncRespName = "readResp"
  private val memAsyncCheckName = "checkAddr"

  def toMethodInvoke(peek: BMemPeek): BMethodInvoke = {
    BMethodInvoke(peek.mem, memAsyncPeekName, List())
  }
  def toMethodInvoke(r: BMemRead): BMethodInvoke = {
    BMethodInvoke(r.mem, memCombReadName, List(r.addr))
  }
  def toMethodInvoke(r: BMemWrite): BMethodInvoke = {
    BMethodInvoke(r.mem, memWriteName, List(r.addr, r.data))
  }
  def toMethodInvoke(r: BMemReadReq): BMethodInvoke = {
    BMethodInvoke(r.mem, memAsyncReadName, List(r.addr))
  }
  def toMethodInvoke(r: BMemCheckAddr): BMethodInvoke = {
    BMethodInvoke(r.mem, memAsyncCheckName, List(r.addr))
  }
  def toMethodInvoke(r: BMemReadResp): BMethodInvoke = {
    BMethodInvoke(r.mem, memAsyncRespName, List())
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

  private val combMemMod = "mkCombMem"
  private val asyncMemMod = "mkAsyncMem"

  def getMemoryModule(mtyp: BSVType): BModule = mtyp match {
    case BCombMemType(_, _) => BModule(name = combMemMod, List())
    case BAsyncMemType(_, _) => BModule(name = asyncMemMod, List())
    case _ => throw UnexpectedBSVType("Not an expected memory type")
  }

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
