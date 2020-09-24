package pipedsl.codegen.bsv

import pipedsl.common.BSVSyntax
import pipedsl.common.BSVSyntax._
import pipedsl.common.Errors.UnexpectedBSVType
import pipedsl.common.Syntax.Id

object BluespecInterfaces {

  val requestMethodName = "req"
  val responseMethodName = "resp"
  val peekMethodName = "peek"
  val checkHandleMethodName = "checkHandle"

  def requestMethod(args: List[BVar], handleTyp: BSVType): BMethodSig =
    BMethodSig(requestMethodName, ActionValue(handleTyp), args)
  def responseMethod: BMethodSig = BMethodSig(responseMethodName, Action, List())
  def peekMethod(rettype: BSVType): BMethodSig = BMethodSig(peekMethodName, Value(rettype), List())
  def checkHandleMethod(handletyp: BSVType): BMethodSig =
    BMethodSig(checkHandleMethodName, Value(BBool), List(BVar("handle", handletyp)))

  private val combMemMod = "mkCombMem"
  private val asyncMemMod = "mkAsyncMem"

  def getMemoryModule(mtyp: BSVType): BModule = mtyp match {
    case BCombMemType(_, _) => BModule(name = combMemMod, List())
    case BAsyncMemType(_, _) => BModule(name = asyncMemMod, List())
    case _ => throw UnexpectedBSVType("Not an expected memory type")
  }

  def getName(prog: BProgram): String = prog.topModule.name
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
    }
  }

  //This class can be used for convenient tracking of expected methods
  //that all generated bsv programs will expose in their top level module
  class InterfaceMap() {
    private case class Signature(req: BMethodSig, resp: BMethodSig, peek: BMethodSig, handle: BMethodSig)

    private var intMap: Map[BInterface, Signature] = Map()
    private var progMap: Map[Id, BProgram] = Map()

    def addModuleInterface(name: Id, prog: BProgram): Unit = {
      //TODO better error messages
      val interface = prog.topModule.typ.get
      val intdef = prog.interfaces.find(i => i.typ == interface).get
      val req = intdef.methods.find(m => m.name == requestMethodName).get
      val resp = intdef.methods.find(m => m.name == responseMethodName).get
      val peek = intdef.methods.find(m => m.name == peekMethodName).get
      val handle = intdef.methods.find(m => m.name == checkHandleMethodName).get
      val sig = Signature(req, resp, peek, handle)
      intMap = intMap + (interface -> sig)
      progMap = progMap + (name -> prog)
    }
    //TODO better error messages
    def getReq(int: BInterface): BMethodSig = intMap(int).req
    def getResp(int: BInterface): BMethodSig = intMap(int).resp
    def getPeek(int: BInterface): BMethodSig = intMap(int).peek
    def getHandle(int: BInterface): BMethodSig = intMap(int).handle

  }

}
