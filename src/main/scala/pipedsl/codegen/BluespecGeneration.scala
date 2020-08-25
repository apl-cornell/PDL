package pipedsl.codegen

import pipedsl.common.BSVSyntax._
import pipedsl.common.DAGSyntax.{PStage, PipelineEdge}
import pipedsl.common.Errors.{IllegalBSVStage, UnexpectedCommand}
import pipedsl.common.Syntax._

//TODO refactor so first stage generation is less unique compared to other stages
object BluespecGeneration {

  private val wireType = "Wire"
  private val wireModuleName = "mkWire"
  private val fifoType = "FIFOF"
  private val fifoModuleName = "mkFIFOF"

  private val memType = ""

  type EdgeInfo = Map[PipelineEdge, BStructDef]

  //Normal Map that special cases when checking the first Stage as part of the key
  //Since The First stage only has 1 incoming 'edge'
  class EdgeMap(firstStage: PStage, firstStageInput: BStruct, edges: EdgeInfo) {

    private val realMap: Map[PipelineEdge, BStruct] = edges map { case (k, v) => (k, v.typ) }

    def apply(e: PipelineEdge): BStruct = {
      realMap.get(e) match {
        case Some(b) => b
        case None if e.to == firstStage => firstStageInput
        case _ => throw new RuntimeException("Missing edge struct type")
      }
    }

  }

  type StageTypes = Map[PStage, BInterfaceDef]

  def getFifoType(typ: BSVType): BInterface = {
    BInterface(fifoType, List(BVar("elemtyp", typ)))
  }

  def getWireType(typ: BSVType): BInterface = {
    BInterface(wireType, List(BVar("elemtyp", typ)))
  }

  def getBSV(firstStage: PStage, rest: List[PStage]): BProgram = {
    //Data types for passing between stages
    val edgeStructInfo = getEdgeStructInfo(firstStage +: rest)
    //First stage should have exactly one input edge by definition
    val firstStageStruct = edgeStructInfo(firstStage.inEdges.head)
    val completeEdgeMap = new EdgeMap(firstStage, firstStageStruct.typ, edgeStructInfo)
    //Module for each stage
    val stgMap = (firstStage +: rest).foldLeft(Map[PStage, BModuleDef]())((m, s) => {
      m + (s -> getStageModule(s, completeEdgeMap))
    })
    val structDefs = edgeStructInfo.values.toList
    //TODO define the top level module interface
    BProgram(topModule = getTopModule(firstStage, rest, completeEdgeMap, stgMap),
      imports = List(BImport(fifoType)), structs = structDefs, interfaces = List(), modules = stgMap.values.toList)
  }

  private def getEdgeStructInfo(stgs: Iterable[PStage]): Map[PipelineEdge, BStructDef] = {
    stgs.foldLeft[Map[PipelineEdge, BStructDef]](Map())((m, s) => {
      s.inEdges.foldLeft[Map[PipelineEdge, BStructDef]](m)((ms, e) => {
        ms + (e -> getEdgeStruct(e))
      })
    })
  }

  private def getEdgeStruct(e: PipelineEdge): BStructDef = {
    val styp = BStruct(genEdgeName(e), getBsvStructFields(e.values))
    BStructDef(styp, List("Bits", "Eq"))
  }

  private def getBsvStructFields(inputs: Iterable[Id]): List[BVar] = {
    inputs.foldLeft(List[BVar]())((l, id) => {
      l :+ BVar(id.v, toBSVType(id.typ.get))
    })
  }

  private def genEdgeName(e: PipelineEdge) = {
    "E_" + e.from.name.v + "_TO_" + e.to.name.v
  }

  private def genParamName(e: PipelineEdge): String = {
    "fifo_" + e.from.name.v + "_" + e.to.name.v
  }

  private def genParamName(s: PStage): String = {
    "s_" + s.name
  }

  private def genModuleName(s: PStage): String = {
    "mk" + s.name.v
  }

  private def getStageModule(stg: PStage, edgeMap: EdgeMap): BModuleDef = {
    //Define params (fifos which transmit data to and from this stage)
    val outMap = stg.outEdges.foldLeft[Map[PipelineEdge, BVar]](Map())((m, e) => {
      val edgeStructType = edgeMap(e)
      m + (e -> BVar(genParamName(e.to), getFifoType(edgeStructType)))
    })
    val paramMap = outMap ++ stg.inEdges.foldLeft[Map[PipelineEdge, BVar]](Map())((m, e) => {
      val edgeStructType = edgeMap(e)
      m + (e -> BVar(genParamName(e.from), getFifoType(edgeStructType)))
    })
    //Generate the combinational connections
    val sBody = getStageBody(stg, paramMap, edgeMap)
    //Generate the set of execution rules for reading args and writing outputs
    val execRules: List[BRuleDef] = getExecRules(stg, edgeMap, paramMap)
    //TODO No methods at the moment since all communication is via FIFOs
    // this will probably need to change eventually
    BModuleDef(name = genModuleName(stg), typ = None,
      params = paramMap.values.toList, body = sBody, rules = execRules, methods = List())
  }

  //TODO - conditional recv/sends assume mutual exclusion at the moment
  private def getExecRules(stg: PStage, edgeMap: EdgeMap, paramMap: Map[PipelineEdge, BVar]): List[BRuleDef] = {
    val (condIn, uncondIn) = stg.inEdges.partition(e => e.condRecv.isDefined)
    val (condOut, uncondOut) = stg.outEdges.partition(e => e.condSend.isDefined)
    val execRules: List[BRuleDef] = (condIn.nonEmpty, condOut.nonEmpty) match {
      case (true, true) => throw IllegalBSVStage("Cannot conditionally read & write in a single stage")
      case (true, false) => {
        //has conditional inputs but no conditional outputs
        //1 rule for each cond exec, single output execute rule
        val outstmts = getEdgeStatements(stg, stg.outEdges, edgeMap, paramMap)
        val outconds = getEdgeConditions(stg, stg.outEdges, edgeMap, paramMap)
        val outRule = BRuleDef("execute", List(), outstmts)
        val uncondInStmts = getEdgeStatements(stg, uncondIn, edgeMap, paramMap)
        val inRules = condIn.foldLeft(List[BRuleDef]())((l, e) => {
          val inStmts = getEdgeStatements(stg, List(e), edgeMap, paramMap, readInputWires = true) ++ uncondInStmts
          //each rule has the read stmts for that edge, all the unconditional reads
          l :+ BRuleDef("input_" + l.size, outconds :+ toBSVExpr(e.condRecv.get), inStmts)
        })
        outRule +: inRules
      }
      case (false, true) => {
        //has conditional outputs but not inputs
        val uncondStatements = getEdgeStatements(stg, uncondIn ++ uncondOut, edgeMap, paramMap)
        //one rule for each cond out which also triggers the unconditional edges
        condOut.foldLeft(List[BRuleDef]())((l, e) => {
          val outstmt = getEdgeStatements(stg, List(e), edgeMap, paramMap)
          val newRule = BRuleDef("exec_" + l.size,
            List(toBSVExpr(e.condSend.get)), uncondStatements ++ outstmt)
          l :+ newRule
        })
      }
      case (false, false) => {
        //Single execute rule to do everything
        val allstmts = getEdgeStatements(stg, stg.allEdges, edgeMap, paramMap)
        List(BRuleDef("execute", List(), allstmts))
      }
    }
    execRules
  }

  private def getEdgeStatements(s: PStage, es: Iterable[PipelineEdge],
    edgeMap: EdgeMap, paramMap: Map[PipelineEdge, BVar], readInputWires: Boolean = false): List[BStatement] = {
    es.foldLeft(List[BStatement]())((l, e) => {
      val stmts = if (e.to == s) {
        val wireAssigns: List[BStatement] = if (readInputWires) {
          e.values.foldLeft(List[BStatement]())((le, v) => {
            val paramExpr = BMethodInvoke(paramMap(e), "first", List())
            le :+ BModAssign(
              BVar(v.v, getWireType(toBSVType(v.typ.get))),
              BStructAccess(paramExpr, BVar(v.v, toBSVType(v.typ.get))))
          })
        } else {
          List[BStatement]()
        }
        wireAssigns :+ BExprStmt(BMethodInvoke(paramMap(e), "deq", List()))
      } else {
        val op = getCanonicalStruct(edgeMap(e))
        List(BExprStmt(BMethodInvoke(paramMap(e), "enq", List(op))))
      }
      l ++ stmts
    })
  }

  private def getEdgeConditions(s: PStage, es: Iterable[PipelineEdge],
    edgeMap: EdgeMap, paramMap: Map[PipelineEdge, BVar]): List[BExpr] = {
    es.foldLeft(List[BExpr]())((l, e) => {
      val stmt = if (e.to == s) {
        BMethodInvoke(paramMap(e), "notEmpty", List())
      } else {
        BMethodInvoke(paramMap(e), "notFull", List())
      }
      l :+ stmt
    })
  }

  private def getStageBody(stg: PStage, pmap: Map[PipelineEdge, BVar], edgeMap: EdgeMap): List[BStatement] = {
    //First define all of the variables read by some edge
    val (condIn, uncondIn) = stg.inEdges.partition(e => e.condRecv.isDefined)
    var body: List[BStatement] = List()
    //unconditional reads are just variable declarations w/ values
    uncondIn.foreach(e => {
      //First element in read queue
      val paramExpr = BMethodInvoke(pmap(e), "first", List())
      e.values.foreach(v => {
        body = body :+ BDecl(BVar(v.v, toBSVType(v.typ.get)),
          BStructAccess(paramExpr, BVar(v.v, toBSVType(v.typ.get))))
      })
    })
    //conditional reads are Wires
    condIn.foreach(e => {
      //First element in read queue
      val paramExpr = BMethodInvoke(pmap(e), "first", List())
      e.values.foreach(v => {
        body = body :+ BModInst(lhs = BVar(v.v, getWireType(toBSVType(v.typ.get))),
          rhs = BModule(wireModuleName, List()))
      })
    })
    body = body ++ getCombinationalCommands(stg.cmds)
    body
  }

  private def getTopModule(firstStg: PStage, otherStgs: List[PStage], edgeMap: EdgeMap, stgMap: Map[PStage, BModuleDef]): BModuleDef = {
    //Body instantiates all of the params (fifos & memories) and then all of the stages
    //One fifo per edge in the graph
    val allEdges = (firstStg +: otherStgs).foldLeft(Set[PipelineEdge]())((es, s) => {
      es ++ s.inEdges ++ s.outEdges
    })
    val edgeFifos = allEdges.foldLeft(Map[PipelineEdge, BModInst]())((m, e) => {
      m + (e -> BModInst(BVar(genParamName(e), edgeMap(e)), BModule(fifoModuleName, List())))
    })
    val mkStgs = (firstStg +: otherStgs).map(s => {
      val moddef = stgMap(s)
      val modtyp = moddef.typ match {
        case Some(t) => t
        case None => BInterface(moddef.name, List())
      }
      val args = s.allEdges.map(e => {
        edgeFifos(e).lhs
      }).toList
      BModInst(BVar(genParamName(s), modtyp), BModule(moddef.name, args))
    })
    val stmts = edgeFifos.values.toList ++ mkStgs
    //TODO Execute rule is an unconditional send to the first stage
    BModuleDef(name = "mkTop", typ = None, params = List(), body = stmts,
      rules = List(), methods = List())
  }

  private def getCombinationalCommands(cmds: Iterable[Command]): List[BStatement] = {
    cmds.foldLeft(List[BStatement]())((l, c) => {
      getCombinationalCommand(c) match {
        case Some(bs) => l :+ bs
        case None => l
      }
    })
  }

  private def getCombinationalCommand(cmd: Command): Option[BStatement] = cmd match {
    case CAssign(lhs, rhs) =>
      Some(BDecl(BVar(lhs.id.v, toBSVType(lhs.typ.get)), toBSVExpr(rhs)))
    case ICondCommand(cond: Expr, c: Command) =>
      Some(BIf(toBSVExpr(cond), List(getCombinationalCommand(c).get), List()))
    case CExpr(exp) => Some(BExprStmt(toBSVExpr(exp)))
    case CRecv(lhs, rhs) => None
    case CCall(id, args) => None
    case CLockOp(mem, op) => None
    case CCheck(predVar) => None
    case CEmpty => None
    case v: ISpeculate => None
    case v: IUpdate => None
    case v: ICheck => None
    case v: IMemRecv => None
    case v: IMemSend => None
    case v: ISend => None
    case v: IRecv => None
    case CIf(cond, cons, alt) => throw UnexpectedCommand(cmd)
    case CSeq(c1, c2) => throw UnexpectedCommand(cmd)
    case CTBar(c1, c2) => throw UnexpectedCommand(cmd)
    case COutput(exp) => throw UnexpectedCommand(cmd)
    case CReturn(exp) => throw UnexpectedCommand(cmd)
    case CSpeculate(predVar, predVal, verify, body) => throw UnexpectedCommand(cmd)
    case CSplit(cases, default) => throw UnexpectedCommand(cmd)
  }


}
