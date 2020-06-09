package pipedsl

import pipedsl.common.DAGSyntax._
import pipedsl.common.Errors.{UnexpectedExpr, UnexpectedType}
import pipedsl.common.Syntax
import pipedsl.common.Syntax._

object PipeCompiler {

  var stgCounter = 0
  var varCounter = 0
  val usedNames: scala.collection.mutable.Set[String] = scala.collection.mutable.Set[String]()

  def freshVar(baseName: String): Id = {
    var n = baseName
    while (usedNames(n)) {
      n = baseName + "_" + varCounter
      varCounter += 1
    }
    usedNames.add(n)
    Id(n)
  }

  def nextStageId(): Id = {
    val s = s"Stage__$stgCounter"
    stgCounter += 1
    Id(s)
  }


  def compileToDag(m: ModuleDef): PStage = {
    val extMems: Map[Id, Process] = m.modules.foldLeft[Map[Id,Process]](Map()) ((l, m) => {
      l + (m.name -> (m.typ match {
        case memt: TMemType => new PMemory(m.name, memt)
        case modt: TModType => new PBlackBox(m.name, modt)
        case _ => throw UnexpectedType(m.pos, m.name.toString, "Module or Memory", m.typ)
      }))
    })
    val stageList = splitToStages(m.body)
    stageList.foreach( p => {
      p.cmd = mergeIfCommands(p.cmd)
    })
    stageList.head
  }

  def splitToStages(c: Command): List[PStage] = c match {
    case CTBar(c1, c2) => {
      val firstStages = splitToStages(c1)
      val secondStages = splitToStages(c2)
      //sequential pipeline, last stage in c1 sends to first in c2
      val lastc1 = firstStages.last
      val firstc2 = secondStages.head
      lastc1.succs = lastc1.succs :+ firstc2
      firstc2.preds = firstc2.preds :+ lastc1
      firstStages ++ secondStages
    }
      //TODO once we add other control structures (like split+join) update this
    case _ => {
      List(new PStage(nextStageId(), c, List(), List(), List(), List(), List()))
    }
  }

  /**
   * Expect no time-control commands (tbar, split, join)
   *
   * @param c
   * @return
   */
  private def mergeIfCommands(c: Command): Command = c match {
    case CSeq(c1, c2) => CSeq(mergeIfCommands(c1), mergeIfCommands(c2))
    case CIf(cond, cons, alt) => {
      val tbr = mergeIfCommands(cons)
      val fbr = mergeIfCommands(alt)
      val renameMap: Map[Id,EVar] = Map[Id, EVar]()
      val tcmds = flattenCommands(tbr)
      val (renameTcmds, tmap) = renameAssignees(tcmds, renameMap, "_t")
      val fcmds = flattenCommands(fbr)
      val (renameFCmds, fmap) = renameAssignees(fcmds, renameMap, "_f")
      val sharedAssign = tmap.keySet.intersect(fmap.keySet)
      val condAssigns = sharedAssign.foldLeft[List[CAssign]](List())( (l, id) => {
        l :+ CAssign(EVar(id), ETernary(cond, tmap(id), fmap(id)))
      })
      sequenceCommands(renameTcmds ++ renameFCmds ++ condAssigns)
    }
    case _ => c
  }

  /**
   *
   * @param cs
   * @param renameMap
   * @return
   */
  def renameAssignees(cs: List[Command], renameMap: Map[Id, EVar], suffix: String): (List[Command], Map[Id, EVar]) = {
    var nmap = renameMap
    val ncs = cs.foldLeft[List[Command]](List()) ((l, c) => c match {
      case CAssign(lhs, rhs) => {
      val nrhs = renameVars(rhs, nmap)
      val nlhs = lhs match {
        case ev@EVar(id) if !nmap.contains(id) => {
          val tmp = ev.copy(freshVar(id.toString + suffix))
          tmp.copyMeta(ev)
          nmap = nmap + (id -> tmp)
          tmp
        }
      }
      l :+ CAssign(nlhs, nrhs).setPos(c.pos)
      }
      case Syntax.CEmpty => l
      case CExpr(exp) => l :+ CExpr(renameVars(exp, nmap)).setPos(c.pos)
      case COutput(exp) => l :+ COutput(renameVars(exp, nmap)).setPos(c.pos)
      case CReturn(exp) => l :+ CReturn(renameVars(exp, nmap)).setPos(c.pos)
      case CCall(id, args) => l :+ CCall(id, args.map[Expr](a => renameVars(a, nmap))).setPos(c.pos)
      case CRecv(lhs, rhs) => l :+ CRecv(renameVars(lhs, nmap), renameVars(rhs, nmap)).setPos(c.pos)
      case CIf(cond, cons, alt) => l :+ CIf(renameVars(cond, nmap), cons, alt).setPos(c.pos)
      case CDecl(id, typ, thisCycle) => l :+ c
    })
    (ncs, nmap)
  }
  /**
   *
   * @param e
   * @param renameMap
   * @return
   */
  def renameVars(e: Expr, renameMap: Map[Id, EVar]): Expr = e match {
    case eb@EBinop(_, e1, e2) => eb.copy(e1 = renameVars(e1, renameMap), e2 = renameVars(e2, renameMap)).copyMeta(e)
    case er@ERecAccess(rec, _) => er.copy(rec = renameVars(rec, renameMap)).copyMeta(e)
    case em@EMemAccess(_, index) => em.copy(index = renameVars(index, renameMap)).copyMeta(e)
    case ex@EBitExtract(num, _, _) => ex.copy(num = renameVars(num, renameMap)).copyMeta(e)
    case et@ETernary(cond, tval, fval) => et.copy(renameVars(cond, renameMap), renameVars(tval, renameMap), renameVars(fval, renameMap)).copyMeta(e)
    case ef@EApp(_, args) => ef.copy(args = args.map[Expr](a => renameVars(a,renameMap))).copyMeta(e)
    case ev@EVar(id) => {
      if (renameMap.contains(id)) {
        renameMap(id)
      } else {
        ev
      }
    }
    case _ => e
  }
  /**
   * This only flattens away CSeq commands, not ifs or other control flow.
   * It also assumes there are not time-control commands (tbar, split, join)
   */
  private def flattenCommands(c: Command): List[Command] = c match {
    case CSeq(c1, c2) => flattenCommands(c1) ++ flattenCommands(c2)
    case _ => List(c)
  }

  private def sequenceCommands(cs: List[Command]): Command = {
    cs.foldRight[Command](CEmpty)( (c , ce) => {
      CSeq(c, ce).setPos(c.pos)
    })
  }
  /*
   * Turns the recursive definition of commands into a list
   * of non-control commands. This assumes there are no TBar
   * or other pipeline structure commands in c.
   */
  def splitCommands(c: Command): List[StageCommand] = c match {
    case CSeq(c1, c2) =>
      splitCommands(c1) ++ splitCommands(c2)
    case CIf(cons, cond, alt) => {
      val cts = splitCommands(cond)
      val cfs = splitCommands(alt)
      val varsWritten = getWriteVarsC(cts).intersect(getWriteVarsC(cfs))
      val newCts = cts.foldLeft[List[StageCommand]](List())((l, c) => {
        c match {
          case SAssign(lhs, rhs) => {
            l
          }
          case SReceive(lhs, rhs,x) => l
          case _ => l :+ c
        }
      })
      newCts
    }
    case _ => List(toStageCmd(c))
  }

  /**
   * Adds a send to s1 and a receive to s2
   * which communicates all of the pipeline variables used in s2
   * returns the Channel created to communicate between these stages
   * @param s1
   * @param s2
   * @return
   */
    /*
  def sendPipelineVariables(s1: PStage, s2: PStage): Channel = {
    val extSends = s1.body.foldLeft[List[ExtRead]](List())( (l, c) => {
      l ++ getExtSends(c)
    })
    val recvars = getReadVarsC(s2.body)
    val s1tos2Sends = recvars -- extSends.foldLeft[Set[EVar]](Set())((s, exts) => { s + exts._1 })
    val s1Tos2 = Channel(s1, s2)
    val (sends, recvs) = s1tos2Sends.foldLeft[(List[SSend], List[SReceive])]((s1.succs, s2.preds))((l, v) => {
      (l._1 :+ SSend(None, v, s1Tos2), l._2 :+ SReceive(None, v, s1Tos2))
    })
    s1.succs = sends
    s2.preds = recvs
    s1Tos2
  }
  type ExtRead = (EVar, EVar, Id)
  */
  /*
   * This creates the appropriate send and receive operations between
   * stage1, an external module and stage2 for statements of the form:
   * v <- m[a]
   */
  /*
  def sendExternalReads(s1: PStage, s2: PStage, extmems: Map[Id, Process]): Unit = {
    val extSends = s1.body.foldLeft[List[ExtRead]](List())( (l, c) => {
     l ++ getExtSends(c)
    })
    val recvars = getReadVarsC(s2.body)
    val s1tomemtos2 = extSends.filter(t => { recvars(t._1) } )
    s1tomemtos2.foreach(t => {
      val recvar = t._1
      val sendvar = t._2
      val mem = extmems(t._3)
    })
  } */
  /*def convertToStage(c: Command, ext: Map[Id, Process]): PStage = c match {
    case CTBar(c1, c2) => {
      val stg1 = convertToStage(c1, ext)
      val stg2 = convertToStage(c2, ext)
      val extSends = getExtSends(stg1.body)
      //TODO for each different receiver, create a channel
      //TODO then create send and recv objects to represent communication on those channels
      val recvars = getReadVarsC(stg2.body)
      // vars sent from s1 to s2 directly are all that are read,
      // but not sent via an external module/memory read
      val s1tos2Sends = recvars -- extSends.foldLeft[Set[EVar]](Set())((s, exts) => { s + exts._1 })
      val s1Tos2 = Channel(stg1, stg2)
      val s = Send(None, s1tos2Sends.toList, s1Tos2)
      val r = Receive(None, s1tos2Sends.toList, s1Tos2)
      stg1.succs = List(stg2)
      stg2.preds = List(stg1)
      stg1
    }
    case _ => {
      new PStage(nextStageId(), List(), c, List())
    }
  }*/

  /*
  //Get the variable we're sending and the variable we expect
  //To receive into
  //Returns (RecVar, SendVar, ExtMod)

  def getExtSends(c: Command): List[ExtRead] = c match {
    case CSeq(c1, c2) => getExtSends(c1) ++ getExtSends(c2)
    case CIf(_, cons, alt) => getExtSends(cons) ++ getExtSends(alt)
    case CAssign(_, _) => List()
    case CRecv(lhs, rhs) =>(lhs, rhs) match {
      case (e:EVar, EMemAccess(mem, idx@EVar(_))) => {
        List((e, idx, mem))
      }
      case (EMemAccess(_,_), _) => throw UnexpectedExpr(lhs)
      case _ => List()
    }
    case _ => List()
  }
  */
  /*
  //All variables that are read from prior stage
  def getReadVarsC(cs: List[Command]): Set[EVar] = {
    var result: Set[EVar] = Set()
    var writevars: Set[EVar] = Set()
    for (c <- cs) {
      c match {
          //TODO turn ifs into straight-line code before using this
        case CIf(cond, cons, alt) =>
          writevars = writevars ++ getWriteVarsC(cons) ++ getWriteVarsC(alt)
          result = result ++ getReadVarsE(cond) ++ getReadVarsC(splitCommands(cons)) ++ getReadVarsC(splitCommands(alt))
        case CAssign(lhs, rhs) =>
          result = result ++ getReadVarsE(rhs)
          writevars ++ getWriteVarsE(lhs)
        case CRecv(lhs, rhs) =>  result = result ++ getReadVarsE(rhs)
        case CCall(id, args) =>
          result = args.foldLeft[Set[EVar]](result)((l, a) => {
            l ++ getReadVarsE(a)
          })
        case COutput(exp) => result = result ++ getReadVarsE(exp)
        case CReturn(exp) => result = result ++ getReadVarsE(exp)
        case CExpr(exp) => result = result ++ getReadVarsE(exp)
        case _ => {}
      }
    }
    result -- writevars
  }
 */
  //All variables written by this stage
  //including variables that won't be available until the next time stage
  //Does NOT support control commands (if, seq, tbar, etc.)
  def getWriteVarsC(cs: List[StageCommand]): Set[EVar] = {
    cs.foldLeft[Set[EVar]](Set()) ((s, c) => {
      c match {
        case SAssign(lhs, _) => getWriteVarsE(lhs)
        case SReceive(_, into, _) => Set(into)
        case _ => s
      }
    })
  }

  //All variables referenced in these expressions
  def getReadVarsE(e: Expr): Set[EVar] = e match {
    case EBinop(_, e1, e2) => getReadVarsE(e1) ++ getReadVarsE(e2)
    case ERecAccess(rec, _) => getReadVarsE(rec)
    case EBitExtract(num, _, _) => getReadVarsE(num)
    case ETernary(cond, tval, fval) => getReadVarsE(cond) ++ getReadVarsE(tval) ++ getReadVarsE(fval)
    case EApp(_, args) => args.foldLeft[Set[EVar]](Set())( (s, a) => { s ++ getReadVarsE(a) })
    case EMemAccess(_, index) => getReadVarsE(index)
    case ev: EVar => Set(ev)
    case _ => Set()
  }

  //We only record writes to variables, not memories here
  def getWriteVarsE(e: Expr): Set[EVar] = e match {
    case ev: EVar => Set(ev)
    case _ => Set()
  }
}
