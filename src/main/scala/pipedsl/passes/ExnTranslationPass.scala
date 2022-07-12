package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.passes.Passes.{ModulePass, ProgPass}
import pipedsl.typechecker.BaseTypeChecker.replaceNamedType

class ExnTranslationPass extends ModulePass[ModuleDef] with ProgPass[Prog]{
  private var exnArgIdMap = Map[Id, Id]()
  private var exnArgTypeMap = Map[Id, Type]()

  private val localExnFlag = EVar(Id("_localExnFlag"))

  override def run(m: ModuleDef): ModuleDef =
  {
    if(is_excepting(m)){
      val new_m = addExnVars(m)
      new_m.name.typ = m.name.typ
      val modified_exnblk = m.except_blk.map(convertExnArgsId)
      createNewStg(new_m.copy(body = new_m.body, commit_blk = new_m.commit_blk, except_blk = modified_exnblk).copyMeta(m))
    } else {
      m
    }
  }

  override def run(p: Prog): Prog = p.copy(moddefs = p.moddefs.map(m => run(m)))

  def addExnVars(m: ModuleDef): ModuleDef =
  {
    localExnFlag.typ = Some(TBool())
    localExnFlag.id.typ = localExnFlag.typ
    val fixed_except = m.except_blk match {
      case ExceptFull(args, c) =>
        var arg_count = 0
        args.foreach(arg => {
          arg.typ match {
            case Some(t: Type) =>
              val newExnArgId = Id("_exnArg_"+arg_count.toString())
              arg_count += 1
              exnArgIdMap = exnArgIdMap + (arg -> newExnArgId)
              exnArgTypeMap = exnArgTypeMap + (newExnArgId -> t)
            case _ =>
              arg_count += 1
          }
        })
      case ExceptEmpty() => CEmpty()
    }

    val newAssignments = exnArgIdMap.foldLeft(CSeq(CEmpty(), CEmpty()))((c, id_mapping) => {
      val (lhs, rhs) = id_mapping
      val setCurrArg = CAssign(EVar(lhs), EBool(false))
      CSeq(c, setCurrArg)
  })
    m.copy(body = CSeq(IStageClear(), convertPrimitives(m.body)), commit_blk = m.commit_blk, except_blk = m.except_blk).copyMeta(m)
  }

  def convertPrimitives(c: Command): Command = {
    c match {
      case CSeq(c1, c2) => CSeq(convertPrimitives(c1), convertPrimitives(c2)).copyMeta(c)
      case CIf(cond, cons, alt) => CIf(cond, convertPrimitives(cons), convertPrimitives(alt)).copyMeta(c)
      case CTBar(c1, c2) => CTBar(convertPrimitives(c1), CSeq(IStageClear(), convertPrimitives(c2))).copyMeta(c)
      case CSplit(cases, default) =>
        val newCases = cases.map(c => CaseObj(c.cond, convertPrimitives(c.body)))
        CSplit(newCases, convertPrimitives(default)).copyMeta(c)
      case CExcept(args) =>
        val setLocalErrFlag = CAssign(localExnFlag, EBool(true)).copyMeta(c)
        var arg_count = 0
        val setArgs: Command = args.foldLeft[Command](CSeq(setLocalErrFlag, CEmpty()))((c, arg) => {
          arg.typ match {
            case Some(t: Type) =>
              val translatedVarId = Id("_exnArg_"+arg_count.toString())
              translatedVarId.setType(exnArgTypeMap.getOrElse(translatedVarId, TVoid()))
              val translatedVar = EVar(translatedVarId)
              translatedVar.typ = translatedVarId.typ
              val setCurrArg = CAssign(translatedVar, arg).copyMeta(c)
              arg_count += 1
              CSeq(c, setCurrArg).copyMeta(c)
            case _ => c
          }
      })
        setArgs
      case _ => c
    }
  }

  def convertExnArgsId(c: Command): Command = {
    c match {
      case CSeq(c1, c2) => CSeq(convertExnArgsId(c1), convertExnArgsId(c2)).copyMeta(c)
      case CIf(cond, cons, alt) => CIf(cond, convertExnArgsId(cons), convertExnArgsId(alt)).copyMeta(c)
      case CTBar(c1, c2) => CSeq(convertExnArgsId(c1), convertExnArgsId(c2)).copyMeta(c);
      case CSplit(cases, default) =>
        val newCases = cases.map(c => CaseObj(c.cond, convertExnArgsId(c.body)))
        CSplit(newCases, convertExnArgsId(default)).copyMeta(c)
      case CAssign(v, exp) =>
        val newv = EVar(exnArgIdMap.getOrElse(v.id, v.id)).setPos(v.pos)
        newv.typ = Some(exnArgTypeMap.getOrElse(v.id, TVoid()))
        CAssign(newv, exp).copyMeta(c)
      case CPrint(args) =>
        val newArgs = args.foldLeft(List[Expr]())((l, arg) => {
          arg match {
            case EVar(id) =>
              val newv = EVar(exnArgIdMap.getOrElse(id, id)).setPos(c.pos)
              newv.typ = Some(exnArgTypeMap.getOrElse(id, TVoid()))
              l :+ newv
            case _ => l
          }
        })
        CPrint(newArgs).copyMeta(c)
      case _ => c
    }
  }

  def createNewStg(m: ModuleDef): ModuleDef = {
    val commit_stmts = m.commit_blk match {
      case Some(c) => c
      case _ => CEmpty()
    }
    val except_stmts = m.except_blk match {
      case ExceptFull(_, c) =>
        val setGlobalExnFlag = ISetGlobalExnFlag(true)
        val unsetGlobalExnFlag = ISetGlobalExnFlag(false)
        val abortStmts = m.modules.foldLeft(CSeq(CEmpty(), CEmpty()))((c, mod) =>
        mod.typ match {
          case TLockedMemType(mem, _, _) => CSeq(c, IAbort(mod.name))
          case _ => c
        })
        CSeq(setGlobalExnFlag, CSeq(abortStmts, CTBar(c, unsetGlobalExnFlag)))
      case ExceptEmpty() => CEmpty()
    }
    val checkLocalFlag = CIf(localExnFlag, commit_stmts, except_stmts)
    val newBody = CSeq(m.body, checkLocalFlag)

    //TODO require memory or module types
    m.copy(body = newBody, commit_blk = m.commit_blk, except_blk = m.except_blk).copyMeta(m)
  }
}
