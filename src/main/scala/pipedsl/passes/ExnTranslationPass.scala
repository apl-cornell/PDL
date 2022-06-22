package pipedsl.passes

import pipedsl.common.Syntax._
import pipedsl.passes.Passes.{ModulePass, ProgPass}
import pipedsl.typechecker.BaseTypeChecker.replaceNamedType

object ExnTranslationPass extends ModulePass[ModuleDef] with ProgPass[Prog]{
  private var exnArgMap = Map[Id, Id]()

  override def run(m: ModuleDef): ModuleDef =
  {
    val fixed_commit = m.commit_blk match {
      case Some(c) => c
      case _ => CEmpty()
    }

    val fixed_except = m.except_blk match {
      case ExceptFull(_, c) => c
      case ExceptEmpty() => CEmpty()
    }

    val new_m = addExnVars(m)
    new_m.name.typ = m.name.typ
    print(new_m.body)
    val modified_exnblk = m.except_blk.map(convertExnArgsId)
    createNewStg(new_m.copy(body = new_m.body, commit_blk = new_m.commit_blk, except_blk = modified_exnblk))
  }

  override def run(p: Prog): Prog = p.copy(moddefs = p.moddefs.map(m => run(m)))

  def addExnVars(m: ModuleDef): ModuleDef =
  {
    val fixed_except = m.except_blk match {
      case ExceptFull(args, c) =>
        var arg_count = 0
        args.foreach(arg => {
          exnArgMap = exnArgMap + (arg -> Id("_exnArg_"+arg_count.toString()))
          arg_count += 1
        })
      case ExceptEmpty() => CEmpty()
    }

    val newAssignments = exnArgMap.foldLeft(CSeq(CEmpty(), CEmpty()))((c, id_mapping) => {
      val (lhs, rhs) = id_mapping
      val setCurrArg = CAssign(EVar(lhs), EBool(false))
      CSeq(c, setCurrArg)
  })
    m.copy(body = CSeq(IStageClear(), convertPrimitives(m.body)), commit_blk = m.commit_blk, except_blk = m.except_blk)
  }

  def convertPrimitives(c: Command): Command = {
    c match {
      case CSeq(c1, c2) => CSeq(convertPrimitives(c1), convertPrimitives(c2)).copyMeta(c)
      case CIf(cond, cons, alt) => CIf(cond, convertPrimitives(cons), convertPrimitives(alt)).copyMeta(c)
      case CTBar(c1, c2) => CTBar(convertPrimitives(c1), CSeq(IStageClear(), convertPrimitives(c2))).copyMeta(c)
      case CSplit(cases, default) =>
        val newCases = List[CaseObj]()
        val newDefault = convertPrimitives(default)
        for (index <- cases.indices) {
          val newBody = convertPrimitives(cases(index).body)
          newCases :+ cases(index).copy(body = newBody)
        }
        CSplit(newCases, newDefault)
      case CExcept(args) =>
        val localflag = EVar(Id("_localExnFlag"))
        localflag.typ = Some(TBool())
        localflag.id.typ = localflag.typ

        val setLocalErrFlag = CAssign(localflag, EBool(true)).copyMeta(c)
        var arg_count = 0
        val setArgs: Command = args.foldLeft[Command](CSeq(setLocalErrFlag, CEmpty()))((c, arg) => {
          val setCurrArg = CAssign(EVar(Id("_exnArg_"+arg_count.toString())), arg).copyMeta(c)
          arg_count += 1
          CSeq(c, setCurrArg).copyMeta(c)
      })
        setArgs
      case _ => c
    }
  }

  def convertExnArgsId(c: Command): Command = {
    c match {
      case CSeq(c1, c2) => CSeq(convertExnArgsId(c1), convertExnArgsId(c2))
      case CIf(cond, cons, alt) => CIf(cond, convertExnArgsId(cons), convertExnArgsId(alt))
      case CTBar(c1, c2) => CSeq(convertExnArgsId(c1), convertExnArgsId(c2));
      case CSplit(cases, default) =>
        val newCases = List[CaseObj]()
        val newDefault = convertExnArgsId(default)
        for (index <- cases.indices) {
          val newBody = convertExnArgsId(cases(index).body)
          newCases :+ cases(index).copy(body = newBody)
        }
        CSplit(newCases, newDefault)
      case CAssign(v, exp) =>
        val newv = EVar(exnArgMap.getOrElse(v.id, v.id)).setPos(v.pos)
        CAssign(newv, exp)
      case CPrint(args) =>
        val newArgs = args.foldLeft(List[Expr]())((l, arg) => {
          arg match {
            case EVar(id) => l :+ EVar(exnArgMap.getOrElse(id, id)).setPos(arg.pos)
            case _ => l
          }
        })
        CPrint(newArgs)
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
//        m.modules.filter()
        CSeq(IAbort(m.name), c)
      case ExceptEmpty() => CEmpty()
    }
    val localflag = EVar(Id("_localExnFlag"))
    localflag.typ = Some(TBool())
    localflag.id.typ = localflag.typ
    val checkLocalFlag = CIf(localflag, commit_stmts, except_stmts)
    val newBody = CTBar(m.body, checkLocalFlag)

    val inputTyps = m.inputs.foldLeft[List[Type]](List())((l, p) => { l :+ p.typ })
    //TODO require memory or module types
    m.name.typ = Some(TModType(inputTyps, List[Type](), m.ret, Some(m.name)))
    m.copy(body = newBody, commit_blk = None, except_blk = ExceptEmpty())
  }
}
