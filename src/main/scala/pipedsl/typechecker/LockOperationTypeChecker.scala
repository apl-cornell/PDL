package pipedsl.typechecker;

import pipedsl.common.Errors.{IllegalLockModification, IllegalMemoryAccessOperation, MalformedLockTypes, UnexpectedCase}
import pipedsl.common.Locks.{General, LockGranularity, Specific}
import pipedsl.common.Syntax.{CAssign, CIf, CLockOp, CRecv, CSeq, CSpeculate, CSplit, CTBar, Command, ECall, EMemAccess, EVar, Id, LockArg, LockRead, LockType, LockWrite, ModuleDef, Prog}

/**
 * A class to check whether a program's lock types are correct, and to check the memory accesses are correct
 * given those lock types, i.e. Read and Write.
 *
 */
class LockOperationTypeChecker() {

  private var memLockOpAnnotationMap: Map[Id, Map[LockArg, LockType]] = Map().withDefaultValue(Map())
  private var currentMod: Id = Id("-invalid-")

  private def getLockAnnotationMap: Map[LockArg, LockType] = {
    memLockOpAnnotationMap(currentMod)
  }

  private def updateLockAnnotationMap(lid: LockArg, lt: LockType): Unit = {
    memLockOpAnnotationMap = memLockOpAnnotationMap.updated(currentMod, getLockAnnotationMap + (lid -> lt))
  }

  /**
   * Checks if the program has correct lock types
   * as defined in the comment at the top of this class.
   * @param p the program to be checked
   * @throws MalformedLockTypes error if lock types are not well formed
   * @throws IllegalMemoryAccessOperation error if access memory without correct READ or WRITE capability
   */
  def check(p:Prog) : Unit = {
    val Prog(_, moddefs, _) = p
    moddefs.foreach(m => checkModule(m))
  }

  private def checkModule(moduleDef: ModuleDef): Unit = {
    currentMod = moduleDef.name
    checkCommand(moduleDef.body)
  }

  private def checkCommand(command: Command): Unit = command match {
    case CSeq(c1, c2) => checkCommand(c1); checkCommand(c2)
    case CTBar(c1, c2) => checkCommand(c1); checkCommand(c2)
    case CIf(_, cons, alt) => checkCommand(cons); checkCommand(alt)
    case CSpeculate(_, _, verify, body) => checkCommand(verify); checkCommand(body)
    case CSplit(cases, default) =>
      cases.foreach(c => checkCommand(c.body))
      checkCommand(default)
    case c@CLockOp(mem, _, lockType) =>
      if (!c.isSpecific && lockType.isDefined)
        throw MalformedLockTypes("Can only specify lock type for address specific locks")
      if (lockType.isDefined) {
        if (getLockAnnotationMap.get(mem).isDefined && getLockAnnotationMap(mem) != lockType.get) {
          throw MalformedLockTypes("Only one lock type per an address specific lock is allowed")
        }
        c.memOpType = lockType
        mem.memOpType = lockType
        updateLockAnnotationMap(mem, lockType.get)
      } else if (c.isSpecific) {
        if (getLockAnnotationMap.get(mem).isEmpty) {
          throw MalformedLockTypes("Address specific locks must have an associated lock type")
        }
        c.memOpType = getLockAnnotationMap.get(mem)
        mem.memOpType = c.memOpType
      }
      //general locks stay as none
    case c@CRecv(lhs, rhs) => (lhs, rhs) match {
      case (e@EMemAccess(mem, index), _) => {
        //check if it exists and is lock read, otherwise is ok
        getLockAnnotationMap.get(LockArg(mem, Some(index.asInstanceOf[EVar]))) match {
          case Some(LockRead()) => throw IllegalMemoryAccessOperation(c.pos)
          case _ => {
            e.memOpType = Some(LockWrite())
            c.memOpType = Some(LockWrite())
          }
        }
      }
      case (_, e@EMemAccess(mem, index)) => {
        getLockAnnotationMap.get(LockArg(mem, Some(index.asInstanceOf[EVar]))) match {
          case Some(LockWrite()) => throw IllegalMemoryAccessOperation(c.pos)
          case _ => {
            e.memOpType = Some(LockRead())
            c.memOpType = Some(LockRead())
          }
        }

      }
      case (_, ECall(mod, _)) =>
      case _ => throw UnexpectedCase(c.pos)
    }

    case c@CAssign(lhs, rhs) => (lhs, rhs) match {
        //annotate the assign command and memory access
      case (_, e@EMemAccess(mem, index)) => {
        //check if it exists and is lock write, otherwise is ok (not existing means it is both read and write)
        getLockAnnotationMap.get(LockArg(mem, Some(index.asInstanceOf[EVar]))) match {
          case Some(LockWrite()) => throw IllegalMemoryAccessOperation(c.pos)
          case _ => {
            e.memOpType = Some(LockRead())
            c.memOpType = Some(LockRead())
          }
        }
      }
      case _ =>
    }
    case _ =>
  }
}

