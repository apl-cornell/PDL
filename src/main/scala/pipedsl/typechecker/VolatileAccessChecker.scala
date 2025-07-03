/* VolatileAccessChecker.scala */
package pipedsl.typechecker

import pipedsl.common.Syntax._
import pipedsl.typechecker.Environments._
import pipedsl.common.Errors._
import pipedsl.common.Locks.{General, Released}
import pipedsl.common.Syntax
import pipedsl.typechecker.TypeChecker.TypeChecks

object VolatileAccessChecker {

  private var volMemsRead: Set[Id] = Set()
  private var volMemsWrite: Set[Id] = Set()

  def check(p :Prog) :Unit =
    {
      p.moddefs.foreach(checkModule)
    }

  private def checkModule(m :ModuleDef) :Unit =
  {
    m.except_blk match {
      case ExceptEmpty() => 
        checkNoMultiVolatileAccess(m.body)
      case ExceptFull(_, c) =>
        checkNoVolatileWriteInBody(m.body)
        checkNoMultiVolatileAccess(m.body)
        volMemsRead = Set()
        volMemsWrite = Set()
        checkNoMultiVolatileAccess(m.commit_blk.get)
        checkNoMultiVolatileAccess(c)
    }
  }

  private def checkNoVolatileWriteInBody(c :Command) : Unit = c match {
    case CRecv(EMemAccess(mem, _, _, _, _, _), _) if isVolatileMemory(mem) => throw IllegalVolatileWrite(c.pos)
    case CSeq(c1, c2) => 
      checkNoVolatileWriteInBody(c1)
      checkNoVolatileWriteInBody(c2)
    case CTBar(c1, c2) => 
      checkNoVolatileWriteInBody(c1)
      checkNoVolatileWriteInBody(c2)
    case CIf(_, cons, alt) => 
      checkNoVolatileWriteInBody(cons)
      checkNoVolatileWriteInBody(alt) //TODO: miss conditions
    case _ => ()
  }

  private def checkNoMultiVolatileAccess(c :Command) : Unit = c match {
    case CRecv(EMemAccess(mem, _, _, _, _, _), _) if isVolatileMemory(mem) => 
      if (volMemsWrite.contains(mem)) throw NoMultipleVolatileAccess(c.pos)
      volMemsWrite += mem
    case CRecv(_, EMemAccess(mem, _, _, _, _, _)) if isVolatileMemory(mem) => 
      if (volMemsRead.contains(mem)) throw NoMultipleVolatileAccess(c.pos)
      volMemsRead += mem
    case CSeq(c1, c2) =>
      checkNoMultiVolatileAccess(c1)
      checkNoMultiVolatileAccess(c2)
    case CTBar(c1, c2) =>
      checkNoMultiVolatileAccess(c1)
      checkNoMultiVolatileAccess(c2)
    case CIf(_, cons, alt) =>
      checkNoMultiVolatileAccess(cons)
      checkNoMultiVolatileAccess(alt) //TODO: miss conditions
    case _ => ()
  }
}
