package pipedsl.typechecker

import pipedsl.common.Errors.{IllegalLockAcquisition, IllegalOOOLockRelease}
import pipedsl.common.Locks.Released
import pipedsl.common.Syntax.{CIf, CLockOp, CSeq, CSplit, CTBar, Command, LockArg, ModuleDef, Prog}

/**
 * Object that checks that address specific locks are released in order
 */
object LockReleaseChecker {

  /**
   * Checks that the locks in program are released in thread order. It checks
   * this by just making sure that no two branches have the same release
   * argument in any of their releases. We can just
   * naively check these release statements because the lock checker
   * already ensures that all lock operations and memory accesses are legal
   * @param p program to be checked
   */
  def check(p: Prog): Unit = {
    p.moddefs.foreach(m => checkModule(m))
  }
  def checkModule(m: ModuleDef): Unit = {
    checkCommand(m.body, Set())
  }

  //released is the set of locks released up until that point in the program
  def checkCommand(c: Command, released: Set[LockArg]): Set[LockArg] = c match {
    case CSeq(c1, c2) => val s1 = checkCommand(c1, released); checkCommand(c2, s1)
    case CTBar(c1, c2) =>val s1 = checkCommand(c1, released); checkCommand(c2, s1)
    case CIf(cond, cons, alt) => {
      //Solely keeps track of the released commands in branches
      val s1 = checkCommand(cons, Set())
      val s2 = checkCommand(alt,  Set())
      if (s1.intersect(s2).nonEmpty) {
        throw IllegalOOOLockRelease(c.pos)
      }
      s1.union(s2) ++ released
    }
    case CSplit(cases, default) => {
      val caseRelList = (default :: cases.map(c => c.body)).map(c => checkCommand(c, Set()))
      for (i <- 0 to caseRelList.size-1) {
        for (j <- 0 to caseRelList.size-1) {
          if (i != j) {
            val l1 = caseRelList(i)
            val l2 = caseRelList(j)
            //If any locks were released in any two branches, error
            if (l1.intersect(l2).nonEmpty) {
              throw IllegalLockAcquisition(c.pos)
            }
          }
        }
      }
      caseRelList.foldLeft[Set[LockArg]](caseRelList(0))((env1, env2) => env1.union(env2)) ++ released
    }
      //only keep track of specific locks
    case c@CLockOp(mem, op, _) if c.isSpecific => op match {
      case Released => released + mem
      case _ => released
    }
    case _ => released
  }
}
