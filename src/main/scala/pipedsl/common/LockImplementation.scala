package pipedsl.common

import pipedsl.common.Syntax._


object LockImplementation {

  trait LockInterface {
    //Determines whether or not this lock type can support the given list of
    //commands in a given stage
    //returns: true if no conflicts, false if has conflicts
    def checkConflicts(mem: LockArg, lops: Iterable[Command]): Boolean
    def mergeLockOps(mem: LockArg, lops: Iterable[Command]): Iterable[Command]
  }

  def largMatches(mem: LockArg, mid: Id, addr: EVar): Boolean = {
    mem.id == mid && (mem.evar.isEmpty || mem.evar.get == addr)
  }
  private def getReserveCommand(mem: LockArg, cs: Iterable[Command]): Option[IReserveLock] = {
    cs.find {
      case IReserveLock(_,l) if l == mem => true
      case _ => false
    }.asInstanceOf[Option[IReserveLock]]
  }
  private def getReserveWrite(mem: LockArg, cs: Iterable[Command]): Option[IReserveLock] = {
    cs.find {
      case r@IReserveLock(_,l) if l == mem && r.memOpType.contains(LockWrite) => true
      case _ => false
    }.asInstanceOf[Option[IReserveLock]]
  }
  private def getReserveRead(mem: LockArg, cs: Iterable[Command]): Option[IReserveLock] = {
    cs.find {
      case r@IReserveLock(_,l) if l == mem && r.memOpType.contains(LockRead) => true
      case _ => false
    }.asInstanceOf[Option[IReserveLock]]
  }
  private def getReleaseCommand(mem: LockArg, cs: Iterable[Command]): Option[IReleaseLock] = {
    cs.find {
      case IReleaseLock(l,_) if l == mem => true
      case _ => false
    }.asInstanceOf[Option[IReleaseLock]]
  }
  private def getCheckOwnedCommand(mem: LockArg, cs: Iterable[Command]): Option[ICheckLockOwned] = {
    cs.find {
      case ICheckLockOwned(l,_) if mem == l => true
      case _ => false
    }.asInstanceOf[Option[ICheckLockOwned]]
  }
  private def hasMemoryWrite(mem: LockArg, cs: Iterable[Command]): Boolean = {
    cs.exists {
      case IMemWrite(mid, addr,_) if largMatches(mem, mid, addr) => true
      case IMemSend(_, isWrite, mid, _, addr) if isWrite  && largMatches(mem, mid, addr) => true
      case _ => false
    }
  }
  private def hasAsyncMemoryRead(mem: LockArg, cs: Iterable[Command]): Boolean = {
    cs.exists {
      case IMemSend(_, isWrite, mid,_, addr) if !isWrite && largMatches(mem, mid, addr)=> true
      case _ => false
    }
  }
  /**
   * This represents the lock implementation as a generic queue that
   * allows one reservation to be allocated at a time. This implements
   * no forwarding or anything; it simply queues up reservation requests.
   * It also does not require interposing on the memory requests and can therefore be
   * used for any memory type.
   */
  class LockQueue extends LockInterface {
    override def mergeLockOps(mem: LockArg, lops: Iterable[Command]): Iterable[Command] = {
      //res + rel -> checkfree
      //res + checkowned -> res + checkfree
      //else same
      val rescmd = getReserveCommand(mem, lops)
      val relcmd = getReleaseCommand(mem, lops)
      val checkownedCmd = getCheckOwnedCommand(mem, lops)
      if (rescmd.isDefined && relcmd.isDefined) {
        List(ICheckLockFree(mem))
      } else if (rescmd.isDefined && checkownedCmd.isDefined) {
        List(rescmd.get, ICheckLockFree(mem))
      } else {
        lops
      }
    }
    //no possible conflicts since all ops are mergeable if conflicting
    override def checkConflicts(mem: LockArg, lops: Iterable[Command]): Boolean = false
  }

  /**
   * This represents a renaming register file.
   * Reserve statements either translate to _reading a name_ (R) or _allocating a new name_ (W).
   *
   */
  class RenameRegfile extends LockInterface {
    override def mergeLockOps(mem: LockArg, lops: Iterable[Command]): Iterable[Command] = {
      //This modules doesn't support any "merging")
      lops
    }
    override def checkConflicts(mem: LockArg, lops: Iterable[Command]): Boolean = {
      //base ops: reserve, checkowned, release
      //conflicts: reserve(W) + write(W), or reserve(W) + release(W) ,or write(W) + release(W)
      //TODO does checkowned(R/W) conflict w/ anyting? I don't think so
      val rescmd = getReserveWrite(mem, lops)
      val relcmd = getReleaseCommand(mem, lops)
      val memWrite = hasMemoryWrite(mem, lops)
      //conflicts
      if (rescmd.isDefined && relcmd.isDefined ||
          rescmd.isDefined && memWrite ||
          memWrite && relcmd.isDefined) {
        false
        //no conflicts
      } else {
        true
      }
    }
  }

  class LoadStoreQueue extends LockInterface {
    override def mergeLockOps(mem: LockArg, lops: Iterable[Command]): Iterable[Command] = {
      //don't need to do any merging! (no optimizations for releasing in the same cycle as acquiring)
      lops
    }
    override def checkConflicts(mem: LockArg, lops: Iterable[Command]): Boolean = {
      //base ops: reserve, checkowned, release
      //conflicts are the same as renaming register file:
      //res(W) + write, res(W) + rel(W), write + rel(W)
      //but also has conflicts;
      //res(R) + read, res(R/W) + checkowned
      val reswrite = getReserveWrite(mem, lops)
      val resRead = getReserveRead(mem, lops)
      val relcmd = getReleaseCommand(mem, lops)
      val memWrite = hasMemoryWrite(mem, lops)
      val asyncMemRead = hasAsyncMemoryRead(mem, lops)
      val check = getCheckOwnedCommand(mem, lops)
      //conflicts
      if ((reswrite.isDefined && relcmd.isDefined) ||
        (reswrite.isDefined && memWrite) ||
        (memWrite && relcmd.isDefined) ||
        (resRead.isDefined && asyncMemRead) ||
        ((resRead.isDefined || reswrite.isDefined) && check.isDefined)) {
        false
      } else {
        //no conflicts
        true
      }
    }
  }
  /*
   *  Locks:
   *    reserve(R)
   *    reserve(W)
   *    block(lock)
   *    read(R_lock)
   *    write(W_lock)
   *    release(lock)
   *
   *  General Memories (no address):
   * 
   *   isEmpty(); (lock free)
   *   reserve();
   *   owns(id);
   *   write(id);
   *   read(id);
   *   release(id);
   *
   *  Combinational Memories:
   * 
   *   readName(addr); (R)
   *   allocName(addr); (W)
   *   isValid(name); (R/W)
   *   read(name); (R)
   *   write(name); (W)
   *   commit(name); (R/W)
   *   
   *  Asynchronous Memories:
   *
   *   reserveRead(addr); (R)
   *   reserveWrite(addr); (RW)
   *   isValid(name); (R/W)
   *   read(name); (R)
   *   write(name); (W)
   *   commitRead(name); (R)
   *   commitWrite(name); (W)
   */

 ////////////////////////////////////////

  /*
   *  In general, we need to be able to translate between
   *  lock operations and the low-level implementation interfaces.
   * 
   * Functionality we need for this:
   * 
   *  (1) checkConflicts
   * 
   *  Given a set of lock operations which are scheduled for
   *  a given stage, return True is schedulable, else False.
   *  e.g., A renaming registerfile can 'reserve(R), block, read, and free'
   *  in a single-cycle. 
   *  On the other hand, they cannot support same-cycle 'reserve(W)', and 'free'.
   * 
   *  --- Ideally checkConflicts is implemented automatically given some kind of schedule.
   *      Since there is a strict ordering that each thread will use to call these methods,
   *      we can specify it simply as a list of sets. Each set represents non-conflicting operations.
   *
   *  (2) mergeOps
   *
   *  Given a set of lock operations scheduled for a given stage, return the
   *  merged set of operations. It is convenient to specify this separately from
   *  conflicts since it can be assumed that anything "not merged" is translated
   *  obviously.
   *  
   *  E.g., the General Lock Memory (w/o addresses) merges "reserve, read + free"
   *  into "isEmpty, read"
   * 
   *  (3) assignPorts
   *  
   *  Given a set of _translated_ operations, assign port numbers to each one
   *  or return ERROR if the implementation doesn't support the necessary number of ports.
   * 
   *  E.g., the Renaming Register file supports 2 readNames()
   *  per cycle. If there are 2 readNames() w/ different arguments, they will be assigned
   *  different ports; if there are >2, then an error is returned.
   * 
   */



}
