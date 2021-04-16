package pipedsl.common

import pipedsl.common.Errors.{MissingType, UnexpectedLockImpl}
import pipedsl.common.Locks.{General, LockGranularity, Specific}
import pipedsl.common.Syntax.Latency.Combinational
import pipedsl.common.Syntax._


object LockImplementation {

  sealed case class MethodInfo(name: String, doesModify: Boolean, usesArgs: List[Expr])

  private val lqueue = new LockQueue()
  private val falqueue = new FALockQueue()
  private val rename = new RenameRegfile()
  private val lsq = new LoadStoreQueue()

  /**
   * This is used when the lock implementation for a memory is left unspecified:
   * therefore it must be compatible with any kind of memory.
   * @return The default lock implementation which can be used with any memory.
   */
  def getDefaultLockImpl: LockInterface = lqueue

  private val implMap: Map[String, LockInterface] = Map(
    lqueue.shortName -> lqueue,
    falqueue.shortName -> falqueue,
    rename.shortName -> rename,
    lsq.shortName -> lsq
  )
  /**
   * Lookup the lock implementation based on its name, only the string
   * value of the Id is used for lookup.
   * @param n The name of the lock type to lookup
   * @return The implementation for that lock type
   */
  def getLockImpl(n: Id): LockInterface = {
    if (implMap.contains(n.v)) {
      implMap(n.v)
    } else {
      throw UnexpectedLockImpl(n)
    }
  }

  /**
   * Lookup the lock implementation based on the type
   * of the memory referenced in the LockArg.
   * @param l The LockArg that is used to lock a memory
   * @return The lock implementation associated with the memory
   */
  def getLockImpl(l: LockArg): LockInterface = {
    val mem = l.id
    if (mem.typ.isEmpty) {
      throw MissingType(mem.pos, mem.v)
    } else {
      val mtyp = mem.typ.get
      mtyp.matchOrError(mem.pos, "Lock Argument", "Memory") {
        case TLockedMemType(_,_,limpl)=> limpl
          //TODO remove this and make memories + modules the same
        case _:TModType => getDefaultLockImpl //modules only use the default lock impl for now
      }
    }
  }

  sealed trait LockInterface {

    //Convenience method to generate module names
    protected val combSuffix = "CombMem"
    protected val asyncSuffix = "AsyncMem"
    protected def getSuffix(m: TMemType): String = if (m.readLatency == Combinational) combSuffix else asyncSuffix

    /**
     * Determines whether or not this lock type can support the given list of
     * commands in a given stage
     * @param mem The memory (or memory location) to consider for conflicts
     * @param lops The set of operations to consider for conflicts
     * @return true if no conflicts, false if conflicts
     */
    def checkConflicts(mem: LockArg, lops: Iterable[Command]): Boolean

    /**
     * Merges the given set of commands which are meant to occur in
     * a single stage. This is only intended to accept LOCK modifying commands
     * (nothing else). If other commands are included in the list, they may not be returned in the result.
     * @param mem The memory (or memory location) whose ops we are merging
     * @param lops The operations to merge
     * @return The list of merged operations
     */
    def mergeLockOps(mem: LockArg, lops: Iterable[Command]): Iterable[Command]

    /**
     * This returns true iff this lock implementation
     * is compatible with the given memory type.
     * @param mtyp The memory type
     * @return true iff this lock can be backed by a memory of the given type
     */
    def isCompatible(mtyp: TMemType): Boolean

    /**
     * Given a list of commands, return the
     * same list with the port information annotated.
     * Each type of operation supported by this lock implementation,
     * that is in this list will be assigned a different port number.
     * If this implementation doesn't support that many ports, it will throw an exception.
     * @param mem The memory/lock we're assigning ports for.
     * @param lops The operations to annotate
     * @return The annoated list of operations, iteration order should be preserved
     *         and non-lock commands should be left unannotated.
     */
    def assignPorts(mem: LockArg, lops: Iterable[Command]): Iterable[Command]

    /**
     * Returns the lock granularity supported by this implementation
     * (which indicates whether or not
     * addreses are specified in its operations).
     * @return Specific if this lock requires addresses in its operations else General
     */
    def granularity: LockGranularity

    /**
     * Each lock implementation may require any non-empty subset
     * of the address and the lock handle to serve read and write
     * requests.
     * @param addr The expression representing the request address
     * @param lock The expression representing the lock held at this point.
     * @return The expression argument needed to complete a read
     */
    def getReadArgs(addr: Expr, lock: Expr): Expr

    /**
     * Each lock implementation may require any non-empty subset
     * of the address and the lock handle to serve read and write
     * requests.
     * @param addr The expression representing the request address
     * @param lock The expression representing the lock held at this point.
     * @return The expression arguments needed to complete a write
     *         (excluding the data).
     */
    def getWriteArgs(addr: Expr, lock: Expr): Expr

    def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo]

    def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo]

    def getReserveInfo(l: IReserveLock): Option[MethodInfo]

    def getCanReserveInfo(l: IReserveLock): Option[MethodInfo]

    def getReleaseInfo(l: IReleaseLock): Option[MethodInfo]

    def getModInstArgs(m: TMemType, szParams: List[Int]): List[Int]

    def getTypeArgs(szParams: List[Int]): List[Int] = List()

    def shortName: String

    def getModuleName(m: TMemType): String

    def getModuleInstName(m: TMemType): String =  "mk" + getModuleName(m)

    //TODO put this somewhere like Syntax
    protected def extractHandle(h: EVar): Expr = {
      val e = EFromMaybe(h).setPos(h.pos)
      e.typ = h.typ.get.matchOrError(h.pos, "Lock Handle", "Maybe type") {
        case TMaybe(t) => Some(t)
      }
      e
    }
  }

  /**
   * This represents the lock implementation as a generic queue that
   * allows one reservation to be allocated at a time. This implements
   * no forwarding or anything; it simply queues up reservation requests.
   * It also does not require interposing on the memory requests and can therefore be
   * used for any memory type.
   */
  private class LockQueue extends LockInterface {

    override def shortName: String = "Queue"

    override def getModuleName(m: TMemType): String = "QueueLock" + getSuffix(m)

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
    override def isCompatible(mtyp: TMemType): Boolean = true

    /**
     * Given a list of commands, return the
     * same list with the port information annotated.
     * Each type of operation supported by this lock implementation,
     * that is in this list will be assigned a different port number.
     * If this implementation doesn't support that many ports, it will throw an exception.
     *
     * @param mem  The memory/lock we're assigning ports for.
     * @param lops The operations to annotate
     * @return The annoated list of operations, iteration order should be preserved
     *         and non-lock commands should be left unannotated.
     */
    override def assignPorts(mem: LockArg, lops: Iterable[Command]): Iterable[Command] = lops
    //TODO implement for real

    override def granularity: LockGranularity = General
    //TODO placing the interface name (lock.) here is weird but OK i guess
    override def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo] = {
      Some(MethodInfo("lock.isEmpty", doesModify = false, List()))
    }

    override def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo] = {
      Some(MethodInfo("lock.owns", doesModify = false, List()))
    }

    override def getReserveInfo(l: IReserveLock): Option[MethodInfo] = {
      Some(MethodInfo("lock.res", doesModify = true, List()))
    }

    override def getCanReserveInfo(l: IReserveLock): Option[MethodInfo] = None

    override def getReleaseInfo(l: IReleaseLock): Option[MethodInfo] = {
      Some(MethodInfo("lock.rel", doesModify = true, List()))
    }

    /**
     * Each lock implementation may require any non-empty subset
     * of the address and the lock handle to serve read and write
     * requests.
     *
     * @param addr The expression representing the request address
     * @param lock The expression representing the lock held at this point.
     * @return An iterable (e.g., List) of the expression arguments needed to complete a read
     */
    override def getReadArgs(addr: Expr, lock: Expr): Expr = addr

    /**
     * Each lock implementation may require any non-empty subset
     * of the address and the lock handle to serve read and write
     * requests.
     *
     * @param addr The expression representing the request address
     * @param lock The expression representing the lock held at this point.
     * @return An iterable (e.g., List) of the expression arguments needed to complete a write
     *         (excluding the data).
     */
    override def getWriteArgs(addr: Expr, lock: Expr): Expr = addr

    override def getModInstArgs(m: TMemType, szParams: List[Int]): List[Int] = List()

  }

  //This is a different implementation which uses the address in some parameters
  //since it allows locking distinct addresses at once
  private class FALockQueue extends LockQueue {

    private val defaultNumLocks = 4

    override def shortName: String = "FAQueue"
    override def getModuleName(m: TMemType): String = "AddrLock" + getSuffix(m)
    override def getModuleInstName(m: TMemType): String =  "mkFA" + getModuleName(m)

    override def granularity: LockGranularity = Specific

    override def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo] = {
      Some(MethodInfo("lock.isEmpty", doesModify = false, List(l.mem.evar.get)))
    }

    override def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo] = {
      Some(MethodInfo("lock.owns", doesModify = false, List(l.handle, l.mem.evar.get)))
    }

    override def getCanReserveInfo(l: IReserveLock): Option[MethodInfo] = {
      Some(MethodInfo("lock.canRes", doesModify = false, List(l.mem.evar.get)))
    }

    override def getReserveInfo(l: IReserveLock): Option[MethodInfo] = {
      Some(MethodInfo("lock.res", doesModify = true, List(l.mem.evar.get)))
    }

    override def getReleaseInfo(l: IReleaseLock): Option[MethodInfo] = {
      Some(MethodInfo("lock.rel", doesModify = true, List(l.handle, l.mem.evar.get)))
    }

    override def getTypeArgs(szParams: List[Int]): List[Int] = List(szParams.headOption.getOrElse(defaultNumLocks))
  }

  /**
   * This represents a renaming register file.
   * Reserve statements either translate to _reading a name_ (R) or _allocating a new name_ (W).
   *
   */
  private class RenameRegfile extends LockInterface {

    override def shortName: String = "RenameRF"

    override def getModuleName(m: TMemType): String = "RenameRF"

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
    override def isCompatible(mtyp: TMemType): Boolean = {
      //only compatible if we have the 'regfile' timing behaviors of "combinational read" and "sequential write"
      mtyp.readLatency == Latency.Combinational && mtyp.writeLatency == Latency.Sequential
    }

    /**
     * Given a list of commands, return the
     * same list with the port information annotated.
     * Each type of operation supported by this lock implementation,
     * that is in this list will be assigned a different port number.
     * If this implementation doesn't support that many ports, it will throw an exception.
     *
     * @param mem  The memory/lock we're assigning ports for.
     * @param lops The operations to annotate
     * @return The annoated list of operations, iteration order should be preserved
     *         and non-lock commands should be left unannotated.
     */
    override def assignPorts(mem: LockArg, lops: Iterable[Command]): Iterable[Command] = lops
    //TODO implement for real

    override def granularity:LockGranularity = Specific

    override def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo] = None

    override def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo] = l.memOpType match {
      case Some(LockRead) => Some(MethodInfo("isValid", doesModify = false, List(extractHandle(l.handle))))
      case Some(LockWrite) => None
      case None => None //TODO should be an exception
    }

    override def getReserveInfo(l: IReserveLock): Option[MethodInfo] = l.memOpType match {
      case Some(LockRead) => Some(MethodInfo("readName", doesModify = false, List(l.mem.evar.get)))
      case Some(LockWrite) => Some(MethodInfo("allocName", doesModify = true, List(l.mem.evar.get)))
      case None => None //TODO should be an exception
    }

    override def getCanReserveInfo(l: IReserveLock): Option[MethodInfo] = None

    override def getReleaseInfo(l: IReleaseLock): Option[MethodInfo] = l.memOpType match {
      case Some(LockRead) => None
      case Some(LockWrite) => Some(MethodInfo("commit", doesModify = true, List(extractHandle(l.handle))))
      case None => None //TODO should be an exception
    }

    /**
     * Each lock implementation may require any non-empty subset
     * of the address and the lock handle to serve read and write
     * requests.
     *
     * @param addr The expression representing the request address
     * @param lock The expression representing the lock held at this point.
     * @return An iterable (e.g., List) of the expression arguments needed to complete a read
     */
    override def getReadArgs(addr: Expr, lock: Expr): Expr = lock

    /**
     * Each lock implementation may require any non-empty subset
     * of the address and the lock handle to serve read and write
     * requests.
     *
     * @param addr The expression representing the request address
     * @param lock The expression representing the lock held at this point.
     * @return An iterable (e.g., List) of the expression arguments needed to complete a write
     *         (excluding the data).
     */
    override def getWriteArgs(addr: Expr, lock: Expr): Expr = lock

    def getModInstArgs(m: TMemType, szParams: List[Int]): List[Int] = {
      //TODO make default more configurable
      val aregs = Utilities.exp2(m.addrSize)
      val pregs = if (szParams.isEmpty) aregs * 2 else szParams.head
      List(Utilities.exp2(m.addrSize), pregs)
    }
}

  /**
   * This represents a front to asynchronously responding memories,
   * which take multiple cycles to service reads + writes.
   * It includes a LoadStoreQueue which forwards data to avoid extra memory accesses.
   */
  private class LoadStoreQueue extends LockInterface {

    override def shortName: String = "LSQ"
    override def getModuleName(m: TMemType): String = "LSQ"

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
    override def isCompatible(mtyp: TMemType): Boolean = {
      mtyp.readLatency == Latency.Asynchronous && mtyp.writeLatency == Latency.Asynchronous
    }
    override def toString: String = "LSQ"

    /**
     * Given a list of commands, return the
     * same list with the port information annotated.
     * Each type of operation supported by this lock implementation,
     * that is in this list will be assigned a different port number.
     * If this implementation doesn't support that many ports, it will throw an exception.
     *
     * @param mem  The memory/lock we're assigning ports for.
     * @param lops The operations to annotate
     * @return The annoated list of operations, iteration order should be preserved
     *         and non-lock commands should be left unannotated.
     */
    override def assignPorts(mem: LockArg, lops: Iterable[Command]): Iterable[Command] = lops
    //TODO implement for real

    override def granularity: LockGranularity = Specific

    override def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo] = None

    override def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo] = l.memOpType match {
      case Some(LockRead) => Some(MethodInfo("isValid", doesModify = false, List(extractHandle(l.handle))))
      case Some(LockWrite) => None
      case None => None //TODO should be an exception
    }

    override def getReserveInfo(l: IReserveLock): Option[MethodInfo] = l.memOpType match {
      case Some(LockRead) => Some(MethodInfo("reserveRead", doesModify = true, List(l.mem.evar.get)))
      case Some(LockWrite) => Some(MethodInfo("reserveWrite", doesModify = true, List(l.mem.evar.get)))
      case None => None //TODO should be an exception
    }

    override def getCanReserveInfo(l: IReserveLock): Option[MethodInfo] = None

    override def getReleaseInfo(l: IReleaseLock): Option[MethodInfo] = l.memOpType match {
      case Some(LockRead) => Some(MethodInfo("commitRead", doesModify = true, List(extractHandle(l.handle))))
      case Some(LockWrite) => Some(MethodInfo("commitWrite", doesModify = true, List(extractHandle(l.handle))))
      case None => None //TODO should be an exception
    }

    /**
     * Each lock implementation may require any non-empty subset
     * of the address and the lock handle to serve read and write
     * requests.
     *
     * @param addr The expression representing the request address
     * @param lock The expression representing the lock held at this point.
     * @return An iterable (e.g., List) of the expression arguments needed to complete a read
     */
    override def getReadArgs(addr: Expr, lock: Expr): Expr = lock

    /**
     * Each lock implementation may require any non-empty subset
     * of the address and the lock handle to serve read and write
     * requests.
     *
     * @param addr The expression representing the request address
     * @param lock The expression representing the lock held at this point.
     * @return An iterable (e.g., List) of the expression arguments needed to complete a write
     *         (excluding the data).
     */
    override def getWriteArgs(addr: Expr, lock: Expr): Expr = lock

    def getModInstArgs(m: TMemType, szParams: List[Int]): List[Int] = List()
}

  //The following are internal helper functions
  private def largMatches(mem: LockArg, mid: Id, addr: EVar): Boolean = {
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
