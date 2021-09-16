package pipedsl.common

import pipedsl.common.Errors.{MissingType, UnexpectedLockImpl}
import pipedsl.common.Locks.{General, LockGranularity, Specific}
import pipedsl.common.Syntax.Annotations.PortAnnotation
import pipedsl.common.Syntax.Latency.{Combinational, Latency, Sequential}
import pipedsl.common.Syntax._


object LockImplementation {

  sealed case class MethodInfo(name: String, doesModify: Boolean, usesArgs: List[Expr])

  private val lqueue = new LockQueue()
  private val falqueue = new FALockQueue()
  private val bypassQueue = new BypassQueue()
  private val bypassRF = new BypassRF()
  private val rename = new RenameRegfile()
  private val forwardRename = new ForwardingRegfile()
  private val lsq = new LoadStoreQueue()

  //Stand-in Type Variables for Address, Data and Lock Handle
  private val addrType = TNamedType(Id("addr"))
  private val dataType = TNamedType(Id("data"))
  private val handleType = TNamedType(Id("handle"))
  //Lock Object Method Names
  private val canResName = "canRes"
  private val canResReadName = canResName + "_r"
  private val canResWriteName = canResName + "_w"

  private val resName = "res"
  private val resReadName = resName + "_r"
  private val resWriteName = resName + "_w"

  private val blockName = "owns"
  private val blockReadName = blockName + "_r"
  private val blockWriteName = blockName + "_w"

  private val accessName = "req" //name used for synchronous (non-sequential/combinational) memories
  private val readName = "read"
  private val writeName = "write"

  private val releaseName = "rel"
  private val releaseReadName = releaseName + "_r"
  private val releaseWriteName = releaseName + "_w"

  private val canAtomicName = "canAtom"
  private val atomicReadName = "atom_r"
  private val atomicWriteName = "atom_w"

  private def toPortString(port: Option[Int]): String = port match {
    case Some(value) => value.toString
    case None => ""
  }

  //TODO meta program all this stuff so it takes up so much less space
  ////Methods for typechecking info
  private def getCanReserveName(l:  LockInterface, op: Option[LockType]): Id = l.granularity match {
    case Locks.Specific =>
      Id(op match {
        case Some(Syntax.LockRead) => canResReadName
        case Some(Syntax.LockWrite) => canResWriteName
        case None => canResName
      })
    case Locks.General => Id(canResName)
  }

  def getCanReserve(l: LockInterface, op: Option[LockType]): Option[(TFun, Latency)] = {
    l.getType.methods.get(getCanReserveName(l, op))
  }

  private def getReserveName(l:  LockInterface, op: Option[LockType]): Id = l.granularity match {
    case Locks.Specific =>
      Id(op match {
        case Some(Syntax.LockRead) => resReadName
        case Some(Syntax.LockWrite) => resWriteName
        case None => resName
      })
    case Locks.General => Id(resName)
  }

  def getReserve(l: LockInterface, op: Option[LockType]): Option[(TFun, Latency)] = {
    l.getType.methods.get(getReserveName(l, op))
  }

  private def getBlockName(l:  LockInterface, op: Option[LockType]): Id = l.granularity match {
    case Locks.Specific =>
      Id(op match {
        case Some(Syntax.LockRead) => blockReadName
        case Some(Syntax.LockWrite) => blockWriteName
        case None => blockName
      })
    case Locks.General => Id(blockName)
  }

  def getBlock(l: LockInterface, op: Option[LockType]): Option[(TFun, Latency)] = {
    l.getType.methods.get(getBlockName(l, op))
  }

  private def getAccessName(l:  LockInterface, op: Option[LockType]): Id = l.granularity match {
    case Locks.Specific =>
      Id(op match {
        case Some(Syntax.LockRead) => readName
        case Some(Syntax.LockWrite) => writeName
        case None => accessName
      })
    case Locks.General => Id(accessName)
  }

  def getAccess(l: LockInterface, op: Option[LockType]): Option[(TFun, Latency)] = {
    l.getType.methods.get(getAccessName(l, op))
  }

  private def getReleaseName(l:  LockInterface, op: Option[LockType]): Id = l.granularity match {
    case Locks.Specific =>
      Id(op match {
        case Some(Syntax.LockRead) => releaseReadName
        case Some(Syntax.LockWrite) => releaseWriteName
        case None => releaseName
      })
    case Locks.General => Id(releaseName)
  }

  def getRelease(l: LockInterface, op: Option[LockType]): Option[(TFun, Latency)] = {
    l.getType.methods.get(getReleaseName(l, op))
  }

  //-------Methods for translation info--------------------------\\
  def getCanReserveInfo(l: IReserveLock): Option[MethodInfo] = {
    val interface = getLockImpl(l.mem)
    getCanReserve(interface, l.memOpType) match {
      case Some((funTyp, latency)) =>
        val args = getArgs(funTyp, l.mem.evar)
        val methodName = getCanReserveName(interface, l.memOpType).v + toPortString(l.portNum)
        Some(MethodInfo(methodName, latency != Combinational, args))
      case None => None
    }
  }

  def getReserveInfo(l: IReserveLock): Option[MethodInfo] = {
    val interface = getLockImpl(l.mem)
    getReserve(interface, l.memOpType) match {
      case Some((funTyp, latency)) =>
        val args = getArgs(funTyp, l.mem.evar)
        val methodName = getReserveName(interface, l.memOpType).v + toPortString(l.portNum)
        Some(MethodInfo(methodName, latency != Combinational, args))
      case None => None
    }
  }

  def getBlockInfo(l: ICheckLockOwned): Option[MethodInfo] = {
    val interface = getLockImpl(l.mem)
    getBlock(interface, l.memOpType) match {
      case Some((funTyp, latency)) =>
        val args = getArgs(funTyp, l.mem.evar, Some(l.inHandle))
        val methodName = getBlockName(interface, l.memOpType).v + toPortString(l.portNum)
        Some(MethodInfo(methodName, latency != Combinational, args))
      case None => None
    }
  }

  def getReadInfo(mem: Id, addr: Expr, inHandle: Option[Expr], portNum: Option[Int]): Option[MethodInfo] = {
    val interface = getLockImplFromMemTyp(mem)
    getAccess(interface, Some(LockRead)) match {
      case Some((funTyp, latency)) =>
        val args = getArgs(funTyp, Some(addr), inHandle)
        val methodName = getAccessName(interface, Some(LockRead)).v + toPortString(portNum)
        Some(MethodInfo(methodName, latency != Combinational, args))
      case None => None
    }
  }

  def getWriteInfo(mem: Id, addr: Expr, inHandle: Expr, data: Expr, portNum: Option[Int]): Option[MethodInfo] = {
      val interface = getLockImplFromMemTyp(mem)
      val (funTyp, latency) = getAccess(interface, Some(LockWrite)).get
      val args = getArgs(funTyp, Some(addr), Some(inHandle), Some(data))
      val methodName = getAccessName(interface, Some(LockWrite)).v + toPortString(portNum)
      Some(MethodInfo(methodName, latency != Combinational, args))
  }

  def getRequestInfo(mem: Id, addr: Expr, inHandle: Expr,
                     data: Option[Expr], portNum: Option[Int]): Option[MethodInfo] = {
    val interface = getLockImplFromMemTyp(mem)
    val (funTyp, _) = getAccess(interface, None).get
    val args = getArgs(funTyp, Some(addr), Some(inHandle), data)
    val methodName = getAccessName(interface, None).v + toPortString(portNum)
    Some(MethodInfo(methodName, doesModify = true, args))
  }

  def getReleaseInfo(l: IReleaseLock): Option[MethodInfo] = {
    val interface = getLockImpl(l.mem)
    getRelease(interface, l.memOpType) match {
      case Some((funTyp, latency)) =>
        val args = getArgs(funTyp, l.mem.evar , Some(l.inHandle))
        val methodName = getReleaseName(interface, l.memOpType).v + toPortString(l.portNum)
        Some(MethodInfo(methodName, latency != Combinational, args))
      case None => None
    }
  }

  private def extractHandle(h: Expr): Expr = {
    val e = EFromMaybe(h).setPos(h.pos)
    e.typ = h.typ.get.matchOrError(h.pos, "Lock Handle", "Maybe type") {
      case TMaybe(t) => Some(t)
    }
    e
  }
  private def getArgs(fun: TFun, addr: Option[Expr] = None,
              handle: Option[Expr] = None, data: Option[Expr] = None): List[Expr] = {
    fun.args.foldLeft(List[Expr]())((l, argTyp) => {
      argTyp match {
          //TODO throw better exception if missing arg
        case t: TNamedType if t == dataType => l :+ data.get
        case t: TNamedType if t == addrType => l :+ addr.get
        case t: TNamedType if t == handleType => l :+ extractHandle(handle.get)
        case _ => l //should be unreachable TODO throw badly formatted type
      }
    })
  }
  //--------------------END TRANSLATION HELPERS------------------------------\\

  /**
   * This is used when the lock implementation for a memory is left unspecified:
   * therefore it must be compatible with any kind of memory.
   * @return The default lock implementation which can be used with any memory.
   */
  def getDefaultLockImpl: LockInterface = lqueue

  private val implMap: Map[String, LockInterface] = Map(
    lqueue.shortName -> lqueue,
    falqueue.shortName -> falqueue,
    bypassQueue.shortName -> bypassQueue,
    bypassRF.shortName -> bypassRF,
    rename.shortName -> rename,
    forwardRename.shortName -> forwardRename,
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

  private def getLockImplFromMemTyp(mem: Id): LockInterface = {
    mem.typ match
    {
      case Some(mtyp) => mtyp.matchOrError(mem.pos, "Memory Access", "Memory")
      {
        case TLockedMemType(_, _, limpl) => limpl
        case _ :TModType => getDefaultLockImpl
      }
      case None => throw MissingType(mem.pos, mem.v)
    }
  }

  def getLockImpl(m :EMemAccess): LockInterface = getLockImplFromMemTyp(m.mem)

  //---------BEGIN LOCK DEFINITIONS-----------\\

  sealed trait LockInterface {

    //Convenience method to generate module names
    protected val combSuffix = "CombMem"
    protected val asyncSuffix = "AsyncMem"
    protected def getSuffix(m: TMemType): String = {
      if (m.readLatency == Combinational) {
        combSuffix
      } else {
        if (Math.max(m.readPorts, m.writePorts) < 2) asyncSuffix
        else asyncSuffix + "2"
      }
    }

    /**
     * The type that describes the functionality exposed by the
     * lock object. Locks have a TObject type, which exposes a number of methods.
     * These method types are used primarily by code generation to correctly generate
     * "calls" (i.e. interface attachments) to locks.
     *
     * These types also come with a latency which is used by the TimingTypeChecker to
     * ensure that they are called in a way that is synthesizable.
     * @return
     */
    def getType: TObject

    /**
     * Returns the lock granularity supported by this implementation
     * (which indicates whether or not
     * addreses are specified in its operations).
     * @return Specific if this lock requires addresses in its operations else General
     */
    def granularity: LockGranularity

    def addReadPort: Boolean = false

    def addWritePort: Boolean = false

    def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo]

    def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo]

    def getReserveInfo(l: IReserveLock): Option[MethodInfo]

    def getCanReserveInfo(l: IReserveLock): Option[MethodInfo]

    def getReleaseInfo(l: IReleaseLock): Option[MethodInfo]

    def getModInstArgs(m: TMemType, szParams: List[Int]): List[Int]

    //LSQ doesn't need a separate lock id so use this to differentiate
    def useUniqueLockId(): Boolean = true

    def getTypeArgs(szParams: List[Int]): List[Int] = List()

    def shortName: String

    def getModuleName(m: TMemType): String

    def getModuleInstName(m: TMemType): String =  "mk" + getModuleName(m)

    def getClientName: String = ".mem.bram_client"

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

    private val queueLockName = Id("QueueLock")
    override def getType: TObject = TObject(queueLockName, List(),
      Map(
        Id(resName)    -> (TFun(List(), handleType), Sequential),
        Id(blockName)    -> (TFun(List(handleType), TBool()), Combinational),
        Id(accessName) -> (TFun(List(addrType), TVoid()), Combinational),
        Id(readName)  -> (TFun(List(addrType), dataType), Combinational),
        Id(writeName) -> (TFun(List(addrType, dataType), TVoid()), Combinational),
        Id(releaseName)    -> (TFun(List(addrType), TVoid()), Sequential),
        Id(canAtomicName) -> (TFun(List(), TBool()), Combinational),
        Id(atomicReadName)   -> (TFun(List(addrType), dataType), Combinational),
        Id(atomicWriteName)  -> (TFun(List(addrType, dataType), TVoid()), Sequential)))

    override def shortName: String = "Queue"

    override def getModuleName(m: TMemType): String = queueLockName.v + getSuffix(m)

    override def granularity: LockGranularity = General
    //TODO placing the interface name (lock.) here is weird but OK i guess
    override def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo] = {
      Some(MethodInfo("lock.isEmpty", doesModify = false, List()))
    }

    override def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo] = {
      Some(MethodInfo("lock.owns", doesModify = false, List(extractHandle(l.inHandle))))
    }

    override def getReserveInfo(l: IReserveLock): Option[MethodInfo] = {
      Some(MethodInfo("lock.res", doesModify = true, List()))
    }

    override def getCanReserveInfo(l: IReserveLock): Option[MethodInfo] = None

    override def getReleaseInfo(l: IReleaseLock): Option[MethodInfo] = {
      Some(MethodInfo("lock.rel", doesModify = true, List(extractHandle(l.inHandle))))
    }

    override def getModInstArgs(m: TMemType, szParams: List[Int]): List[Int] = List()

  }

  //This is a different implementation which uses the address in some parameters
  //since it allows locking distinct addresses at once
  private class FALockQueue extends LockQueue {
    private val lockName = Id("FAQueue")
    private val queueType = TNamedType(lockName)
    override def getType: TObject =
      TObject(lockName, List(), Map(
        Id(resReadName)    -> (TFun(List(addrType), handleType), Sequential),
        Id(resWriteName)    -> (TFun(List(addrType), handleType), Sequential),
        Id(blockReadName)    -> (TFun(List(handleType, addrType), TBool()), Combinational),
        Id(blockWriteName)    ->  (TFun(List(handleType, addrType), TBool()), Combinational),
        Id(accessName) -> (TFun(List(addrType), TVoid()), Combinational),
        Id(readName)  -> (TFun(List(addrType), dataType), Combinational),
        Id(writeName) -> (TFun(List(addrType, dataType), TVoid()), Combinational),
        Id(releaseReadName)    -> (TFun(List(handleType, addrType), TVoid()), Sequential),
        Id(releaseWriteName)    -> (TFun(List(handleType, addrType), TVoid()), Sequential),
        Id(canAtomicName)    -> (TFun(List(addrType), TBool()), Combinational),
        Id(atomicReadName)   -> (TFun(List(addrType), dataType), Combinational),
        Id(atomicWriteName)   -> (TFun(List(addrType, dataType), TVoid()), Combinational)))


    private val defaultNumLocks = 4

    override def shortName: String = "FAQueue"
    override def getModuleName(m: TMemType): String = "AddrLock" + getSuffix(m)
    override def getModuleInstName(m: TMemType): String =  "mkFA" + getModuleName(m)

    override def granularity: LockGranularity = Specific

    override def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo] = {
      Some(MethodInfo("lock.isEmpty", doesModify = false, List(l.mem.evar.get)))
    }

    override def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo] = {
      Some(MethodInfo("lock.owns", doesModify = false, List(extractHandle(l.inHandle), l.mem.evar.get)))
    }

    override def getCanReserveInfo(l: IReserveLock): Option[MethodInfo] = {
      Some(MethodInfo("lock.canRes", doesModify = false, List(l.mem.evar.get)))
    }

    override def getReserveInfo(l: IReserveLock): Option[MethodInfo] = {
      Some(MethodInfo("lock.res", doesModify = true, List(l.mem.evar.get)))
    }

    override def getReleaseInfo(l: IReleaseLock): Option[MethodInfo] = {
      Some(MethodInfo("lock.rel", doesModify = true, List(extractHandle(l.inHandle), l.mem.evar.get)))
    }

    override def getTypeArgs(szParams: List[Int]): List[Int] = List(szParams.headOption.getOrElse(defaultNumLocks))
  }

  /**
   * This implementation _only_ supports atomic reads (i.e., no reserving read locks)
   * and does not support atomic writes.
   */
  private class BypassQueue extends LockInterface {

    override def getType: TObject = TObject(Id("BypassQueue"), List(), Map(
      Id(canResName) -> (TFun(List(addrType), TBool()), Combinational),
      Id(resWriteName) -> (TFun(List(addrType), handleType), Sequential),
      Id(blockWriteName) -> (TFun(List(handleType), TBool()), Combinational),
      Id(writeName) -> (TFun(List(handleType, dataType), TVoid()), Combinational),
      Id(releaseWriteName) -> (TFun(List(handleType), TVoid()), Sequential),
      Id(canAtomicName) -> (TFun(List(addrType), TBool()), Combinational),
      Id(atomicReadName) -> (TFun(List(addrType), dataType), Combinational)
    ))

    private val defaultNumLocks = 4

    override def granularity: LockGranularity = Specific

    override def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo] = l.memOpType match {
      case Some(LockWrite) => Some(MethodInfo("canWrite", doesModify = false, List(l.mem.evar.get)))
      case Some(LockRead)  => Some(MethodInfo("canRead", doesModify = false, List(l.mem.evar.get)))
      case None => throw new RuntimeException("Bad lock info")//TODO better exception
    }

    override def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo] = {
      Some(MethodInfo("owns", doesModify = false, List(extractHandle(l.inHandle))))
    }

    override def getReserveInfo(l: IReserveLock): Option[MethodInfo] = {
      Some(MethodInfo("reserve", doesModify = true, List(l.mem.evar.get)))
    }

    override def getCanReserveInfo(l: IReserveLock): Option[MethodInfo] = {
      Some(MethodInfo("canRes", doesModify = false, List(l.mem.evar.get)))
    }

    override def getReleaseInfo(l: IReleaseLock): Option[MethodInfo] = {
      Some(MethodInfo("commit", doesModify = true, List(extractHandle(l.inHandle))))
    }

    override def getTypeArgs(szParams: List[Int]): List[Int] =
      List(szParams.headOption.getOrElse(defaultNumLocks))
    override def getModInstArgs(m: TMemType, szParams: List[Int]): List[Int] = List()

    override def shortName: String = "BypassQueue"

    override def getModuleName(m: TMemType): String = "BypassLockCombMem"
}

  /**
   * This implementation is a bypassing register file with separate cycle reserves and reads
   * (unlike the Bypass Queue which requires them to be concurrent)
   */
  private class BypassRF extends LockInterface {
    private val lockName = Id("BypassRF")
    override def getType: TObject = TObject(lockName, List(),
      Map(
        Id(resReadName)    -> (TFun(List(addrType), handleType), Sequential),
        Id(resWriteName)    -> (TFun(List(addrType), handleType), Sequential),
        Id(blockReadName)    -> (TFun(List(), TBool()), Combinational),
        Id(readName)  -> (TFun(List(handleType), dataType), Combinational),
        Id(writeName) -> (TFun(List(handleType, dataType), TVoid()), Sequential),
        Id(releaseReadName)    -> (TFun(List(), TVoid()), Sequential),
        Id(releaseWriteName)   -> (TFun(List(handleType), TVoid()), Sequential)))


    override def granularity: LockGranularity = Specific

    private def getPortString(l: PortAnnotation): String = if (l.portNum.isDefined) l.portNum.get.toString else ""

    override def addReadPort: Boolean = true

    override def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo] = None

    override def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo] = l.memOpType match {
      case Some(LockRead) => Some(MethodInfo("owns" + getPortString(l), doesModify = false, List()))
      case Some(LockWrite) => None
      case None => None //TODO throw error
    }

    override def getReserveInfo(l: IReserveLock): Option[MethodInfo] = l.memOpType match {
      case Some(LockRead) =>
        Some(MethodInfo("reserveRead" + getPortString(l), doesModify = true, List(l.mem.evar.get)))
      case Some(LockWrite) =>  Some(MethodInfo("reserveWrite", doesModify = true, List(l.mem.evar.get)))
      case None => None //TODO throw error
    }

    override def getCanReserveInfo(l: IReserveLock): Option[MethodInfo] = None

    override def getReleaseInfo(l: IReleaseLock): Option[MethodInfo] = l.memOpType match {
      case Some(LockRead) =>  Some(MethodInfo("freeRead" + getPortString(l), doesModify = true, List()))
      case Some(LockWrite) =>  Some(MethodInfo("freeWrite", doesModify = true, List(extractHandle(l.inHandle))))
      case None => None //TODO throw error
    }

    override def getTypeArgs(szParams: List[Int]): List[Int] = List()
    override def getModInstArgs(m: TMemType, szParams: List[Int]): List[Int] =
      List(Utilities.exp2(m.addrSize))

    override def shortName: String = "BypassRF"

    override def getModuleName(m: TMemType): String = "BypassRF"
  }

  /**
   * This represents a renaming register file.
   * Reserve statements either translate to _reading a name_ (R) or _allocating a new name_ (W).
   *
   */
  private class RenameRegfile extends LockInterface {

    override def getType: TObject = TObject(Id("RenameRF"), List(), Map(
      Id(resReadName) -> (TFun(List(addrType), handleType), Combinational),
      Id(resWriteName) -> (TFun(List(addrType), handleType), Sequential),
      Id(blockReadName) -> (TFun(List(handleType), TBool()), Combinational),
      Id(readName) -> (TFun(List(handleType), dataType), Combinational),
      Id(writeName) -> (TFun(List(handleType, dataType), TVoid()), Combinational),
      Id(releaseWriteName) -> (TFun(List(handleType), TVoid()), Sequential)
    ))

    override def shortName: String = "RenameRF"

    override def getModuleName(m: TMemType): String = "RenameRF"

    override def granularity:LockGranularity = Specific

    override def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo] = None

    override def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo] = l.memOpType match {
      case Some(LockRead) => Some(MethodInfo("isValid", doesModify = false, List(extractHandle(l.inHandle))))
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
      case Some(LockWrite) => Some(MethodInfo("commit", doesModify = true, List(extractHandle(l.inHandle))))
      case None => None //TODO should be an exception
    }

    def getModInstArgs(m: TMemType, szParams: List[Int]): List[Int] = {
      //TODO make default more configurable
      val aregs = Utilities.exp2(m.addrSize)
      val pregs = if (szParams.isEmpty) aregs * 2 else szParams.head
      List(Utilities.exp2(m.addrSize), pregs)
    }
  }

  private class ForwardingRegfile extends RenameRegfile {
    override def shortName: String = "ForwardRenameRF"
    override def getModuleInstName(m: TMemType): String =  "mk" + shortName
  }

  /**
   * This represents a front to asynchronously responding memories,
   * which take multiple cycles to service reads + writes.
   * It includes a LoadStoreQueue which forwards data to avoid extra memory accesses.
   */
  private class LoadStoreQueue extends LockInterface {

    override def getType: TObject = TObject(Id("LSQ"), List(), Map(
      Id(resReadName) -> (TFun(List(addrType), handleType), Sequential),
      Id(resWriteName) -> (TFun(List(addrType), handleType), Sequential),
      Id(blockReadName) -> (TFun(List(handleType), TBool()), Combinational),
      Id(accessName) -> (TFun(List(handleType), TVoid()), Combinational),
      Id(releaseReadName) -> (TFun(List(handleType), TVoid()), Sequential),
      Id(releaseWriteName) -> (TFun(List(handleType), TVoid()), Sequential)
    ))

    override def shortName: String = "LSQ"
    override def getModuleName(m: TMemType): String = "LSQ"

    override def toString: String = "LSQ"

    override def granularity: LockGranularity = Specific

    override def getCheckEmptyInfo(l: ICheckLockFree): Option[MethodInfo] = None

    override def getCheckOwnsInfo(l: ICheckLockOwned): Option[MethodInfo] = l.memOpType match {
      case Some(LockRead) => Some(MethodInfo("isValid", doesModify = false, List(extractHandle(l.inHandle))))
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
      case Some(LockRead) => Some(MethodInfo("commitRead", doesModify = true, List(extractHandle(l.inHandle))))
      case Some(LockWrite) => Some(MethodInfo("commitWrite", doesModify = true, List(extractHandle(l.inHandle))))
      case None => None //TODO should be an exception
    }

    def getModInstArgs(m: TMemType, szParams: List[Int]): List[Int] = List()

    override def useUniqueLockId(): Boolean = false

    override def getClientName: String = ".bram_client"
  }
}
