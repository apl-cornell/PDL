package Locks;

import FIFOF :: *;
import Ehr :: *;
import Vector :: *;
import ConfigReg :: *;
export LockId(..);
export QueueLock(..);
export CheckpointQueueLock(..);
export AddrLock(..);

export mkQueueLock;
export mkCountingLock;
export mkCheckpointQueueLock;
export mkFAAddrLock;
export mkDMAddrLock;

typedef UInt#(TLog#(n)) LockId#(numeric type n);

interface QueueLock#(type id);
   method ActionValue#(id) res1();
   method Bool owns1(id i);
   method Action rel1(id i);
   method Bool isEmpty();
   method Bool canRes1();
endinterface

interface CheckpointQueueLock#(type id, type cid);
   method ActionValue#(id) res1();
   method Bool owns1(id i);
   method Action rel1(id i);
   method Bool isEmpty();
   method Bool canRes1();
   method ActionValue#(cid) checkpoint();
   method Action rollback(cid id, Bool doRoll, Bool doRel);
endinterface

interface AddrLock#(type id, type addr, numeric type size);
   method Bool isEmpty(addr loc);
   method Bool canRes1(addr loc);
   method Bool owns1(id tid, addr loc);
   method Action rel1(id tid, addr loc);
   method ActionValue#(id) res1(addr loc);
endinterface

module mkQueueLock(QueueLock#(LockId#(d)));

   Reg#(LockId#(d)) nextId <- mkReg(0);
   FIFOF#(LockId#(d)) held <- mkSizedFIFOF(valueOf(d));
   
   Reg#(LockId#(d)) cnt <- mkReg(0);
   
   Bool lockFree = !held.notEmpty;
   LockId#(d) owner = held.first;

   method Bool isEmpty();
      return lockFree;
   endmethod
   
   method Bool canRes1();
      return held.notFull;
   endmethod
   
   //Returns True if thread `tid` already owns the lock
   method Bool owns1(LockId#(d) tid);
      return owner == tid;
   endmethod
	       
   //Releases the lock iff thread `tid` owns it already
   method Action rel1(LockId#(d) tid);
      if (owner == tid)
	 begin
	    held.deq();
	 end
   endmethod
   
   //Reserves the lock and returns the associated id
   method ActionValue#(LockId#(d)) res1();
      held.enq(nextId);
      nextId <= nextId + 1;
      cnt <= cnt + 1;
      return nextId;
   endmethod
      
endmodule

module mkCountingLock(QueueLock#(LockId#(d)));

   Ehr#(2, LockId#(d)) nextId <- mkEhr(0);   
   Reg#(LockId#(d)) owner <- mkReg(0);
   Reg#(Bool) empty <- mkReg(True);

   Bool full = !empty && nextId[0] == owner;

   RWire#(Bool) doRes <- mkRWire();
   RWire#(LockId#(d)) doRel <- mkRWire();
   
   (*fire_when_enabled*)
   rule updateEmpty;
      let res = fromMaybe(False, doRes.wget());
      //did reserve but not release, definitely not empty
      if (res &&& doRel.wget() matches tagged Invalid) empty <= False;
      //did release and no reserve, empty if new owner equals nextId (i.e., next person to call res)
      if (!res &&& doRel.wget() matches tagged Valid.nextOwner) empty <= nextOwner == nextId[1];
      //else must still be same (non-empty)
   endrule

   method Bool isEmpty();
      return empty;
   endmethod
   
   method Bool canRes1();
      return !full;
   endmethod
   
   //Returns True if thread `tid` already owns the lock
   method Bool owns1(LockId#(d) tid);
      return owner == tid;
   endmethod
	       
   //Releases the lock
   method Action rel1(LockId#(d) tid);
      owner <= owner + 1;
      doRel.wset(owner + 1);
   endmethod
   
   //Reserves the lock and returns the associated id
   method ActionValue#(LockId#(d)) res1();
      nextId[0] <= nextId[0] + 1;
      doRes.wset(True);
      return nextId[0];
   endmethod
      
endmodule

module mkCheckpointQueueLock(CheckpointQueueLock#(LockId#(d), LockId#(d)));

   Ehr#(2, LockId#(d)) nextId <- mkEhr(0);
   Reg#(LockId#(d)) owner <- mkReg(0);
   Reg#(Bool) empty <- mkReg(True);
   
   RWire#(Bool) doRes <- mkRWire();
   RWire#(LockId#(d)) doRel <- mkRWire();
   
   //for when you're doing not rollback
   (*fire_when_enabled*)
   rule updateEmpty;
      let res = fromMaybe(False, doRes.wget());
      if (res &&& doRel.wget() matches tagged Invalid) empty <= False;
      if (!res &&& doRel.wget() matches tagged Valid.nextOwner) empty <= nextOwner == nextId[1];
   endrule
   
   method Bool isEmpty();
      return empty;
   endmethod
   
   method Bool canRes1();
      return empty || nextId[0] != owner;
   endmethod
   
   //Returns True if thread `tid` already owns the lock
   method Bool owns1(LockId#(d) tid);
      return owner == tid;
   endmethod
	       
   //Releases the lock, assume `tid` owns it
   method Action rel1(LockId#(d) tid);
      owner <= owner + 1; //assign next owner
      doRel.wset(owner + 1);
   endmethod
   
   //Reserves the lock and returns the associated id
   method ActionValue#(LockId#(d)) res1();
      nextId[0] <= nextId[0] + 1;
      doRes.wset(True);
      return nextId[0];
   endmethod
   
   method ActionValue#(LockId#(d)) checkpoint();
      //return point after this cycle's reservations
      return nextId[1];
   endmethod
   
   method Action rollback(LockId#(d) i, Bool doRoll, Bool doRollRel);
      //rollback release is noop
      //conflicts with other update rules - cannot res/rel and rollback
      if (doRoll)
	 begin
	    nextId[0] <= i;
	    empty <= i == owner; //if i is Owner, then this is actually empty after rollback
	 end
   endmethod   
   
endmodule

typedef UInt#(TLog#(n)) LockIdx#(numeric type n);

module mkFAAddrLock(AddrLock#(LockId#(d), addr, numlocks)) provisos(Bits#(addr, szAddr), Eq#(addr));

   
   Vector#(numlocks, QueueLock#(LockId#(d))) lockVec <- replicateM( mkCountingLock() );
   Vector#(numlocks, Reg#(Maybe#(addr))) entryVec <- replicateM( mkConfigReg(tagged Invalid) );
   //Signal that a reservation used an already-allocated lock this cycle
   //which tells the relevant "free lock" rules not to execute
   Vector#(numlocks, RWire#(Bool)) resVec <- replicateM(mkRWire());

   //Allow each lock entry to be freed once it's no longer used,
   //but tagged as valid, *AND* was there isn't a reservation this cycle to it.
   for (Integer i = 0; i < valueOf(numlocks); i = i + 1) begin      
      rule freelock(lockVec[i].isEmpty && isValid(entryVec[i]) && !isValid(resVec[i].wget()));
	 entryVec[i] <= tagged Invalid;
      endrule
   end
  
   //returns the index of the lock associated with loc
   //returns invalid if no lock is associated with loc
   function Maybe#(LockIdx#(numlocks)) getLockIndex(addr loc);
      Maybe#(LockIdx#(numlocks)) result = tagged Invalid;
      for (Integer idx = 0; idx < valueOf(numlocks); idx = idx + 1)
	    if (result matches tagged Invalid &&& 
	       entryVec[idx] matches tagged Valid.t &&& t == loc)
	       result = tagged Valid fromInteger(idx);
      return result;
   endfunction

   function Maybe#(QueueLock#(LockId#(d))) getLock(addr loc);
    if (getLockIndex(loc) matches tagged Valid.idx)
       return tagged Valid lockVec[idx];
    else
       return tagged Invalid;
   endfunction
   
   //gets the first index where an address is not assigned to the lock OR the lock is empty
   //returns invalid if all locks are in use
   function Maybe#(LockIdx#(numlocks)) getFreeLock();
      Maybe#(LockIdx#(numlocks)) result = tagged Invalid;
      for (Integer idx = 0; idx < valueOf(numlocks); idx = idx + 1)
	    if (result matches tagged Invalid &&&
		entryVec[idx] matches tagged Invalid)
		  result = tagged Valid fromInteger(idx);		  
      return result;
   endfunction

   //returns true iff there is a lock location free
   function Bool isFreeLock();
      return isValid(getFreeLock());
   endfunction   

   //true if lock is associated w/ addr or there is a location free
   method Bool canRes1(addr loc);
      let lockAddr = getLock(loc);
      if (lockAddr matches tagged Valid.l) return True;
      else return isFreeLock();
   endmethod
   
   //true if lock associated w/ address is empty or there is a lock lockation free
   method Bool isEmpty(addr loc);
        Maybe#(QueueLock#(LockId#(d))) addrLock = getLock(loc);
        if (addrLock matches tagged Valid.l)
           return l.isEmpty();
        else
           return isFreeLock();
   endmethod

   method Bool owns1(LockId#(d) tid, addr loc);
      Maybe#(QueueLock#(LockId#(d))) addrLock = getLock(loc);
      Bool hasFree = isFreeLock();
      if (addrLock matches tagged Valid.lock)
	    //in this case loc is associated with lockVec[idx]
	    //so check to see if the requested thread owns it
	    return lock.owns1(tid);
      else
	    //otherwise none of the existing locks
	    //are associated with loc - so if any of them are
	    //free then this location is acquirable
	    return hasFree;
   endmethod
      
   method Action rel1(LockId#(d) tid, addr loc);
      Maybe#(LockIdx#(numlocks)) lockIdx = getLockIndex(loc);
      if (lockIdx matches tagged Valid.idx)
	 begin
	    QueueLock#(LockId#(d)) lock = lockVec[idx];
	    lock.rel1(tid);
	    //disassociate the lock from the address once its empty in a separate rule
	 end
      //else no lock is associated with loc, do nothing
   endmethod
   
   method ActionValue#(LockId#(d)) res1(addr loc);
      Maybe#(LockIdx#(numlocks)) lockIdx = getLockIndex(loc);
      if (lockIdx matches tagged Valid.idx)
	 begin
	    let lock = lockVec[idx];
	    resVec[idx].wset(True);
	    let nid <- lock.res1();
            return nid;
	 end
      else
	    begin
	        Maybe#(LockIdx#(numlocks)) freeLock = getFreeLock();
	        if (freeLock matches tagged Valid.idx)
	            begin
		            entryVec[idx] <= tagged Valid loc;
		            let nid <- lockVec[idx].res1();
		            return nid;
	            end
	        else return 0;
	    end
   endmethod

endmodule: mkFAAddrLock

module mkDMAddrLock(AddrLock#(LockId#(d), addr, unused)) provisos(PrimIndex#(addr, szAddr));
   
   Vector#(TExp#(szAddr), QueueLock#(LockId#(d))) lockVec <- replicateM( mkCountingLock() );

   method Bool isEmpty(addr loc);
      return lockVec[loc].isEmpty();
   endmethod

   method Bool owns1(LockId#(d) tid, addr loc);
      return lockVec[loc].owns1(tid);
   endmethod
   
   method Bool canRes1(addr loc);
      return True;
   endmethod
   
   method Action rel1(LockId#(d) tid, addr loc);
      lockVec[loc].rel1(tid);
   endmethod
   
   method ActionValue#(LockId#(d)) res1(addr loc);
      let id <- lockVec[loc].res1();
      return id;
   endmethod
   
endmodule: mkDMAddrLock

endpackage
