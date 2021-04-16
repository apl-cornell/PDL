package Locks;

import FIFOF :: *;
import Vector :: *;
import ConfigReg :: *;
export LockId(..);
export QueueLock(..);
export AddrLock(..);

export mkQueueLock;
export mkFAAddrLock;
export mkDMAddrLock;

typedef UInt#(TLog#(n)) LockId#(numeric type n);

interface QueueLock#(type id);
   method ActionValue#(id) res();
   method Bool owns(id i);
   method Action rel(id i);
   method Bool isEmpty();
   method Bool canRes();
endinterface

interface AddrLock#(type id, type addr, numeric type size);
   method Bool isEmpty(addr loc);
   method Bool canRes(addr loc);
   method Bool owns(id tid, addr loc);
   method Action rel(id tid, addr loc);
   method ActionValue#(id) res(addr loc);
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
   
   method Bool canRes();
      return held.notFull;
   endmethod
   
   //Returns True if thread `tid` already owns the lock
   method Bool owns(LockId#(d) tid);
      return owner == tid;
   endmethod
	       
   //Releases the lock iff thread `tid` owns it already
   method Action rel(LockId#(d) tid);
      if (owner == tid)
	 begin
	    held.deq();
	 end
   endmethod
   
   //Reserves the lock and returns the associated id
   method ActionValue#(LockId#(d)) res();
      held.enq(nextId);
      nextId <= nextId + 1;
      cnt <= cnt + 1;
      return nextId;
   endmethod
   
endmodule

typedef UInt#(TLog#(n)) LockIdx#(numeric type n);

module mkFAAddrLock(AddrLock#(LockId#(d), addr, numlocks)) provisos(Bits#(addr, szAddr), Eq#(addr));

   
   Vector#(numlocks, QueueLock#(LockId#(d))) lockVec <- replicateM( mkQueueLock() );
   Vector#(numlocks, Reg#(Maybe#(addr))) entryVec <- replicateM( mkConfigReg(tagged Invalid) );

   //Allow each lock entry to be freed once it's no longer used,
   //but tagged as valid
   for (Integer i = 0; i < valueOf(numlocks); i = i + 1) begin      
      rule freelock;
	 if (lockVec[i].isEmpty && isValid(entryVec[i]))
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
   method Bool canRes(addr loc);
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

   method Bool owns(LockId#(d) tid, addr loc);
      Maybe#(QueueLock#(LockId#(d))) addrLock = getLock(loc);
      Bool hasFree = isFreeLock();
      if (addrLock matches tagged Valid.lock)
	    //in this case loc is associated with lockVec[idx]
	    //so check to see if the requested thread owns it
	    return lock.owns(tid);
      else
	    //otherwise none of the existing locks
	    //are associated with loc - so if any of them are
	    //free then this location is acquirable
	    return hasFree;
   endmethod
      
   method Action rel(LockId#(d) tid, addr loc);
      Maybe#(LockIdx#(numlocks)) lockIdx = getLockIndex(loc);
      if (lockIdx matches tagged Valid.idx)
	 begin
	    QueueLock#(LockId#(d)) lock = lockVec[idx];
	    lock.rel(tid);
	    //disassociate the lock from the address once its empty in a separate rule
	 end
      //else no lock is associated with loc, do nothing
   endmethod
   
   method ActionValue#(LockId#(d)) res(addr loc);
      Maybe#(QueueLock#(LockId#(d))) addrLock = getLock(loc);
      if (addrLock matches tagged Valid.lock)
	 begin
	    let nid <- lock.res();
            return nid;
	 end
      else
	    begin
	        Maybe#(LockIdx#(numlocks)) freeLock = getFreeLock();
	        if (freeLock matches tagged Valid.idx)
	            begin
		            entryVec[idx] <= tagged Valid loc;
		            let nid <- lockVec[idx].res();
		            return nid;
	            end
	        else return 0;
	    end
   endmethod

endmodule: mkFAAddrLock

module mkDMAddrLock(AddrLock#(LockId#(d), addr, unused)) provisos(PrimIndex#(addr, szAddr));
   
   Vector#(TExp#(szAddr), QueueLock#(LockId#(d))) lockVec <- replicateM( mkQueueLock() );

   method Bool isEmpty(addr loc);
      return lockVec[loc].isEmpty();
   endmethod

   method Bool owns(LockId#(d) tid, addr loc);
      return lockVec[loc].owns(tid);
   endmethod
   
   method Bool canRes(addr loc);
      return True;
   endmethod
   
   method Action rel(LockId#(d) tid, addr loc);
      lockVec[loc].rel(tid);
   endmethod
   
   method ActionValue#(LockId#(d)) res(addr loc);
      let id <- lockVec[loc].res();
      return id;
   endmethod
   
endmodule: mkDMAddrLock

endpackage
