package Locks;

import FIFOF :: * ;
import Vector :: *;

export Lock(..);
export AddrLock(..);
export LockId(..);
export mkLock;
export mkFAAddrLock;

typedef UInt#(TLog#(n)) LockId#(numeric type n);

interface Lock#(type id);
   method Bool isEmpty();
   method Bool owns(id tid);
   method Action rel(id tid);
   method ActionValue#(id) res();
endinterface

interface AddrLock#(type id, type addr, numeric type size);
   method Bool isEmpty(addr loc);
   method Bool owns(id tid, addr loc);
   method Action rel(id tid, addr loc);
   method ActionValue#(id) res(addr loc);
endinterface

interface BypassAddrLock#(type id, type addr, numeric type size, type elem);
   method Bool isEmpty(addr loc);
   method Bool owns(id tid, addr loc);
   method Action rel(id tid, addr loc);
   method ActionValue#(id) res(addr loc);
   method Action commit(addr loc, elem data);
   method elem read(addr loc);
endinterface

module mkLock(Lock#(LockId#(d)));

   Reg#(LockId#(d)) nextId <- mkReg(0);
   FIFOF#(LockId#(d)) held <- mkSizedFIFOF(valueOf(d));
   
   Bool lockFree = !held.notEmpty;
   LockId#(d) owner = held.first;

   method Bool isEmpty();
      return lockFree;
   endmethod
   
   //Returns True if thread `tid` already owns the lock
   method Bool owns(LockId#(d) tid);
      return owner == tid;
   endmethod
      
   //Releases the lock iff thread `tid` owns it already
   method Action rel(LockId#(d) tid);
       if (owner == tid) held.deq();
   endmethod
   
   //Reserves the lock and returns the associated id
   method ActionValue#(LockId#(d)) res();
      held.enq(nextId);
      nextId <= nextId + 1;
      return nextId;
   endmethod
   
endmodule: mkLock


typedef UInt#(TLog#(n)) LockIdx#(numeric type n);

module mkFAAddrLock(AddrLock#(LockId#(d), addr, numlocks)) provisos(Bits#(addr, szAddr), Eq#(addr));

   Vector#(numlocks, Lock#(LockId#(d))) lockVec <- replicateM( mkLock() );
   Vector#(numlocks, Reg#(Maybe#(addr))) entryVec <- replicateM( mkReg(tagged Invalid) );

   rule invalidateLocks;
      for (LockIdx#(numlocks) idx = 0; idx < fromInteger(valueOf(numlocks) - 1); idx = idx + 1)
	 if (entryVec[idx] matches tagged Valid.t &&& lockVec[idx].isEmpty())
	    entryVec[idx] <= tagged Invalid;
   endrule
   //returns the index of the lock associated with loc
   //returns invalid if no lock is associated with loc
   function Maybe#(LockIdx#(numlocks)) getLockIndex(addr loc);
      Maybe#(LockIdx#(numlocks)) result = tagged Invalid;
      for (LockIdx#(numlocks) idx = 0; idx < fromInteger(valueOf(numlocks) - 1); idx = idx + 1)
	    if (result matches tagged Invalid &&& 
	       entryVec[idx] matches tagged Valid.t &&& t == loc)
	       result = tagged Valid idx;
      return result;
   endfunction


   
   function Maybe#(Lock#(LockId#(d))) getLock(addr loc);
    if (getLockIndex(loc) matches tagged Valid.idx)
       return tagged Valid lockVec[idx];
    else
       return tagged Invalid;
   endfunction
   
   //gets the first index where an address is not assigned to the lock
   //returns invalid if all locks are in use
   function Maybe#(LockIdx#(numlocks)) getFreeLock();
      Maybe#(LockIdx#(numlocks)) result = tagged Invalid;
      for (LockIdx#(numlocks) idx = 0; idx < fromInteger(valueOf(numlocks) - 1); idx = idx + 1)
	    if (result matches tagged Invalid &&& 
	       entryVec[idx] matches tagged Invalid)
	       result = tagged Valid idx;
      return result;
   endfunction

   //returns true iff there is a lock location free
   function Bool isFreeLock();
      return isValid(getFreeLock());
   endfunction   

   //true if no lock is associated w/ addr and there is a lock lockation free
   method Bool isEmpty(addr loc);
        Maybe#(Lock#(LockId#(d))) addrLock = getLock(loc);
        if (addrLock matches tagged Invalid)
            return isFreeLock();
        else
            return False;
   endmethod

   method Bool owns(LockId#(d) tid, addr loc);
      Maybe#(Lock#(LockId#(d))) addrLock = getLock(loc);
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
	    Lock#(LockId#(d)) lock = lockVec[idx];
	    lock.rel(tid);
//	    $display("Address %d released %t", loc, $time());
	    //disassociate the lock from the address once its empty in a separate rule
	 end
      //else no lock is associated with loc, do nothing
   endmethod
   
   method ActionValue#(LockId#(d)) res(addr loc);
      Maybe#(Lock#(LockId#(d))) addrLock = getLock(loc);
      if (addrLock matches tagged Valid.lock)
	 begin
	    let nid <- lock.res();
//	    $display("Address %d reserved %t", loc, $time());
            return nid;
	 end
      else
	    begin
	        Maybe#(LockIdx#(numlocks)) freeLock = getFreeLock();
	        if (freeLock matches tagged Valid.idx)
	            begin
		            entryVec[idx] <= tagged Valid loc;
		            let nid <- lockVec[idx].res();
//		       	    $display("Address %d reserved %t", loc, $time());
		            return nid;
	            end
	        else return ?;
	    end
   endmethod

endmodule: mkFAAddrLock
   
endpackage
