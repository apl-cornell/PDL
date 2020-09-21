package Locks;

import FIFOF :: * ;
import Vector :: *;

export Lock(..);
export AddrLock(..);
export mkLock;
export mkAddrLock;

interface Lock#(type id);
   method Bool isEmpty();
   method Bool owns(id tid);
   method Action acq(id tid);
   method Action rel(id tid);
   method Action res(id tid);
endinterface

interface AddrLock#(type id, type addr);
   method Bool owns(id tid, addr loc);
   method Action acq(id tid, addr loc);
   method Action rel(id tid, addr loc);
   method Action res(id tid, addr loc);
endinterface

module mkLock(Lock#(id) ) provisos(Bits#(id, szId), Eq#(id));
   
   FIFOF#(id) held <- mkFIFOF;
   
   Bool lockFree = !held.notEmpty;
   id owner = held.first;

   method Bool isEmpty();
      return lockFree;
   endmethod
   
   //Returns True if thread `tid` may acquire the lock right now
   //or already owns the lock
   method Bool owns(id tid);
      return lockFree || owner == tid;
   endmethod
   
   //If the lock is free, then tid acquires it
   method Action acq(id tid);
      if (lockFree) held.enq(tid);
   endmethod
   
   //Releases the lock iff thread `tid` owns it already
   method Action rel(id tid);
       if (owner == tid) held.deq();
   endmethod
   
   //Puts `tid` on the queue to reserve the lock
   method Action res(id tid);
      held.enq(tid);
   endmethod
   
endmodule: mkLock


`define DEFAULT_LOCK_ENTRIES 4
`define LAST_ENTRY 3
typedef UInt#(TLog#(n)) LockIdx#(numeric type n);
`define LOCK_VEC_BITS LockIdx#(`DEFAULT_LOCK_ENTRIES)

module mkAddrLock(AddrLock#(id, addr)) provisos(Bits#(id, szId), Eq#(id), Bits#(addr, szAddr), Eq#(addr));

   Vector#(`DEFAULT_LOCK_ENTRIES, Lock#(id)) lockVec <- replicateM( mkLock());
   Vector#(`DEFAULT_LOCK_ENTRIES, Reg#(Maybe#(addr))) entryVec <- replicateM( mkReg(tagged Invalid) );

   //returns the index of the lock associated with loc
   //returns invalid if no lock is associated with loc
   function Maybe#(`LOCK_VEC_BITS) getLockIndex(addr loc);
      Maybe#(`LOCK_VEC_BITS) result = tagged Invalid;
      for (`LOCK_VEC_BITS idx = 0; idx < `LAST_ENTRY; idx = idx + 1)
	    if (result matches tagged Invalid &&& 
	       entryVec[idx] matches tagged Valid.t &&& t == loc)
	       result = tagged Valid idx;
      return result;
   endfunction

   
   function Maybe#(Lock#(id)) getLock(addr loc);
    if (getLockIndex(loc) matches tagged Valid.idx)
       return tagged Valid lockVec[idx];
    else
       return tagged Invalid;
   endfunction
   
   //gets the first index where an address is not assigned to the lock
   //returns invalid if all locks are in use
   function Maybe#(`LOCK_VEC_BITS) getFreeLock();
      Maybe#(`LOCK_VEC_BITS) result = tagged Invalid;
      for (`LOCK_VEC_BITS idx = 0; idx < `LAST_ENTRY; idx = idx + 1)
	    if (result matches tagged Invalid &&& 
	       entryVec[idx] matches tagged Invalid)
	       result = tagged Valid idx;
      return result;
   endfunction

   //returns true iff there is a lock location free
   function Bool isFreeLock();
      return isValid(getFreeLock());
   endfunction   
   
   method Bool owns(id tid, addr loc);
      Maybe#(Lock#(id)) addrLock = getLock(loc);
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
   
   method Action acq(id tid, addr loc);
      Maybe#(Lock#(id)) addrLock = getLock(loc);
      if (addrLock matches tagged Valid.lock)
	 lock.acq(tid);
      else
	 begin
	    Maybe#(`LOCK_VEC_BITS) freeLock = getFreeLock();
	    if (freeLock matches tagged Valid.idx)
	    begin
	       lockVec[idx].acq(tid);
	       entryVec[idx] <= tagged Valid loc;
	    end
	 end
   endmethod
   
   method Action rel(id tid, addr loc);
      Maybe#(`LOCK_VEC_BITS) lockIdx = getLockIndex(loc);
      if (lockIdx matches tagged Valid.idx)
	 begin
	    Lock#(id) lock = lockVec[idx];
	    lock.rel(tid);
	    //disassociated the lock from the address once its empty
	    if (lock.isEmpty()) entryVec[idx] <= tagged Invalid;
	 end
      //else no lock is associated with loc, do nothing
   endmethod
   
   method Action res(id tid, addr loc);
      Maybe#(Lock#(id)) addrLock = getLock(loc);
      if (addrLock matches tagged Valid.lock)
	 lock.res(tid);
      else
	 begin
	    Maybe#(`LOCK_VEC_BITS) freeLock = getFreeLock();
	    if (freeLock matches tagged Valid.idx)
	       begin
		  lockVec[idx].res(tid);
		  entryVec[idx] <= tagged Valid loc;
	       end
	 end
   endmethod

endmodule: mkAddrLock
   
endpackage
