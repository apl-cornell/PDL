package Locks;

import FIFOF :: * ;

export Lock(..);
export mkLock;

interface Lock#(type id);
   method Bool owns(id tid);
   method Action acq(id tid);
   method Action rel(id tid);
   method Action res(id tid);
endinterface


module mkLock(Lock#(id) ) provisos(Bits#(id, szId), Eq#(id));
   
   FIFOF#(id) held <- mkFIFOF;
   
   Bool lockFree = !held.notEmpty;
   id owner = held.first;

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

endpackage
