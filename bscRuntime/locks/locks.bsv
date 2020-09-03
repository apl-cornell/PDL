import FIFOF :: * ;

interface Lock#(type id);
   method Bool owns(id tid);
   method Action acq(id tid);
   method Action rel(id tid);
   method Action res(id tid);
endinterface


module mkLock(Lock#(type id));
   
   FIFOF#(ID) held <- mkFIFOF;
   
   Bool lockFree = !held.notEmpty;
   ID owner = held.first;

   //Returns True if thread `tid` may acquire the lock right now
   method Bool owns(ID tid);
      return lockFree || owner == tid;
   endmethod
   
   //Returns True if the lock was acquired, else False
   method Action acq(ID tid);
      if (lockFree || owner == tid) held.enq(tid);
   endmethod
   
   //Releases the lock iff thread `tid` owns it already
   method Action rel(ID tid);
   if (held.notEmpty && owner == tid)
    begin
	  held.deq();
    end
   endmethod
   
   //Puts `tid` on the queue to reserve the lock
   method Action res(ID tid);
      held.enq(tid);
   endmethod
   
endmodule: mkLock
