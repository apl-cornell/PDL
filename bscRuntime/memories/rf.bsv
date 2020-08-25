import FIFOF :: * ;

typedef Bit#(4) ID;

interface Memory#(type elem, type addr);
   method Value read(ID tid);
   method Action write(ID tid);
endinterface


module mkLock(Lock);
   
   FIFOF#(ID) held <- mkFIFOF;
   
   Bool lockFree = !held.notEmpty;
   ID owner = held.first;

   //Returns True if thread `tid` may acquire the lock right now
   method Bool check(ID tid);
      return lockFree || owner == tid;
   endmethod
   
   //Returns True if the lock was acquired, else False
   method ActionValue#(Bool) acq(ID tid);
      if (lockFree || owner == tid)
      begin
	    held.enq(tid);
	    return True;
	 end
      else
	 return False;
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
