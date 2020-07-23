import FIFOF :: * ;

typedef Bit#(4) ID;

interface Lock;
   method ActionValue#(Bool) acq(ID tid);
   method Action rel();
   method Action res(ID tid);
endinterface


module mkLock(Lock);
   
   FIFOF#(ID) held <- mkFIFOF;
   
   Bool lockFree = !held.notEmpty;
   ID owner = held.first;
   method ActionValue#(Bool) acq(ID tid);
      if (lockFree || owner == tid)
	 begin
	    held.enq(tid);
	    return True;
	 end
      else
	 return False;
   endmethod
   
   method Action rel();
      held.deq();
   endmethod
   
   method Action res(ID tid);
      held.enq(tid);
   endmethod
   
endmodule: mkLock
