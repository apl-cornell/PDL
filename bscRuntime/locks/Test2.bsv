import Locks :: *;
import FIFO :: *;

typedef enum { Start, A1, A2, R1, R2, Done } State deriving (Bits, Eq);
typedef UInt#(7) ThreadID;

typedef UInt#(5) Address;
typedef UInt#(32) Data;


(*synthesize*)
module mkTop();
   
   

   AddrLock#(LockId#(4), Address, 4) l1 <- mkFAAddrLock();
   function Action dorel(LockId#(4) l, ThreadID t, Address a);
      return action
		if (l1.owns(l, a))
		   begin
		      l1.rel(l, a);
		      $display("Thread %d releasing lock for address %d, %t", t, a, $time());
		   end
		else
		   begin
		      $display("Thread %d cannot release lock for address %d, %t", t, a, $time());
		   end
	     endaction;
   endfunction
   
   function ActionValue#(LockId#(4)) res(ThreadID t, Address a);
      return actionvalue
		      let ml <- l1.res(a);
		      if (ml matches tagged Valid.l)
			 begin
			    $display("Thread %d reserved lock for address %d, %t", t, a, $time());
			    return l;
			 end
		      else
			 begin
			    $display("Thread %d CANT reserve lock for address %d, %t", t, a, $time());		    
			    return 0;
			 end
	     endactionvalue;
   endfunction

   ThreadID t1 = 0;
   ThreadID t2 = 3;
   Address a1 = 15;
   Address a2 = 17;

   Reg#(Address) tolock <- mkReg(0);
   Reg#(Address) tofree <- mkReg(0);
   Reg#(LockId#(4)) lockreg1 <- mkReg(0);
   FIFO#(LockId#(4)) lockids <- mkSizedFIFO(10);
   //TODO acquire more than 1 location per cycle
   rule acq(l1.canRes(tolock));
      let l <- res(t1, tolock);
      tolock <= tolock + 1;
      lockids.enq(l);
   endrule
   
   rule rel1(l1.owns(lockids.first, tofree));
      dorel(lockids.first, t1, tofree);
      tofree <= tofree + 1;
      lockids.deq();
   endrule      
   
endmodule
