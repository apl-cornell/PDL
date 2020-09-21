import Locks :: *;

typedef enum { Start, A1, A2, R1, R2, Done } State deriving (Bits, Eq);
typedef UInt#(7) ThreadID;
typedef UInt#(5) Address;
typedef UInt#(32) Data;


(*synthesize*)
module mkTop();
   
   
   Reg#(State) s <- mkReg(Start);
   AddrLock#(ThreadID, Address) l1 <- mkAddrLock();
   function Action dorel(ThreadID t, Address a);
      return action
		if (l1.owns(t, a))
		   begin
		      l1.rel(t, a);
		      $display("Thread %d releasing lock for address %d", t, a);
		   end
		else
		   begin
		      $display("Thread %d cannot release lock for addres %d", t, a);
		   end
	     endaction;
   endfunction
   
   function Action acq(ThreadID t, Address a);
      return action
		if (l1.owns(t, a))
		   begin
		      l1.acq(t, a);
		      $display("Thread %d acquired lock for address %d", t, a);
		   end
		else
		   begin
		      $display("Thread %d cannot acquire lock for address %d", t, a);
		   end
	     endaction;
   endfunction

   ThreadID t1 = 0;
   ThreadID t2 = 3;
   Address a1 = 15;
   Address a2 = 17;

   //TODO acquire more than 1 location per cycle
   rule start(s == Start);
      acq(t1, a1);
      s <= A1;
   endrule
   
   rule exec1(s == A1);
      acq(t2, a1);
      dorel(t1, a1);
      s <= Done;
   endrule
   

   rule stop(s == Done);
      $finish();
   endrule
   
   
endmodule
