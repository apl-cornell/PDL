import Locks :: *;

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
		      $display("Thread %d releasing lock for address %d", t, a);
		   end
		else
		   begin
		      $display("Thread %d cannot release lock for address %d", t, a);
		   end
	     endaction;
   endfunction
   
   function ActionValue#(LockId#(4)) res(ThreadID t, Address a);
      return actionvalue
		      let l <- l1.res(a);
		      $display("Thread %d reserved lock for address %d", t, a);
		if (l matches tagged Valid.l2) return l2;
		else return 0;
	     endactionvalue;
   endfunction

   Reg#(State) s <- mkReg(Start);   
   ThreadID t1 = 0;
   ThreadID t2 = 3;
   Address a1 = 15;
   Address a2 = 17;

   Reg#(LockId#(4)) lockreg1 <- mkReg(0);
   Reg#(LockId#(4)) lockreg2 <- mkReg(0);

   //TODO acquire more than 1 location per cycle
   rule start(s == Start);
      let l <- res(t1, a1);
      lockreg1 <= l;
      s <= A1;
   endrule
   
   rule acq1(s == A1);
      let l <- res(t2, a1);
      lockreg2 <= l;
      s <= A2;
   endrule

   rule acq2(s == A2);
      let l <- res(t2, a2);
      lockreg1 <= l;
      dorel(lockreg1, t1, a1);
      s <= R1;
   endrule
   
   rule r1rule(s == R1 && l1.owns(lockreg2, a1) && l1.owns(lockreg1, a2));
      dorel(lockreg2, t2, a1);
      dorel(lockreg1, t2, a2);
      s <= Done;
   endrule

   rule stop(s == Done);
      $finish();
   endrule
   
   
endmodule
