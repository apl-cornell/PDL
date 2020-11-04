import Locks :: *;

typedef enum { Start, A1, A2, R1, R2, Done } State deriving (Bits, Eq);
typedef UInt#(7) ThreadID;

typedef UInt#(5) Address;
typedef UInt#(32) Data;


(*synthesize*)
module mkTop();
   
   

   AddrLock#(LockId#(2), Address, 32) l1 <- mkDMAddrLock();
   
   function Action dorel(LockId#(2) l, ThreadID t, Address a);
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
   
   function ActionValue#(LockId#(2)) res(ThreadID t, Address a);
      return actionvalue
		      let l <- l1.res(a);
		      $display("Thread %d reserved lock for address %d, %t", t, a, $time());
		if (l matches tagged Valid.l2) return l2;
		else return 0;
	     endactionvalue;
   endfunction

   ThreadID t1 = 0;
   ThreadID t2 = 3;
   Address a1 = 15;
   Address a2 = 17;

   Reg#(LockId#(2)) lockreg1 <- mkReg(0);
   Reg#(LockId#(2)) lockreg2 <- mkReg(0);
   Reg#(LockId#(2)) lockreg3 <- mkReg(0);

   Reg#(Bool) t1has <- mkReg(False);
   Reg#(Bool) t2has <- mkReg(False);

   //TODO acquire more than 1 location per cycle
   rule acq1(!t1has);
      let l <- res(t1, a1);
      lockreg1 <= l;
      t1has <= True;
   endrule

   rule rel1(l1.owns(lockreg1, a1));
      dorel(lockreg1, t1, a1);
      t1has <= False;
   endrule
   
   rule acq2(!t2has);
      let l <- res(t2, a2);
      lockreg2 <= l;
      let lx <- res(t2, a1);
      lockreg3 <= lx;
      t2has <= True;
   endrule

   rule rel2(l1.owns(lockreg2, a2) && l1.owns(lockreg3, a1));
      dorel(lockreg2, t2, a2);
      dorel(lockreg3, t2, a1);
      t2has <= False;
   endrule
      
   
endmodule
