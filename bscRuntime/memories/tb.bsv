import Locks :: *;
import Memories::*;
import FIFO::*;
import Speculation :: *;
import RegFile :: *;
typedef UInt#(5) ADDR;
typedef UInt#(32) DATA;

(*synthesize*)
module mkTop();
   
   
   Reg#(UInt#(4)) cnt <- mkReg(0);


   RegFile#(ADDR, DATA) rf <- mkRegFileFull();
   BypassLockCombMem#(ADDR, DATA, LockId#(4), 4) lock <- mkBypassLockCombMem(rf);
   
   rule init(cnt == 0);
      rf.upd(0, 1234);
      cnt <= 1;
   endrule
   
   ADDR addr = 3;
   Reg#(LockId#(4)) idreg <- mkReg(unpack(0));

   rule doRes(cnt == 1);
      let id <- lock.reserve(addr);
      idreg <= id;
      cnt <= 2;
   endrule
   
   rule doWrite(cnt == 2);
      $display("Can read conflicting address: %b should be 0", lock.canRead(addr));
      lock.write(idreg, 1111);
      cnt <= 3;
   endrule
   
   rule doRead(cnt == 3);
      $display("Can read conflicting address: %b should be 1", lock.canRead(addr));
      $display("Read value %d", lock.read(addr));
//      lock.commit(idreg);
      let id <- lock.reserve(addr);
      idreg <= id;
      cnt <= 4;
   endrule
   
   rule done(cnt == 4);
      $display("Can read conflicting address: %b should be 0", lock.canRead(addr));
      $display("Read value %d", lock.read(addr));      
      $finish();
   endrule
   // AsyncMem#(UInt#(32), UInt#(5), MemId#(2)) mem <- mkAsyncMem();
   // FIFO#(MemId#(2)) rids <- mkSizedFIFO(10);
   
   // Reg#(Bool) once <- mkReg(False);
   // rule g1 (!once);
   //    let xid <- mem.req(0, ?, False);
   //    rids.enq(xid);
   //    $display("Made first req %t", $time());
   //    once <= True;
   // endrule

   // rule g2 (mem.checkRespId(rids.first));
   //    rids.deq();
   //    let xid <- mem.req(0, 1, True);
   //    mem.resp();
   //    $display("Made req & resp, %t", $time());
   // endrule
   
   
endmodule
