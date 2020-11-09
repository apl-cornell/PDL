import Memories::*;
import FIFO::*;



(*synthesize*)
module mkTop();
   
   AsyncMem#(UInt#(32), UInt#(5), MemId#(2)) mem <- mkAsyncMem();
   FIFO#(MemId#(2)) rids <- mkSizedFIFO(10);
   
   Reg#(Bool) once <- mkReg(False);
   rule g1 (!once);
      let xid <- mem.req(0, ?, False);
      rids.enq(xid);
      $display("Made first req %t", $time());
      once <= True;
   endrule

   rule g2 (mem.checkRespId(rids.first));
      rids.deq();
      let xid <- mem.req(0, 1, True);
      mem.resp();
      $display("Made req & resp, %t", $time());
   endrule
endmodule
