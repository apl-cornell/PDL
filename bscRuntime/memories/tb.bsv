import Memories::*;
import FIFO::*;
import Speculation :: *;


(*synthesize*)
module mkTop();
   
   SpecTable#(SpecId#(4)) st <- mkSpecTable();
   
   Reg#(UInt#(4)) cnt <- mkReg(0);
   
   Reg#(Maybe#(SpecId#(4))) sid <- mkReg(tagged Invalid);
   
   rule speculate (cnt <= 3);
      let id <- st.alloc();
      cnt <= cnt + 1;
      if (cnt == 2) sid <= tagged Valid id;
   endrule

   rule invalid (cnt > 3 &&& sid matches tagged Valid.id);
      $display("Running Invalid");
      st.invalidate(id);
      sid <= tagged Invalid;
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
