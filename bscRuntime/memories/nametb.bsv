import Named::*;
import FIFO::*;


typedef UInt#(2) Tag;
typedef UInt#(4) State;

module testLSQ();
   
   AsyncMem#(UInt#(32), UInt#(16), Tag) lsq <- mkLSQ(False, "");

   Reg#(State) st <- mkReg(0);
   
   Reg#(Tag) t1 <- mkReg(0);
   Reg#(Tag) t2 <- mkReg(0);
   
   rule s_0 (st == 0);
      let n <- lsq.reserveWrite(0);
      t1 <= n;
      st <= 1;
      $display("StQ Tag %d", n);
   endrule

   rule s_1 (st == 1);
      let n <- lsq.reserveRead(0);
      t2 <= n;
      st <= 2;
      $display("LdQ Tag %d", n);
   endrule
   
   UInt#(32) magic = 1337;
   
   rule s_2 (st == 2);
      lsq.write(t1, magic);
      st <= 3;
   endrule
   
   rule s_3 (st == 3);
      let ready = lsq.isValid(t2);
      $display("Store was forwarded correctly: %b", ready);
      let data = lsq.read(t2);
      $display("Load observed %d", data);
   endrule

endmodule
   
module testRename();
   //Test the Renaming Register File a Bit
   CombMem#(UInt#(32), UInt#(5), UInt#(6)) rf <- mkRenameRF(32, 64, False, "");
   FIFO#(UInt#(6)) writes <- mkSizedFIFO(10);
   FIFO#(UInt#(6)) reads <- mkSizedFIFO(10);
   FIFO#(UInt#(6)) commits <- mkSizedFIFO(10);

   Reg#(UInt#(5)) arch <- mkReg(0);
   
   rule res;
      let rname = rf.readName(arch);      
      let wname <- rf.allocName(arch);
      arch <= arch + 1;
      writes.enq(wname);
      reads.enq(rname);
      $display("Read name %d at %t", rname, $time());
      $display("Allocated name %d at %t", wname, $time());
   endrule

   Reg#(UInt#(32)) count <- mkReg(0);
   
   rule readwrite (rf.isValid(reads.first()));
      writes.deq();
      rf.write(writes.first(), count);
      $display("Wrote %d to name %d at %t", count, writes.first(), $time());
      commits.enq(writes.first());
      //reads
      reads.deq();
      let x = rf.read(reads.first());
      $display("Read %d for name %d at %t", x, reads.first(), $time());
   endrule

   rule commit;
      if (count > 1000) $finish();	 
      count <= count + 1;
      rf.commit(commits.first());
      commits.deq();
   endrule
endmodule 
  
(*synthesize*)
module mkTop();
   
   let tb <- testLSQ();

   Reg#(UInt#(32)) timer <- mkReg(0);
   
   `ifndef SIM_TIME
   `define SIM_TIME 1000
   `endif

   rule endtb;
      timer <= timer + 1;
      if (timer > `SIM_TIME) $finish();
   endrule
endmodule
