import VerilogLibs::*;
import FIFO::*;



(*synthesize*)
module mkTop();
   
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

   Reg#(UInt#(32)) timer <- mkReg(0);
   rule alwaysgo;
      $display("Timer %t", $time());
      if (timer > 1000) $finish();
      timer <= timer + 1;
   endrule
   
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
