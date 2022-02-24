import VerilogLibs::*;
import FIFO::*;
import RegFile :: *;


(*synthesize*)
module mkTop();
   
   //Test the Renaming Register File a Bit
   CheckpointRF#(UInt#(5), UInt#(32), UInt#(6), UInt#(2)) rf <- mkCheckpointRF(32, 64, 4, False, "");
   FIFO#(UInt#(6)) writes <- mkSizedFIFO(32);
   FIFO#(UInt#(6)) reads <- mkSizedFIFO(32);
   FIFO#(UInt#(6)) commits <- mkSizedFIFO(32);
   FIFO#(UInt#(2)) checkpoints <- mkSizedFIFO(32);
   
   Reg#(UInt#(5)) arch <- mkReg(0);
   
   rule res;
      let rname = rf.renamerf.res_r1(arch);      
      let wname <- rf.renamerf.res_w1(arch);
      arch <= arch + 1;
      writes.enq(wname);
      reads.enq(rname);
      let cname <- rf.checkpoint();      
      checkpoints.enq(cname);      
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
   
   rule readwrite (rf.renamerf.owns_r1(reads.first()));
      writes.deq();
      rf.renamerf.write(writes.first(), count);
      count <= count + 1;
      $display("Wrote %d to name %d at %t", count, writes.first(), $time());
      commits.enq(writes.first());
      //reads
      reads.deq();
      let x = rf.renamerf.read(reads.first());
      $display("Read %d for name %d at %t", x, reads.first(), $time());
   endrule

   rule commit;
      checkpoints.deq();
      rf.commit(checkpoints.first(), False, True);
      rf.renamerf.rel_w1(commits.first());
      commits.deq();
      $display("Freeing old name for %d at %t", commits.first(), $time());
      $display("Freeing checkpoint %d at %t", checkpoints.first(), $time());
   endrule
endmodule

(*synthesize*)
module mkTmp();  
   //Test our rollbacking version of the renaming register file
   CheckpointRF#(UInt#(5), UInt#(32), UInt#(6), UInt#(2)) rf <- mkCheckpointRF(32, 64, 4, False, "");
   FIFO#(UInt#(6)) writes <- mkSizedFIFO(32);
   FIFO#(UInt#(6)) reads <- mkSizedFIFO(32);
   FIFO#(UInt#(2)) checkpoints <- mkSizedFIFO(32);
   FIFO#(UInt#(6)) commits <- mkSizedFIFO(32);

   Reg#(UInt#(32)) count <- mkReg(0);   
   Reg#(UInt#(5)) arch <- mkReg(0);   
   Reg#(UInt#(32)) timer <- mkReg(0);
   
   Wire#(Bool) test <- mkWire();
   
   rule alwaysgo;
      $display("Timer %t", $time());
      if (timer > 1000) $finish();
      timer <= timer + 1;
   endrule

   
   rule res;
      let rname = rf.renamerf.res_r1(arch);      
      let wname <- rf.renamerf.res_w1(arch);
      let cname <- rf.checkpoint();
      test <= True;
      arch <= arch + 1;
      writes.enq(wname);
      reads.enq(rname);
      checkpoints.enq(cname);
      $display("Read name %d at %t", rname, $time());
      $display("Allocated name %d at %t", wname, $time());
      $display("Made checkpoint %d at %t", cname, $time());
   endrule
   
   rule readwrite (rf.renamerf.owns_r1(reads.first()));
      writes.deq();
      rf.renamerf.write(writes.first(), count);
      count <= count + 1;
      $display("Wrote %d to name %d at %t", count, writes.first(), $time());
      commits.enq(writes.first());
      //reads
      reads.deq();
      let x = rf.renamerf.read(reads.first());
      $display("Read %d for name %d at %t", x, reads.first(), $time());      
   endrule
  
   rule commit;
      test <= True;      
      rf.commit(checkpoints.first(), True, True);
      rf.renamerf.rel_w1(commits.first());
      commits.clear();
      checkpoints.clear();
      reads.clear();
      writes.clear();
      $display("Freeing old name for %d at %t", commits.first(), $time());
      $display("Rolling back to checkpoint %d at %t", checkpoints.first(), $time());
   endrule

endmodule
