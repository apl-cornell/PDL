import Cpu :: *;
import Memories :: *;

(*synthesize*)
module top();
   MemCombRead#(UInt#(32), UInt#(5)) rf <- mkCombMem();
   AsyncMem#(UInt#(16), UInt#(32)) imem <- mkAsyncMem();
   Cpu dut <- mkCpu( rf, imem);
   Reg#(UInt#(32)) count <- mkReg(0);
   rule exec(count == 0);
      $display("Started Execution, %d", $time());
      dut.start( 0 );
   endrule
   
   rule inc;
      count <= count + 1;
      if (count > 100) $finish(0);
   endrule

endmodule
