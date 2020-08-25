import FIFO :: *;
import SpecialFIFOs :: * ;
import BRAM :: *;
import DefaultValue :: *;

typedef struct { Bit#(10) y; } InputN deriving(Bits, Eq);
typedef struct { Bit#(1) x; } Stage1_TO_Stage2 deriving(Bits, Eq);


module mkStage1 (FIFO #(InputN) s_input, FIFO #(Stage1_TO_Stage2) s_1to2, BRAM1Port #(Bit#(8), Bit#(8)) dmem, Empty none);

   InputN first = s_input.first();
   Bit#(1) x = first.y[0:0];

   rule execute;
      s_input.deq();
      s_1to2.enq( Stage1_TO_Stage2 { x : x }) ;
      dmem.portA.request.put( BRAMRequest { write: False, responseOnWrite: False, address: first.y[8:1], datain: 8'b0 });
   endrule

endmodule

module mkStage2 (FIFO #(Stage1_TO_Stage2) s1_to_s2, BRAM1Port #(Bit#(8), Bit#(8)) dmem, Empty none);
   
   Stage1_TO_Stage2 first = s1_to_s2.first();
   Wire#(Bit#(8)) mval <- mkWire;
   Bit #(9) out = { first.x, mval };

   rule readmem;
      let mout <- dmem.portA.response.get;
      mval <= mout;
   endrule
   
   (* fire_when_enabled *)
   rule execute;
      s1_to_s2.deq();
      $display("out is %b; %d", out, $time());
   endrule
   
endmodule

(* synthesize *)
module mkTop();

   FIFO#(InputN) f1 <- mkFIFO;
   FIFO#(Stage1_TO_Stage2) f2 <- mkFIFO;
   BRAM_Configure cfg = defaultValue;
   cfg.allowWriteResponseBypass = False;
   BRAM1Port#(Bit#(8), Bit#(8)) dut0 <- mkBRAM1Server(cfg);
   
   mkStage1(f1, f2, dut0);
   mkStage2(f2, dut0);
   
   Reg #(Bit#(10)) x <- mkReg(0);
   
   rule exec;
      f1.enq( InputN { y: x } );
      x <= x + 1;
      if (x > 100) $finish(0);
      $display("enqueuing %d", x);
   endrule
endmodule: mkTop
