import FIFOF :: * ;

typedef struct { Bit#(8) x; Bit#(12) y; } InputN deriving(Bits, Eq);
typedef struct { Bit#(10) z; } InputNPlusOne deriving(Bits, Eq);

interface StageN;
   method Action recv(InputN d_in);
endinterface

interface StageNPO;
   method Action recv(InputNPlusOne d_in);
endinterface


module mkStageN (StageNPO nstage, StageN stg);

   FIFOF #(InputN) inputF <- mkFIFOF;
 
   rule execute;
      InputN first = inputF.first;
      Bit#(10) outvar = { first.x[3:0], first.y[5:0] };
      $display("send stage 2, %0d", $time());
      inputF.deq();
      nstage.recv( InputNPlusOne{ z: outvar} );
   endrule

   method Action recv(InputN d_in);
      inputF.enq(d_in);
   endmethod

endmodule: mkStageN

module mkStageNPlusOne(StageNPO);   

   FIFOF #(InputNPlusOne) inputF <- mkFIFOF;
   
   rule execute;
      $display("send stage 3, %0d,  %0b", $time(), inputF.first);
      inputF.deq();
   endrule
   
   method Action recv(InputNPlusOne d_in);
      inputF.enq(d_in);
   endmethod
   

endmodule: mkStageNPlusOne

(* synthesize *)
module mkTop();

   StageNPO snpo <- mkStageNPlusOne();
   StageN sn <- mkStageN(snpo);
   Reg #(InputN) start <- mkReg(InputN{ x:8'h00, y:12'h000 });
   rule execute;
      start <= InputN { x: start.x + 1, y: start.y + 1 };
      sn.recv(start);
      $display("send stage 1, %0d", $time());
      if (start.x > 100) $finish(0);
   endrule: execute

endmodule: mkTop
