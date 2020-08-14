import FIFOF :: * ;

typedef struct { Bit#(10) y; } InputN deriving(Bits, Eq);
typedef struct { Bit#(10) z; } InputNPlusOne deriving(Bits, Eq);

interface StageN;
   method Action recv(InputN d_in);
endinterface

interface StageNPO;
   method Action recv(InputNPlusOne d_in);
endinterface


//Maybe should pull the communication FIFOs out of the
//modules so that we can actually instantiate feedback loops properly?

module mkStageN (FIFOF #(InputN) inputF, StageNPO nstage, StageN stg);
 
   rule execute;
      InputN first = inputF.first;
      Bit#(10) outvar = first.y;
      $display("send stage 2, %0d", $time());
      inputF.deq();
      nstage.recv( InputNPlusOne{ z: outvar } );
   endrule

   method Action recv(InputN d_in);
      inputF.enq(d_in);
   endmethod

endmodule: mkStageN

module mkStageNPlusOne(FIFOF #(InputN)  beginStg, StageNPO stg);

   FIFOF #(InputNPlusOne) inputF <- mkFIFOF;
    
   rule execute;
      $display("send stage 3, %0d,  %0b", $time(), inputF.first);
      inputF.deq();
      beginStg.enq( InputN { y: inputF.first.z + 1 } );
   endrule
   
   method Action recv(InputNPlusOne d_in);
      inputF.enq(d_in);
   endmethod
   

endmodule: mkStageNPlusOne

(* synthesize *)
module mkTop();

   FIFOF #(InputN) inputF <- mkFIFOF;
   StageNPO snpo <- mkStageNPlusOne(inputF);
   StageN sn <- mkStageN(inputF, snpo);

   Reg #(InputN) start <- mkReg(InputN{ y:10'h000 });
   Reg #(Bool) done <- mkReg(False);
   rule execute (!done);
      done <= True;
      start <= InputN { y: start.y + 1 };
      sn.recv(start);
      $display("send stage 1, %0d", $time());

   endrule: execute

   rule stop;
      start <= InputN { y: start.y + 1 };
      if (start.y > 100) $finish(0);   
   endrule
   
endmodule: mkTop
