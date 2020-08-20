import FIFO :: *;
import SpecialFIFOs :: * ;

typedef struct { Bit#(10) y; } InputN deriving(Bits, Eq);
typedef struct { Bool cond; } InputJoin deriving(Bits, Eq);

interface StageIf;
   method Action recv(InputN d_in);
endinterface

interface StageLeft;
   method Action recv(InputN d_in);
endinterface

interface StageJoin;
   method Action recvBool(InputJoin d_in);
   method Action recvLeft(InputN d_in);
   method Action recvRight(InputN d_in);
endinterface


//Maybe should pull the communication FIFOs out of the
//modules so that we can actually instantiate feedback loops properly?

module mkStageIf (StageLeft left, StageLeft right, StageJoin joinstg, StageIf stg);
 
   FIFO #(InputN) inputF <- mkPipelineFIFO;
   InputN first = inputF.first();
   Bit#(10) outvar = first.y;   
   Bool cond = outvar > 10'd100;

   rule executeLeft(cond);
      inputF.deq();
      left.recv(first);
      joinstg.recvBool(InputJoin{ cond: cond });
   endrule

   rule executeRight(!cond);
      inputF.deq();
      right.recv(first);
      joinstg.recvBool(InputJoin{ cond: cond });
   endrule

   method Action recv(InputN d_in);
      inputF.enq(d_in);
   endmethod

endmodule

module mkStageLeft (StageJoin joinstg, StageLeft stg);
   FIFO #(InputN) inputF <- mkPipelineFIFO;
   
   rule execute;
      InputN first = inputF.first;
      inputF.deq();
      joinstg.recvLeft(first);
   endrule
   
   method Action recv(InputN d_in);
      inputF.enq(d_in);
   endmethod

endmodule

module mkStageRight (StageJoin joinstg, StageLeft stg);
   FIFO #(InputN) inputF <- mkPipelineFIFO;
   
   rule execute;
      InputN first = inputF.first;
      inputF.deq();
      joinstg.recvRight(first);
   endrule
   
   method Action recv(InputN d_in);
      inputF.enq(d_in);
   endmethod

endmodule

module mkStageJoin (StageJoin stg);
   
   FIFO #(InputN) inputLeft <- mkPipelineFIFO;
   FIFO #(InputN) inputRight <- mkPipelineFIFO;
   FIFO #(Bool) inputBool <- mkSizedFIFO(3);

   Bool cond = inputBool.first;
   
   Wire #(InputN) val <- mkWire;
   Reg #(Bool) fv <- mkReg(False);
   
   rule readLeft(cond && !fv);
      inputBool.deq();
      val <= inputLeft.first;
      inputLeft.deq();
   endrule
   
   rule readRight(!cond && !fv);
      inputBool.deq();
      val <= inputRight.first;
      inputRight.deq();
   endrule
   
   (* fire_when_enabled *)
   rule execute(!fv);
      $display("val is %d; %d", val.y, $time());
   endrule
   
   rule reset(fv);
      fv <= False;
   endrule
      
   method Action recvBool(InputJoin d_in);
      inputBool.enq(d_in.cond);
   endmethod
   
   method Action recvLeft(InputN d_in);
      inputLeft.enq(d_in);
   endmethod
      
   method Action recvRight(InputN d_in);
      inputRight.enq(d_in);
   endmethod

endmodule

(* synthesize *)
module mkTop();

   StageJoin joinstg <- mkStageJoin();
   StageLeft leftstg <- mkStageLeft(joinstg);
   StageLeft rightstg <- mkStageRight(joinstg);
   StageIf ifstg <- mkStageIf(leftstg, rightstg, joinstg);
   Reg #(InputN) start <- mkReg(InputN{ y:10'h000 });

   rule execute;
      start <= InputN { y: start.y + 1 };
      ifstg.recv(start);
      if (start.y > 100) $finish(0);   
   endrule: execute
endmodule: mkTop
