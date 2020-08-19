import FIFOF :: * ;

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
 
   FIFOF #(InputN) inputF <- mkFIFOF;
   
   rule execute;
      InputN first = inputF.first;
      Bit#(10) outvar = first.y;
      inputF.deq();
      if (outvar[0:0] == 1'b1)
	 begin
	    $display("if stage true, %d", $time());
	    left.recv(first);
	    joinstg.recvBool(InputJoin{ cond: True });
	 end
      else
	 begin
	    $display("if stage false, %d", $time());
	    right.recv(first);
	    joinstg.recvBool(InputJoin{ cond: False });
	 end
   endrule

   method Action recv(InputN d_in);
      inputF.enq(d_in);
   endmethod

endmodule

module mkStageLeft (StageJoin joinstg, StageLeft stg);
   FIFOF #(InputN) inputF <- mkFIFOF;
   
   rule execute;
      InputN first = inputF.first;
      inputF.deq();
      $display("Value on Left %d", first.y);
      joinstg.recvLeft(first);
   endrule
   
   method Action recv(InputN d_in);
      inputF.enq(d_in);
   endmethod

endmodule

module mkStageRight (StageJoin joinstg, StageLeft stg);
   FIFOF #(InputN) inputF <- mkFIFOF;
   
   rule execute;
      InputN first = inputF.first;
      inputF.deq();
      $display("Value on Right %d", first.y);
      joinstg.recvRight(first);
   endrule
   
   method Action recv(InputN d_in);
      inputF.enq(d_in);
   endmethod

endmodule

module mkStageJoin (StageJoin stg);
   
   FIFOF #(InputN) inputLeft <- mkFIFOF;
   FIFOF #(InputN) inputRight <- mkFIFOF;
   FIFOF #(Bool) inputBool <- mkFIFOF;

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
      $display("deq right");
   endrule
   
   (* fire_when_enabled *)
   rule execute(!fv);
      $display("Time: %d", $time());
      $display("val is %d", val.y);
      fv <= True;
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
