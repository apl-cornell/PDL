package SpecialQueues;

import Ehr :: *;
import ConfigReg :: *;
import FIFOF :: *;

export OutputQ(..);

export mkOutputFIFOF;
export mkNBFIFOF;


interface OutputQ#(type tag, type data);
   method data first();
   method Bool canRead(tag t);
   method Action deq();
   method Bool canWrite(tag t);
   method Action enq(data d);
endinterface

module mkOutputFIFOF(ttyp initT, OutputQ#(ttyp, dtyp) res) provisos
   (Bits#(ttyp, szttyp),Bits#(dtyp, szdtyp), Arith#(ttyp), Eq#(ttyp));

   Ehr#(2, ttyp) nextTag <- mkEhr(initT);
   Reg#(Maybe#(dtyp)) val <- mkConfigReg(tagged Invalid);
   
   method dtyp first();
      if (val matches tagged Valid.x)
	 return x;
      else
	 return ?;
   endmethod
   
   method Bool canRead(ttyp t);
      return nextTag[0] == t && isValid(val);
   endmethod
   
   method Action deq();
      nextTag[0] <= nextTag[0] + 1;
      val <= tagged Invalid;
   endmethod
   //Order write operations _after_ read operations
   //to allow concurrent deq and enq
   method Bool canWrite(ttyp t);
      return nextTag[1] == t;
   endmethod
   
   method Action enq(dtyp d);
      val <= tagged Valid d;
   endmethod   
   
endmodule

module mkNBFIFOF(FIFOF#(dtyp)) provisos (Bits#(dtyp, szdtyp));
   
   FIFOF#(dtyp) f <- mkFIFOF();
   //allow multiple writes in the same cycle
   RWire#(dtyp) enq_data <- mkRWireSBR();
   
   (*fire_when_enabled*)
   rule doEnq (enq_data.wget() matches tagged Valid.d);
      f.enq(d);
   endrule

   //only allow the LAST enq each cycle to work, drop the others
   method Action enq(dtyp a) if (f.notFull());
      enq_data.wset(a);
   endmethod
   
   method Action deq();
      f.deq();
   endmethod
   
   method dtyp first();
      return f.first();
   endmethod
   
   method Bool notFull();
      return f.notFull();
   endmethod
   
   method Bool notEmpty();
      return f.notEmpty();
   endmethod
   
   method Action clear();
      f.clear();
   endmethod
   
endmodule


endpackage
