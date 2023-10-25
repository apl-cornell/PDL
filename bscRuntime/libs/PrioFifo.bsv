// PrioFifo.bsv
package PrioFifo;

import FIFOF :: *;
import RWire :: *;


export mkNBFIFOF;

module mkNBFIFOF(FIFOF#(dtyp)) provisos (Bits#(dtyp, szdtyp));
   
   FIFOF#(dtyp) f <- mkFIFOF();
   //allow multiple writes in the same cycle
   RWire#(dtyp) enq_data <- mkRWireSBR();

   (*fire_when_enabled*)
   rule doEnq (enq_data.wget() matches tagged Valid.d);
      f.enq(d);
   endrule
   //only allow the FIRST enq each cycle to work, drop the others
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
