// StgFIFOs.bsv
package StgFIFOs;

import Ehr :: *;
import ConfigReg :: *;
import FIFOF :: *;

export mkStgFIFOF;

// Change to a CReg, two element Queue for per cycle.
module mkStgFIFOF(FIFOF#(dtyp)) provisos (Bits#(dtyp, szdtyp));

   FIFOF#(dtyp) f <- mkFIFOF();
   //allow multiple writes in the same cycle
   RWire#(dtyp) enq_data <- mkRWireSBR();
   RWire#(Bool) doClear <- mkRWireSBR();

   //Make sure no enq could happen during clear (takes 2 cycles)

   (*conflict_free = "doEnqRule, doFIFOClearRule"*)
   (*fire_when_enabled, no_implicit_conditions*)
   rule doEnqRule (enq_data.wget() matches tagged Valid.d);
      f.enq(d);
   endrule

   (*fire_when_enabled, no_implicit_conditions*)
   rule doFIFOClearRule (doClear.wget() matches tagged Valid.d);
      f.clear();
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
      doClear.wset(True);
   endmethod

endmodule

endpackage
