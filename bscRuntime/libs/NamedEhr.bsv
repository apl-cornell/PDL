package Named;

import RegFile :: *;
import Ehr :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import BRAMCore::*;
import DReg :: *;
import Vector :: *;

export LSQ(..);
export mkLSQ;

interface LSQ#(type addr, type elem, type name);
   
endinterface;

//TODO make size of queues a parameter
module mkLSQ#(parameter Bool init, parameter String fileInit)(AsyncMem#(elem, addr, name)) provisos
   (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Eq#(addr), Literal#(name));

   let memSize = 2 ** valueOf(szAddr);
   let hasOutputReg = False;
   BRAM_PORT #(addr, elem) memory <- (init) ? mkBRAMCore1Load(memSize, hasOutputReg, fileInit, False) : mkBRAMCore1(memSize, hasOutputReg);

   //TODO make size of store queue parameter
   Integer stSize = 5;
   Vector#(5, Reg#(addr)) stQ <- replicateM( mkReg(unpack(0)));
   Vector#(5, Reg#(Maybe#(elem))) stDQ <- replicateM (mkReg(tagged Invalid));
   //TODO make size of ld queue parameter

   function Maybe#(name) getMatchingStore(addr a);
      Maybe#(name) result = tagged Invalid;
      for (Integer i = 0; i < stSize; i = i + 1) begin
	 if (result matches tagged Invalid &&& stQ[i] == a)
	    result = tagged Valid fromInteger(i);
      end
      return result;
   endfunction
   
   //TODO
   method ActionValue#(name) reserveEntry(addr a, Bool isWrite);
      return unpack(0);
   endmethod
   //TODO
   method Action write(name n, elem b);
   endmethod
   //TODO
   method Bool isValid(name n);
      return False;
   endmethod
   //TODO
   method elem read(name n);
      return unpack(0);
   endmethod
   //TODO
   method Action commit(name n);
   endmethod
endmodule

endpackage
