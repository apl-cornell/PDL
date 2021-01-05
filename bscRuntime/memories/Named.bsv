package Named;

import RegFile :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import BRAMCore::*;
import DReg :: *;
import Vector :: *;

export CombMem(..);
export AsyncMem(..);
export MemId(..);

typedef UInt#(TLog#(n)) MemId#(numeric type n);

//these are the memory interfaces we suppport
//the first is used for memories that support combinational reads

interface CombMem#(type elem, type addr, type name);
   method name readName(addr a); //get name to read data later
   method Bool isValid(name n);  //check if safe to read
   method elem read(name a);    //do the read
   method ActionValue#(name) allocName(addr a); //allocate a new name to be written
   method Action write(name a, elem b); //write data given allocated name
   method Action commit(name a); //indicate old name for a can be "freed"
   // method Action abort(name a); use for speculative threads that die so name a can be "freed" since not going to be written
endinterface

typedef struct { Bool canRead; n name; } MapEntry#(type n) deriving (Eq, Bits);

module mkRenameRF#(parameter Bool init, parameter String fileInit)(CombMem#(elem, addr, name)) provisos
   (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Literal#(name), Bounded#(name), PrimIndex#(addr, an));
   
   RegFile#(name, elem) regfile <- (init) ? mkRegFileFullLoad(fileInit) : mkRegFileFull();
   module mkMapEntry#(Integer i)(Reg#(MapEntry#(name)));
      let r <- mkReg(MapEntry { canRead: False, name: fromInteger(i) });
      return r;
   endmodule
   Vector#(TExp#(szAddr), Reg#(MapEntry#(name))) namefile <- genWithM(mkMapEntry);

   
   //TODO
   method name readName(addr a);
      return namefile[a].name;
   endmethod
   
   //TODO
   method Bool isValid(name n);
      return False;
   endmethod
   
   //TODO
   method elem read(name n);
      return regfile.sub(n);
   endmethod 
   
   //TODO
   method ActionValue#(name) allocName(addr a);
      return namefile[a].name;
   endmethod
   
   //TODO
   method Action write(name n, elem b);
      let x = 0;
   endmethod
   
   //TODO
   method Action commit(name n);
      let x = 0;
   endmethod
   
endmodule

//TODO
//this one is used for asynchronous reads which involve a request and response
interface AsyncMem#(type elem, type addr, type id);
    method ActionValue#(id) req(addr a, elem b, Bool isWrite);
    method elem peekResp(id i);
    method Bool checkRespId(id i);
    method Action resp(id i);
endinterface

endpackage
