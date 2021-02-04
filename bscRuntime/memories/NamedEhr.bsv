package Named;

import RegFile :: *;
import Ehr :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import BRAMCore::*;
import DReg :: *;
import Vector :: *;

export CombMem(..);
export AsyncMem(..);
export MemId(..);

export mkRenameRF;

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


//TODO
//this one is used for asynchronous reads which involve a request and response
interface AsyncMem#(type elem, type addr, type name);
   method ActionValue#(name) reserveEntry(addr a, Bool isWrite); //reserve an operational slot for either loading or storing
   method Action write(name n, elem b); //start executing the write
   method Bool isValid(name n); //check if reading the value returns a valid value
   method elem read(name n); //do the read
   method Action commit(name n); //indicate we're done reading/writing this location
   //method Action abort(name a); use for speculative threads that die so name a can be thrown out
endinterface
   

module mkRenameRF#(parameter Integer aregs, parameter Integer pregs, parameter Bool init, parameter String fileInit)(CombMem#(elem, addr, name)) provisos
   (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Bounded#(name),
    PrimIndex#(addr, an), PrimIndex#(name, nn));
   

   RegFile#(name, elem) regfile <- (init) ? mkRegFileLoad(fileInit, 0, fromInteger(pregs - 1)) : mkRegFile(0, fromInteger(pregs - 1));
   
   //Initial mapping for all arch regs is identity
   module mkMapEntry#(Integer i)(Reg#(name));
      let r <- mkReg(unpack(fromInteger(i)));
      return r;
   endmodule

   function Bool freeFromInteger(Integer i);
      return i < aregs ? False : True;
   endfunction
   
   module mkFreeEntry#(Integer i)(Reg#(Bool));
      let b = i < aregs ? False : True;
      let r <- mkReg(b);
      return r;
   endmodule
   
   module mkOldEntry#(Integer i)(Reg#(name));
      let r <- mkReg(fromInteger(i));
      return r;
   endmodule
   
   //initially arch i -> phys i
   Vector#(TExp#(szAddr), Ehr#(1, name)) namefile <- genWithM(compose(mkEhr, fromInteger));
   Vector#(TExp#(szName), Ehr#(2, Bool)) busyfile <- genWithM(compose(mkEhr, freeFromInteger));
   Vector#(TExp#(szName), Ehr#(2, Bool)) freeList <- genWithM(compose(mkEhr, freeFromInteger));
   Vector#(TExp#(szName), Ehr#(2, name)) oldNames <- genWithM(compose(mkEhr, fromInteger));

   function Maybe#(name) getFreeName();
      Maybe#(name) result = tagged Invalid;
      for (Integer i = 0; i < pregs; i = i + 1) begin
	 if (result matches tagged Invalid &&& freeList[i][0])
	    result = tagged Valid fromInteger(i);
      end
      return result;
   endfunction
   
   method name readName(addr a);
      return namefile[a][0];
   endmethod
   
   //Valid if NOT busy
   //read before writes to regfile, since writes not bypassed
   method Bool isValid(name n);
      return !busyfile[n][1];
   endmethod
   
   method elem read(name n);
      return regfile.sub(n);
   endmethod 
   
   //if there is a free entry in the freelist then allocate it
   //and save old mapping for arch 
   //busyfile[n] = True is an invariant that should hold here
   method ActionValue#(name) allocName(addr a) if (getFreeName matches tagged Valid.n);   
      busyfile[n][0] <= True;
      freeList[n][0] <= False;
      oldNames[n][0] <= namefile[a][0];
      namefile[a][0] <= n;
      return n;
   endmethod
   
   //Writing data makes it no longer busy
   //write busyfile after checking validity
   method Action write(name n, elem b);
      regfile.upd(n, b);
      busyfile[n][1] <= False;
   endmethod
   
   //Frees register that this one overwrote
   //And reset its busy status
   method Action commit(name n);
//      $display("Freed name %d at %t", oldNames[n], $time());
      let oldn = oldNames[n][1];
      freeList[oldn][1] <= True;
   endmethod
   
endmodule

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
