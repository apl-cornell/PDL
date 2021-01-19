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
   
//TODO make physical size a parameter (rather than dictated by type of name)
module mkRenameRF#(parameter Bool init, parameter String fileInit)(CombMem#(elem, addr, name)) provisos
   (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Bounded#(name),
    PrimIndex#(addr, an), PrimIndex#(name, nn));
   
   RegFile#(name, elem) regfile <- (init) ? mkRegFileFullLoad(fileInit) : mkRegFileFull();
   
   //Initial mapping for all arch regs is identity
   module mkMapEntry#(Integer i)(Reg#(name));
      let r <- mkReg(unpack(fromInteger(i)));
      return r;
   endmodule

   
   //Initially the arch-numbered physical registers are both NOT FREE and NOT BUSY, others are
   Integer numArch = valueOf(TExp#(szAddr));
   module mkFreeEntry#(Integer i)(Reg#(Bool));
      let b = i < numArch ? False : True;
      let r <- mkReg(b);
      return r;
   endmodule
   
   Vector#(TExp#(szAddr), Reg#(name)) namefile <- genWithM(mkMapEntry);
   Vector#(TExp#(szName), Reg#(Bool)) busyfile <- genWithM(mkFreeEntry);
   Vector#(TExp#(szName), Reg#(Bool)) freeList <- genWithM(mkFreeEntry);
   Vector#(TExp#(szName), Reg#(name)) oldNames <- replicateM(mkReg(fromInteger(0)));

   function Maybe#(name) getFreeName();
      Maybe#(name) result = tagged Invalid;
      for (Integer i = 0; i < valueOf(TExp#(szName)); i = i + 1) begin
	 if (result matches tagged Invalid &&& freeList[i])
	    result = tagged Valid fromInteger(i);
      end
      return result;
   endfunction
   
   method name readName(addr a);
      return namefile[a];
   endmethod
   
   //Valid if NOT busy
   method Bool isValid(name n);
      return !busyfile[n];
   endmethod
   
   method elem read(name n);
      return regfile.sub(n);
   endmethod 
   
   //if there is a free entry in the freelist then allocate it
   //and save old mapping for arch 
   //busyfile[n] = True is an invariant that should hold here
   method ActionValue#(name) allocName(addr a) if (getFreeName matches tagged Valid.n);   
      freeList[n] <= False;
      oldNames[n] <= namefile[a];
      return n;
   endmethod
   
   //Writing data makes it no longer busy
   method Action write(name n, elem b);
      regfile.upd(n, b);
      busyfile[n] <= False;
   endmethod
   
   //Frees register that this one overwrote
   //And reset its busy status
   method Action commit(name n);
      freeList[oldNames[n]] <= True;
      busyfile[oldNames[n]] <= True;
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
