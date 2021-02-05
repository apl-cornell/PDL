package Named;

import RegFile :: *;
import FIFO :: *;
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
   method ActionValue#(name) reserveRead(addr a);
   method ActionValue#(name) reserveWrite(addr a);
   method Action write(name n, elem b); //start executing the write
   method Bool isValid(name n); //check if reading the value returns a valid value
   method elem read(name n); //do the read
   method Action commitRead(name n); //indicate we're done reading/writing this location
   method Action commitWrite(name n);
   //method Action abort(name a); use for speculative threads that die so name a can be thrown out
endinterface
   
//TODO make physical size a parameter (rather than dictated by type of name)
module mkRenameRF#(parameter Integer aregs, parameter Integer pregs, parameter Bool init, parameter String fileInit)(CombMem#(elem, addr, name)) provisos
   (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Bounded#(name),
    PrimIndex#(addr, an), PrimIndex#(name, nn));
   
   RegFile#(name, elem) regfile <- (init) ? mkRegFileLoad(fileInit, 0, fromInteger(pregs - 1)) : mkRegFile(0, fromInteger(pregs - 1));
   
   //Initial mapping for all arch regs is identity
   module mkMapEntry#(Integer i)(Reg#(name));
      let r <- mkReg(unpack(fromInteger(i)));
      return r;
   endmodule

      module mkFreeEntry#(Integer i)(Reg#(Bool));
      let b = i < aregs ? False : True;
      let r <- mkReg(b);
      return r;
   endmodule
   
   module mkOldEntry#(Integer i)(Reg#(name));
      let r <- mkReg(fromInteger(i));
      return r;
   endmodule
   
   Vector#(TExp#(szAddr), Reg#(name)) namefile <- genWithM(mkMapEntry);
   Vector#(TExp#(szName), Reg#(Bool)) busyfile <- genWithM(mkFreeEntry);
   Vector#(TExp#(szName), Reg#(Bool)) freeList <- genWithM(mkFreeEntry);
   Vector#(TExp#(szName), Reg#(name)) oldNames <- genWithM(mkOldEntry);

   function Maybe#(name) getFreeName();
      Maybe#(name) result = tagged Invalid;
      for (Integer i = 0; i < pregs; i = i + 1) begin
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
      busyfile[n] <= True;
      freeList[n] <= False;
      oldNames[n] <= namefile[a];
      namefile[a] <= n;
      return n;
   endmethod
   
   //Writing data makes it no longer busy
   method Action write(name n, elem b);
      regfile.upd(n, b);
      busyfile[n] <= False;
   endmethod
   
   //Frees register that this one overwrote
   method Action commit(name n);
      $display("Freed name %d at %t", oldNames[n], $time());
      freeList[oldNames[n]] <= True;
   endmethod
   
endmodule

typedef struct {
   addr a;
   Maybe#(data) d;
   Bool isValid;
} StQEntry#(type addr, type data) deriving(Bits, Eq);

typedef struct {
   addr a;
   data d;
} StIssue#(type addr, type data) deriving(Bits, Eq);

typedef struct {
   addr a;
   Maybe#(data) d;
   Maybe#(entId) str;
   Bool isValid;
} LdQEntry#(type addr, type data, type entId) deriving(Bits, Eq);

`define STQ_SIZE 4
typedef `STQ_SIZE StQSize;
`define LDQ_SIZE 4
typedef `LDQ_SIZE LdQSize;

//TODO make size of queues a parameter
module mkLSQ#(parameter Bool init, parameter String fileInit)(AsyncMem#(elem, addr, name)) provisos
   (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Eq#(addr), PrimIndex#(name, nn), Arith#(name));

   let memSize = 2 ** valueOf(szAddr);
   let hasOutputReg = False;
   BRAM_PORT #(addr, elem) memory <- (init) ? mkBRAMCore1Load(memSize, hasOutputReg, fileInit, False) : mkBRAMCore1(memSize, hasOutputReg);

   Vector#(StQSize, Reg#(StQEntry#(addr, elem))) stQ <- replicateM( mkReg(StQEntry { a: unpack(0), d: tagged Invalid, isValid: False }));
   FIFO#(StIssue#(addr, elem)) stIssueQ <- mkFIFO();
   Vector#(LdQSize, Reg#(LdQEntry#(addr, elem, name))) ldQ <- replicateM( mkReg(LdQEntry { a: unpack(0), d: tagged Invalid, str: tagged Invalid, isValid: False }));
   Vector#(LdQSize, Reg#(Bool)) ldIssued <- replicateM(mkReg(False));
   
   Reg#(name) stHead <- mkReg(unpack(0));
   Reg#(name) stIssue <- mkReg(unpack(0));
   Reg#(name) ldHead <- mkReg(unpack(0));      
   
   Bool okToSt = stQ[stHead].isValid == False;
   Bool okToLd = ldQ[ldHead].isValid == False;
   
   //search starting at the _newest_ store
   //newest store is at head - 1 (and go backwards)
   function Maybe#(name) getMatchingStore(addr a);
      Maybe#(name) result = tagged Invalid;
      for (name i = (stHead - 1); i != stHead; i = i - 1) begin
	 if (result matches tagged Invalid &&& stQ[i].isValid &&& stQ[i].a == a)
	    result = tagged Valid i;
      end
      if (result matches tagged Invalid &&& stQ[stHead].isValid &&& stQ[stHead].a == a)
	 result = tagged Valid stHead;
      return result;
   endfunction
   
   //search starting at the _oldest_ load (which is at head)
   function Maybe#(name) getIssuingLoad();
      Maybe#(name) result = tagged Invalid;
      let newest = ldHead - 1;
      for (name l = ldHead; l != newest; l = l + 1) begin
	 //no result, load is valid, not dependent on a store and not already issued
	 if (result matches tagged Invalid &&& ldQ[l].isValid &&&
	    !(isValid(ldQ[l].str)) &&& !ldIssued[l])
	    result = tagged Valid l;
      end
      if (result matches tagged Invalid &&& ldQ[newest].isValid &&&
	  !(isValid(ldQ[newest].str)) &&& !ldIssued[newest])
	 result = tagged Valid newest;
      return result;
	    
   endfunction

   rule issueSt;
      let st = stIssueQ.first();
      memory.put(True, st.a, st.d);
      stIssueQ.deq();
   endrule
   
   Reg#(Maybe#(name)) nextData <- mkDReg(tagged Invalid);
   
   rule issueLd (getIssuingLoad matches tagged Valid.idx);
      let ld = ldQ[idx];
      memory.put(False, ld.a, ?);
      nextData <= tagged Valid idx;
      ldIssued[idx] <= True;
   endrule
   
   rule moveLdData (nextData matches tagged Valid.idx);
      let ld = ldQ[idx];
      ldQ[idx] <= LdQEntry { a: ld.a, d: tagged Valid memory.read, str: ld.str, isValid: ld.isValid };
   endrule
   
   method ActionValue#(name) reserveRead(addr a) if (okToLd);
      Maybe#(name) matchStr = getMatchingStore(a);
      Maybe#(elem) data = tagged Invalid;
      //if matching store, copy its data over (which may be invalid)
      if (matchStr matches tagged Valid.idx)
	 begin
	    data = stQ[idx].d;
	 end
      //If data is valid, then leave matching store invalid so no dependency
      if (data matches tagged Valid.d)
	 begin
	    matchStr = tagged Invalid;
	 end
      LdQEntry#(addr, elem, name) nentry = LdQEntry { a: a, d: data, str: matchStr, isValid: True };
      ldQ[ldHead] <= nentry;
      ldHead <= ldHead + 1;
      return ldHead;
   endmethod
   
   method ActionValue#(name) reserveWrite(addr a) if (okToSt);
      StQEntry#(addr, elem) nentry = StQEntry { a: a, d: tagged Invalid, isValid: True };
      stQ[stHead] <= nentry;
      stHead <= stHead + 1;
      return stHead;
   endmethod
   

   method Action write(name n, elem b);
      let entry = stQ[n];
      let nentry = StQEntry { a: entry.a, d: tagged Valid b, isValid: entry.isValid };
      stQ[n] <= nentry;
      //forward data to dependent loads
      for (Integer i = 0; i < valueOf(LdQSize); i = i + 1) begin
	 let lentry = ldQ[i];
	 if (lentry.str matches tagged Valid.s &&& s == n)
	    ldQ[i] <= LdQEntry { a: lentry.a, d: tagged Valid b, str: tagged Invalid, isValid: lentry.isValid };
      end
   endmethod

   //checks if it's safe to read data associated w/ ldq entry
   method Bool isValid(name n);
      let entry = ldQ[n];
      //TODO we could maybe ignore the entry.isValid check since
      //this should only be called on valid entries
      return (entry.isValid && isValid(entry.d));
   endmethod


   method elem read(name n);
      if (ldQ[n].d matches tagged Valid.data)
	 return data;
      else
	 return unpack(0);
   endmethod

   //Load may or may not ever have been issued to main mem
   method Action commitRead(name n);
      ldQ[n] <= LdQEntry { a: ?, d: ?, str: tagged Invalid, isValid: False };
      ldIssued[n] <= False;
   endmethod
   
   //Only Issue stores after committing
   method Action commitWrite(name n);
      stQ[n] <= StQEntry { a: ?, d: ?, isValid: False };
      elem data = unpack(0);
      if (stQ[n].d matches tagged Valid.dt)
	 data = dt;
      stIssueQ.enq(StIssue { a: stQ[n].a, d: data });
   endmethod

endmodule

endpackage
