package Named;

import RegFile :: *;
import FIFO :: *;
import BRAMCore::*;
import DReg :: *;
import Vector :: *;

export CombMem(..);
export AsyncMem(..);

export mkRenameRF;
export mkLSQ;

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
   (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Eq#(addr), PrimIndex#(name, nn), Ord#(name), Arith#(name));

   let memSize = 2 ** valueOf(szAddr);
   let hasOutputReg = False;
   BRAM_PORT #(addr, elem) memory <- (init) ? mkBRAMCore1Load(memSize, hasOutputReg, fileInit, False) : mkBRAMCore1(memSize, hasOutputReg);

   ///Store Stuff
   Reg#(name) stHead <- mkReg(unpack(0));
   Vector#(StQSize, Reg#(Bool)) stQValid <- replicateM (mkReg(False));
   Vector#(StQSize, Reg#(addr)) stQAddr <- replicateM (mkReg(unpack(0)));
   Vector#(StQSize, Reg#(Maybe#(elem))) stQData <- replicateM (mkReg(tagged Invalid));
   FIFO#(StIssue#(addr, elem)) stIssueQ <- mkFIFO();   
   ///Load Stuff
   Reg#(name) ldHead <- mkReg(unpack(0));         
   Vector#(LdQSize, Reg#(Bool)) ldQValid <- replicateM (mkReg(False));
   Vector#(LdQSize, Reg#(addr)) ldQAddr <- replicateM (mkReg(unpack(0)));
   Vector#(LdQSize, Reg#(Maybe#(elem))) ldQData <- replicateM (mkReg(tagged Invalid));
   Vector#(LdQSize, Reg#(Maybe#(name))) ldQStr <- replicateM (mkReg(tagged Invalid));
   Vector#(LdQSize, Reg#(Bool)) ldQIssued <- replicateM(mkReg(False));   

   Bool okToSt = !stQValid[stHead];
   Bool okToLd = !ldQValid[ldHead];

   //return true if a is older than b, given a queue head (oldest entry) h
   function Bool isOlder(name a, name b, name h);
      let nohmid = a < b && !(a < h && b >= h);
      let hmid = b < h && a >= h;
      return nohmid || hmid;
   endfunction
   
   function Bool isNewer(name a, name b, name h);
      return !isOlder(a, b, h);
   endfunction

   function Bool isNewerStore(name a, name b);
      return isNewer(a, b, stHead);
   endfunction
   
   function Bool isOlderLoad(name a, name b);
      return isOlder(a, b, ldHead);
   endfunction
   
   //search starting at the _newest_ store
   //newest store is at head - 1 (and go backwards)
   function Maybe#(name) getMatchingStore(addr a);
      
      Maybe#(name) result = tagged Invalid;
      for (Integer i = 0; i < valueOf(StQSize); i = i + 1) begin
	 if (stQValid[i] && stQAddr[i] == a)
	    begin
	       if (result matches tagged Valid.idx)
		  begin
		     if (isNewerStore(fromInteger(i), idx)) result = tagged Valid fromInteger(i);
		  end
	       else result = tagged Valid fromInteger(i);
	    end
      end
      return result;
   endfunction
   
   //search starting at the _oldest_ load (which is at head)
   function Maybe#(name) getIssuingLoad();
      Maybe#(name) result = tagged Invalid;
      for (Integer i = 0; i < valueOf(LdQSize); i = i + 1) begin
	 if (ldQValid[i] && !isValid(ldQData[i]) && !isValid(ldQStr[i]) && !ldQIssued[i])
	    begin
	       if (result matches tagged Valid.idx)
		  begin
		     if (isOlderLoad(fromInteger(i), idx)) result = tagged Valid fromInteger(i);
		  end
	       else result = tagged Valid fromInteger(i);
	    end
      end
      return result;
   endfunction
   
   rule issueSt;
      let st = stIssueQ.first();
      memory.put(True, st.a, st.d);
      stIssueQ.deq();
   endrule
   
   Reg#(Maybe#(name)) nextData <- mkDReg(tagged Invalid);
   
   rule issueLd (getIssuingLoad matches tagged Valid.idx);
      memory.put(False, ldQAddr[idx], ?);
      nextData <= tagged Valid idx;
      ldQIssued[idx] <= True;
   endrule
   
   rule moveLdData (nextData matches tagged Valid.idx);
      ldQData[idx] <= tagged Valid memory.read;
   endrule
      
   method ActionValue#(name) reserveRead(addr a) if (okToLd);
      Maybe#(name) matchStr = getMatchingStore(a);
      Maybe#(elem) data = tagged Invalid;
      //if matching store, copy its data over (which may be invalid)
      if (matchStr matches tagged Valid.idx)
	 begin
	    data = stQData[idx];
	 end
      //If data is valid, then leave matching store invalid so no dependency
      if (data matches tagged Valid.d)
	 begin
	    matchStr = tagged Invalid;
	 end
      
      ldQValid[ldHead] <= True;
      ldQAddr[ldHead] <= a;
      ldQData[ldHead] <= data;
      ldQStr[ldHead] <= matchStr;
      
      ldHead <= ldHead + 1;
      return ldHead;
   endmethod
   
   method ActionValue#(name) reserveWrite(addr a) if (okToSt);
      stQValid[stHead] <= True;
      stQAddr[stHead] <= a;
      stQData[stHead] <= tagged Invalid;
      stHead <= stHead + 1;
      return stHead;
   endmethod
   

   method Action write(name n, elem b);
      stQData[n] <= tagged Valid b;
      //forward data to all dependent loads
      for (Integer i = 0; i < valueOf(LdQSize); i = i + 1) begin
	 if (ldQStr[i] matches tagged Valid.s &&& s == n)
	    begin
	       ldQStr[i] <= tagged Invalid;
	       ldQData[i] <= tagged Valid b;
	    end
      end
   endmethod

   //checks if it's safe to read data associated w/ ldq entry
   method Bool isValid(name n);
      //TODO we could maybe ignore the ldQValid[n] check
      //this should only be called on valid entries
      return ldQValid[n] && isValid(ldQData[n]);
   endmethod


   method elem read(name n);
      if (ldQData[n] matches tagged Valid.data)
	 return data;
      else
	 return unpack(0);
   endmethod

   //Load may or may not ever have been issued to main mem
   method Action commitRead(name n);
      ldQValid[n] <= False;
      ldQStr[n] <= tagged Invalid;
      ldQIssued[n] <= False;
   endmethod
   
   //Only Issue stores after committing
   method Action commitWrite(name n);
      stQValid[n] <= False;
      elem data = unpack(0);
      if (stQData[n] matches tagged Valid.dt)
	 data = dt;
      stIssueQ.enq(StIssue { a: stQAddr[n], d: data });
   endmethod

endmodule

endpackage
