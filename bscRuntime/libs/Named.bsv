package Named;

import Memories :: *;
import RegFile :: *;
import FIFO :: *;
import BRAMCore::*;
import DReg :: *;
import Vector :: *;
import Ehr :: *;

export RenameRF(..);
export mkRenameRF;
export mkLSQ;

interface RenameRF#(type elem, type addr, type name);
   method name readName(addr a);
   method Bool isValid(name n);
   method elem read(name a);
   method ActionValue#(name) allocName(addr a);
   method Action write(name a, elem b);
   method Action commit(name a);
   // method Action abort(name a); use for speculative threads that die so name a can be "freed" since not going to be written
endinterface
   

module mkRenameRF#(parameter Integer aregs, parameter Integer pregs, parameter Bool init, parameter String fileInit)(
 RegFile#(name, elem) regfile, RenameRF#(elem, addr, name) _unused_) provisos
   (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Bounded#(name),
    PrimIndex#(addr, an), PrimIndex#(name, nn));
   
//TODO generate in testbench code (not part of the library)
//    RegFile#(name, elem) regfile <- (init) ? mkRegFileLoad(fileInit, 0, fromInteger(pregs - 1)) : mkRegFile(0, fromInteger(pregs - 1));
   
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
module mkLSQ#(parameter Bool init, parameter String fileInit)(BRAM_PORT #(addr, elem) memory, AsyncAddrMem#(elem, addr, name) _unused_) provisos
   (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Eq#(addr), PrimIndex#(name, nn), Ord#(name), Arith#(name));

   /*
    * Schedule for This LSQ
    * 
    * isValid < everything -> don't consider data written this cycle (avoid combinational bypass)
    * read < everything -> to match isValid -> only read the beginning of cycle values
    * reserveRead < everything -> reads beginning of cycle values (for queue and current stores) (concurrent reserveWrite doesn't forward data)
    * reserveWrite < everything -> reads beginning of cycle state
    * reserves < write -> can write in the same cycle as reserving, also forwards data to load q
    * 
    * reserveRead < commitRead < issueLd -> can free ld entry at any time -> will not issue mem request if freed in same cycle.
    * ld response is always 1 cycle, so an issued ld will always have a place to put its data.
    * (if issueLd; commitread next cycle, then data will be written, but just never used, won't overwrite anything important)
    * 
    * everything < commitWrite -> can commit write in the same cycle as writing the data (gets pushed to store issue queue)
    */
   

   //TODO generate in testbench code
   //    let memSize = 2 ** valueOf(szAddr);
   //    let hasOutputReg = False;
   //    BRAM_PORT #(addr, elem) memory <- (init) ? mkBRAMCore1Load(memSize, hasOutputReg, fileInit, False) : mkBRAMCore1(memSize, hasOutputReg);

   ///Store Stuff
   Reg#(name) stHead <- mkReg(unpack(0));
   Vector#(StQSize, Ehr#(2, Bool)) stQValid <- replicateM(mkEhr(False));
   Vector#(StQSize, Reg#(addr)) stQAddr <- replicateM (mkReg(unpack(0)));
   Vector#(StQSize, Ehr#(3, Maybe#(elem))) stQData <- replicateM (mkEhr(tagged Invalid));
   FIFO#(StIssue#(addr, elem)) stIssueQ <- mkFIFO();
   ///Load Stuff
   Reg#(name) ldHead <- mkReg(unpack(0));
   Vector#(LdQSize, Ehr#(2, Bool)) ldQValid <- replicateM (mkEhr(False));
   Vector#(LdQSize, Reg#(addr)) ldQAddr <- replicateM (mkReg(unpack(0)));
   Vector#(LdQSize, Ehr#(3, Maybe#(elem))) ldQData <- replicateM (mkEhr(tagged Invalid));
   Vector#(LdQSize, Ehr#(3, Maybe#(name))) ldQStr <- replicateM (mkEhr(tagged Invalid));
   Vector#(LdQSize, Ehr#(2, Bool)) ldQIssued <- replicateM(mkEhr(False));

   //check with beginning of cycle values
   Bool okToSt = !stQValid[stHead][0];
   Bool okToLd = !ldQValid[ldHead][0];

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
	 if (stQValid[i][0] && stQAddr[i] == a)
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
   
   //search starting at the _oldest_ load
   //always read start of cycle values ([0] from Ehrs) -> loads will issue (no earlier than)
   //the first cycle that they can issue
   function Maybe#(name) getIssuingLoad();
      Maybe#(name) result = tagged Invalid;
      for (Integer i = 0; i < valueOf(LdQSize); i = i + 1) begin
	 //read ldQIssued _after_ commit so we don't issue a load that just got freed
	 if (ldQValid[i][0] && !isValid(ldQData[i][0]) && !isValid(ldQStr[i][0]) && !ldQIssued[i][1])
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
   
   //TODO avoid starvation between issueSt and issueLd (currently one always has precedence over the other)
   //this shouldn't cause liveness issues in real designs but we would need to deal w/ this
   //when considering other memory models   
   rule issueSt;
      let st = stIssueQ.first();
      memory.put(True, st.a, st.d);
      stIssueQ.deq();
      $display("Issuing Memory Store for addr %d, data %d, %t", st.a, st.d, $time());
   endrule
   
   Reg#(Maybe#(name)) nextData <- mkDReg(tagged Invalid);
   
   //run this _after_ commits so that we don't issue a load that's getting freed this cycle
   rule issueLd (getIssuingLoad matches tagged Valid.idx);
      $display("Issuing Memory Load for tag %d, addr %d, %t", idx, ldQAddr[idx], $time());
      memory.put(False, ldQAddr[idx], ?);
      nextData <= tagged Valid idx;
      ldQIssued[idx][1] <= True;
   endrule
   
   rule moveLdData (nextData matches tagged Valid.idx);
      //schedule this last for simplicity (can change later)
      ldQData[idx][2] <= tagged Valid memory.read;
   endrule
      
   method ActionValue#(name) reserveRead(addr a) if (okToLd);
      Maybe#(name) matchStr = getMatchingStore(a);
      Maybe#(elem) data = tagged Invalid;
      //if matching store, copy its data over (which may be invalid)
      if (matchStr matches tagged Valid.idx)
	 begin
	    data = stQData[idx][0]; //changing this index could enable combinational bypass
	 end
      //If data is valid, then leave matching store invalid so no dependency
      if (data matches tagged Valid.d)
	 begin
	    matchStr = tagged Invalid;
	 end
      
      ldQValid[ldHead][0] <= True;
      ldQAddr[ldHead] <= a;
      ldQData[ldHead][0] <= data;
      ldQStr[ldHead][0] <= matchStr;
      
      ldHead <= ldHead + 1;
      return ldHead;
   endmethod
   
   method ActionValue#(name) reserveWrite(addr a) if (okToSt);
      //Using index [0] means these are the first writes -- [1] reads can combinationally observe these writes
      stQValid[stHead][0] <= True;
      stQAddr[stHead] <= a;
      stQData[stHead][0] <= tagged Invalid;
      stHead <= stHead + 1;
      return stHead;
   endmethod
   

   //ldQStr[i][1] -> read & write _after_ reserves (write to [0])
   //ldQData[i][1] ->  write _after_ reserve
   method Action write(name n, elem b);
      stQData[n][1] <= tagged Valid b; //_can_ reserve and write same location in one cycle (write happens after)
      //forward data to all dependent loads
      for (Integer i = 0; i < valueOf(LdQSize); i = i + 1) begin
	 if (ldQStr[i][1] matches tagged Valid.s &&& s == n)
	    begin
	       ldQStr[i][1] <= tagged Invalid;
	       //order this after reserve (so reserve addr ;write addr forwards data appropriately)
	       ldQData[i][1] <= tagged Valid b;
	    end
      end
   endmethod

   //checks if it's safe to read data associated w/ ldq entry
   method Bool isValid(name n);
      //TODO we could maybe ignore the ldQValid[n] check
      //this should only be called on valid entries
      return ldQValid[n][0] && isValid(ldQData[n][0]); //read early (0) so can't observe written values -> will need to wait until next cycle
   //if we increase these EHR indices, we could allow comb bypass
   endmethod


   method elem read(name n);
      //this index needs to be >= used by isValid
      //0 => implies data must have been written last cycle & reservation made last cycle
      //1 => reservation may have been made this cycle
      //2 => data may have been written this cycle
      if (ldQData[n][0] matches tagged Valid.data)
	 return data;
      else
	 return unpack(0);
   endmethod

   //Load may or may not ever have been issued to main mem
   //write _after_ all others
   method Action commitRead(name n);
      ldQValid[n][1] <= False;
      ldQStr[n][2] <= tagged Invalid;
      ldQIssued[n][0] <= False;
   endmethod
   
   //Only Issue stores after committing
   method Action commitWrite(name n);
      stQValid[n][1] <= False;
      elem data = unpack(0);
      if (stQData[n][2] matches tagged Valid.dt) //if _write_ occurred this cycle we want to observe it
	 data = dt;
      stIssueQ.enq(StIssue { a: stQAddr[n], d: data });
   endmethod

endmodule

endpackage
