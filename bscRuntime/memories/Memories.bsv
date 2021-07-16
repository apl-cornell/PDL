package Memories;

import RegFile :: *;
import FIFO :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import BRAMCore::*;
import DReg :: *;
import ConfigReg :: *;
import Vector :: *;
import Locks :: *;
import Ehr :: *;
import RevertingVirtualReg :: *;

export MemId(..);
export BramPort(..);
export AsyncMem(..);
export QueueLockCombMem(..);
export QueueLockAsyncMem(..);
export BypassLockCombMem(..);
export AddrLockCombMem(..);
export AddrLockAsyncMem(..);
export LSQ(..);

export mkRegFile;
export mkBramPort;
export mkQueueLockCombMem;
export mkQueueLockAsyncMem;
export mkFAAddrLockCombMem;
export mkFAAddrLockAsyncMem;
export mkDMAddrLockCombMem;
export mkDMAddrLockAsyncMem;
export mkLSQ;
export mkBypassLockCombMem;

typedef UInt#(TLog#(n)) MemId#(numeric type n);

//return true if a is older than b, given a queue head (oldest entry) h
function Bool isOlder(UInt#(sz) a, UInt#(sz) b, UInt#(sz) h);
   let nohmid = a < b && !(a < h && b >= h);
   let hmid = b < h && a >= h;
   return nohmid || hmid;
endfunction

function Bool isNewer(UInt#(sz) a, UInt#(sz) b, UInt#(sz) h);
   return !isOlder(a, b, h);
endfunction

//Types of memories X Locks:
//For the built-in types of locks & mems:

interface BramPort#(type addr, type elem, type mid, numeric type nsz);
   interface BRAM_PORT_BE#(addr, elem, nsz) port;
endinterface


interface AsyncMem#(type addr, type elem, type mid, numeric type nsz);
   method ActionValue#(mid) req(addr a, elem b, Bit#(nsz) wmask);
   method elem peekResp(mid a);
   method Bool checkRespId(mid a);
   method Action resp(mid a);
endinterface

// (General vs. Addr Specific) X (Combinational vs. Async)

interface QueueLockCombMem#(type addr, type elem, type id);
   method elem read(addr a);
   method Action write(addr a, elem b);
   interface QueueLock#(id) lock;   
endinterface

interface QueueLockAsyncMem#(type addr, type elem, type rid, numeric type nsz, type lid);
   interface AsyncMem#(addr, elem, rid, nsz) mem;
   interface QueueLock#(lid) lock;
endinterface

interface AddrLockCombMem#(type addr, type elem, type id, numeric type size);
   method elem read (addr a);
   method Action write(addr a, elem b);
   interface AddrLock#(id, addr, size) lock;
endinterface

interface BypassLockCombMem#(type addr, type elem, type id, numeric type size);  
   method Bool canRead(addr a);
   method Bool canWrite(addr a);
   method Bool canRes(addr a);
   method ActionValue#(id) reserve(addr a);
   method Bool owns(id i);
   method Action commit(id i);
   method elem read(addr a);
   method Action write(id a, elem b);
endinterface

interface AddrLockAsyncMem#(type addr, type elem, type rid, numeric type nsz, type lid, numeric type size);
   interface AsyncMem#(addr, elem, rid, nsz) mem;
   interface AddrLock#(lid, addr, size) lock;
endinterface

interface LSQ#(type addr, type elem, type name, numeric type nsz);
   interface AsyncMem#(name, elem, name, nsz) mem;
   method ActionValue#(name) reserveRead(addr a);
   method ActionValue#(name) reserveWrite(addr a);
   method Bool isValid(name n);
   method Action commitRead(name n);
   method Action commitWrite(name n);
endinterface

module mkRegFile#(parameter Bool init, parameter String initFile)(RegFile#(addr, elem))
   provisos (Bits#(addr,szAddr), Bits#(elem,szElem), Bounded#(addr));
   RegFile#(addr, elem) rf;
   if (init)
      rf <- mkRegFileWCFLoad(initFile, minBound, maxBound);
   else
      rf <- mkRegFileWCF(minBound, maxBound);
   return rf;
endmodule

module mkBramPort#(parameter Bool init, parameter String file)(BramPort#(addr, elem, MemId#(inflight), nsz))
   provisos (Bits#(addr,szAddr), Bits#(elem,szElem), Mul#(TDiv#(szElem, nsz), nsz, szElem));
   BRAM_PORT_BE#(addr, elem, nsz) p;
   let memSize = 2 ** valueOf(szAddr);
   let hasOutputReg = False;
   if (init)
      p <- mkBRAMCore1BELoad(memSize, hasOutputReg, file, False);
   else
      p <- mkBRAMCore1BE(memSize, hasOutputReg);
   
   interface port = p;

endmodule

//data read from memory made available IMMEDIATELY
//rep & resp can go in any order
module mkAsyncMem(BramPort#(addr, elem, MemId#(inflight), n) memwrap, AsyncMem#(addr, elem, MemId#(inflight), n) _unused_)
   provisos(Bits#(addr, szAddr), Bits#(elem, szElem));
   
   let memory = memwrap.port;
   let outDepth = valueOf(inflight);
   
   //this must be at least size 2 to work correctly (safe bet)
   Vector#(inflight, Ehr#(2, elem)) outData <- replicateM( mkEhr(unpack(0)) );
   Vector#(inflight, Ehr#(2, Bool)) valid <- replicateM( mkEhr(False) );
   
   Reg#(MemId#(inflight)) head <- mkReg(0);
   Wire#(MemId#(inflight)) freeEntry <- mkWire();
      
   Bool okToRequest = valid[head][1] == False;
   
   Reg#(Maybe#(MemId#(inflight))) nextData <- mkDReg(tagged Invalid);
   (*fire_when_enabled*)
   rule moveToOutFifo (nextData matches tagged Valid.idx);
      outData[idx][0] <= memory.read;
      valid[idx][0] <= True;
   endrule

   (*fire_when_enabled*)
   rule freeResp;
      valid[freeEntry][1] <= False;
   endrule
   
   method ActionValue#(MemId#(inflight)) req(addr a, elem b, Bit#(n) wmask) if (okToRequest);
      memory.put(wmask, a, b);
      head <= head + 1;
      nextData <= tagged Valid head;
      return head;
   endmethod
      
   method elem peekResp(MemId#(inflight) a);
      return outData[a][1];
   endmethod
      
   method Bool checkRespId(MemId#(inflight) a);
      return valid[a][1] == True;
   endmethod
      
   //Make this invisible to other ops this cycle but happen at any time
   method Action resp(MemId#(inflight) a);
      freeEntry <= a;
   endmethod
   
endmodule

module mkQueueLockCombMem(RegFile#(addr, elem) rf, QueueLockCombMem#(addr, elem, LockId#(d)) _unused_);

   QueueLock#(LockId#(d)) l <- mkQueueLock();
   
   interface lock = l;
   
   method elem read(addr a);
      return rf.sub(a);
   endmethod
      
   method Action write(addr a, elem b);
      rf.upd(a, b);
   endmethod
   
endmodule

module mkFAAddrLockCombMem(RegFile#(addr, elem) rf, AddrLockCombMem#(addr, elem, LockId#(d), numlocks) _unused_)
   provisos (Bits#(addr, szAddr), Eq#(addr));

   AddrLock#(LockId#(d), addr, numlocks) l <- mkFAAddrLock();
   method elem read(addr a);
      return rf.sub(a);
   endmethod
   
   method Action write(addr a, elem b);
      rf.upd(a, b);
   endmethod
   
   interface lock = l;
endmodule

module mkDMAddrLockCombMem(RegFile#(addr, elem) rf, AddrLockCombMem#(addr, elem, LockId#(d), numlocks) _unused_)
   provisos (PrimIndex#(addr, szAddr));

   AddrLock#(LockId#(d), addr, numlocks) l <- mkDMAddrLock();
   method elem read(addr a);
      return rf.sub(a);
   endmethod
   
   method Action write(addr a, elem b);
      rf.upd(a, b);
   endmethod
   
   interface lock = l;
endmodule
   
module mkQueueLockAsyncMem(BramPort#(addr, elem, MemId#(inflight), n) memory, QueueLockAsyncMem#(addr, elem, MemId#(inflight), n, LockId#(d)) _unused_)
   provisos(Bits#(addr, szAddr), Bits#(elem, szElem));
   
   AsyncMem#(addr, elem, MemId#(inflight), n) amem <- mkAsyncMem(memory);
   QueueLock#(LockId#(d)) l <- mkQueueLock();
      
   interface mem = amem;
   interface lock = l;
   
endmodule

module mkFAAddrLockAsyncMem(BramPort#(addr, elem, MemId#(inflight), n) memory, AddrLockAsyncMem#(addr, elem, MemId#(inflight), n, LockId#(d), numlocks) _unused_)
   provisos(Bits#(addr, szAddr), Bits#(elem, szElem), Eq#(addr));
   
   AsyncMem#(addr, elem, MemId#(inflight), n) amem <- mkAsyncMem(memory);
   AddrLock#(LockId#(d), addr, numlocks) l <- mkFAAddrLock();
   
   interface mem = amem;
   interface lock = l;
   
endmodule

module mkDMAddrLockAsyncMem(BramPort#(addr, elem, MemId#(inflight), n) memory, AddrLockAsyncMem#(addr, elem, MemId#(inflight), n, LockId#(d), numlocks) _unused_)
   provisos(PrimIndex#(addr, szAddr), Bits#(addr, szAddr), Bits#(elem, szElem));
   
   AsyncMem#(addr, elem, MemId#(inflight), n) amem <- mkAsyncMem(memory);
   AddrLock#(LockId#(d), addr, numlocks) l <- mkDMAddrLock();
   
   interface mem = amem;
   interface lock = l;
   
endmodule


typedef struct {
   data d;
   mask m;
} StReq#(type data, type mask) deriving (Bits, Eq);

typedef struct {
   addr a;
   mask m;
   data d;
} StIssue#(type addr, type data, type mask) deriving(Bits, Eq);

typedef struct {
   addr a;
   Maybe#(data) d;
   Maybe#(entId) str;
   Bool isValid;
} LdQEntry#(type addr, type data, type entId) deriving(Bits, Eq);

module mkLSQ(BramPort#(addr, elem, MemId#(inflight), n) memwrap,
 LSQ#(addr, elem, MemId#(inflight), n) _unused_) provisos
   (Bits#(elem, szElem), Bits#(addr, szAddr), Eq#(addr));

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

   let memory = memwrap.port;


   ///Store Stuff
   Reg#(MemId#(inflight)) stHead <- mkReg(unpack(0));
   Vector#(inflight, Ehr#(2, Bool)) stQValid <- replicateM(mkEhr(False));
   Vector#(inflight, Reg#(addr)) stQAddr <- replicateM (mkReg(unpack(0)));
   Vector#(inflight, Ehr#(3, Maybe#(StReq#(elem, Bit#(n))))) stQData <- replicateM (mkEhr(tagged Invalid));
   FIFO#(StIssue#(addr, elem, Bit#(n))) stIssueQ <- mkFIFO();
   ///Load Stuff
   Reg#(MemId#(inflight)) ldHead <- mkReg(unpack(0));
   Vector#(inflight, Ehr#(2, Bool)) ldQValid <- replicateM (mkEhr(False));
   Vector#(inflight, Reg#(addr)) ldQAddr <- replicateM (mkReg(unpack(0)));
   Vector#(inflight, Ehr#(3, Maybe#(elem))) ldQData <- replicateM (mkEhr(tagged Invalid));
   Vector#(inflight, Ehr#(3, Maybe#(MemId#(inflight)))) ldQStr <- replicateM (mkEhr(tagged Invalid));
   Vector#(inflight, Ehr#(2, Bool)) ldQIssued <- replicateM(mkEhr(False));

   //check with beginning of cycle values
   Bool okToSt = !stQValid[stHead][0];
   Bool okToLd = !ldQValid[ldHead][0];
   function Bool isNewerStore(MemId#(inflight) a, MemId#(inflight) b);
      return isNewer(a, b, stHead);
   endfunction
   
   function Bool isOlderLoad(MemId#(inflight) a, MemId#(inflight) b);
      return isOlder(a, b, ldHead);
   endfunction
   
   //search starting at the _newest_ store
   //newest store is at head - 1 (and go backwards)
   function Maybe#(MemId#(inflight)) getMatchingStore(addr a);
      
      Maybe#(MemId#(inflight)) result = tagged Invalid;
      for (Integer i = 0; i < valueOf(inflight); i = i + 1) begin
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
   function Maybe#(MemId#(inflight)) getIssuingLoad();
      Maybe#(MemId#(inflight)) result = tagged Invalid;
      for (Integer i = 0; i < valueOf(inflight); i = i + 1) begin
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
   
   //ldQStr[i][1] -> read & write _after_ reserves (write to [0])
   //ldQData[i][1] ->  write _after_ reserve
   function Action write(MemId#(inflight) n, elem b, Bit#(n) wmask);
      return action
		stQData[n][1] <= tagged Valid StReq { d: b, m: wmask }; //_can_ reserve and write same location in one cycle (write happens after)
				    //forward data to all dependent loads
				    for (Integer i = 0; i < valueOf(inflight); i = i + 1) begin
				       if (ldQStr[i][1] matches tagged Valid.s &&& s == n)
					  begin
					     ldQStr[i][1] <= tagged Invalid;					     
					     //order this after reserve (so reserve addr ;write addr forwards data appropriately)
					     if (wmask == '1) ldQData[i][1] <= tagged Valid b;
					     else ldQData[i][1] <= tagged Invalid;
					     //If the store doesn't actually write all of the data, we need to do a real load so don't forward
					  end
				    end
	     endaction;
   endfunction

   function elem read(MemId#(inflight) n);
      //this index needs to be >= used by isValid
      //0 => implies data must have been written last cycle & reservation made last cycle
      //1 => reservation may have been made this cycle
      //2 => data may have been written this cycle
      if (ldQData[n][0] matches tagged Valid.data)
	 return data;
      else
	 return unpack(0);
   endfunction

   //TODO avoid starvation between issueSt and issueLd (currently one always has precedence over the other)
   //this shouldn't cause liveness issues in real designs but we would need to deal w/ this
   //when considering other memory models   
   rule issueSt;
      let st = stIssueQ.first();
      memory.put(st.m, st.a, st.d);
      stIssueQ.deq();
//      $display("Issuing Memory Store for addr %d, data %d, %t", st.a, st.d, $time());
   endrule
   
   Reg#(Maybe#(MemId#(inflight))) nextData <- mkDReg(tagged Invalid);
   
   //run this _after_ commits so that we don't issue a load that's getting freed this cycle
   rule issueLd (getIssuingLoad matches tagged Valid.idx);
  //    $display("Issuing Memory Load for tag %d, addr %d, %t", idx, ldQAddr[idx], $time());
      memory.put(0, ldQAddr[idx], ?);
      nextData <= tagged Valid idx;
      ldQIssued[idx][1] <= True;
   endrule
   
   rule moveLdData (nextData matches tagged Valid.idx);
      //schedule this last for simplicity (can change later)
      ldQData[idx][2] <= tagged Valid memory.read;
   endrule
      
   method ActionValue#(MemId#(inflight)) reserveRead(addr a) if (okToLd);
      Maybe#(MemId#(inflight)) matchStr = getMatchingStore(a);
      Maybe#(StReq#(elem, Bit#(n))) rreq = tagged Invalid;
      Maybe#(elem) data = tagged Invalid;
      //if matching store, copy its data over (which may be invalid)
      if (matchStr matches tagged Valid.idx)
	 begin
	    rreq = stQData[idx][0]; //changing this index could enable combinational bypass
	 end
      //If data is valid and mask is full, then leave matching store invalid so no dependency
      if (rreq matches tagged Valid.r &&& r.m == '1)
	 begin
	    matchStr = tagged Invalid;
	    data = tagged Valid r.d;
	 end
      
      ldQValid[ldHead][0] <= True;
      ldQAddr[ldHead] <= a;
      ldQData[ldHead][0] <= data;
      ldQStr[ldHead][0] <= matchStr;
      
      ldHead <= ldHead + 1;
      return ldHead;
   endmethod
   
   method ActionValue#(MemId#(inflight)) reserveWrite(addr a) if (okToSt);
      //Using index [0] means these are the first writes -- [1] reads can combinationally observe these writes
      stQValid[stHead][0] <= True;
      stQAddr[stHead] <= a;
      stQData[stHead][0] <= tagged Invalid;
      stHead <= stHead + 1;
      return stHead;
   endmethod
   


   //checks if it's safe to read data associated w/ ldq entry
   method Bool isValid(MemId#(inflight) n);
      //TODO we could maybe ignore the ldQValid[n] check
      //this should only be called on valid entries
      return ldQValid[n][0] && isValid(ldQData[n][0]); //read early (0) so can't observe written values -> will need to wait until next cycle
   //if we increase these EHR indices, we could allow comb bypass
   endmethod

   //Load may or may not ever have been issued to main mem
   //write _after_ all others
   method Action commitRead(MemId#(inflight) n);
      ldQValid[n][1] <= False;
      ldQStr[n][2] <= tagged Invalid;
      ldQIssued[n][0] <= False;
   endmethod
   
   //Only Issue stores after committing
   method Action commitWrite(MemId#(inflight) n);
      stQValid[n][1] <= False;
      elem data = unpack(0);
      Bit#(n) wmask = '0;
      if (stQData[n][2] matches tagged Valid.dt) //if _write_ occurred this cycle we want to observe it
	 begin
	    data = dt.d;
	    wmask = dt.m;
	 end
      stIssueQ.enq(StIssue { a: stQAddr[n], m: wmask, d: data });
   endmethod

   interface AsyncMem mem;
      method ActionValue#(MemId#(inflight)) req(MemId#(inflight) a, elem b, Bit#(n) wmask);
	 write(a, b, wmask);
	 return a;
      endmethod

      method elem peekResp(MemId#(inflight) i);
	 return read(i);
      endmethod

      //Dummy methods needed to fit interface
      method Bool checkRespId(MemId#(inflight) i);
	 return True;
      endmethod

      method Action resp(MemId#(inflight) i);
      endmethod

   endinterface
   
endmodule

typedef struct {
   addr a;
   data d;
   } WriteReq#(type addr, type data) deriving (Bits, Eq);

module mkBypassLockCombMem(RegFile#(addr, elem) rf, BypassLockCombMem#(addr, elem, LockId#(n), n) _unused_)
   provisos (Bits#(addr, szAddr), Bits#(elem, szElem), Eq#(addr));

   Vector#(n, Reg#(Maybe#(addr))) resVec <- replicateM(mkConfigReg(tagged Invalid));
   Vector#(n, Reg#(Maybe#(elem))) dataVec <- replicateM(mkConfigReg(tagged Invalid));   
   Vector#(n, RWire#(elem)) bypassWire <- replicateM(mkRWireSBR());

   Reg#(LockId#(n)) head <- mkReg(0);
   Reg#(LockId#(n)) owner <- mkConfigReg(0);

   Wire#(LockId#(n)) toCommit <- mkWire();
   
   function Maybe#(LockId#(n)) getMatchingEntry(addr a);
      Maybe#(LockId#(n)) result = tagged Invalid;
      for (Integer i = 0; i < valueOf(n); i = i + 1) begin
	 if (resVec[i] matches tagged Valid.raddr &&& raddr == a)
	    begin
	       if (result matches tagged Valid.idx)
		  begin
		     if (isNewer(fromInteger(i), idx, head)) result = tagged Valid fromInteger(i);
		  end
	       else result = tagged Valid fromInteger(i);
	    end
	 end
      return result;
   endfunction
   
   
   Bool headFree = !isValid(resVec[head]);
   Bool unowned = !isValid(resVec[owner]);
   
   function Maybe#(elem) readBypassData(LockId#(n) ent);
      let vecData = dataVec[ent];
      let wData   = bypassWire[ent];
      Maybe#(elem) result = tagged Invalid;
      if (wData.wget matches tagged Valid.bdata) result = tagged Valid bdata;      
      else if (vecData matches tagged Valid.vdata) result = tagged Valid vdata;   

      return result;
   endfunction
/**
   rule debug(True);
      for (Integer j = 0; j < valueOf(n); j = j + 1) begin
	 $display("AddrValid %b Addr %d, Data Valid %b Data %d %t",
	    isValid(resVec[j]), fromMaybe(?, resVec[j]),
	    isValid(readBypassData(fromInteger(j))),
	    fromMaybe(?, readBypassData(fromInteger(j))), $time());
      end
   endrule **/
   
   (*fire_when_enabled*)
   rule doCommit;
      let rfdata = fromMaybe(?, readBypassData(toCommit));
      let rfaddr = fromMaybe(?, resVec[toCommit]);
      rf.upd(rfaddr, rfdata);
   endrule
   
   //can read THIS CycLe
   method Bool canRead(addr a);
      Bool canGo = False;
      let wdataEnt = getMatchingEntry(a);
      if (wdataEnt matches tagged Valid.id)
	 begin
	    canGo = isValid(readBypassData(id));
	 end
      else canGo = True;
      return canGo;
   endmethod
   
   //can write This Cycle
   method Bool canWrite(addr a);
      return unowned;
   endmethod
   
   method Bool canRes(addr a);
      return headFree;
   endmethod
   
   method ActionValue#(LockId#(n)) reserve(addr a) if (headFree);
      head <= head + 1;
      resVec[head] <= tagged Valid a;
      dataVec[head] <= tagged Invalid;
      return head;
   endmethod   
   
   method Bool owns(LockId#(n) id);
      return True;
   endmethod
   
   method Action commit(LockId#(n) id);
      resVec[id] <= tagged Invalid;
      owner <= owner + 1;
      toCommit <= id;
   endmethod
   
   method elem read(addr a);
      elem outdata = rf.sub(a);
      let wdataEnt = getMatchingEntry(a);
      if (wdataEnt matches tagged Valid.id) outdata = fromMaybe(?, readBypassData(id));
      return outdata;
   endmethod
   
   method Action write(LockId#(n) id, elem b);
      dataVec[id] <= tagged Valid b;
      bypassWire[id].wset(b);
   endmethod
   
endmodule


endpackage
