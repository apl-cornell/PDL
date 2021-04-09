package Memories;

import RegFile :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import BRAMCore::*;
import DReg :: *;
import Vector :: *;


typedef UInt#(TLog#(n)) LockId#(numeric type n);
typedef UInt#(TLog#(n)) MemId#(numeric type n);

//Types of memories X Locks:
//For the built-in types of locks & mems:

interface AsyncMem#(type addr, type elem, type mid);
   method ActionValue#(mid) req(addr a, elem b, Bool isWrite);
   method elem peekResp(mid a);
   method Bool checkRespId(mid a);
   method Action resp(mid a);
endinterface

// (General vs. Addr Specific) X (Combinational vs. Async)
interface QueueLock#(type id);
   method ActionValue#(id) res();
   method Bool owns(id i);
   method Action rel(id i);
   method Bool isEmpty();
   method Bool canRes();
endinterface

interface QueueLockCombMem#(type addr, type elem, type id);
   method elem read(addr a);
   method Action write(addr a, elem b);
   interface QueueLock#(id) lock;   
endinterface

interface QueueLockAsyncMem#(type addr, type elem, type rid, type lid);
   method ActionValue#(rid) req(addr a, elem b, Bool isWrite);
   method elem peekResp(rid i);
   method Bool checkRespId(rid i);
   method Action resp(rid i);
   interface QueueLock#(lid) lock;
endinterface


module mkAsyncMem(BRAM_PORT #(addr, elem) memory, AsyncMem#(addr, elem, MemId#(inflight)) _unused_)
   provisos(Bits#(addr, szAddr), Bits#(elem, szElem));
   
   let outDepth = valueOf(inflight);
   
   //this must be at least size 2 to work correctly (safe bet)
   Vector#(inflight, Reg#(elem)) outData <- replicateM( mkReg(unpack(0)) );
   Vector#(inflight, Reg#(Bool)) valid <- replicateM( mkReg(False) );
   
   Reg#(MemId#(inflight)) head <- mkReg(0);
   Bool okToRequest = valid[head] == False;
   
   Reg#(Maybe#(MemId#(inflight))) nextData <- mkDReg(tagged Invalid);
   rule moveToOutFifo (nextData matches tagged Valid.idx);
      outData[idx] <= memory.read;
      valid[idx] <= True;
   endrule
   
   method ActionValue#(MemId#(inflight)) req(addr a, elem b, Bool isWrite) if (okToRequest);
      memory.put(isWrite, a, b);
      head <= head + 1;
      nextData <= tagged Valid head;
      return head;
   endmethod
      
   method elem peekResp(MemId#(inflight) a);
      return outData[a];
   endmethod
      
   method Bool checkRespId(MemId#(inflight) a);
      return valid[a] == True;
   endmethod
      
   method Action resp(MemId#(inflight) a);
      valid[a] <= False;
   endmethod
   
endmodule

module mkQueueLock(QueueLock#(LockId#(d)));

   Reg#(LockId#(d)) nextId <- mkReg(0);
   FIFOF#(LockId#(d)) held <- mkSizedFIFOF(valueOf(d));
   
   Reg#(LockId#(d)) cnt <- mkReg(0);
   
   Bool lockFree = !held.notEmpty;
   LockId#(d) owner = held.first;

   method Bool isEmpty();
      return lockFree;
   endmethod
   
   method Bool canRes();
      return held.notFull;
   endmethod
   
   //Returns True if thread `tid` already owns the lock
   method Bool owns(LockId#(d) tid);
      return owner == tid;
   endmethod
	       
   //Releases the lock iff thread `tid` owns it already
   method Action rel(LockId#(d) tid);
      if (owner == tid)
	 begin
	    held.deq();
	 end
   endmethod
   
   //Reserves the lock and returns the associated id
   method ActionValue#(LockId#(d)) res();
      held.enq(nextId);
      nextId <= nextId + 1;
      cnt <= cnt + 1;
      return nextId;
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

module mkQueueLockAsyncMem(BRAM_PORT#(addr, elem) memory, QueueLockAsyncMem#(addr, elem, MemId#(inflight), LockId#(d)) _unused_)
   provisos(Bits#(addr, szAddr), Bits#(elem, szElem));
   
   AsyncMem#(addr, elem, MemId#(inflight)) amem <- mkAsyncMem(memory);
   QueueLock#(LockId#(d)) l <- mkQueueLock();
   
   method ActionValue#(MemId#(inflight)) req(addr a, elem b, Bool isWrite);
      let r <- amem.req(a, b, isWrite);
      return r;
   endmethod
   
   method elem peekResp(MemId#(inflight) i);
      return amem.peekResp(i);
   endmethod
   
   method Bool checkRespId(MemId#(inflight) i);
      return amem.checkRespId(i);
   endmethod
   
   method Action resp(MemId#(inflight) i);
      amem.resp(i);
   endmethod   
   
   interface lock = l;
   
endmodule
   
endpackage
