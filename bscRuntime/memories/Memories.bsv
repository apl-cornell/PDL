package Memories;

import RegFile :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import BRAMCore::*;
import DReg :: *;
import Vector :: *;
import Locks :: *;

export MemId(..);
export QueueLockCombMem(..);
export QueueLockAsyncMem(..);

export mkQueueLockCombMem;
export mkQueueLockAsyncMem;

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

interface AddrLockCombMem#(type addr, type elem, type id, numeric type size);
   method elem read (addr a);
   method Action write(addr a, elem b);
   interface AddrLock#(id, addr, size) lock;
endinterface

interface AddrLockAsyncMem#(type addr, type elem, type rid, type lid, numeric type size);
   method ActionValue#(rid) req(addr a, elem b, Bool isWrite);
   method elem peekResp(rid i);
   method Bool checkRespId(rid i);
   method Action resp(rid i);
   interface AddrLock#(lid, addr, size) lock;
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

module mkFAAddrLockAsyncMem(BRAM_PORT#(addr, elem) memory, AddrLockAsyncMem#(addr, elem, MemId#(inflight), LockId#(d), numlocks) _unused_)
   provisos(Bits#(addr, szAddr), Bits#(elem, szElem), Eq#(addr));
   
   AsyncMem#(addr, elem, MemId#(inflight)) amem <- mkAsyncMem(memory);
   AddrLock#(LockId#(d), addr, numlocks) l <- mkFAAddrLock();
   
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

module mkDMAddrLockAsyncMem(BRAM_PORT#(addr, elem) memory, AddrLockAsyncMem#(addr, elem, MemId#(inflight), LockId#(d), numlocks) _unused_)
   provisos(PrimIndex#(addr, szAddr), Bits#(addr, szAddr), Bits#(elem, szElem));
   
   AsyncMem#(addr, elem, MemId#(inflight)) amem <- mkAsyncMem(memory);
   AddrLock#(LockId#(d), addr, numlocks) l <- mkDMAddrLock();
   
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
