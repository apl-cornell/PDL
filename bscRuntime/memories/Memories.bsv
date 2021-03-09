package Memories;

import RegFile :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import BRAMCore::*;
import DReg :: *;
import Vector :: *;

export GeneralLock(..);
export CombMem(..);
export AsyncMem(..);
export GeneralCombMem(..);
export GeneralAsyncMem(..);
export CombAddrMem(..);
export AsyncAddrMem(..);
export MemId(..);

export mkCombMem;

typedef UInt#(TLog#(n)) MemId#(numeric type n);

//Types of memories X Locks:

// (General vs. Addr Specific) X (Combinational vs. Async)
interface GeneralLock#(type id);
   method ActionValue#(id) res();
   method Bool owns(id i);
   method Action rel(id i);
endinterface

interface CombMem#(type elem, type addr);
   method elem read(addr a);
   method Action write(addr a, elem b);
endinterface

interface GeneralCombMem#(type elem, type addr, type id);
   interface CombMem#(elem, addr) mem;
   interface GeneralLock#(id) lock;   
endinterface

interface AsyncMem#(type elem, type addr, type rid);
   method ActionValue#(rid) req(addr a, elem b, Bool isWrite);
   method elem peekResp(rid i);
   method Bool checkRespId(rid i);
   method Action resp(rid i);
endinterface

interface GeneralAsyncMem#(type elem, type addr, type rid, type lid);
   interface AsyncMem#(elem, addr, rid) mem;
   interface GeneralLock#(lid) lock;
endinterface

//these are the memory interfaces we suppport
//the first is used for memories that support combinational reads
interface CombAddrMem#(type elem, type addr, type name); //Addr Comb
   method name readName(addr a); //get name to read data later
   method Bool isValid(name n);  //check if safe to read
   method elem read(name a);    //do the read
   method ActionValue#(name) allocName(addr a); //allocate a new name to be written
   method Action write(name a, elem b); //write data given allocated name
   method Action commit(name a); //indicate old name for a can be "freed"
   // method Action abort(name a); use for speculative threads that die so name a can be "freed" since not going to be written
endinterface

//this one is used for asynchronous reads which involve a request and response
interface AsyncAddrMem#(type elem, type addr, type name); //Addr Async
   method ActionValue#(name) reserveRead(addr a);
   method ActionValue#(name) reserveWrite(addr a);
   method Action write(name n, elem b); //start executing the write
   method Bool isValid(name n); //check if reading the value returns a valid value
   method elem read(name n); //do the read
   method Action commitRead(name n); //indicate we're done reading/writing this location
   method Action commitWrite(name n);
   //method Action abort(name a); use for speculative threads that die so name a can be thrown out
endinterface

module mkCombMem(RegFile#(addr, elem) rf, CombMem#(elem, addr) _unused_);
   
   method elem read(addr a);
      return rf.sub(a);
   endmethod
      
   method Action write(addr a, elem b);
      rf.upd(a, b);
   endmethod
endmodule

module mkAsyncMem(BRAM_PORT #(addr, elem) memory, AsyncMem#(elem, addr, MemId#(inflight)) _unused_)
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

endpackage
