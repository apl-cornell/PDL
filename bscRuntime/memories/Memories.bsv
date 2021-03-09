package Memories;

import RegFile :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import BRAMCore::*;
import DReg :: *;
import Vector :: *;

export CombMem(..);
export AsyncMem(..);
export CombAddrMem(..);
export AsyncAddrMem(..);
export MemId(..);

typedef UInt#(TLog#(n)) MemId#(numeric type n);

//Types of memories X Locks:

// (General vs. Addr Specific) X (Combinational vs. Async)
interface CombMem#(type elem, type addr, type id); //General Comb
   method ActionValue#(id) res();
   method Bool owns(id i);
   method elem read(addr a);
   method Action write(addr a, elem b);
   method Action rel(id i);
endinterface

interface AsyncMem#(type elem, type addr, type lid, type rid); //General Async
   method ActionValue#(lid) res();
   method Bool owns(lid i);
   method ActionValue#(rid) req(addr a, elem b, Bool isWrite);
   method elem peekResp(rid i);
   method Bool checkRespId(rid i);
   method Action resp(rid i);
   method Action rel(lid i);
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


endpackage
