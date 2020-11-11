package Memories;

import RegFile :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import BRAMCore::*;
import DReg :: *;
import Vector :: *;

export CombMem(..);
export AsyncMem(..);
export MemId(..);
export mkCombMem;
export mkLat1Mem;

typedef UInt#(TLog#(n)) MemId#(numeric type n);

//these are the memory interfaces we suppport
//the first is used for memories that support combinational reads

interface CombMem#(type elem, type addr);
   method elem read(addr a);
   method Action write(addr a, elem b);
endinterface

//this one is used for asynchronous reads which involve a request and response
interface AsyncMem#(type elem, type addr, type id);
    method ActionValue#(id) req(addr a, elem b, Bool isWrite);
    method elem peekResp(id i);
    method Bool checkRespId(id i);
    method Action resp(id i);
endinterface

//wrapper around the built-in register file
module mkCombMem(CombMem#(elem, addr)) provisos(Bits#(elem, szElem), Bits#(addr, szAddr), Bounded#(addr));

    RegFile#(addr, elem) rf <- mkRegFileFull();

    method elem read(addr a);
       return rf.sub(a);
    endmethod

    method Action write(addr a, elem b);
        rf.upd(a, b);
    endmethod

endmodule

// typedef struct { Bool isWrite; a addr; d data; i id; } MemReq#(type a, type d, type i) deriving (Eq, Bits);
// module mkAsyncMem(AsyncMem#(elem, addr, MemId#(inflight))) provisos(Bits#(elem, szElem), Bits#(addr, szAddr), Bounded#(addr));

//     Reg#(MemId#(inflight)) nextId <- mkReg(0);
//     RegFile#(addr, elem) rf <- mkRegFileFull();
//     FIFOF#(MemReq#(addr, elem, MemId#(inflight))) reqs <- mkSizedFIFOF(valueOf(inflight));

//     elem nextOut = rf.sub(reqs.first.addr);
//     MemId#(inflight) respId = reqs.first.id;


//     method ActionValue#(MemId#(inflight)) req(addr a, elem b, Bool isWrite);
//         nextId <= nextId + 1;
//         reqs.enq(MemReq { isWrite: isWrite, addr: a, data: b, id: nextId} );
//         return nextId;
//     endmethod
   
//     method Bool checkRespId(MemId#(inflight) a);
//         return respId == a;
//     endmethod

//     method elem peekResp();
//         return nextOut;
//     endmethod

//      method Action resp();
//         if (reqs.first.isWrite) rf.upd(reqs.first.addr, reqs.first.data);
// 	reqs.deq();
//      endmethod

// endmodule

//SizedReg is totally stolen from the BSC source library (BRAM.bsv) - its just not an exported interface/module
//////////////////////////////////////////////////////////////////////////////////
// SizedReg allow an elaboration time argument to determine the register width.
// However, the method are limited to compile time arguments.
// This interface allows a hard set,  2 concurrent add ports, and various compare
// value methods.
//Not exported
interface SizedReg ;
   method Action _write (Integer i);
   method Action addA (Integer i);
   method Action addB (Integer d);
   method Bool isLessThan (Integer i);
   method Bool isGreaterThan (Integer i);
   method Bool isEqualTo (Integer i);
endinterface

//Not exported
module mkSizedReg (Integer maxVal, Integer initialVal, SizedReg ifc);
   SizedReg _ifc = ? ;
   if (maxVal < 2)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(2) '(fromInteger(initialVal)) );
   else if (maxVal < 4)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(3) '(fromInteger(initialVal)) );
   else if (maxVal < 8)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(4) '(fromInteger(initialVal)) );
   else if (maxVal < 16)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(5) '(fromInteger(initialVal)) );
   else if (maxVal < 32)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(6) '(fromInteger(initialVal)) );
   else if (maxVal < 64)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(7) '(fromInteger(initialVal)) );
   else if (maxVal < 128)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(8) '(fromInteger(initialVal)) );
   else if (maxVal < 256)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(9) '(fromInteger(initialVal)) );
   else if (maxVal < 512)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(10) '(fromInteger(initialVal)) );
   else if (maxVal < 1024)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(11) '(fromInteger(initialVal)) );
   else if (maxVal < 2*1024)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(12) '(fromInteger(initialVal)) );
   else if (maxVal < 4*1024)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(13) '(fromInteger(initialVal)) );
   else if (maxVal < 8*1024)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(14) '(fromInteger(initialVal)) );
   else if (maxVal < 16*1024)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(15) '(fromInteger(initialVal)) );
   else if (maxVal < 32*1024)
      (*hide*)
      _ifc <- mkSizedRegInt(Int#(16) '(fromInteger(initialVal)) );
   else
      _ifc = error ("Sized Register is too big: " + fromInteger (maxVal) );
   return _ifc;
endmodule

//Not exported
// scheduling is should be forced so that (read, is*) SB (addA, addB) SB  write
module mkSizedRegInt (a init, SizedReg ifc)
   provisos (Bits#(a,sa),
             Arith#(a),
             Eq#(a),
             Ord#(a) );

   (*hide*)
   Reg#(a) _cnt <- mkReg(init);
   (*hide*)
   RWire#(a)  _adda <- mkRWire ;
   (*hide*)
   RWire#(a)  _addb <- mkRWire ;
   (*hide*)
   RWire#(a)  _fwrite <- mkRWire ;

   rule finalAdd ( isValid(_adda.wget) || isValid (_addb.wget) || isValid(_fwrite.wget) ) ;
      if (_fwrite.wget matches tagged Valid .w) begin
         _cnt <= w;
      end
      else begin
         a va = fromMaybe (0, _adda.wget);
         a vb = fromMaybe (0, _addb.wget);
         _cnt <= _cnt + va + vb ;
      end
   endrule

   method Action _write (Integer i);
      _fwrite.wset (fromInteger(i));
   endmethod
   method Action addA (Integer i);
      _adda.wset (fromInteger(i));
   endmethod
   method Action addB (Integer i);
      _addb.wset (fromInteger(i));
   endmethod
   method Bool isLessThan (Integer i) ;
      return _cnt < fromInteger (i);
   endmethod
   method Bool isGreaterThan (Integer i) ;
      return _cnt > fromInteger (i);
   endmethod
   method Bool isEqualTo (Integer i) ;
      return _cnt == fromInteger (i);
   endmethod
endmodule


//this is a wrapper for a bram module with exactly 1 cycle latency
module mkLat1Mem(AsyncMem#(elem, addr, MemId#(inflight))) provisos(Bits#(elem, szElem), Bits#(addr, szAddr), Bounded#(addr));
  
   let memSize = 2 ** valueOf(szAddr);
   let hasOutputReg = False;
   BRAM_PORT #(addr, elem) memory <- mkBRAMCore1(memSize, hasOutputReg);
   
   let outDepth = valueOf(inflight);
   
   Vector#(inflight, Reg#(elem)) outData <- replicateM( mkReg(unpack(0)) );
   Vector#(inflight, Reg#(Bool)) valid <- replicateM( mkReg(False) );
   
   Reg#(MemId#(inflight)) head <- mkReg(0);
   Reg#(MemId#(inflight)) tail <- mkReg(0);
   Bool empty = head == tail;
   Bool full = tail == head + 1;
   Bool okToRequest = !full;
   
   Reg#(Maybe#(MemId#(inflight))) nextData <- mkDReg(tagged Invalid);
   rule moveToOutFifo (nextData matches tagged Valid.idx);
      outData[idx] <= memory.read;
      valid[idx] <= True;
   endrule
   
   rule updateTail(valid[tail] == False && !empty);
      tail <= tail + 1;
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
