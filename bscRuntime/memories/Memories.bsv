package Memories;

import RegFile :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;


export MemCombRead(..);
export AsyncMem(..);
export MemId(..);
export mkCombMem;
export mkAsyncMem;

typedef UInt#(TLog#(n)) MemId#(numeric type n);

//these are the memory interfaces we suppport
//the first is used for memories that support combinational reads

interface MemCombRead#(type elem, type addr);
   method elem read(addr a);
   method Action write(addr a, elem b);
endinterface

//this one is used for asynchronous reads which involve a request and response
interface AsyncMem#(type elem, type addr, type id);
    method ActionValue#(id) req(addr a, elem b, Bool isWrite);
    method elem peekResp();
    method Bool checkRespId(id i);
    method Action resp();
endinterface

//wrapper around the built-in register file
module mkCombMem(MemCombRead#(elem, addr)) provisos(Bits#(elem, szElem), Bits#(addr, szAddr), Bounded#(addr));

    RegFile#(addr, elem) rf <- mkRegFileFull();

    method elem read(addr a);
       return rf.sub(a);
    endmethod

    method Action write(addr a, elem b);
        rf.upd(a, b);
    endmethod

endmodule

typedef struct { a addr; i id; } MemReq#(type a, type i) deriving (Eq, Bits);
//Todo build like..a real memory here on BRAMS or something
module mkAsyncMem(AsyncMem#(elem, addr, MemId#(inflight))) provisos(Bits#(elem, szElem), Bits#(addr, szAddr), Bounded#(addr));

    Reg#(MemId#(inflight)) nextId <- mkReg(0);
    RegFile#(addr, elem) rf <- mkRegFileFull();
    FIFOF#(MemReq#(addr, MemId#(inflight))) reqs <- mkSizedFIFOF(valueOf(inflight));

    elem nextOut = rf.sub(reqs.first.addr);
    MemId#(inflight) respId = reqs.first.id;

    method ActionValue#(MemId#(inflight)) req(addr a, elem b, Bool isWrite);
        if (isWrite)
            begin
                rf.upd(a, b);
                return ?;
            end
        else
            begin
                reqs.enq(MemReq { addr: a, id: nextId} );
                nextId <= nextId + 1;
                return nextId;
            end
    endmethod

    method Bool checkRespId(MemId#(inflight) a);
        return respId == a;
    endmethod

    method elem peekResp();
        return nextOut;
    endmethod

     method Action resp();
        reqs.deq();
     endmethod

endmodule

endpackage
