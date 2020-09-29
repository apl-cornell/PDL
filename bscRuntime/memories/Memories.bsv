package Memories;

import RegFile :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;



export MemCombRead(..);
export AsyncMem(..);
export mkCombMem;
export mkAsyncMem;

//these are the memory interfaces we suppport
//the first is used for memories that support combinational reads

interface MemCombRead#(type elem, type addr);
   method elem read(addr a);
   method Action write(addr a, elem b);
endinterface

//this one is used for asynchronous reads which involve a request and response
interface AsyncMem#(type elem, type addr);
    method Action readReq(addr a);
    method elem peekRead();
    method Bool checkAddr(addr a);
    method Action readResp();
    method Action write(addr a, elem b);
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

//Todo build like..a real memory here on BRAMS or something
module mkAsyncMem(AsyncMem#(elem, addr)) provisos(Bits#(elem, szElem), Bits#(addr, szAddr), Bounded#(addr));

    RegFile#(addr, elem) rf <- mkRegFileFull();
    FIFOF#(addr) reqs <- mkPipelineFIFOF();

    elem nextOut = rf.sub(reqs.first);

    method Action readReq(addr a);
        reqs.enq(a);
    endmethod

    method Bool checkAddr(addr a);
        return reqsfirst == a;
    endmethod

    method elem peekRead();
        return nextOut;
    endmethod

     method Action readResp();
        reqs.deq();
     endmethod

     method Action write(addr a, elem b);
        rf.upd(a, b);
     endmethod

endmodule

endpackage
