package Memories;

import GetPut :: *;
import ClientServer :: *;
import FIFO :: *;
import FIFOF :: *;
import BRAMCore::*;
import DReg :: *;
import ConfigReg :: *;
import Vector :: *;


export MemId(..);
export BramPort(..);
export AsyncMem(..);

export mkBramPort;
export mkAsyncMem;

typedef UInt#(TLog#(n)) MemId#(numeric type n);

//Types of memories X Locks:
//For the built-in types of locks & mems:

interface BramPort#(type addr, type elem, type mid, numeric type nsz);
   interface Server#(Tuple3#(Bit#(nsz), addr, elem), elem) bram_server;
endinterface

interface AsyncMem#(type addr, type elem, type mid, numeric type nsz);
   method ActionValue#(mid) req(addr a, elem b, Bit#(nsz) wmask);
   method elem peekResp(mid a);
   method Bool checkRespId(mid a);
   method Action resp(mid a);
   interface Client#(Tuple3#(Bit#(nsz), addr, elem), elem) bram_client;
endinterface


module mkBramPort#(parameter Bool init, parameter String file)(BramPort#(addr, elem, MemId#(inflight), nsz))
   provisos (Bits#(addr,szAddr), Bits#(elem,szElem), Mul#(TDiv#(szElem, nsz), nsz, szElem));
   BRAM_PORT_BE#(addr, elem, nsz) p;
   let memSize = 2 ** valueOf(szAddr);
   let hasOutputReg = False;
   if (init)
      p <- mkBRAMCore1BELoad(memSize, hasOutputReg, file, False);
   else
      p <- mkBRAMCore1BE(memSize, hasOutputReg);
        
   Reg#(Bool) doRead <- mkDReg(False);
   Wire#(elem) nextData <- mkWire();

   (* fire_when_enabled *)
   rule moveToOutFifo (doRead);
      nextData <= p.read;
   endrule
   
   interface Server bram_server;
      interface Put request;
	 method Action put (Tuple3#(Bit#(nsz), addr, elem) req);
	    $display("Sending request %t", $time());
	    p.put(tpl_1(req), tpl_2(req), tpl_3(req));
	    doRead <= True;
	 endmethod
      endinterface

      interface Get response;
	 method ActionValue#(elem) get();
	    return nextData;
	 endmethod
      endinterface
   endinterface
   
   
   
endmodule

module mkAsyncMem(AsyncMem#(addr, elem, MemId#(inflight), n) _unused_)
   provisos(Bits#(addr, szAddr), Bits#(elem, szElem));
   
   let outDepth = valueOf(inflight);
   
   Wire#(Tuple3#(Bit#(n), addr, elem)) toMem <- mkWire();
   Wire#(elem) fromMem <- mkWire();
   
   
   //this must be at least size 2 to work corrsectly (safe bet)
   Vector#(inflight, Reg#(elem)) outData <- replicateM( mkConfigReg(unpack(0)) );
   Vector#(inflight, Reg#(Bool)) valid <- replicateM( mkConfigReg(False) );
   
   Reg#(MemId#(inflight)) head <- mkReg(0);
   Bool okToRequest = valid[head] == False;
      
   
   Reg#(Maybe#(MemId#(inflight))) nextData <- mkDReg(tagged Invalid);
   (* fire_when_enabled *)
   rule moveToOutFifo (nextData matches tagged Valid.idx);
      outData[idx] <= fromMem;
      valid[idx] <= True;
   endrule
        
   
   method ActionValue#(MemId#(inflight)) req(addr a, elem b, Bit#(n) wmask) if (okToRequest);
      toMem <= tuple3(wmask, a, b);
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

   
   interface Client bram_client;
      interface Get request;
	 method ActionValue#(Tuple3#(Bit#(n), addr, elem)) get();
	    return toMem;
	 endmethod
      endinterface
   
      interface Put response;
	 method Action put(elem);
	    fromMem <= elem;
	 endmethod
      endinterface
   
   endinterface
   
   
endmodule

  
endpackage
