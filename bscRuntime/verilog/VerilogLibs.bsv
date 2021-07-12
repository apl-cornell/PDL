package VerilogLibs;

import RegFile :: *;
import FIFOF :: *;
import RWire :: *;

export RenameRF(..);
export BHT(..);
export mkRenameRF;
export mkForwardRenameRF;
export mkNBFIFOF;
export mkBHT;

interface RenameRF#(type addr, type elem, type name);
   method name readName(addr a); //get name to read data later
   method Bool isValid(name n);  //check if safe to read
   method elem read(name a);    //do the read
   method ActionValue#(name) allocName(addr a); //allocate a new name to be written
   method Action write(name a, elem b); //write data given allocated name
   method Action commit(name a); //indicate old name for a can be "freed"
   // method Action abort(name a); use for speculative threads that die so name a can be "freed" since not going to be written
endinterface

import "BVI" RenameRF =
 module mkRenameRF#(Integer aregs, Integer pregs, Bool init, String fileInit)(RenameRF#(addr, elem, name)) provisos
    (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Bounded#(name),
     PrimIndex#(addr, an), PrimIndex#(name, nn));

    parameter addr_width = valueOf(szAddr);
    parameter data_width = valueOf(szElem);
    parameter name_width = valueOf(szName);
    parameter lo_arch = 0;
    parameter hi_arch = aregs - 1;
    parameter lo_phys = 0;
    parameter hi_phys = pregs - 1;
    parameter binaryInit = init;
    parameter file = fileInit;
    
    default_clock clk(CLK, (*unused*) clk_gate);
    default_reset rst (RST);
    
    method NAME_OUT readName[2] (ADDR);
    method VALID_OUT isValid[2] (VALID_NAME);
    method D_OUT read[2] (NAME);
    method NAME_OUT allocName(ADDR_IN) enable(ALLOC_E) ready(ALLOC_READY);
    method write[2] (NAME_IN, D_IN) enable(WE);
    method commit(NAME_F) enable(FE);
    
       schedule (readName) CF (readName);
       schedule (readName) CF (isValid, read, allocName, write, commit);
       schedule (isValid) CF (isValid);
       schedule (isValid) CF (read, allocName, write, commit);
       schedule (read) CF (read);
       schedule (read) CF (allocName, write, commit);
       schedule (allocName) C (allocName);
       schedule (allocName) CF (write, commit);
       schedule (write) CF (write, commit);
       schedule (commit) C (commit);
    
 endmodule

import "BVI" ForwardRenameRF =
 module mkForwardRenameRF#(Integer aregs, Integer pregs, Bool init, String fileInit)(RenameRF#(addr, elem, name)) provisos
    (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Bounded#(name),
     PrimIndex#(addr, an), PrimIndex#(name, nn));

    parameter addr_width = valueOf(szAddr);
    parameter data_width = valueOf(szElem);
    parameter name_width = valueOf(szName);
    parameter lo_arch = 0;
    parameter hi_arch = aregs - 1;
    parameter lo_phys = 0;
    parameter hi_phys = pregs - 1;
    parameter binaryInit = init;
    parameter file = fileInit;
    
    default_clock clk(CLK, (*unused*) clk_gate);
    default_reset rst (RST);
    
    method NAME_OUT readName[2] (ADDR);
    method VALID_OUT isValid[2] (VALID_NAME);
    method D_OUT read[2] (NAME);
    method NAME_OUT allocName(ADDR_IN) enable(ALLOC_E) ready(ALLOC_READY);
    method write[2] (NAME_IN, D_IN) enable(WE);
    method commit(NAME_F) enable(FE);
    
       schedule (readName) CF (readName);
       schedule (readName) CF (isValid, read, allocName, write, commit);
       schedule (isValid) CF (isValid);
       schedule (isValid) CF (read, allocName, write, commit);
       schedule (read) CF (read);
       schedule (read) CF (allocName, write, commit);
       schedule (allocName) C (allocName);
       schedule (allocName) CF (write, commit);
       schedule (write) CF (write, commit);
       schedule (commit) C (commit);
    
 endmodule

module mkNBFIFOF(FIFOF#(dtyp)) provisos (Bits#(dtyp, szdtyp));
   
   FIFOF#(dtyp) f <- mkFIFOF();
   //allow multiple writes in the same cycle
   RWire#(dtyp) enq_data <- mkRWireSBR();
   
   (*fire_when_enabled*)
   rule doEnq (enq_data.wget() matches tagged Valid.d);
      f.enq(d);
   endrule

   //only allow the LAST enq each cycle to work, drop the others
   method Action enq(dtyp a) if (f.notFull());
      enq_data.wset(a);
   endmethod
   
   method Action deq();
      f.deq();
   endmethod
   
   method dtyp first();
      return f.first();
   endmethod
   
   method Bool notFull();
      return f.notFull();
   endmethod
   
   method Bool notEmpty();
      return f.notEmpty();
   endmethod
   
   method Action clear();
      f.clear();
   endmethod
   
endmodule

interface BHT#(type addr);
   method addr req(addr pc, addr skip, addr take);
   method Action upd(addr pc, Bool take);
endinterface

import "BVI" BHT = module mkBHT#(Integer entries)(BHT#(addr)) provisos (Bits#(addr,szAddr));
		      parameter num_entries = entries;
		      parameter addr_width = valueOf(szAddr);
		      
		      default_clock clk(CLK, (*unused*) clk_gate);
		      default_reset rst (RST);
		      
		      method TAKE_OUT req (PC_IN_PRED, SKIP_OFF_IN, TAKE_OFF_IN);
		      method upd (PC_IN_RES, TAKE_IN) enable (WE);
		      
			 schedule (req) CF (req, upd);
			 schedule (upd) C (upd);
endmodule

// import "BVI" FIFO2 = module mkNBFIFOF(FIFOF#(dtyp)) provisos (Bits#(dtyp, szdtyp));

// 	parameter width = valueOf(szdtyp);
// 	parameter guarded = 1;
	
// 	default_clock clk(CLK, (*unused*) clk_gate);
// 	default_reset rst (RST);

// 	method enq(D_IN) enable(ENQ) ready(FULL_N);
// 	method deq() enable(DEQ) ready(EMPTY_N);
// 	method D_OUT first() ready(EMPTY_N);
// 	method FULL_N notFull();
// 	method EMPTY_N notEmpty();
// 	method clear() enable(CLR);
	   
// 	   schedule (notFull, notEmpty, first) CF (notFull, notEmpty, first);
// 	   schedule (notFull, notEmpty) SB (enq, deq, clear);
// 	   schedule (first) CF (enq);
// 	   schedule (first) SB (deq, clear);
// 	   schedule (enq) CF (enq, deq);
// 	   schedule (enq) SB (clear);
// 	   schedule (deq) C (deq);
// 	   schedule (deq) SB (clear);
// 	   schedule (clear) SBR (clear);
    
// endmodule

endpackage
