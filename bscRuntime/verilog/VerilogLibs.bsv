package VerilogLibs;

import RegFile :: *;
import FIFOF :: *;
import RWire :: *;

export RenameRF(..);
export CheckpointRF(..);
export BypassRF(..);
export CheckpointBypassRF(..);
export BHT(..);
export mkRenameRF;
export mkForwardRenameRF;
export mkCheckpointRF;
export mkBypassRF;
export mkCheckpointBypassRF;

export mkBHT;

interface RenameRF#(type addr, type elem, type name);
   method name res_r1(addr a); //get name to read data later
   method Bool owns_r1(name n);  //check if safe to read  
   method name res_r2(addr a); //get name to read data later
   method Bool owns_r2(name n);  //check if safe to read
   method elem read(name a);    //do the read
   method ActionValue#(name) res_w1(addr a); //allocate a new name to be written
   method Action rel_w1(name a); //indicate old name for a can be "freed"
   method Action write(name a, elem b); //write data given allocated name
endinterface

interface CheckpointRF#(type addr, type elem, type name, type cid);
   method name res_r1(addr a); //get name to read data later
   method Bool owns_r1(name n);  //check if safe to read  
   method name res_r2(addr a); //get name to read data later
   method Bool owns_r2(name n);  //check if safe to read
   method elem read(name a);    //do the read
   method ActionValue#(name) res_w1(addr a); //allocate a new name to be written
   method Action rel_w1(name a); //indicate old name for a can be "freed"
   method Action write(name a, elem b); //write data given allocated name   
   method ActionValue#(cid) checkpoint();
   method Action rollback(cid c, Bool doRoll, Bool doRel);
endinterface
   
interface BypassRF#(type addr, type elem, type id);
   method ActionValue#(id) res_w1(addr a);
   method ActionValue#(id) res_r1(addr a);
   method ActionValue#(id) res_r2(addr a);
   method Bool owns_r1();
   method Bool owns_r2();
   method Action rel_r1();
   method Action rel_r2();
   method Action rel_w1(id i);      
   method elem read1(id a);
   method elem read2(id a);
   method Action write(id i, elem b);
endinterface

interface CheckpointBypassRF#(type addr, type elem, type id, type cid);
   method ActionValue#(id) res_w1(addr a);
   method ActionValue#(id) res_r1(addr a);
   method ActionValue#(id) res_r2(addr a);
   method Bool owns_r1();
   method Bool owns_r2();
   method Action rel_r1();
   method Action rel_r2();
   method Action rel_w1(id i);      
   method elem read1(id a);
   method elem read2(id a);
   method Action write(id i, elem b);
   method ActionValue#(cid) checkpoint();
   method Action rollback(cid c, Bool doRoll, Bool doRel);   
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
    
    method NAME_OUT_1 res_r1 (ADDR_1);
    method VALID_OUT_1 owns_r1 (VALID_NAME_1);
    method NAME_OUT_2 res_r2 (ADDR_2);
    method VALID_OUT_2 owns_r2 (VALID_NAME_2);
    method D_OUT read[2] (NAME);
    method write[2] (NAME_IN, D_IN) enable(WE);    
    method NAME_OUT res_w1(ADDR_IN) enable(ALLOC_E) ready(ALLOC_READY);
    method rel_w1(NAME_F) enable(FE);
    
       schedule (res_r1, res_r2) CF (res_r1, res_r2);
       schedule (res_r1, res_r2) CF (owns_r1,owns_r2, read, res_w1, write, rel_w1);
       schedule (owns_r1,owns_r2) CF (owns_r1,owns_r2);
       schedule (owns_r1,owns_r2) CF (read, res_w1, write, rel_w1);
       schedule (read) CF (read);
       schedule (read) CF (res_w1, write, rel_w1);
       schedule (res_w1) C (res_w1);
       schedule (res_w1) CF (write, rel_w1);
       schedule (write) CF (write, rel_w1);
       schedule (rel_w1) C (rel_w1);
    
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
    
    method NAME_OUT_1 res_r1 (ADDR_1);
    method VALID_OUT_1 owns_r1 (VALID_NAME_1);
    method NAME_OUT_2 res_r2 (ADDR_2);
    method VALID_OUT_2 owns_r2 (VALID_NAME_2);    
    method D_OUT read[2] (NAME);
    method NAME_OUT res_w1(ADDR_IN) enable(ALLOC_E) ready(ALLOC_READY);
    method rel_w1(NAME_F) enable(FE);
    method write[2] (NAME_IN, D_IN) enable(WE);    
    
       schedule (res_r1,res_r2) CF (res_r1,res_r2);
       schedule (res_r1,res_r2) CF (owns_r1, owns_r2, read, res_w1, write, rel_w1);
       schedule (owns_r1, owns_r2) CF (owns_r1, owns_r2);
       schedule (owns_r1, owns_r2) CF (read, res_w1, write, rel_w1);
       schedule (read) CF (read);
       schedule (read) CF (res_w1, write, rel_w1);
       schedule (res_w1) C (res_w1);
       schedule (res_w1) CF (write, rel_w1);
       schedule (write) CF (write, rel_w1);
       schedule (rel_w1) C (rel_w1);
    
 endmodule

import "BVI" CheckpointRenameRF =
 module mkCheckpointRF#(Integer aregs, Integer pregs, Integer replicas, Bool init, String fileInit)(CheckpointRF#(addr, elem, name, cid)) provisos
    (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Bits#(cid, szCid), Bounded#(name),
     PrimIndex#(addr, an), PrimIndex#(name, nn));

    parameter addr_width = valueOf(szAddr);
    parameter data_width = valueOf(szElem);
    parameter name_width = valueOf(szName);
    parameter replica_width = valueOf(szCid);    
    parameter lo_arch = 0;
    parameter hi_arch = aregs - 1;
    parameter lo_phys = 0;
    parameter hi_phys = pregs - 1;
    parameter num_replicas = replicas;

    parameter binaryInit = init;
    parameter file = fileInit;

    default_clock clk(CLK, (*unused*) clk_gate);
    default_reset rst (RST);
    
    method NAME_OUT_1 res_r1 (ADDR_1);
    method VALID_OUT_1 owns_r1 (VALID_NAME_1);
    method NAME_OUT_2 res_r2 (ADDR_2);
    method VALID_OUT_2 owns_r2 (VALID_NAME_2);
    method D_OUT read[2] (NAME);
    method write[2] (NAME_IN, D_IN) enable(WE);    
    method NAME_OUT res_w1(ADDR_IN) enable(ALLOC_E) ready(ALLOC_READY);
    method rel_w1(NAME_F) enable(FE);

    method CHK_OUT checkpoint() enable(CHK_E) ready(CHK_READY);       
    method rollback (ROLLBK_IN, DO_ROLL, DO_REL) enable(ROLLBK_E);
    
       schedule (checkpoint) C (checkpoint);
       schedule (rollback) C (rollback);
       schedule (checkpoint) CF (rollback);
       schedule (checkpoint, rollback) CF (res_r1, res_r2, owns_r1, owns_r2, read, write, rel_w1);      
       schedule (res_r1, res_r2) CF (res_r1, res_r2);
       schedule (res_r1, res_r2) CF (owns_r1, owns_r2, read, res_w1, write, rel_w1);
       schedule (owns_r1, owns_r2) CF (owns_r1, owns_r2);
       schedule (owns_r1, owns_r2) CF (read, res_w1, write, rel_w1);
       schedule (read) CF (read);
       schedule (read) CF (res_w1, write, rel_w1);
       schedule (res_w1) C (res_w1);
       schedule (res_w1) CF (write, rel_w1, checkpoint, rollback);
       schedule (write) CF (write, rel_w1);
       schedule (rel_w1) C (rel_w1);
        
 endmodule


import "BVI" BypassRF =
module mkBypassRF#(Integer regnum, Bool init, String fileInit)(BypassRF#(addr, elem, name)) provisos
   (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Bounded#(name),
    PrimIndex#(addr, an), PrimIndex#(name, nn));
   
   parameter addr_width = valueOf(szAddr);
   parameter data_width = valueOf(szElem);
   parameter name_width = valueOf(szName);
   parameter lo_arch = 0;
   parameter hi_arch = regnum - 1;
   parameter binaryInit = init;
   parameter file = fileInit;
        
   default_clock clk(CLK, (*unused*) clk_gate);
   default_reset rst (RST);
   
   method NAME_OUT res_w1(ADDR_IN) enable(ALLOC_E) ready(ALLOC_READY);
   method RNAME_OUT_1 res_r1(ADDR_1) enable(RRESE_1) ready (RRES_READY_1);
   method RNAME_OUT_2 res_r2(ADDR_2) enable(RRESE_2) ready (RRES_READY_2);
   method VALID_OUT_1 owns_r1();
   method VALID_OUT_2 owns_r2();
   method write[2](NAME_IN, D_IN) enable (WE);   
   method D_OUT_1 read1(RD_NAME_1);
   method D_OUT_2 read2(RD_NAME_2);
   method rel_r1() enable (FE_1);
   method rel_r2() enable (FE_2);
   method rel_w1(W_F) enable (WFE) ready (F_READY);
   
      schedule (res_w1) C (res_w1);
      schedule (res_w1) CF (res_r1, res_r2, owns_r1, owns_r2, rel_r1, rel_r2, read1, read2, write, rel_w1);
      schedule (res_r1) C (res_r1);
      schedule (res_r2) C (res_r2);
      schedule (res_r1) CF (res_r2);
      schedule (owns_r1, owns_r2, read1, read2, rel_r1, rel_r2) SBR (res_r1, res_r2);
      schedule (read1, read2) CF (owns_r1, owns_r2, read1, read2, rel_r1, rel_r2, rel_w1);
      schedule (owns_r1, owns_r2) CF (owns_r1, owns_r2, rel_r1, rel_r2, rel_w1);
      schedule (write) SBR (res_r1, res_r2, read1, read2, owns_r1, owns_r2);
      schedule (write) CF (rel_r1, rel_r2, write);
      schedule (rel_w1) SBR (write);
      schedule (rel_w1) C (rel_w1);
      schedule (rel_w1) CF (res_r1, res_r2, rel_r1, rel_r2);
      schedule (rel_r1, rel_r2) CF (rel_r1, rel_r2);

endmodule

import "BVI" CheckpointBypassRF =
module mkCheckpointBypassRF#(Integer regnum, Bool init, String fileInit)(CheckpointBypassRF#(addr, elem, name, name)) provisos
   (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Bounded#(name),
    PrimIndex#(addr, an), PrimIndex#(name, nn));
   
   parameter addr_width = valueOf(szAddr);
   parameter data_width = valueOf(szElem);
   parameter name_width = valueOf(szName);
   parameter lo_arch = 0;
   parameter hi_arch = regnum - 1;
   parameter binaryInit = init;
   parameter file = fileInit;
        
   default_clock clk(CLK, (*unused*) clk_gate);
   default_reset rst (RST);
   
   method NAME_OUT res_w1(ADDR_IN) enable(ALLOC_E) ready(ALLOC_READY);
   method RNAME_OUT_1 res_r1(ADDR_1) enable(RRESE_1) ready (RRES_READY_1);
   method RNAME_OUT_2 res_r2(ADDR_2) enable(RRESE_2) ready (RRES_READY_2);
   method VALID_OUT_1 owns_r1();
   method VALID_OUT_2 owns_r2();
   method write[2](NAME_IN, D_IN) enable (WE);
   method D_OUT_1 read1(RD_NAME_1);
   method D_OUT_2 read2(RD_NAME_2);
   method rel_r1() enable (FE_1);
   method rel_r2() enable (FE_2);
   method rel_w1(W_F) enable (WFE) ready (F_READY);
   method CHK_OUT checkpoint() enable (CHK_E) ready (CHK_READY);
   method rollback(ROLLBK_IN, DO_ROLL, DO_REL) enable (ROLLBK_E);
   
      schedule (checkpoint) C (checkpoint);
      schedule (rollback) C (rollback);
      schedule (checkpoint) CF (rollback);
      schedule (checkpoint, rollback) CF (res_r1, res_r2, owns_r1, owns_r2, read1, read2, write, rel_r1, rel_r2, rel_w1);
      schedule (res_w1) C (res_w1);
      schedule (res_w1) CF (res_r1, res_r2, owns_r1, owns_r2, rel_r1, rel_r2, read1, read2, write, rel_w1, checkpoint, rollback);
      schedule (res_r1) C (res_r1);
      schedule (res_r2) C (res_r2);
      schedule (res_r1) CF (res_r2);
      schedule (owns_r1, owns_r2, read1, read2, rel_r1, rel_r2) SBR (res_r1, res_r2);
      schedule (read1, read2) CF (owns_r1, owns_r2, read1, read2, rel_r1, rel_r2, rel_w1);
      schedule (owns_r1, owns_r2) CF (owns_r1, owns_r2, rel_r1, rel_r2, rel_w1);
      schedule (write) SB (res_r1, res_r2, read1, read2, owns_r1, owns_r2);
      schedule (write) CF (rel_r1, rel_r2, write);
      schedule (rel_w1) SBR (write);
      schedule (rel_w1) C (rel_w1);
      schedule (rel_w1) CF (res_r1, res_r2, rel_r1, rel_r2);
      schedule (rel_r1, rel_r2) CF (rel_r1, rel_r2);

endmodule
   
   
interface BHT#(numeric type addr);
   method Int#(addr) req(Int#(addr) pc, Int#(addr) skip, Int#(addr) take);
   method Action upd(Int#(addr) pc, Bool take);
endinterface

import "BVI" BHT = module mkBHT#(Integer entries)(BHT#(szAddr));
		      parameter num_entries = entries;
		      parameter addr_width = valueOf(szAddr);
		      
		      default_clock clk(CLK, (*unused*) clk_gate);
		      default_reset rst (RST);
		      
		      method TAKE_OUT req (PC_IN_PRED, SKIP_OFF_IN, TAKE_OFF_IN);
		      method upd (PC_IN_RES, TAKE_IN) enable (WE);
		      
			 schedule (req) CF (req, upd);
			 schedule (upd) C (upd);
endmodule


endpackage
