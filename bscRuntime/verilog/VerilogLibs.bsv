package VerilogLibs;

export CombMem(..);
export mkRenameRF;

interface CombMem#(type elem, type addr, type name);
   method name readName(addr a); //get name to read data later
   method Bool isValid(name n);  //check if safe to read
   method elem read(name a);    //do the read
   method ActionValue#(name) allocName(addr a); //allocate a new name to be written
   method Action write(name a, elem b); //write data given allocated name
   method Action commit(name a); //indicate old name for a can be "freed"
   // method Action abort(name a); use for speculative threads that die so name a can be "freed" since not going to be written
endinterface

import "BVI" RenameRF =
 module mkRenameRF#(Integer aregs, Integer pregs, Bool init, String fileInit)
    (CombMem#(elem, addr, name)) provisos
    (Bits#(elem, szElem), Bits#(addr, szAddr), Bits#(name, szName), Bounded#(name),
     PrimIndex#(addr, an), PrimIndex#(name, nn));

    parameter addr_width = valueOf(szAddr);
    parameter data_width = valueOf(szElem);
    parameter name_width = valueOf(szName);
    parameter lo_arch = 0;
    parameter hi_arch = aregs - 1;
    parameter lo_phys = 0;
    parameter hi_phys = pregs - 1;
    
    default_clock clk(CLK, (*unused*) clk_gate);
    default_reset rst (RST);
    
    method NAME_OUT_1 readName(ADDR_1);
    method VALID_OUT_1 isValid(VALID_NAME_1);
    method D_OUT_1 read(NAME_1);
    method NAME_OUT allocName(ADDR_IN) enable(ALLOC_E) ready(ALLOC_READY);
    method write(NAME, D_IN) enable(WE);
    method commit(NAME_F) enable(FE);
    
       schedule (readName) C (readName);
       schedule (readName) CF (isValid, read, allocName, write, commit);
       schedule (isValid) C (isValid);
       schedule (isValid) CF (read, allocName, write, commit);
       schedule (read) C (read);
       schedule (read) CF (allocName, write, commit);
       schedule (allocName) C (allocName);
       schedule (allocName) CF (write, commit);
       schedule (write) C (write);
       schedule (write) CF (commit);
       schedule (commit) C (commit);
    
 endmodule

endpackage
