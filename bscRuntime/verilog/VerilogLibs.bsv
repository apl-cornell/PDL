package VerilogLibs;

export RenameRF(..);
export mkRenameRF;

interface RenameRF#(type elem, type addr, type name);
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
    (RenameRF#(elem, addr, name)) provisos
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
    
    method NAME_OUT readName[2] (ADDR);
    method VALID_OUT isValid[2] (VALID_NAME);
    method D_OUT read[2] (NAME);
    method NAME_OUT allocName(ADDR_IN) enable(ALLOC_E) ready(ALLOC_READY);
    method write(NAME_IN, D_IN) enable(WE);
    method commit(NAME_F) enable(FE);
    
       schedule (readName) CF (readName);
       schedule (readName) CF (isValid, read, allocName, write, commit);
       schedule (isValid) CF (isValid);
       schedule (isValid) CF (read, allocName, write, commit);
       schedule (read) CF (read);
       schedule (read) CF (allocName, write, commit);
       schedule (allocName) C (allocName);
       schedule (allocName) CF (write, commit);
       schedule (write) C (write);
       schedule (write) CF (commit);
       schedule (commit) C (commit);
    
 endmodule

endpackage
