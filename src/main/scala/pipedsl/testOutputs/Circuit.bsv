import Memories :: *;
import Hist :: *;
import Functions :: *;



interface TopMod;
    interface Hist _inthg;
endinterface

(* synthesize *)
module mkTB ( Empty _unused_ );
    Reg#( UInt#(3) ) reg_unused_0 <- mkReg ( 0 );
    Reg#( Bool ) started <- mkReg ( False );
    TopMod m <- mkCircuit (  );
    rule initTB (( ! started ));
        UInt#(3) _unused_0 = ?;
        _unused_0 <- m._inthg.req(10'd0);
        reg_unused_0 <= _unused_0;
        started <= True;
    endrule
    rule stopTB (m._inthg.checkHandle(reg_unused_0));
        $finish();
    endrule
endmodule

(* synthesize *)
module mkCircuit ( TopMod _unused_ );
    CombMem#( UInt#(10), UInt#(10) ) f <- mkCombMem ( True, "f" );
    CombMem#( UInt#(32), UInt#(10) ) w <- mkCombMem ( True, "w" );
    CombMem#( UInt#(32), UInt#(10) ) h <- mkCombMem ( True, "h" );
    Hist hg <- mkHist ( f, w, h );
    interface Hist _inthg = hg;
endmodule
