import ClientServer :: *;
import Connectable :: *;
import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import RegFile :: *;
import BRAMCore :: *;
import Hist :: *;
import Outer :: *;
import Functions :: *;



interface TopMod;
    interface Outer _into;
endinterface

(* synthesize *)
module mkTB ( Empty _unused_ ) provisos(  );
    Reg#( Bool ) started <- mkReg ( False );
    Reg#( UInt#(32) ) timer <- mkReg ( 0 );
    TopMod _topMod <- mkCircuit (  );
    Reg#( UInt#(2) ) reg_unused_0 <- mkReg ( 0 );
    rule initTB (( ! started ));
        UInt#(2) _unused_0 = ?;
        _unused_0 <- _topMod._into.req(10'd0);
        reg_unused_0 <= _unused_0;
        started <= True;
    endrule
    rule timerCount ;
        timer <= ( timer + 1 );
    endrule
    rule stopTB (( ( timer >= 32'd1000000 ) || _topMod._into.checkHandle(reg_unused_0) ));
        $finish();
    endrule
endmodule

(* synthesize *)
module mkCircuit ( TopMod _unused_ ) provisos(  );
    RegFile#( UInt#(10), UInt#(16) ) tf <- mkRegFile ( True, "f" );
    AddrLockCombMem#( UInt#(10), UInt#(16), LockId#(4), 4 ) f <- mkFAAddrLockCombMem ( tf );
    RegFile#( UInt#(10), UInt#(32) ) tw <- mkRegFile ( True, "w" );
    AddrLockCombMem#( UInt#(10), UInt#(32), LockId#(4), 4 ) w <- mkFAAddrLockCombMem ( tw );
    RegFile#( UInt#(10), UInt#(32) ) th <- mkRegFile ( True, "h" );
    AddrLockCombMem#( UInt#(10), UInt#(32), LockId#(4), 4 ) h <- mkFAAddrLockCombMem ( th );
    Hist hg <- mkHist ( f, w, h );
    Outer o <- mkOuter ( hg );
    interface Outer _into = o;
endmodule
