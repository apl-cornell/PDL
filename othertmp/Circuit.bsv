import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import RegFile :: *;
import BRAMCore :: *;
import Hist :: *;
import Functions :: *;



interface TopMod;
    interface Hist _inthg;
endinterface

(* synthesize *)
module mkTB ( Empty _unused_ ) provisos(  );
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
module mkCircuit ( TopMod _unused_ ) provisos(  );
    BramPort#( UInt#(10), UInt#(16), MemId#(8), 2 ) tf <- mkBramPort ( False, "" );
    AddrLockAsyncMem#( UInt#(10), UInt#(16), MemId#(8), 2, LockId#(4), 4 ) f <- mkFAAddrLockAsyncMem ( tf );
    BramPort#( UInt#(10), UInt#(32), MemId#(8), 4 ) tw <- mkBramPort ( False, "" );
    AddrLockAsyncMem#( UInt#(10), UInt#(32), MemId#(8), 4, LockId#(4), 4 ) w <- mkFAAddrLockAsyncMem ( tw );
    BramPort#( UInt#(10), UInt#(32), MemId#(8), 4 ) th <- mkBramPort ( False, "" );
    AddrLockAsyncMem#( UInt#(10), UInt#(32), MemId#(8), 4, LockId#(4), 4 ) h <- mkFAAddrLockAsyncMem ( th );
    Hist hg <- mkHist ( f, w, h );
    interface Hist _inthg = hg;
endmodule
