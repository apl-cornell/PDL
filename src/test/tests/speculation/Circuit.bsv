import ClientServer :: *;
import Connectable :: *;
import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import RegFile :: *;
import BRAMCore :: *;
import Testwrite :: *;
import Functions :: *;



interface TopMod;
    interface Client#( Tuple3#( Bit#(4), UInt#(8), Int#(32) ), Int#(32) ) _intti;
    interface Testwrite _intt;
endinterface

(* synthesize *)
module mkTB ( Empty _unused_ ) provisos(  );
    Reg#( Bool ) started <- mkReg ( False );
    Reg#( UInt#(32) ) timer <- mkReg ( 0 );
    TopMod _topMod <- mkCircuit (  );
    BramPort#( UInt#(8), Int#(32), MemId#(8), 4 ) ti <- mkBramPort ( True, "ti" );
    Reg#( UInt#(3) ) reg_unused_0 <- mkReg ( 0 );
    mkConnection(_topMod._intti, ti.bram_server);
    rule initTB (( ! started ));
        UInt#(3) _unused_0 = ?;
        _unused_0 <- _topMod._intt.req(8'd0);
        reg_unused_0 <= _unused_0;
        started <= True;
    endrule
    rule timerCount ;
        timer <= ( timer + 1 );
    endrule
    rule stopTB (( ( timer >= 32'd1000000 ) || _topMod._intt.checkHandle(reg_unused_0) ));
        $finish();
    endrule
endmodule

(* synthesize *)
module mkCircuit ( TopMod _unused_ ) provisos(  );
    AsyncMem#( UInt#(8), Int#(32), MemId#(8), 4 ) ti <- mkAsyncMem (  );
    RegFile#( UInt#(5), Int#(32) ) rename <- mkRegFile ( True, "rename" );
    CheckpointQueueLockCombMem#( UInt#(5), Int#(32), LockId#(4), LockId#(4) ) qrf <- mkCheckpointQueueLockCombMem ( rename );
    Testwrite t <- mkTestwrite ( qrf, ti );
    interface Client _intti = ti.bram_client;
    interface Testwrite _intt = t;
endmodule
