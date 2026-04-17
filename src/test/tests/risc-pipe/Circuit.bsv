import ClientServer :: *;
import Connectable :: *;
import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import RegFile :: *;
import BRAMCore :: *;
import Multi_stg_div :: *;
import Cpu :: *;
import Functions :: *;



interface TopMod;
    interface Client#( Tuple3#( Bit#(4), UInt#(16), Int#(32) ), Int#(32) ) _intti;
    interface Client#( Tuple3#( Bit#(4), UInt#(16), Int#(32) ), Int#(32) ) _inttd;
    interface Cpu _intc;
endinterface

(* synthesize *)
module mkTB ( Empty _unused_ ) provisos(  );
    Reg#( Bool ) started <- mkReg ( False );
    Reg#( UInt#(32) ) timer <- mkReg ( 0 );
    TopMod _topMod <- mkCircuit (  );
    BramPort#( UInt#(16), Int#(32), MemId#(8), 4 ) ti <- mkBramPort ( True, "ti5" );
    BramPort#( UInt#(16), Int#(32), MemId#(8), 4 ) td <- mkBramPort ( True, "td5" );
    Reg#( UInt#(3) ) reg_unused_0 <- mkReg ( 0 );
    mkConnection(_topMod._intti, ti.bram_server);
    mkConnection(_topMod._inttd, td.bram_server);
    rule initTB (( ! started ));
        UInt#(3) _unused_0 = ?;
        _unused_0 <- _topMod._intc.req(16'd0);
        reg_unused_0 <= _unused_0;
        started <= True;
    endrule
    rule timerCount ;
        timer <= ( timer + 1 );
    endrule
    rule stopTB (( ( timer >= 32'd1000000 ) || _topMod._intc.checkHandle(reg_unused_0) ));
        $finish();
    endrule
endmodule

(* synthesize *)
module mkCircuit ( TopMod _unused_ ) provisos(  );
    AsyncMem#( UInt#(16), Int#(32), MemId#(8), 4 ) ti <- mkAsyncMem (  );
    AsyncMem#( UInt#(16), Int#(32), MemId#(8), 4 ) td <- mkAsyncMem (  );
    RenameRF#( UInt#(5), Int#(32), LockId#(64) ) rf <- mkRenameRF ( 32, 64, True, "rf" );
    Multi_stg_div div <- mkMulti_stg_div (  );
    BHT#( 16 ) b <- mkBHT ( 4 );
    Cpu c <- mkCpu ( rf, ti, td, div, b );
    interface Client _intti = ti.bram_client;
    interface Client _inttd = td.bram_client;
    interface Cpu _intc = c;
endmodule
