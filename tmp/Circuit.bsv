import Locks :: *;
import Memories :: *;
import VerilogLibs :: *;
import RegFile :: *;
import BRAMCore :: *;
import Multi_stg_div :: *;
import Cpu :: *;
import Functions :: *;
import Connectable :: *;



interface TopMod;
   interface Cpu _intc;
   interface AsyncMem#( UInt#(16), Int#(32), MemId#(8), 4) imem;
   interface AsyncMem#( UInt#(16), Int#(32), MemId#(8), 4) dmem;
  // interface Inout#(QueueLockAsyncMem#( UInt#(16), Int#(32), MemId#(8), 4, LockId#(4))) _qlck;
   /* add inout stuff?*/
endinterface

(* synthesize *)
module mkTB ( Empty _unused_ ) provisos(  );
   Reg#( UInt#(3) ) reg_unused_0 <- mkReg ( 0 );
   Reg#( Bool ) started <- mkReg ( False );
   TopMod m <- mkCircuit (  );
   
   BramPort#( UInt#(16), Int#(32), MemId#(8), 4 ) ti <- mkBramPort ( True, "ti1" );
   BramPort#( UInt#(16), Int#(32), MemId#(8), 4 ) td <- mkBramPort ( True, "td1" );
 
   mkConnection(m.imem.bram_client, ti.bram_server);
   mkConnection(m.dmem.bram_client, td.bram_server);
   
   rule initTB (( ! started ));
      UInt#(3) _unused_0 = ?;
      _unused_0 <- m._intc.req(16'd0);
      reg_unused_0 <= _unused_0;
      started <= True;
   endrule
   rule stopTB (m._intc.checkHandle(reg_unused_0));
      $finish();
   endrule
endmodule

(* synthesize *)
module mkCircuit ( TopMod _unused_ ) provisos(  );
   
   QueueLockAsyncMem#( UInt#(16), Int#(32), MemId#(8), 4, LockId#(4) ) i <- mkQueueLockAsyncMem ();
   AddrLockAsyncMem#( UInt#(16), Int#(32), MemId#(8), 4, LockId#(4), 4 ) d <- mkFAAddrLockAsyncMem ();
   RegFile#( UInt#(5), Int#(32) ) rf <- mkRegFile ( True, "rf" );
   AddrLockCombMem#( UInt#(5), Int#(32), LockId#(4), 4 ) r <- mkFAAddrLockCombMem ( rf );
   Multi_stg_div div <- mkMulti_stg_div (  );
   Cpu c <- mkCpu ( r, i, d, div );
   interface Cpu _intc = c;
   interface AsyncMem imem = i.mem;
   interface AsyncMem dmem = d.mem;
endmodule
