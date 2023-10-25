// tb.bsv
import Memories :: *;
import Connectable :: *;

typedef UInt#(4) ADDR;
typedef UInt#(32) DATA;


(*synthesize*)
module mkTop();

   Reg#(Bool) isMULHSU <- mkReg(False);
   
   rule muldebug(False);
      Int#(4) x = 0;
      Int#(4) y = -8;
      Int#(4) smagx = abs(x);
      Int#(4) smagy = abs(y);
      Int#(4) sx = signum(x);
      Bool posX = sx == 1;
      Int#(4) sy = signum(y);
      Bool posY = sy == 1;
      if (isMULHSU) sy = 1;
      UInt#(4) magx = unpack(pack(abs(x) + 0));
      UInt#(4) magy = unpack(pack(abs(y) + 0));
      $display("x: %d, signX: %d, magX: %d", x, sx, magx);
      $display("y: %d, signY: %d, magY: %d", y, sy, magy);
      Int#(8) zsign = extend(sx * sy);
      UInt#(8) z = unsignedMul(magx, magy);
      Int#(8) sz = unpack(pack(z)) * zsign;
      $display("z: %d", sz);
   endrule

   

   rule divdebug(False);
      Bit#(4) aa = 15;
      $display("aa: %b", aa);
      Int#(5) x = -12;
      Int#(5) z = -4;
      Int#(5) y = 5;
      UInt#(5) ux = unpack(pack(x));
      UInt#(5) uy = unpack(pack(y));
      UInt#(5) uz = unpack(pack(z));
      $display("x: %d, y: %d, div: %d, rem: %d", x, y, x/y, x % y);
      $display("UNSIGNED x: %d, y: %d, div: %d, rem: %d", ux, uy, ux/uy, ux % uy);
   endrule

   
   Int#(8) x = 7;
   Int#(8) y = 3;
   rule overDebug(False);
      if (y == 3)
	 begin
	    Int#(8) x = 4;
	    end
      $display("x is %d", x);
      
   endrule
   
   BramPort#(ADDR, DATA, MemId#(8), 4) bram <- mkBramPort(False, "");
   AsyncMem#(ADDR, DATA, MemId#(8), 4) amem <- mkAsyncMem();
   
   mkConnection(bram.bram_server, amem.bram_client);
   
   Reg#(UInt#(10)) count <- mkReg(0);
   Reg#(UInt#(10)) stop <- mkReg(0);
   Reg#(Maybe#(MemId#(8))) midreg <- mkReg(tagged Invalid);
   
   ADDR a = 4;
   rule memDebug0(count == 0);
      MemId#(8) mid <- amem.req(a, zeroExtend(stop), '1);
      midreg <= tagged Valid mid;   
      stop <= stop + 1;
      if (stop == 32) count <= 5;
      else       count <= 1;
   endrule

   rule memDebug1(midreg matches tagged Valid.mr &&& amem.checkRespId(mr) &&& count == 1);
      amem.resp(mr);
      let val = amem.peekResp(mr);
      $display("Wrote value %d %t", val, $time());
      MemId#(8) mid <- amem.req(a, ?, 0);
      midreg <= tagged Valid mid;
      count <= 2;
   endrule
   
   rule memDebug2(midreg matches tagged Valid.mr &&& amem.checkRespId(mr) &&& count == 2);
      amem.resp(mr);
      let val = amem.peekResp(mr);
      $display("Read value %d %t", val, $time());
      midreg <= tagged Invalid;
      count <= 0;
   endrule
   

   rule doStuff(count == 5);
      if (stop >= 64) $finish();
      stop <= stop + 1;
   endrule
endmodule
