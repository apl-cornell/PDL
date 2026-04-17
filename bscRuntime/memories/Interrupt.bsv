// Interrupt.bsv -- Timer interrupt controller
//
// Provides a periodic interrupt signal. Goes pending every N cycles,
// stays pending until acknowledged. Used as a volatile memory for
// XPDL interrupt handling.

package Interrupt;

export TimerInterrupt(..);
export mkTimerInterrupt;

interface TimerInterrupt;
   method Bool pending();
   method Action ack();
endinterface

// Simple BSV implementation: counter-based periodic interrupt
module mkTimerInterrupt#(Integer period)(TimerInterrupt);

   Reg#(Bool) isPending <- mkReg(False);
   Reg#(UInt#(32)) timer <- mkReg(0);

   rule tick(!isPending);
      if (timer >= fromInteger(period - 1)) begin
         timer <= 0;
         isPending <= True;
      end
      else
         timer <= timer + 1;
   endrule

   method Bool pending();
      return isPending;
   endmethod

   method Action ack() if (isPending);
      isPending <= False;
   endmethod

endmodule

endpackage
