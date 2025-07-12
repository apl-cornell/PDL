package Interrupt;

import FIFOF :: *;
import Ehr :: *;

interface TimingInterruptController#(numeric type addr);
    method ActionValue#(Bool) req(Int#(addr) a);
    method Action ack(Int#(addr) a);
endinterface

module mkTimingInterruptController(TimingInterruptController#(addr) _unused_);

    Reg#(Bool) status <- mkReg(False);
    Reg#(UInt#(10)) timer <- mkReg(0);
    Wire#(Int#(addr)) getAck <- mkWire();

    // rule to update timer and set status to True every 1000 cycle
    rule updateTimer;
        timer <= timer + 1;
        if (timer == 999) begin
            timer <= 0;
            status <= True;
        end
    endrule

    method ActionValue#(Bool) req(Int#(addr) p);
        return status;
    endmethod

    method Action ack(Int#(addr) a);
        status <= False;
    endmethod
endmodule

endpackage