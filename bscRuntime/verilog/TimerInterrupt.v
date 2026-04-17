`ifdef BSV_ASSIGNMENT_DELAY
`else
`define BSV_ASSIGNMENT_DELAY
`endif
`ifdef BSV_RESET_VALUE
`else
 `define BSV_RESET_VALUE 1
`endif

// Simple timer interrupt: toggles PENDING high every `period` cycles.
// Stays high until ACK_E is asserted.

module TimerInterrupt(CLK, RST,
                      PENDING,      // output: interrupt pending
                      ACK_E         // input: acknowledge (clears pending)
                      );

   parameter period = 1000;
   parameter counter_width = 32;

   input  CLK;
   input  RST;
   output PENDING;
   input  ACK_E;

   reg pending;
   reg [counter_width-1:0] counter;

   assign PENDING = pending;

   always @(posedge CLK) begin
      if (RST == `BSV_RESET_VALUE) begin
         pending <= `BSV_ASSIGNMENT_DELAY 0;
         counter <= `BSV_ASSIGNMENT_DELAY 0;
      end
      else begin
         if (ACK_E)
           pending <= `BSV_ASSIGNMENT_DELAY 0;
         else if (counter >= period - 1) begin
            pending <= `BSV_ASSIGNMENT_DELAY 1;
            counter <= `BSV_ASSIGNMENT_DELAY 0;
         end
         else
           counter <= `BSV_ASSIGNMENT_DELAY counter + 1;
      end
   end

endmodule
