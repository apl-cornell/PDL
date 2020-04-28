module cpu_tb;

   reg reset,clk;
   
   CPU mycup(.clk(clk), .reset(reset));

   initial begin
      reset = 1'b1;
      clk = 1'b0;
      $display($time, " <<Starting Simulation>>");
      #5 reset = 1'b0;
      $display($time, " <<Reset Done>");
      #20;
      $display($time, " Stopping");      
      $finish;
   end

   always begin
      #1 clk = ~clk;
   end
endmodule
