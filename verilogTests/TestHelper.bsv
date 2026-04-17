package TestHelper;

export testAssert;
export testDone;

function Action testAssert(Bool cond, String msg, UInt#(32) cyc);
   return action
      if (cond)
         $display("  ok: %s (cycle %0d)", msg, cyc);
      else
         $display("  FAIL: %s (cycle %0d)", msg, cyc);
   endaction;
endfunction

function Action testDone(String name, UInt#(32) fails);
   return action
      if (fails == 0)
         $display("PASS %s (0 failures)", name);
      else
         $display("FAIL %s (%0d failures)", name, fails);
      $finish(0);
   endaction;
endfunction

endpackage
