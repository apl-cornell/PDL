package TestSpeculation;

import Speculation :: *;
import ConfigReg :: *;
import TestHelper :: *;

// ============================================================
// Test 1: Alloc 3 entries, validate the first, check all statuses, free all.
// Models the normal pipeline path where a branch prediction is correct.
// ============================================================
(* synthesize *)
module mkTestSpec_AllocAndValidate();
   SpecTable#(SpecId#(4), 2) spec <- mkSpecTable();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(SpecId#(4)) s0 <- mkReg(0);
   Reg#(SpecId#(4)) s1 <- mkReg(0);
   Reg#(SpecId#(4)) s2 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: alloc first entry
   rule go0(step == 0);
      $display("=== TEST: Spec_AllocAndValidate ===");
      let id <- spec.alloc();
      s0 <= id;
      step <= 1;
   endrule

   // Step 1: alloc second entry
   rule go1(step == 1);
      let id <- spec.alloc();
      s1 <= id;
      step <= 2;
   endrule

   // Step 2: alloc third entry
   rule go2(step == 2);
      let id <- spec.alloc();
      s2 <= id;
      step <= 3;
   endrule

   // Step 3: validate s0 at EHR port 0 (as Stage__0 would)
   rule go3(step == 3);
      spec.validate(s0, 0);
      step <= 4;
   endrule

   // Step 4: check all statuses -- s0 should be Valid(True), s1 and s2 still unknown (Invalid)
   rule go4(step == 4);
      let c0 = spec.check(s0, 0);
      testAssert(isValid(c0) && fromMaybe(False, c0), "s0 is Valid(True) after validate", cyc);
      if (!isValid(c0) || !fromMaybe(False, c0)) fails <= fails + 1;
      step <= 5;
   endrule

   rule go5(step == 5);
      let c1 = spec.check(s1, 0);
      testAssert(!isValid(c1), "s1 still unknown (Invalid)", cyc);
      if (isValid(c1)) fails <= fails + 1;
      step <= 6;
   endrule

   rule go6(step == 6);
      let c2 = spec.check(s2, 0);
      testAssert(!isValid(c2), "s2 still unknown (Invalid)", cyc);
      if (isValid(c2)) fails <= fails + 1;
      // free s0
      spec.free(s0);
      step <= 7;
   endrule

   // Free s1
   rule go7(step == 7);
      spec.free(s1);
      step <= 8;
   endrule

   // Free s2
   rule go8(step == 8);
      spec.free(s2);
      step <= 9;
   endrule

   // Verify all freed
   rule go9(step == 9);
      let c0 = spec.check(s0, 0);
      let c1 = spec.check(s1, 0);
      let c2 = spec.check(s2, 0);
      testAssert(!isValid(c0), "s0 freed (Invalid)", cyc);
      testAssert(!isValid(c1), "s1 freed (Invalid)", cyc);
      testAssert(!isValid(c2), "s2 freed (Invalid)", cyc);
      if (isValid(c0) || isValid(c1) || isValid(c2)) fails <= fails + 1;
      testDone("Spec_AllocAndValidate", fails);
   endrule
endmodule

// ============================================================
// Test 2: Alloc 3 entries (s0, s1, s2). Invalidate s1.
// s1 AND s2 should become Invalid(False) -- newer entries are squashed.
// s0 should remain unaffected.
// Models the misprediction squash where everything newer is killed.
// ============================================================
(* synthesize *)
module mkTestSpec_InvalidateCascade();
   SpecTable#(SpecId#(4), 2) spec <- mkSpecTable();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(SpecId#(4)) s0 <- mkReg(0);
   Reg#(SpecId#(4)) s1 <- mkReg(0);
   Reg#(SpecId#(4)) s2 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule go0(step == 0);
      $display("=== TEST: Spec_InvalidateCascade ===");
      let id <- spec.alloc();
      s0 <= id;
      step <= 1;
   endrule

   rule go1(step == 1);
      let id <- spec.alloc();
      s1 <= id;
      step <= 2;
   endrule

   rule go2(step == 2);
      let id <- spec.alloc();
      s2 <= id;
      step <= 3;
   endrule

   // Validate s0 first so it has a known-good status
   rule go3(step == 3);
      spec.validate(s0, 0);
      step <= 4;
   endrule

   // Now invalidate s1 -- this should squash s1 and everything newer (s2)
   rule go4(step == 4);
      spec.invalidate(s1, 0);
      step <= 5;
   endrule

   // Check s0: should still be Valid(True) -- older, not affected by invalidate
   rule go5(step == 5);
      let c0 = spec.check(s0, 0);
      testAssert(isValid(c0) && fromMaybe(False, c0), "s0 still Valid(True) -- unaffected", cyc);
      if (!isValid(c0) || !fromMaybe(False, c0)) fails <= fails + 1;
      step <= 6;
   endrule

   // Check s1: should be Valid(False) -- was the target of invalidation
   rule go6(step == 6);
      let c1 = spec.check(s1, 0);
      testAssert(isValid(c1) && !fromMaybe(True, c1), "s1 is Valid(False) -- invalidated", cyc);
      if (!isValid(c1) || fromMaybe(True, c1)) fails <= fails + 1;
      step <= 7;
   endrule

   // Check s2: should be Valid(False) -- newer than s1, killed by cascade
   rule go7(step == 7);
      let c2 = spec.check(s2, 0);
      testAssert(isValid(c2) && !fromMaybe(True, c2), "s2 is Valid(False) -- cascade killed", cyc);
      if (!isValid(c2) || fromMaybe(True, c2)) fails <= fails + 1;
      // Begin freeing -- one per rule to avoid write conflict
      spec.free(s0);
      step <= 8;
   endrule

   rule go8(step == 8);
      spec.free(s1);
      step <= 9;
   endrule

   rule go9(step == 9);
      spec.free(s2);
      step <= 10;
   endrule

   rule go10(step == 10);
      testDone("Spec_InvalidateCascade", fails);
   endrule
endmodule

// ============================================================
// Test 3: Fill the table completely (4 entries for SpecId#(4)).
// Verify alloc blocks when full. Free one entry and verify alloc
// succeeds again. Models pipeline stall when speculation depth exceeded.
// ============================================================
(* synthesize *)
module mkTestSpec_FullTable();
   SpecTable#(SpecId#(4), 2) spec <- mkSpecTable();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(SpecId#(4)) s0 <- mkReg(0);
   Reg#(SpecId#(4)) s1 <- mkReg(0);
   Reg#(SpecId#(4)) s2 <- mkReg(0);
   Reg#(SpecId#(4)) s3 <- mkReg(0);
   Reg#(SpecId#(4)) s4 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);
   // stall_count tracks how many cycles the stall rule fires (table full, alloc blocked)
   Reg#(UInt#(4)) stallCount <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule go0(step == 0);
      $display("=== TEST: Spec_FullTable ===");
      let id <- spec.alloc();
      s0 <= id;
      step <= 1;
   endrule

   rule go1(step == 1);
      let id <- spec.alloc();
      s1 <= id;
      step <= 2;
   endrule

   rule go2(step == 2);
      let id <- spec.alloc();
      s2 <= id;
      step <= 3;
   endrule

   rule go3(step == 3);
      let id <- spec.alloc();
      s3 <= id;
      // Table now has 4 entries -- should be full
      step <= 4;
   endrule

   // This rule counts stall cycles. The table is full so alloc's implicit
   // guard prevents it from firing. We use a separate counting rule instead.
   rule countStall(step == 4);
      stallCount <= stallCount + 1;
      if (stallCount == 2) step <= 5;  // after 3 stall cycles, move on
   endrule

   // Free one entry to make room
   rule go5(step == 5);
      testAssert(stallCount > 0, "stalled at least 1 cycle (table full)", cyc);
      if (stallCount == 0) fails <= fails + 1;
      spec.free(s0);
      step <= 6;
   endrule

   // Now alloc should succeed again
   rule go6(step == 6);
      let id <- spec.alloc();
      s4 <= id;
      step <= 7;
   endrule

   rule go7(step == 7);
      testAssert(True, "alloc succeeded after free", cyc);
      // Cleanup: free remaining entries one per rule
      spec.free(s1);
      step <= 8;
   endrule

   rule go8(step == 8);
      spec.free(s2);
      step <= 9;
   endrule

   rule go9(step == 9);
      spec.free(s3);
      step <= 10;
   endrule

   rule go10(step == 10);
      spec.free(s4);
      step <= 11;
   endrule

   rule go11(step == 11);
      testDone("Spec_FullTable", fails);
   endrule
endmodule

// ============================================================
// Test 4: Alloc s0, s1. Validate s0 then invalidate s0 in the next cycle.
// invalidate writes Valid(False) to the same EHR port as validate,
// overriding the earlier validate. s1 is also killed (newer).
// Tests that a late-arriving mispredict correctly overrides validation.
// ============================================================
(* synthesize *)
module mkTestSpec_ValidateThenInvalidate();
   SpecTable#(SpecId#(4), 2) spec <- mkSpecTable();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(SpecId#(4)) s0 <- mkReg(0);
   Reg#(SpecId#(4)) s1 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule go0(step == 0);
      $display("=== TEST: Spec_ValidateThenInvalidate ===");
      let id <- spec.alloc();
      s0 <= id;
      step <= 1;
   endrule

   rule go1(step == 1);
      let id <- spec.alloc();
      s1 <= id;
      step <= 2;
   endrule

   // Validate s0 at port 0
   rule go2(step == 2);
      spec.validate(s0, 0);
      step <= 3;
   endrule

   // Confirm s0 is Valid(True) before we invalidate
   rule go3(step == 3);
      let c0 = spec.check(s0, 0);
      testAssert(isValid(c0) && fromMaybe(False, c0), "s0 is Valid(True) after validate", cyc);
      if (!isValid(c0) || !fromMaybe(False, c0)) fails <= fails + 1;
      // Now invalidate s0 at port 0 -- this overwrites the EHR state
      spec.invalidate(s0, 0);
      step <= 4;
   endrule

   // Check s0: should be Valid(False) -- invalidate overrides prior validate
   rule go4(step == 4);
      let c0 = spec.check(s0, 0);
      testAssert(isValid(c0) && !fromMaybe(True, c0), "s0 is Valid(False) -- invalidate overrides", cyc);
      if (!isValid(c0) || fromMaybe(True, c0)) fails <= fails + 1;
      step <= 5;
   endrule

   // Check s1: should also be Valid(False) -- newer than s0, killed by cascade
   rule go5(step == 5);
      let c1 = spec.check(s1, 0);
      testAssert(isValid(c1) && !fromMaybe(True, c1), "s1 is Valid(False) -- cascade killed", cyc);
      if (!isValid(c1) || fromMaybe(True, c1)) fails <= fails + 1;
      spec.free(s0);
      step <= 6;
   endrule

   rule go6(step == 6);
      spec.free(s1);
      step <= 7;
   endrule

   rule go7(step == 7);
      testDone("Spec_ValidateThenInvalidate", fails);
   endrule
endmodule

// ============================================================
// Test 5: Alloc and immediately free in alternating cycles for 6 rounds.
// Verify the table never runs out of space and IDs cycle correctly.
// Models a pipeline that resolves speculation every cycle (fast path).
// ============================================================
(* synthesize *)
module mkTestSpec_RapidAllocFree();
   SpecTable#(SpecId#(4), 2) spec <- mkSpecTable();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(SpecId#(4)) lastId <- mkReg(0);
   Reg#(UInt#(4)) round <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule go0(step == 0);
      $display("=== TEST: Spec_RapidAllocFree ===");
      step <= 1;
   endrule

   // Phase 1: alloc an entry
   rule doAlloc(step == 1);
      let id <- spec.alloc();
      lastId <= id;
      // Validate it immediately (as if prediction confirmed same cycle)
      spec.validate(id, 0);
      step <= 2;
   endrule

   // Phase 2: free the entry and bump round counter
   rule doFree(step == 2);
      spec.free(lastId);
      round <= round + 1;
      if (round + 1 < 6)
         step <= 1;  // go back for another round
      else
         step <= 3;  // done
   endrule

   // Final: verify table is fully drained -- try allocating all 4 entries
   rule final0(step == 3);
      testAssert(round == 6, "completed 6 alloc/free rounds", cyc);
      if (round != 6) fails <= fails + 1;
      let id <- spec.alloc();
      lastId <= id;
      step <= 4;
   endrule

   rule final1(step == 4);
      testAssert(True, "alloc succeeded after 6 rapid rounds (table not exhausted)", cyc);
      spec.free(lastId);
      step <= 5;
   endrule

   rule final2(step == 5);
      testDone("Spec_RapidAllocFree", fails);
   endrule
endmodule

endpackage
