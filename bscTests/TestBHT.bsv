package TestBHT;

import VerilogLibs :: *;
import ConfigReg :: *;
import TestHelper :: *;

// ============================================================
// Test 1: Walk through all 4 states of the 2-bit saturating counter.
// TAKE_W (init) -> taken -> TAKE_S -> not-taken -> TAKE_W
//   -> not-taken -> SKIP_W -> not-taken -> SKIP_S
//   -> taken -> SKIP_W -> taken -> TAKE_W
// Verify prediction at each state transition.
// ============================================================
(* synthesize *)
module mkTestBHT_StateMachine();
   BHT#(32) bht <- mkBHT(4);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Int#(32) pc   = 100;
   Int#(32) skip = 4;
   Int#(32) take = 40;

   rule tick; cyc <= cyc + 1; endrule

   // State: TAKE_W (init) -> predicts taken
   rule s0(step == 0);
      $display("=== TEST: BHT_StateMachine ===");
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + take, "TAKE_W: predict taken", cyc);
      if (pred != pc + take) fails <= fails + 1;
      bht.upd(pc, True);  // TAKE_W -> TAKE_S
      step <= 1;
   endrule

   // State: TAKE_S -> predicts taken
   rule s1(step == 1);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + take, "TAKE_S: predict taken", cyc);
      if (pred != pc + take) fails <= fails + 1;
      bht.upd(pc, False);  // TAKE_S -> TAKE_W
      step <= 2;
   endrule

   // State: TAKE_W -> predicts taken
   rule s2(step == 2);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + take, "TAKE_W: predict taken (returned)", cyc);
      if (pred != pc + take) fails <= fails + 1;
      bht.upd(pc, False);  // TAKE_W -> SKIP_W
      step <= 3;
   endrule

   // State: SKIP_W -> predicts not-taken
   rule s3(step == 3);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + skip, "SKIP_W: predict not-taken", cyc);
      if (pred != pc + skip) fails <= fails + 1;
      bht.upd(pc, False);  // SKIP_W -> SKIP_S
      step <= 4;
   endrule

   // State: SKIP_S -> predicts not-taken
   rule s4(step == 4);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + skip, "SKIP_S: predict not-taken", cyc);
      if (pred != pc + skip) fails <= fails + 1;
      bht.upd(pc, True);  // SKIP_S -> SKIP_W
      step <= 5;
   endrule

   // State: SKIP_W -> predicts not-taken
   rule s5(step == 5);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + skip, "SKIP_W: predict not-taken (returned)", cyc);
      if (pred != pc + skip) fails <= fails + 1;
      bht.upd(pc, True);  // SKIP_W -> TAKE_W
      step <= 6;
   endrule

   // State: TAKE_W -> predicts taken (full cycle completed)
   rule s6(step == 6);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + take, "TAKE_W: predict taken (full cycle done)", cyc);
      if (pred != pc + take) fails <= fails + 1;
      testDone("BHT_StateMachine", fails);
   endrule
endmodule

// ============================================================
// Test 2: Verify saturation behavior of the 2-bit counter.
// Start at TAKE_W, send 5 consecutive "taken" updates -- counter
// should saturate at TAKE_S (no overflow). Then send 5 "not-taken"
// updates -- counter should saturate at SKIP_S.
// ============================================================
(* synthesize *)
module mkTestBHT_SaturationStrong();
   BHT#(32) bht <- mkBHT(4);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Int#(32) pc   = 200;
   Int#(32) skip = 4;
   Int#(32) take = 60;

   rule tick; cyc <= cyc + 1; endrule

   // Steps 0-4: send 5 "taken" updates (init is TAKE_W)
   rule upd_taken_0(step == 0);
      $display("=== TEST: BHT_SaturationStrong ===");
      bht.upd(pc, True);  // TAKE_W -> TAKE_S
      step <= 1;
   endrule

   rule upd_taken_1(step == 1);
      bht.upd(pc, True);  // TAKE_S -> TAKE_S (saturated)
      step <= 2;
   endrule

   rule upd_taken_2(step == 2);
      bht.upd(pc, True);  // still TAKE_S
      step <= 3;
   endrule

   rule upd_taken_3(step == 3);
      bht.upd(pc, True);  // still TAKE_S
      step <= 4;
   endrule

   rule upd_taken_4(step == 4);
      bht.upd(pc, True);  // still TAKE_S
      step <= 5;
   endrule

   // Step 5: verify still predicts taken after saturation
   rule check_saturated_take(step == 5);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + take, "after 5 taken: still TAKE_S (saturated)", cyc);
      if (pred != pc + take) fails <= fails + 1;
      // Begin sending not-taken updates: TAKE_S -> TAKE_W
      bht.upd(pc, False);
      step <= 6;
   endrule

   // Steps 6-9: 4 more not-taken updates
   rule upd_skip_1(step == 6);
      bht.upd(pc, False);  // TAKE_W -> SKIP_W
      step <= 7;
   endrule

   rule upd_skip_2(step == 7);
      bht.upd(pc, False);  // SKIP_W -> SKIP_S
      step <= 8;
   endrule

   rule upd_skip_3(step == 8);
      bht.upd(pc, False);  // SKIP_S -> SKIP_S (saturated)
      step <= 9;
   endrule

   rule upd_skip_4(step == 9);
      bht.upd(pc, False);  // still SKIP_S
      step <= 10;
   endrule

   // Step 10: verify predicts not-taken after saturation
   rule check_saturated_skip(step == 10);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + skip, "after 5 not-taken: SKIP_S (saturated)", cyc);
      if (pred != pc + skip) fails <= fails + 1;
      testDone("BHT_SaturationStrong", fails);
   endrule
endmodule

// ============================================================
// Test 3: Use 3 different PC values indexing different BHT entries.
// Train each independently. Verify predictions are independent --
// updating one PC's history doesn't affect another's.
// ============================================================
(* synthesize *)
module mkTestBHT_DifferentPCs();
   BHT#(32) bht <- mkBHT(16);  // 16 entries to avoid aliasing

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   // Three PCs spaced apart so they hash to different BHT entries.
   // BHT indexes by low bits of PC, so spacing by entry count avoids aliasing.
   Int#(32) pcA = 0;
   Int#(32) pcB = 4;
   Int#(32) pcC = 8;
   Int#(32) skip = 4;
   Int#(32) take = 20;

   rule tick; cyc <= cyc + 1; endrule

   // All three start at TAKE_W. Train pcA to SKIP: not-taken twice.
   rule s0(step == 0);
      $display("=== TEST: BHT_DifferentPCs ===");
      bht.upd(pcA, False);  // pcA: TAKE_W -> SKIP_W (first not-taken when weakly taken)
      step <= 1;
   endrule

   // Wait: state flip takes effect. Now TAKE_W -> after not-taken...
   // Actually TAKE_W + not-taken -> SKIP_W (or could be TAKE_W depending on init).
   // Referring to test 1: TAKE_W + False -> SKIP_W. Good.
   rule s1(step == 1);
      bht.upd(pcA, False);  // pcA: SKIP_W -> SKIP_S
      step <= 2;
   endrule

   // Train pcB strongly taken: taken twice
   rule s2(step == 2);
      bht.upd(pcB, True);  // pcB: TAKE_W -> TAKE_S
      step <= 3;
   endrule

   rule s3(step == 3);
      bht.upd(pcB, True);  // pcB: TAKE_S -> TAKE_S (saturated)
      step <= 4;
   endrule

   // pcC: leave at TAKE_W (no updates -- default init state)
   // Now check all three predictions independently
   rule s4(step == 4);
      let predA = bht.req(pcA, skip, take);
      testAssert(predA == pcA + skip, "pcA predicts not-taken (trained skip)", cyc);
      if (predA != pcA + skip) fails <= fails + 1;
      step <= 5;
   endrule

   rule s5(step == 5);
      let predB = bht.req(pcB, skip, take);
      testAssert(predB == pcB + take, "pcB predicts taken (trained strong-take)", cyc);
      if (predB != pcB + take) fails <= fails + 1;
      step <= 6;
   endrule

   rule s6(step == 6);
      let predC = bht.req(pcC, skip, take);
      testAssert(predC == pcC + take, "pcC predicts taken (untouched, default TAKE_W)", cyc);
      if (predC != pcC + take) fails <= fails + 1;
      step <= 7;
   endrule

   // Now update pcA and verify pcB/pcC unchanged
   rule s7(step == 7);
      bht.upd(pcA, True);  // pcA: SKIP_S -> SKIP_W
      step <= 8;
   endrule

   rule s8(step == 8);
      let predB = bht.req(pcB, skip, take);
      testAssert(predB == pcB + take, "pcB still taken after pcA update", cyc);
      if (predB != pcB + take) fails <= fails + 1;
      step <= 9;
   endrule

   rule s9(step == 9);
      let predC = bht.req(pcC, skip, take);
      testAssert(predC == pcC + take, "pcC still taken after pcA update", cyc);
      if (predC != pcC + take) fails <= fails + 1;
      testDone("BHT_DifferentPCs", fails);
   endrule
endmodule

// ============================================================
// Test 4: In the real pipeline, req (Start stage) and upd (Stage__0
// verify) fire in the same cycle. Since req CF upd in the BVI schedule,
// req reads the pre-update value. Verify this by sending req and upd
// for the same PC in the same rule and checking that req sees old state.
// ============================================================
(* synthesize *)
module mkTestBHT_SameCycleReqUpd();
   BHT#(32) bht <- mkBHT(4);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Int#(32) pc   = 300;
   Int#(32) skip = 4;
   Int#(32) take = 50;

   rule tick; cyc <= cyc + 1; endrule

   // Init: TAKE_W. Move to SKIP_W by sending two not-taken.
   rule s0(step == 0);
      $display("=== TEST: BHT_SameCycleReqUpd ===");
      bht.upd(pc, False);  // TAKE_W -> SKIP_W
      step <= 1;
   endrule

   // Confirm: SKIP_W predicts not-taken. Also move to SKIP_S.
   rule s1(step == 1);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + skip, "SKIP_W: predict not-taken (baseline)", cyc);
      if (pred != pc + skip) fails <= fails + 1;
      bht.upd(pc, False);  // SKIP_W -> SKIP_S
      step <= 2;
   endrule

   // Now at SKIP_S. Issue req and upd(taken) in the same cycle.
   // req should see SKIP_S (not-taken) even though upd is moving to SKIP_W.
   // This is the critical same-cycle test: req reads pre-update state.
   rule s2(step == 2);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + skip, "same-cycle: req sees SKIP_S (pre-update)", cyc);
      if (pred != pc + skip) fails <= fails + 1;
      bht.upd(pc, True);  // SKIP_S -> SKIP_W (but req already read SKIP_S)
      step <= 3;
   endrule

   // Next cycle: state is now SKIP_W after the update. Verify.
   rule s3(step == 3);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + skip, "next cycle: SKIP_W predicts not-taken", cyc);
      if (pred != pc + skip) fails <= fails + 1;
      // Send taken to move SKIP_W -> TAKE_W
      bht.upd(pc, True);
      step <= 4;
   endrule

   // Now at TAKE_W. Same-cycle test again: req + upd(not-taken).
   // req should see TAKE_W (taken) even though upd is moving to SKIP_W.
   rule s4(step == 4);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + take, "same-cycle: req sees TAKE_W (pre-update)", cyc);
      if (pred != pc + take) fails <= fails + 1;
      bht.upd(pc, False);  // TAKE_W -> SKIP_W (but req already read TAKE_W)
      step <= 5;
   endrule

   // Confirm the update took effect: should be SKIP_W now
   rule s5(step == 5);
      let pred = bht.req(pc, skip, take);
      testAssert(pred == pc + skip, "post-update: SKIP_W predicts not-taken", cyc);
      if (pred != pc + skip) fails <= fails + 1;
      testDone("BHT_SameCycleReqUpd", fails);
   endrule
endmodule

// ============================================================
// Test 5: Two PCs that alias to the same BHT entry (same low bits,
// different high bits). Train with one PC, then predict with the
// aliased PC. They should share the same counter -- this is expected
// BHT behavior (not a bug, just a design tradeoff).
// ============================================================
(* synthesize *)
module mkTestBHT_AliasingBehavior();
   // 4 entries: index = pc[1:0] (low 2 bits determine entry)
   BHT#(32) bht <- mkBHT(4);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   // pcX and pcY alias: same low bits, different high bits.
   // With 4 entries the index is pc mod 4. Both have the same mod-4 value.
   Int#(32) pcX = 5;    // 5 mod 4 = 1
   Int#(32) pcY = 9;    // 9 mod 4 = 1 (aliases with pcX)
   Int#(32) pcZ = 6;    // 6 mod 4 = 2 (different entry, control)
   Int#(32) skip = 4;
   Int#(32) take = 20;

   rule tick; cyc <= cyc + 1; endrule

   // Baseline: all entries start at TAKE_W
   rule s0(step == 0);
      $display("=== TEST: BHT_AliasingBehavior ===");
      let predX = bht.req(pcX, skip, take);
      testAssert(predX == pcX + take, "pcX init: TAKE_W (taken)", cyc);
      if (predX != pcX + take) fails <= fails + 1;
      step <= 1;
   endrule

   // Train pcX to not-taken: TAKE_W -> SKIP_W
   rule s1(step == 1);
      bht.upd(pcX, False);  // TAKE_W -> SKIP_W
      step <= 2;
   endrule

   rule s2(step == 2);
      bht.upd(pcX, False);  // SKIP_W -> SKIP_S
      step <= 3;
   endrule

   // Predict with pcY -- should see the same counter as pcX (aliased)
   rule s3(step == 3);
      let predY = bht.req(pcY, skip, take);
      testAssert(predY == pcY + skip, "pcY aliased: sees SKIP_S from pcX training", cyc);
      if (predY != pcY + skip) fails <= fails + 1;
      step <= 4;
   endrule

   // Predict with pcZ -- different entry, should still be TAKE_W (untouched)
   rule s4(step == 4);
      let predZ = bht.req(pcZ, skip, take);
      testAssert(predZ == pcZ + take, "pcZ non-aliased: still TAKE_W (independent)", cyc);
      if (predZ != pcZ + take) fails <= fails + 1;
      step <= 5;
   endrule

   // Now update via pcY (the alias) and verify pcX sees the change
   rule s5(step == 5);
      bht.upd(pcY, True);  // SKIP_S -> SKIP_W (through alias)
      step <= 6;
   endrule

   rule s6(step == 6);
      bht.upd(pcY, True);  // SKIP_W -> TAKE_W (through alias)
      step <= 7;
   endrule

   // pcX should now see TAKE_W -- the alias update propagated
   rule s7(step == 7);
      let predX = bht.req(pcX, skip, take);
      testAssert(predX == pcX + take, "pcX sees TAKE_W after pcY alias update", cyc);
      if (predX != pcX + take) fails <= fails + 1;
      step <= 8;
   endrule

   // pcZ should still be unaffected
   rule s8(step == 8);
      let predZ = bht.req(pcZ, skip, take);
      testAssert(predZ == pcZ + take, "pcZ still TAKE_W (never aliased)", cyc);
      if (predZ != pcZ + take) fails <= fails + 1;
      testDone("BHT_AliasingBehavior", fails);
   endrule
endmodule

endpackage
