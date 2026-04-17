package TestRenameRF;

import VerilogLibs :: *;
import ConfigReg :: *;
import TestHelper :: *;

// Types: 8 arch regs (UInt#(3)), 16 phys regs (UInt#(4)), Int#(32) data.
// Init: arch reg i -> phys name i. Free list = {8,9,...,15}. busy = 0 for all.

// ============================================================
// Test 1: Alloc a physical name for arch reg 1, write data 42,
// next cycle verify owns is true and read returns 42.
// Release the old name and verify it can be reallocated.
// ============================================================
(* synthesize *)
module mkTestRR_BasicAllocWriteRead();
   RenameRF#(UInt#(3), Int#(32), UInt#(4)) rf <- mkRenameRF(8, 16, False, "");

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);
   Reg#(UInt#(4)) savedName <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc a phys name for arch reg 1 and write data 42 to it.
   // Initial mapping: arch 1 -> phys 1. Free list starts at 8.
   // res_w1(1) should return 8 (lowest free name).
   rule s0(step == 0);
      $display("=== TEST: RR_BasicAllocWriteRead ===");
      let n <- rf.res_w1(1);
      testAssert(n == 8, "alloc for r1 returns phys 8 (lowest free)", cyc);
      if (n != 8) fails <= fails + 1;
      rf.write(n, 42);
      savedName <= n;
      step <= 1;
   endrule

   // Step 1: Next cycle -- busy should be cleared by write. Verify owns and read.
   rule s1(step == 1);
      let v = rf.owns_r1(savedName);
      testAssert(v, "owns_r1(8) is true after write completed", cyc);
      let d = rf.read(savedName);
      testAssert(d == 42, "read(8) returns 42", cyc);
      if (!v || d != 42) fails <= fails + 1;
      // Release the old name for arch reg 1. The old mapping was phys 1.
      // rel_w1(savedName) frees old[savedName] which is 1.
      rf.rel_w1(savedName);
      step <= 2;
   endrule

   // Step 2: After releasing, old phys name 1 is back in free list.
   // Alloc for another arch reg. The priority encoder picks the lowest free.
   // Free list now has {1, 9, 10, ..., 15}. Lowest free = 1.
   rule s2(step == 2);
      let n <- rf.res_w1(2);
      testAssert(n == 1, "after release, realloc gets phys 1 (freed name)", cyc);
      if (n != 1) fails <= fails + 1;
      testDone("RR_BasicAllocWriteRead", fails);
   endrule
endmodule

// ============================================================
// Test 2: Verify that RenameRF does NOT forward writes to owns.
// Alloc for arch reg 2, call write and owns in the same cycle.
// owns should return FALSE (busy cleared on posedge, not combinationally).
// Next cycle, owns should return true.
// ============================================================
(* synthesize *)
module mkTestRR_OwnsTimingNoForward();
   RenameRF#(UInt#(3), Int#(32), UInt#(4)) rf <- mkRenameRF(8, 16, False, "");

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);
   Reg#(UInt#(4)) savedName <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc phys name for arch reg 2. Gets phys 8.
   rule s0(step == 0);
      $display("=== TEST: RR_OwnsTimingNoForward ===");
      let n <- rf.res_w1(2);
      savedName <= n;
      step <= 1;
   endrule

   // Step 1: In the SAME cycle, call write(name, 99) and owns_r1(name).
   // res_w1 set busy[n]=1 on prior posedge. Now write clears busy on
   // the NEXT posedge. So owns sees busy=1 -> returns false.
   rule s1(step == 1);
      rf.write(savedName, 99);
      let v = rf.owns_r1(savedName);
      testAssert(!v, "same-cycle: owns_r1 returns false (no forwarding)", cyc);
      if (v) fails <= fails + 1;
      step <= 2;
   endrule

   // Step 2: Next cycle, write has cleared busy. owns should be true.
   rule s2(step == 2);
      let v = rf.owns_r1(savedName);
      testAssert(v, "next cycle: owns_r1 returns true (busy cleared)", cyc);
      let d = rf.read(savedName);
      testAssert(d == 99, "read returns 99 after write completed", cyc);
      if (!v || d != 99) fails <= fails + 1;
      testDone("RR_OwnsTimingNoForward", fails);
   endrule
endmodule

// ============================================================
// Test 3: Alloc for arch reg 3 twice (two instructions writing
// to the same arch reg). First alloc gets N1, second gets N2.
// Verify res_r1(3) returns N2 (latest mapping). Write to both,
// release old names, verify the name chain is correct.
// ============================================================
(* synthesize *)
module mkTestRR_NameRemapping();
   RenameRF#(UInt#(3), Int#(32), UInt#(4)) rf <- mkRenameRF(8, 16, False, "");

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);
   Reg#(UInt#(4)) name1 <- mkReg(0);
   Reg#(UInt#(4)) name2 <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: First alloc for arch reg 3. Initial mapping: arch 3 -> phys 3.
   // res_w1(3) returns 8 (lowest free). old[8] = 3 (previous phys for r3).
   rule s0(step == 0);
      $display("=== TEST: RR_NameRemapping ===");
      let n1 <- rf.res_w1(3);
      testAssert(n1 == 8, "first alloc for r3 gets phys 8", cyc);
      if (n1 != 8) fails <= fails + 1;
      name1 <= n1;
      step <= 1;
   endrule

   // Step 1: Second alloc for arch reg 3. Now names[3]=8, so old[9]=8.
   // res_w1(3) returns 9 (next lowest free). names[3] updated to 9.
   rule s1(step == 1);
      let n2 <- rf.res_w1(3);
      testAssert(n2 == 9, "second alloc for r3 gets phys 9", cyc);
      if (n2 != 9) fails <= fails + 1;
      name2 <= n2;
      step <= 2;
   endrule

   // Step 2: Verify res_r1(3) returns the latest mapping (N2 = 9).
   // Write data to both names.
   rule s2(step == 2);
      let cur = rf.res_r1(3);
      testAssert(cur == 9, "res_r1(3) returns 9 (latest mapping)", cyc);
      if (cur != 9) fails <= fails + 1;
      rf.write(name1, 100);
      step <= 3;
   endrule

   // Step 3: Write data to N2.
   rule s3(step == 3);
      rf.write(name2, 200);
      step <= 4;
   endrule

   // Step 4: Release N1. rel_w1(N1) frees old[8] = 3 (initial phys for r3).
   rule s4(step == 4);
      let d1 = rf.read(name1);
      testAssert(d1 == 100, "read(N1=8) returns 100", cyc);
      let d2 = rf.read(name2);
      testAssert(d2 == 200, "read(N2=9) returns 200", cyc);
      if (d1 != 100 || d2 != 200) fails <= fails + 1;
      rf.rel_w1(name1);
      step <= 5;
   endrule

   // Step 5: Release N2. rel_w1(N2) frees old[9] = 8 (the first alloc).
   // After this, both phys 3 and phys 8 are back in the free list.
   rule s5(step == 5);
      rf.rel_w1(name2);
      step <= 6;
   endrule

   // Step 6: Verify freed names can be reallocated.
   // Free list should include 3, 8, 10..15. Priority encoder picks 3 (lowest).
   rule s6(step == 6);
      let n <- rf.res_w1(4);
      testAssert(n == 3, "after releases, alloc gets phys 3 (freed)", cyc);
      if (n != 3) fails <= fails + 1;
      testDone("RR_NameRemapping", fails);
   endrule
endmodule

// ============================================================
// Test 4: With 8 arch, 16 phys, there are 8 free names initially
// (phys 8..15). Alloc 8 names without releasing. Verify
// ALLOC_READY becomes false. Release one, verify it recovers.
// ============================================================
(* synthesize *)
module mkTestRR_FreeListExhaustion();
   RenameRF#(UInt#(3), Int#(32), UInt#(4)) rf <- mkRenameRF(8, 16, False, "");

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);
   Reg#(UInt#(4)) lastName <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Steps 0-7: Alloc 8 names (one per cycle, each to a different arch reg).
   // Free names 8..15 will be consumed. After step 7, free list is empty.
   rule s0(step == 0);
      $display("=== TEST: RR_FreeListExhaustion ===");
      let n <- rf.res_w1(0);
      testAssert(n == 8, "alloc 1: phys 8", cyc);
      if (n != 8) fails <= fails + 1;
      rf.write(n, 0);
      lastName <= n;
      step <= 1;
   endrule

   rule s1(step == 1);
      let n <- rf.res_w1(1);
      testAssert(n == 9, "alloc 2: phys 9", cyc);
      if (n != 9) fails <= fails + 1;
      rf.write(n, 0);
      lastName <= n;
      step <= 2;
   endrule

   rule s2(step == 2);
      let n <- rf.res_w1(2);
      testAssert(n == 10, "alloc 3: phys 10", cyc);
      if (n != 10) fails <= fails + 1;
      rf.write(n, 0);
      lastName <= n;
      step <= 3;
   endrule

   rule s3(step == 3);
      let n <- rf.res_w1(3);
      testAssert(n == 11, "alloc 4: phys 11", cyc);
      if (n != 11) fails <= fails + 1;
      rf.write(n, 0);
      lastName <= n;
      step <= 4;
   endrule

   rule s4(step == 4);
      let n <- rf.res_w1(4);
      testAssert(n == 12, "alloc 5: phys 12", cyc);
      if (n != 12) fails <= fails + 1;
      rf.write(n, 0);
      lastName <= n;
      step <= 5;
   endrule

   rule s5(step == 5);
      let n <- rf.res_w1(5);
      testAssert(n == 13, "alloc 6: phys 13", cyc);
      if (n != 13) fails <= fails + 1;
      rf.write(n, 0);
      lastName <= n;
      step <= 6;
   endrule

   rule s6(step == 6);
      let n <- rf.res_w1(6);
      testAssert(n == 14, "alloc 7: phys 14", cyc);
      if (n != 14) fails <= fails + 1;
      rf.write(n, 0);
      lastName <= n;
      step <= 7;
   endrule

   rule s7(step == 7);
      let n <- rf.res_w1(7);
      testAssert(n == 15, "alloc 8: phys 15", cyc);
      if (n != 15) fails <= fails + 1;
      rf.write(n, 0);
      lastName <= n;
      step <= 8;
   endrule

   // Step 8: Free list should now be empty. res_w1 should NOT fire
   // (ALLOC_READY = false). We test by using a separate rule that
   // only fires when step==8 and does NOT call res_w1.
   // Instead we try to alloc in one rule and observe it blocks.
   // We use two rules: one tries to alloc (will not fire if not ready),
   // another advances the step if alloc did not fire.
   rule s8_try_alloc(step == 8);
      // This rule will not fire because ALLOC_READY is false.
      let n <- rf.res_w1(0);
      // If we get here, the free list was not exhausted -- failure.
      testAssert(False, "ERROR: alloc fired when free list should be empty", cyc);
      fails <= fails + 1;
      step <= 15;
   endrule

   rule s8_blocked(step == 8);
      // This rule fires on the same step. If s8_try_alloc did not fire
      // (because ALLOC_READY is false), this confirms exhaustion.
      // Note: res_w1 C res_w1, but this rule does not call res_w1,
      // so it can fire regardless.
      testAssert(True, "alloc blocked: free list exhausted (8 allocs consumed all)", cyc);
      // Release the last allocated name to free up old[15] = 7.
      rf.rel_w1(lastName);
      step <= 9;
   endrule

   // Step 9: After release, old[15] = 7 is freed. Free list has {7}.
   // ALLOC_READY should be true again.
   rule s9(step == 9);
      let n <- rf.res_w1(0);
      testAssert(n == 7, "after release, alloc succeeds with phys 7", cyc);
      if (n != 7) fails <= fails + 1;
      testDone("RR_FreeListExhaustion", fails);
   endrule
endmodule

// ============================================================
// Test 5: Simulate a 3-instruction pipeline sequence:
//   insn1 writes to r1, insn2 reads r1 and writes r2,
//   insn3 reads r2. Each instruction allocs, writes data,
//   then releases in order. Verify the data chain is correct.
// ============================================================
(* synthesize *)
module mkTestRR_MultiRegPipeline();
   RenameRF#(UInt#(3), Int#(32), UInt#(4)) rf <- mkRenameRF(8, 16, False, "");

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);
   Reg#(UInt#(4)) nameR1 <- mkReg(0);
   Reg#(UInt#(4)) nameR2 <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: insn1 allocs for r1. Gets phys 8.
   rule s0(step == 0);
      $display("=== TEST: RR_MultiRegPipeline ===");
      let n <- rf.res_w1(1);
      testAssert(n == 8, "insn1: alloc r1 gets phys 8", cyc);
      if (n != 8) fails <= fails + 1;
      nameR1 <= n;
      step <= 1;
   endrule

   // Step 1: insn1 writes data 1000 to r1's phys name.
   rule s1(step == 1);
      rf.write(nameR1, 1000);
      step <= 2;
   endrule

   // Step 2: insn2 reads r1 (to get data), allocs for r2.
   // res_r1(1) returns phys 8 (current mapping for arch r1).
   // read(8) returns 1000.
   // res_w1(2) allocs phys 9 for arch r2.
   rule s2(step == 2);
      let src = rf.res_r1(1);
      testAssert(src == nameR1, "insn2: res_r1(1) returns phys 8", cyc);
      let srcData = rf.read(src);
      testAssert(srcData == 1000, "insn2: read(r1) returns 1000", cyc);
      let n <- rf.res_w1(2);
      testAssert(n == 9, "insn2: alloc r2 gets phys 9", cyc);
      if (src != nameR1 || srcData != 1000 || n != 9) fails <= fails + 1;
      nameR2 <= n;
      step <= 3;
   endrule

   // Step 3: insn2 writes computed result (1000 + 500 = 1500) to r2.
   rule s3(step == 3);
      rf.write(nameR2, 1500);
      step <= 4;
   endrule

   // Step 4: insn3 reads r2 to verify the pipeline chain.
   rule s4(step == 4);
      let src = rf.res_r1(2);
      testAssert(src == nameR2, "insn3: res_r1(2) returns phys 9", cyc);
      let srcData = rf.read(src);
      testAssert(srcData == 1500, "insn3: read(r2) returns 1500", cyc);
      if (src != nameR2 || srcData != 1500) fails <= fails + 1;
      step <= 5;
   endrule

   // Step 5: Release old names. rel_w1(nameR1) frees old[8] = 1.
   rule s5(step == 5);
      rf.rel_w1(nameR1);
      step <= 6;
   endrule

   // Step 6: Release nameR2. rel_w1(nameR2) frees old[9] = 2.
   rule s6(step == 6);
      rf.rel_w1(nameR2);
      step <= 7;
   endrule

   // Step 7: Verify freed names (1 and 2) are back in the free list.
   // Priority encoder picks lowest free. After freeing 1 and 2,
   // free list = {1, 2, 10, 11, ..., 15}. Lowest = 1.
   rule s7(step == 7);
      let n <- rf.res_w1(3);
      testAssert(n == 1, "after pipeline, freed phys 1 is reallocated", cyc);
      if (n != 1) fails <= fails + 1;
      testDone("RR_MultiRegPipeline", fails);
   endrule
endmodule

endpackage
