package TestForwardRenameRF;

import VerilogLibs :: *;
import ConfigReg :: *;
import TestHelper :: *;

// Types: 8 arch regs (UInt#(3)), 16 phys regs (UInt#(4)), Int#(32) data.
// ForwardRenameRF has combinational write-to-read forwarding:
//   - read(n) in the same cycle as write(n, d) returns d (bypassed).
//   - owns_r2(n) returns true when read(n) and write(n, d) fire together,
//     because FWD22 = WE_2 & (NAME_IN_2 == NAME_2) feeds VALID_OUT_2.
// Note: The BSV write method maps to Verilog port 2 (NAME_IN_2/D_IN_2/WE_2)
// and read maps to port 2 (NAME_2/D_OUT_2). Port 1 is not exposed by BSV.

// ============================================================
// Test 1: Alloc for arch reg 1. In the SAME cycle, write data 77
// and read the same name. Verify read returns 77 (combinational
// forward). Also verify owns_r2 returns true in the same cycle.
// ============================================================
(* synthesize *)
module mkTestFRR_BasicForward();
   RenameRF#(UInt#(3), Int#(32), UInt#(4)) rf <- mkForwardRenameRF(8, 16, False, "");

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);
   Reg#(UInt#(4)) savedName <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc phys name for arch reg 1. Gets phys 8 (lowest free).
   // res_w1 sets busy[8]=1 on posedge.
   rule s0(step == 0);
      $display("=== TEST: FRR_BasicForward ===");
      let n <- rf.res_w1(1);
      testAssert(n == 8, "alloc for r1 returns phys 8", cyc);
      if (n != 8) fails <= fails + 1;
      savedName <= n;
      step <= 1;
   endrule

   // Step 1: Same-cycle write + read + owns.
   // BSC assigns write to port 1 and read to port 1 typically.
   // FWD11 = WE_1 & (NAME_IN_1==NAME_1) -> forwarding on port 1.
   // Check both owns ports -- at least one should forward.
   rule s1(step == 1);
      rf.write(savedName, 77);
      let d = rf.read(savedName);
      testAssert(d == 77, "same-cycle: read returns 77 (forwarded)", cyc);
      let v1 = rf.owns_r1(savedName);
      let v2 = rf.owns_r2(savedName);
      testAssert(v1 || v2, "same-cycle: at least one owns port forwards", cyc);
      if (d != 77 || !(v1 || v2)) fails <= fails + 1;
      step <= 2;
   endrule

   // Step 2: Next cycle, no write. read should return 77 from phys reg.
   rule s2(step == 2);
      let d = rf.read(savedName);
      testAssert(d == 77, "next cycle: read returns 77 (from phys reg)", cyc);
      let v = rf.owns_r1(savedName);
      testAssert(v, "next cycle: owns_r1 returns true (busy cleared)", cyc);
      if (d != 77 || !v) fails <= fails + 1;
      testDone("FRR_BasicForward", fails);
   endrule
endmodule

// ============================================================
// Test 2: Alloc for arch reg 2. Call write and read in same cycle
// to verify forwarding. Then next cycle without writing, verify
// read still returns the written data from phys reg storage.
// ============================================================
(* synthesize *)
module mkTestFRR_ForwardVsNoForward();
   RenameRF#(UInt#(3), Int#(32), UInt#(4)) rf <- mkForwardRenameRF(8, 16, False, "");

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);
   Reg#(UInt#(4)) savedName <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc for arch reg 2.
   rule s0(step == 0);
      $display("=== TEST: FRR_ForwardVsNoForward ===");
      let n <- rf.res_w1(2);
      testAssert(n == 8, "alloc for r2 returns phys 8", cyc);
      if (n != 8) fails <= fails + 1;
      savedName <= n;
      step <= 1;
   endrule

   // Step 1: Same-cycle write(8, 55) + read(8). Forwarding active.
   rule s1(step == 1);
      rf.write(savedName, 55);
      let d = rf.read(savedName);
      testAssert(d == 55, "same-cycle: read returns 55 (forwarded)", cyc);
      if (d != 55) fails <= fails + 1;
      step <= 2;
   endrule

   // Step 2: Next cycle, no write. read(8) should return 55 from phys[8].
   // No forwarding path active (WE_2=0), so data comes from register.
   rule s2(step == 2);
      let d = rf.read(savedName);
      testAssert(d == 55, "next cycle: read returns 55 (from phys reg, no fwd)", cyc);
      if (d != 55) fails <= fails + 1;
      step <= 3;
   endrule

   // Step 3: Write a new value with forwarding, verify read sees new data.
   rule s3(step == 3);
      rf.write(savedName, -123);
      let d = rf.read(savedName);
      testAssert(d == -123, "same-cycle: read returns -123 (forwarded new value)", cyc);
      if (d != -123) fails <= fails + 1;
      step <= 4;
   endrule

   // Step 4: Verify persisted value.
   rule s4(step == 4);
      let d = rf.read(savedName);
      testAssert(d == -123, "persisted: read returns -123 from phys reg", cyc);
      if (d != -123) fails <= fails + 1;
      testDone("FRR_ForwardVsNoForward", fails);
   endrule
endmodule

// ============================================================
// Test 3: Alloc two different phys names (for two arch regs).
// Write to name A in one cycle with forwarding read, then write
// to name B in the next cycle with forwarding read. Verify each
// forwarding path works independently.
// (BSV exposes one write port, so we test sequential forwarding
// to two different names across cycles.)
// ============================================================
(* synthesize *)
module mkTestFRR_TwoNameForward();
   RenameRF#(UInt#(3), Int#(32), UInt#(4)) rf <- mkForwardRenameRF(8, 16, False, "");

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);
   Reg#(UInt#(4)) nameA <- mkReg(0);
   Reg#(UInt#(4)) nameB <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc phys name for arch reg 3. Gets phys 8.
   rule s0(step == 0);
      $display("=== TEST: FRR_TwoNameForward ===");
      let nA <- rf.res_w1(3);
      testAssert(nA == 8, "alloc for r3 returns phys 8", cyc);
      if (nA != 8) fails <= fails + 1;
      nameA <= nA;
      step <= 1;
   endrule

   // Step 1: Alloc phys name for arch reg 4. Gets phys 9.
   rule s1(step == 1);
      let nB <- rf.res_w1(4);
      testAssert(nB == 9, "alloc for r4 returns phys 9", cyc);
      if (nB != 9) fails <= fails + 1;
      nameB <= nB;
      step <= 2;
   endrule

   // Step 2: Write to name A with forwarding read.
   rule s2(step == 2);
      rf.write(nameA, 333);
      let dA = rf.read(nameA);
      testAssert(dA == 333, "fwd read nameA: returns 333", cyc);
      if (dA != 333) fails <= fails + 1;
      step <= 3;
   endrule

   // Step 3: Write to name B with forwarding read. Also verify name A
   // persisted from last cycle (read without forwarding).
   rule s3(step == 3);
      rf.write(nameB, 444);
      let dB = rf.read(nameB);
      testAssert(dB == 444, "fwd read nameB: returns 444", cyc);
      if (dB != 444) fails <= fails + 1;
      step <= 4;
   endrule

   // Step 4: Read both names without any write (no forwarding active).
   // Both should return their persisted values.
   rule s4(step == 4);
      let dA = rf.read(nameA);
      testAssert(dA == 333, "persisted read nameA: returns 333", cyc);
      if (dA != 333) fails <= fails + 1;
      step <= 5;
   endrule

   rule s5(step == 5);
      let dB = rf.read(nameB);
      testAssert(dB == 444, "persisted read nameB: returns 444", cyc);
      if (dB != 444) fails <= fails + 1;
      testDone("FRR_TwoNameForward", fails);
   endrule
endmodule

// ============================================================
// Test 4: Verify that forwarding takes priority over stale
// register data. Write value X to a name, wait for it to persist.
// Then in a later cycle, write a NEW value Y to the same name
// and read in the same cycle. The read should return Y (forwarded),
// not X (stale phys reg value). This confirms the forwarding
// mux selects D_IN over phys[NAME] when WE is active.
// ============================================================
(* synthesize *)
module mkTestFRR_WriteForwardPriority();
   RenameRF#(UInt#(3), Int#(32), UInt#(4)) rf <- mkForwardRenameRF(8, 16, False, "");

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);
   Reg#(UInt#(4)) savedName <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc for arch reg 5. Gets phys 8.
   rule s0(step == 0);
      $display("=== TEST: FRR_WriteForwardPriority ===");
      let n <- rf.res_w1(5);
      savedName <= n;
      step <= 1;
   endrule

   // Step 1: Write initial value 100 to phys 8 (with forwarding read).
   rule s1(step == 1);
      rf.write(savedName, 100);
      let d = rf.read(savedName);
      testAssert(d == 100, "initial write+read: forwarded 100", cyc);
      if (d != 100) fails <= fails + 1;
      step <= 2;
   endrule

   // Step 2: No write. Verify 100 persisted in phys reg.
   rule s2(step == 2);
      let d = rf.read(savedName);
      testAssert(d == 100, "persisted: phys reg holds 100", cyc);
      if (d != 100) fails <= fails + 1;
      step <= 3;
   endrule

   // Step 3: Overwrite with 200. Same-cycle read should see 200 (forwarded),
   // NOT the stale 100 from the phys reg. This proves forwarding priority.
   rule s3(step == 3);
      rf.write(savedName, 200);
      let d = rf.read(savedName);
      testAssert(d == 200, "overwrite+read: forwarded 200 (not stale 100)", cyc);
      // Also verify owns_r2 sees forwarded validity
      let v = rf.owns_r2(savedName);
      testAssert(v, "overwrite: owns_r2 true via forwarding", cyc);
      if (d != 200 || !v) fails <= fails + 1;
      step <= 4;
   endrule

   // Step 4: Verify the overwritten value persisted.
   rule s4(step == 4);
      let d = rf.read(savedName);
      testAssert(d == 200, "persisted: phys reg now holds 200", cyc);
      if (d != 200) fails <= fails + 1;
      testDone("FRR_WriteForwardPriority", fails);
   endrule
endmodule

// ============================================================
// Test 5: Alloc for arch reg 5, write data, and in the same cycle
// call res_r1(5). In the Verilog, res_w1 updates names[] on posedge,
// so res_r1 in the same cycle sees the OLD mapping (not the newly
// allocated name). Next cycle, res_r1(5) should return the new name.
// ============================================================
(* synthesize *)
module mkTestFRR_AllocAndImmediateRead();
   RenameRF#(UInt#(3), Int#(32), UInt#(4)) rf <- mkForwardRenameRF(8, 16, False, "");

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);
   Reg#(UInt#(4)) allocName <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc for arch reg 5. Initial mapping: arch 5 -> phys 5.
   // res_w1(5) returns phys 8, updates names[5]=8 on posedge.
   // In the SAME cycle, res_r1(5) reads names[5] combinationally,
   // which is still 5 (the old mapping, pre-posedge).
   rule s0(step == 0);
      $display("=== TEST: FRR_AllocAndImmediateRead ===");
      let n <- rf.res_w1(5);
      testAssert(n == 8, "alloc for r5 returns phys 8", cyc);
      allocName <= n;
      // Same cycle: res_r1(5) should return OLD mapping (phys 5).
      let cur = rf.res_r1(5);
      testAssert(cur == 5, "same-cycle: res_r1(5) returns OLD mapping (phys 5)", cyc);
      // Also check res_r2 for same behavior
      let cur2 = rf.res_r2(5);
      testAssert(cur2 == 5, "same-cycle: res_r2(5) returns OLD mapping (phys 5)", cyc);
      if (n != 8 || cur != 5 || cur2 != 5) fails <= fails + 1;
      step <= 1;
   endrule

   // Step 1: Next cycle. names[5] is now 8 (updated on posedge).
   // res_r1(5) should return 8 (new mapping).
   // Write data to the allocated name so we can verify the full chain.
   rule s1(step == 1);
      let cur = rf.res_r1(5);
      testAssert(cur == 8, "next cycle: res_r1(5) returns NEW mapping (phys 8)", cyc);
      if (cur != 8) fails <= fails + 1;
      rf.write(allocName, 999);
      step <= 2;
   endrule

   // Step 2: Verify the data is readable via the new mapping.
   rule s2(step == 2);
      let cur = rf.res_r1(5);
      let d = rf.read(cur);
      testAssert(d == 999, "read via new mapping returns 999", cyc);
      // Also verify owns is true (busy cleared by write last cycle).
      let v = rf.owns_r1(cur);
      testAssert(v, "owns_r1 returns true (write completed)", cyc);
      if (d != 999 || !v) fails <= fails + 1;
      testDone("FRR_AllocAndImmediateRead", fails);
   endrule
endmodule

endpackage
