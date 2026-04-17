package TestBypassLock;

import Locks :: *;
import Memories :: *;
import RegFile :: *;
import ConfigReg :: *;
import TestHelper :: *;

typedef UInt#(5) Addr;
typedef UInt#(32) Data;

// ============================================================
// Test 1: Full lifecycle -- reserve, write, bypass-read, release, verify RF
// Models the golden path: Stage__0 res_w1, execute write, Stage__25 atom_r, writeback rel_w1.
// ============================================================
(* synthesize *)
module mkTestBP_ReserveWriteReadRelease();
   RegFile#(Addr, Data) rf <- mkRegFileFull();
   BypassLockCombMem#(Addr, Data, LockId#(4), 4) mem <- mkBypassLockCombMem(rf);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) wid <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Addr target = 5;

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: BP_ReserveWriteReadRelease ===");
      let id <- mem.res_w1(target);
      wid <= id;
      step <= 1;
   endrule

   // Write data to the reserved slot (separate rule from read)
   rule s1(step == 1);
      mem.write(wid, 42);
      step <= 2;
   endrule

   // canAtom_r1 should be true now (data available via bypass)
   // atom_r reads bypass -- separate rule from write
   rule s2(step == 2);
      testAssert(mem.canAtom_r1(target), "canAtom true after write", cyc);
      if (!mem.canAtom_r1(target)) fails <= fails + 1;
      step <= 3;
   endrule

   rule s3(step == 3);
      let v = mem.atom_r(target);
      testAssert(v == 42, "bypass read == 42", cyc);
      if (v != 42) fails <= fails + 1;
      step <= 4;
   endrule

   // Release (commit to RF) -- rel_w1 triggers doCommit rule
   rule s4(step == 4);
      mem.rel_w1(wid);
      step <= 5;
   endrule

   // After commit, RF should have the value; no more bypass entries
   rule s5(step == 5);
      let v = mem.atom_r(target);
      testAssert(v == 42, "rf value == 42 after commit", cyc);
      if (v != 42) fails <= fails + 1;
      testDone("BP_ReserveWriteReadRelease", fails);
   endrule
endmodule

// ============================================================
// Test 2: Read before write -- canAtom should be false until data written
// Models the stall behavior of owns_r1 in Stage__25.
// ============================================================
(* synthesize *)
module mkTestBP_ReadBeforeWrite();
   RegFile#(Addr, Data) rf <- mkRegFileFull();
   BypassLockCombMem#(Addr, Data, LockId#(4), 4) mem <- mkBypassLockCombMem(rf);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) wid <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Addr target = 10;

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: BP_ReadBeforeWrite ===");
      let id <- mem.res_w1(target);
      wid <= id;
      step <= 1;
   endrule

   // Check canAtom BEFORE writing data -- should be false (data not yet available)
   rule s1(step == 1);
      testAssert(!mem.canAtom_r1(target), "canAtom false before write", cyc);
      if (mem.canAtom_r1(target)) fails <= fails + 1;
      step <= 2;
   endrule

   // Now write data
   rule s2(step == 2);
      mem.write(wid, 99);
      step <= 3;
   endrule

   // canAtom should now be true (data written to dataVec)
   rule s3(step == 3);
      testAssert(mem.canAtom_r1(target), "canAtom true after write", cyc);
      if (!mem.canAtom_r1(target)) fails <= fails + 1;
      step <= 4;
   endrule

   // Read the bypassed value and verify
   rule s4(step == 4);
      let v = mem.atom_r(target);
      testAssert(v == 99, "bypass read == 99", cyc);
      if (v != 99) fails <= fails + 1;
      step <= 5;
   endrule

   // Clean up: release
   rule s5(step == 5);
      mem.rel_w1(wid);
      step <= 6;
   endrule

   rule s6(step == 6);
      testDone("BP_ReadBeforeWrite", fails);
   endrule
endmodule

// ============================================================
// Test 3: Two writes to same address -- newest data wins on bypass read
// Models WAW hazard resolution: back-to-back instructions writing to rd=x5.
// ============================================================
(* synthesize *)
module mkTestBP_TwoWritesSameAddr();
   RegFile#(Addr, Data) rf <- mkRegFileFull();
   BypassLockCombMem#(Addr, Data, LockId#(4), 4) mem <- mkBypassLockCombMem(rf);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) wid0 <- mkReg(0);
   Reg#(LockId#(4)) wid1 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Addr target = 5;

   rule tick; cyc <= cyc + 1; endrule

   // Reserve first write
   rule s0(step == 0);
      $display("=== TEST: BP_TwoWritesSameAddr ===");
      let id <- mem.res_w1(target);
      wid0 <= id;
      step <= 1;
   endrule

   // Reserve second write to same address
   rule s1(step == 1);
      let id <- mem.res_w1(target);
      wid1 <= id;
      step <= 2;
   endrule

   // Write data to the first (older) reservation
   rule s2(step == 2);
      mem.write(wid0, 100);
      step <= 3;
   endrule

   // Write data to the second (newer) reservation
   rule s3(step == 3);
      mem.write(wid1, 200);
      step <= 4;
   endrule

   // Read should return 200 (newest matching entry's data)
   rule s4(step == 4);
      let v = mem.atom_r(target);
      testAssert(v == 200, "bypass returns newest == 200", cyc);
      if (v != 200) fails <= fails + 1;
      step <= 5;
   endrule

   // Release in order: first (older) reservation
   rule s5(step == 5);
      mem.rel_w1(wid0);
      step <= 6;
   endrule

   // Release second (newer) reservation -- this commits 200 to RF
   rule s6(step == 6);
      mem.rel_w1(wid1);
      step <= 7;
   endrule

   // RF should have 200 after both committed
   rule s7(step == 7);
      let v = mem.atom_r(target);
      testAssert(v == 200, "rf value == 200 after both released", cyc);
      if (v != 200) fails <= fails + 1;
      testDone("BP_TwoWritesSameAddr", fails);
   endrule
endmodule

// ============================================================
// Test 4: Write and read different addresses -- no interference
// Reserve writes to addr 3 and addr 7, verify bypass returns correct data for each.
// ============================================================
(* synthesize *)
module mkTestBP_WriteReadDifferentAddrs();
   RegFile#(Addr, Data) rf <- mkRegFileFull();
   BypassLockCombMem#(Addr, Data, LockId#(4), 4) mem <- mkBypassLockCombMem(rf);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) widA <- mkReg(0);
   Reg#(LockId#(4)) widB <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Addr addrA = 3;
   Addr addrB = 7;

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: BP_WriteReadDifferentAddrs ===");
      let id <- mem.res_w1(addrA);
      widA <= id;
      step <= 1;
   endrule

   rule s1(step == 1);
      let id <- mem.res_w1(addrB);
      widB <= id;
      step <= 2;
   endrule

   // Write data to addr A
   rule s2(step == 2);
      mem.write(widA, 111);
      step <= 3;
   endrule

   // Write data to addr B
   rule s3(step == 3);
      mem.write(widB, 222);
      step <= 4;
   endrule

   // Read addr A -- should return 111
   rule s4(step == 4);
      let vA = mem.atom_r(addrA);
      testAssert(vA == 111, "addrA bypass == 111", cyc);
      if (vA != 111) fails <= fails + 1;
      step <= 5;
   endrule

   // Read addr B -- should return 222
   rule s5(step == 5);
      let vB = mem.atom_r(addrB);
      testAssert(vB == 222, "addrB bypass == 222", cyc);
      if (vB != 222) fails <= fails + 1;
      step <= 6;
   endrule

   // Verify canAtom is independent per address: addr 20 has no reservation
   rule s6(step == 6);
      testAssert(mem.canAtom_r1(20), "unrelated addr 20 canAtom true", cyc);
      if (!mem.canAtom_r1(20)) fails <= fails + 1;
      step <= 7;
   endrule

   // Clean up: release both
   rule s7(step == 7);
      mem.rel_w1(widA);
      step <= 8;
   endrule

   rule s8(step == 8);
      mem.rel_w1(widB);
      step <= 9;
   endrule

   rule s9(step == 9);
      testDone("BP_WriteReadDifferentAddrs", fails);
   endrule
endmodule

// ============================================================
// Test 5: In-order commit of 3 writes to different addresses
// Reserve w0, w1, w2 to addrs 1, 2, 3. Write data, release in order.
// After each release, verify the RF has the committed value.
// Models the in-order commit requirement of the pipeline.
// ============================================================
(* synthesize *)
module mkTestBP_CommitOrder();
   RegFile#(Addr, Data) rf <- mkRegFileFull();
   BypassLockCombMem#(Addr, Data, LockId#(8), 8) mem <- mkBypassLockCombMem(rf);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) wid0 <- mkReg(0);
   Reg#(LockId#(8)) wid1 <- mkReg(0);
   Reg#(LockId#(8)) wid2 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Addr addr0 = 1;
   Addr addr1 = 2;
   Addr addr2 = 3;

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: BP_CommitOrder ===");
      let id <- mem.res_w1(addr0);
      wid0 <= id;
      step <= 1;
   endrule

   rule s1(step == 1);
      let id <- mem.res_w1(addr1);
      wid1 <= id;
      step <= 2;
   endrule

   rule s2(step == 2);
      let id <- mem.res_w1(addr2);
      wid2 <= id;
      step <= 3;
   endrule

   // Write data to all three (one per cycle to avoid double-write on dataVec)
   rule s3(step == 3);
      mem.write(wid0, 10);
      step <= 4;
   endrule

   rule s4(step == 4);
      mem.write(wid1, 20);
      step <= 5;
   endrule

   rule s5(step == 5);
      mem.write(wid2, 30);
      step <= 6;
   endrule

   // Release w0 (commit addr0 = 10 to RF)
   rule s6(step == 6);
      mem.rel_w1(wid0);
      step <= 7;
   endrule

   // Verify addr0 committed in RF (no bypass entry left for addr0)
   rule s7(step == 7);
      let v0 = mem.atom_r(addr0);
      testAssert(v0 == 10, "addr0 committed == 10", cyc);
      if (v0 != 10) fails <= fails + 1;
      step <= 8;
   endrule

   // Release w1 (commit addr1 = 20 to RF)
   rule s8(step == 8);
      mem.rel_w1(wid1);
      step <= 9;
   endrule

   // Verify addr1 committed
   rule s9(step == 9);
      let v1 = mem.atom_r(addr1);
      testAssert(v1 == 20, "addr1 committed == 20", cyc);
      if (v1 != 20) fails <= fails + 1;
      step <= 10;
   endrule

   // Release w2 (commit addr2 = 30 to RF)
   rule s10(step == 10);
      mem.rel_w1(wid2);
      step <= 11;
   endrule

   // Verify addr2 committed
   rule s11(step == 11);
      let v2 = mem.atom_r(addr2);
      testAssert(v2 == 30, "addr2 committed == 30", cyc);
      if (v2 != 30) fails <= fails + 1;
      step <= 12;
   endrule

   // Verify addr0 still holds its value
   rule s12(step == 12);
      let v0 = mem.atom_r(addr0);
      testAssert(v0 == 10, "addr0 still == 10", cyc);
      if (v0 != 10) fails <= fails + 1;
      step <= 13;
   endrule

   // Verify addr1 still holds its value, then done
   rule s13(step == 13);
      let v1 = mem.atom_r(addr1);
      testAssert(v1 == 20, "addr1 still == 20", cyc);
      if (v1 != 20) fails <= fails + 1;
      testDone("BP_CommitOrder", fails);
   endrule
endmodule

endpackage
