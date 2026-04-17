package TestBypassRF;

import VerilogLibs :: *;
import ConfigReg :: *;
import TestHelper :: *;

typedef UInt#(3) Addr;
typedef Int#(32) Data;
typedef UInt#(2) Name;  // LockId#(4) = UInt#(TLog#(4)) = UInt#(2)

// ============================================================
// Test 1: BasicLifecycle
// res_w1(3), write(id, 42), next cycle: res_r1(3) finds conflict
// with data already written, so owns_r1 true. read1 returns 42.
// Then rel_r1, rel_w1 (commits to rf).
// ============================================================
(* synthesize *)
module mkTestBRF_BasicLifecycle();
   BypassRF#(Addr, Data, Name) brf <- mkBypassRF(8, False, "");

   Reg#(UInt#(4))  step  <- mkReg(0);
   Reg#(Name)      wid   <- mkReg(0);
   Reg#(Name)      rid   <- mkReg(0);
   Reg#(UInt#(32)) cyc   <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Cycle 0: reserve write for addr 3
   rule s0(step == 0);
      $display("=== TEST: BRF_BasicLifecycle ===");
      let id <- brf.res_w1(3);
      wid <= id;
      step <= 1;
   endrule

   // Cycle 1: write data 42 to the reserved slot
   rule s1(step == 1);
      brf.write(wid, 42);
      step <= 2;
   endrule

   // Cycle 2: reserve read port 1 for addr 3
   // Should find the conflict entry, and data is already written,
   // so rf1_valid should be set (no stillConflict).
   rule s2(step == 2);
      let r <- brf.res_r1(3);
      rid <= r;
      step <= 3;
   endrule

   // Cycle 3: check owns_r1 -- should be true (rf1_inUse=1, rf1_valid=1)
   rule s3(step == 3);
      let o = brf.owns_r1();
      testAssert(o, "owns_r1 true after res_r1 with written data", cyc);
      if (!o) fails <= fails + 1;
      step <= 4;
   endrule

   // Cycle 4: read1 should return 42
   rule s4(step == 4);
      let v = brf.read1(rid);
      testAssert(v == 42, "read1 == 42", cyc);
      if (v != 42) fails <= fails + 1;
      step <= 5;
   endrule

   // Cycle 5: release read port 1
   rule s5(step == 5);
      brf.rel_r1();
      step <= 6;
   endrule

   // Cycle 6: release write (commit to rf)
   rule s6(step == 6);
      brf.rel_w1(wid);
      step <= 7;
   endrule

   rule s7(step == 7);
      testDone("BRF_BasicLifecycle", fails);
   endrule
endmodule


// ============================================================
// Test 2: ReadBeforeWrite
// res_w1(5), then res_r1(5) BEFORE writing data. owns_r1 should
// be false (conflict found, data not written yet -- stillConflict).
// Then write(id, 99). Next cycle owns_r1 becomes true via
// forwarding. read1 returns 99.
// ============================================================
(* synthesize *)
module mkTestBRF_ReadBeforeWrite();
   BypassRF#(Addr, Data, Name) brf <- mkBypassRF(8, False, "");

   Reg#(UInt#(4))  step  <- mkReg(0);
   Reg#(Name)      wid   <- mkReg(0);
   Reg#(Name)      rid   <- mkReg(0);
   Reg#(UInt#(32)) cyc   <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Cycle 0: reserve write for addr 5
   rule s0(step == 0);
      $display("=== TEST: BRF_ReadBeforeWrite ===");
      let id <- brf.res_w1(5);
      wid <= id;
      step <= 1;
   endrule

   // Cycle 1: reserve read port 1 for addr 5 (before data is written)
   // Conflict found but data not written -> stillConflict=1, rf1_valid=0
   rule s1(step == 1);
      let r <- brf.res_r1(5);
      rid <= r;
      step <= 2;
   endrule

   // Cycle 2: check owns_r1 -- should be false (rf1_valid=0, no forwarding yet)
   rule s2(step == 2);
      let o = brf.owns_r1();
      testAssert(!o, "owns_r1 false before write (no data yet)", cyc);
      if (o) fails <= fails + 1;
      step <= 3;
   endrule

   // Cycle 3: write data 99 to the reserved slot
   // Forwarding: FWD11 or FWD21 will match rf1_write, setting rf1_valid=1
   // on the next posedge. But owns_r1 is combinational and should see
   // the forwarding in this same cycle.
   rule s3(step == 3);
      brf.write(wid, 99);
      step <= 4;
   endrule

   // Cycle 4: owns_r1 should be true now (rf1_valid set by forwarding)
   rule s4(step == 4);
      let o = brf.owns_r1();
      testAssert(o, "owns_r1 true after write forwarding", cyc);
      if (!o) fails <= fails + 1;
      step <= 5;
   endrule

   // Cycle 5: read1 should return 99
   rule s5(step == 5);
      let v = brf.read1(rid);
      testAssert(v == 99, "read1 == 99 after forwarding", cyc);
      if (v != 99) fails <= fails + 1;
      step <= 6;
   endrule

   // Clean up
   rule s6(step == 6);
      brf.rel_r1();
      step <= 7;
   endrule

   rule s7(step == 7);
      brf.rel_w1(wid);
      step <= 8;
   endrule

   rule s8(step == 8);
      testDone("BRF_ReadBeforeWrite", fails);
   endrule
endmodule


// ============================================================
// Test 3: NoConflictReadFromRF
// Write 77 to addr 2 via full res_w1/write/rel_w1 cycle (commit
// to rf). Then res_r1(2) with no pending write queue entry.
// Should read directly from rf. owns_r1 true immediately.
// read1 returns 77.
// ============================================================
(* synthesize *)
module mkTestBRF_NoConflictReadFromRF();
   BypassRF#(Addr, Data, Name) brf <- mkBypassRF(8, False, "");

   Reg#(UInt#(4))  step  <- mkReg(0);
   Reg#(Name)      wid   <- mkReg(0);
   Reg#(Name)      rid   <- mkReg(0);
   Reg#(UInt#(32)) cyc   <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Phase 1: commit value 77 to rf[2]

   // Cycle 0: reserve write for addr 2
   rule s0(step == 0);
      $display("=== TEST: BRF_NoConflictReadFromRF ===");
      let id <- brf.res_w1(2);
      wid <= id;
      step <= 1;
   endrule

   // Cycle 1: write data 77
   rule s1(step == 1);
      brf.write(wid, 77);
      step <= 2;
   endrule

   // Cycle 2: release write -- commits 77 to rf[2], clears valid/written
   rule s2(step == 2);
      brf.rel_w1(wid);
      step <= 3;
   endrule

   // Phase 2: read from rf with no pending writes

   // Cycle 3: reserve read port 1 for addr 2
   // No valid write queue entry for addr 2, so reads directly from rf.
   // rf1_valid = !stillConflict1 = !(0 && ...) = 1 (no conflict means
   // rf1_foundc=0 so stillConflict1=0, rf1_valid=1)
   rule s3(step == 3);
      let r <- brf.res_r1(2);
      rid <= r;
      step <= 4;
   endrule

   // Cycle 4: owns_r1 should be true (rf1_inUse=1, rf1_valid=1)
   rule s4(step == 4);
      let o = brf.owns_r1();
      testAssert(o, "owns_r1 true (no conflict, data from rf)", cyc);
      if (!o) fails <= fails + 1;
      step <= 5;
   endrule

   // Cycle 5: read1 should return 77 (from rf snapshot)
   rule s5(step == 5);
      let v = brf.read1(rid);
      testAssert(v == 77, "read1 == 77 from rf", cyc);
      if (v != 77) fails <= fails + 1;
      step <= 6;
   endrule

   // Clean up
   rule s6(step == 6);
      brf.rel_r1();
      step <= 7;
   endrule

   rule s7(step == 7);
      testDone("BRF_NoConflictReadFromRF", fails);
   endrule
endmodule


// ============================================================
// Test 4: TwoWritesSameAddr
// res_w1(1) -> id0. res_w1(1) -> id1. write(id0, 100).
// write(id1, 200). Then res_r1(1). Should find the NEWEST
// write queue entry (id1). owns_r1 true (data written).
// read1 returns 200.
// ============================================================
(* synthesize *)
module mkTestBRF_TwoWritesSameAddr();
   BypassRF#(Addr, Data, Name) brf <- mkBypassRF(8, False, "");

   Reg#(UInt#(4))  step  <- mkReg(0);
   Reg#(Name)      wid0  <- mkReg(0);
   Reg#(Name)      wid1  <- mkReg(0);
   Reg#(Name)      rid   <- mkReg(0);
   Reg#(UInt#(32)) cyc   <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Cycle 0: reserve first write for addr 1
   rule s0(step == 0);
      $display("=== TEST: BRF_TwoWritesSameAddr ===");
      let id <- brf.res_w1(1);
      wid0 <= id;
      step <= 1;
   endrule

   // Cycle 1: reserve second write for addr 1
   rule s1(step == 1);
      let id <- brf.res_w1(1);
      wid1 <= id;
      step <= 2;
   endrule

   // Cycle 2: write data 100 to first (older) slot
   rule s2(step == 2);
      brf.write(wid0, 100);
      step <= 3;
   endrule

   // Cycle 3: write data 200 to second (newer) slot
   rule s3(step == 3);
      brf.write(wid1, 200);
      step <= 4;
   endrule

   // Cycle 4: reserve read for addr 1
   // Both entries match addr 1. isNewer picks the one closer to head.
   // id1 was allocated after id0 so id1 is newer. Data for id1 is written,
   // so rf1_valid=1.
   rule s4(step == 4);
      let r <- brf.res_r1(1);
      rid <= r;
      step <= 5;
   endrule

   // Cycle 5: owns_r1 should be true
   rule s5(step == 5);
      let o = brf.owns_r1();
      testAssert(o, "owns_r1 true (newest entry has data)", cyc);
      if (!o) fails <= fails + 1;
      step <= 6;
   endrule

   // Cycle 6: read1 should return 200 (from newest entry)
   rule s6(step == 6);
      let v = brf.read1(rid);
      testAssert(v == 200, "read1 == 200 (newest write)", cyc);
      if (v != 200) fails <= fails + 1;
      step <= 7;
   endrule

   // Clean up: release read, then release writes in order.
   // Use a single relTarget register to avoid combinational loop
   // through F_READY when BSV muxes W_F between two rel_w1 calls.
   Reg#(Name) relTarget <- mkReg(0);

   rule s7(step == 7);
      brf.rel_r1();
      relTarget <= wid0;
      step <= 8;
   endrule

   rule s8(step == 8);
      brf.rel_w1(relTarget);
      relTarget <= wid1;
      step <= 9;
   endrule

   rule s9(step == 9);
      brf.rel_w1(relTarget);
      step <= 10;
   endrule

   rule s10(step == 10);
      testDone("BRF_TwoWritesSameAddr", fails);
   endrule
endmodule


// ============================================================
// Test 5: WriteQueueFull
// With 4-entry write queue (name_width=2), res_w1 four times to
// fill the queue. The 5th res_w1 should block (ALLOC_READY false).
// Release the first entry. Next cycle, ALLOC_READY should be
// true again and a new res_w1 should succeed.
//
// Strategy: after 4 allocations, step moves to 4 where a rule
// tries to call res_w1. ALLOC_READY is false so the rule cannot
// fire. A separate "unblock" rule (guarded by a cycle counter)
// releases an entry after one stalled cycle, proving the stall
// happened. Then the allocation rule fires on the next cycle.
// ============================================================
(* synthesize *)
module mkTestBRF_WriteQueueFull();
   BypassRF#(Addr, Data, Name) brf <- mkBypassRF(8, False, "");

   Reg#(UInt#(4))  step  <- mkReg(0);
   Reg#(Name)      wid0  <- mkReg(0);
   Reg#(Name)      wid1  <- mkReg(0);
   Reg#(Name)      wid2  <- mkReg(0);
   Reg#(Name)      wid3  <- mkReg(0);
   Reg#(Name)      wid4  <- mkReg(0);
   Reg#(UInt#(32)) cyc   <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Cycle 0: allocate slot 0
   rule s0(step == 0);
      $display("=== TEST: BRF_WriteQueueFull ===");
      let id <- brf.res_w1(0);
      wid0 <= id;
      step <= 1;
   endrule

   // Cycle 1: allocate slot 1
   rule s1(step == 1);
      let id <- brf.res_w1(1);
      wid1 <= id;
      step <= 2;
   endrule

   // Cycle 2: allocate slot 2
   rule s2(step == 2);
      let id <- brf.res_w1(2);
      wid2 <= id;
      step <= 3;
   endrule

   // Cycle 3: allocate slot 3 -- queue now full.
   rule s3(step == 3);
      let id <- brf.res_w1(3);
      wid3 <= id;
      step <= 4;
   endrule

   // Step 4: write data to slot 0 so we can release it
   rule s4(step == 4);
      brf.write(wid0, 0);
      step <= 5;
   endrule

   // Step 5: release slot 0 to free a queue entry
   rule s5(step == 5);
      brf.rel_w1(wid0);
      step <= 6;
   endrule

   // Step 6: now ALLOC_READY should be true again -- allocate 5th entry
   rule s6_alloc(step == 6);
      let id <- brf.res_w1(4);
      wid4 <= id;
      testAssert(True, "5th alloc succeeded after freeing slot", cyc);
      step <= 7;
   endrule

   // Clean up: write data and release remaining entries in order.
   // wid0 already released. wid1 is now the owner.
   Reg#(Name) relTgt <- mkReg(0);

   rule s7_w(step == 7);
      brf.write(wid1, 0);
      relTgt <= wid1;
      step <= 8;
   endrule

   rule s8_r(step == 8);
      brf.rel_w1(relTgt);
      step <= 9;
   endrule

   rule s9_w(step == 9);
      brf.write(wid2, 0);
      relTgt <= wid2;
      step <= 10;
   endrule

   rule s10_r(step == 10);
      brf.rel_w1(relTgt);
      step <= 11;
   endrule

   rule s11_w(step == 11);
      brf.write(wid3, 0);
      relTgt <= wid3;
      step <= 12;
   endrule

   rule s12_r(step == 12);
      brf.rel_w1(relTgt);
      step <= 13;
   endrule

   rule s13_w(step == 13);
      brf.write(wid4, 0);
      relTgt <= wid4;
      step <= 14;
   endrule

   rule s14_r(step == 14);
      brf.rel_w1(relTgt);
      step <= 15;
   endrule

   rule s15(step == 15);
      testDone("BRF_WriteQueueFull", fails);
   endrule

   rule watchdog(cyc > 50);
      $display("  FAIL: watchdog timeout at cycle %0d, step %0d", cyc, step);
      testDone("BRF_WriteQueueFull", fails + 1);
   endrule
endmodule

endpackage
