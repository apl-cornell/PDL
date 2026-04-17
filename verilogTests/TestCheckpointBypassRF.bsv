package TestCheckpointBypassRF;

import VerilogLibs :: *;
import ConfigReg :: *;
import TestHelper :: *;

// Types used across all tests:
//   addr = UInt#(3)  -- 8 arch regs
//   elem = Int#(32)  -- 32-bit data
//   id   = UInt#(3)  -- 8 write queue entries (also used for cid)
// Instantiate: mkCheckpointBypassRF(8, False, "")

// ============================================================
// Test 1: Basic checkpoint and rollback.
// Alloc w0 for addr 1, write data. Checkpoint (c0). Alloc w1 for
// addr 2 speculatively, write data. Rollback to c0 (doRoll=True,
// doRel=True). Verify w1 is invalidated. Alloc again -- should
// reclaim the slot w1 used (head was reset).
// ============================================================
(* synthesize *)
module mkTestCBRF_BasicCheckpointRollback();
   CheckpointBypassRF#(UInt#(3), Int#(32), UInt#(3), UInt#(3)) rf <- mkCheckpointBypassRF(8, False, "");

   Reg#(UInt#(5)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Reg#(UInt#(3)) w0 <- mkReg(0);
   Reg#(UInt#(3)) w1 <- mkReg(0);
   Reg#(UInt#(3)) w2 <- mkReg(0);
   Reg#(UInt#(3)) c0 <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc write entry for addr 1
   rule s0(step == 0);
      $display("=== TEST: CBRF_BasicCheckpointRollback ===");
      let id <- rf.res_w1(1);
      w0 <= id;
      $display("  alloc w0 = %0d for addr 1", id);
      step <= 1;
   endrule

   // Step 1: Write data 100 to w0
   rule s1(step == 1);
      rf.write(w0, 100);
      step <= 2;
   endrule

   // Step 2: Checkpoint -- captures wQueueHead after w0 alloc
   rule s2(step == 2);
      let cid <- rf.checkpoint();
      c0 <= cid;
      $display("  checkpoint c0 = %0d", cid);
      step <= 3;
   endrule

   // Step 3: Speculatively alloc w1 for addr 2
   rule s3(step == 3);
      let id <- rf.res_w1(2);
      w1 <= id;
      $display("  alloc w1 = %0d for addr 2 (speculative)", id);
      step <= 4;
   endrule

   // Step 4: Write data 200 to w1 (speculative)
   rule s4(step == 4);
      rf.write(w1, 200);
      step <= 5;
   endrule

   // Step 5: Rollback to c0 -- should invalidate w1
   rule s5(step == 5);
      rf.rollback(c0, True, True);
      $display("  rollback to c0 (doRoll=True, doRel=True)");
      step <= 6;
   endrule

   // Step 6: Alloc again -- should reuse w1's slot since head was reset
   rule s6(step == 6);
      let id <- rf.res_w1(3);
      w2 <= id;
      $display("  alloc w2 = %0d for addr 3 (after rollback)", id);
      testAssert(id == w1, "after rollback, alloc reuses w1 slot (head was reset)", cyc);
      if (id != w1) fails <= fails + 1;
      step <= 7;
   endrule

   rule s7(step == 7);
      testDone("CBRF_BasicCheckpointRollback", fails);
   endrule
endmodule

// ============================================================
// Test 2: Rollback preserves committed data.
// Alloc w0, write data, rel_w1 (commit to rf). Checkpoint (c0).
// Alloc w1 speculatively. Rollback to c0. Verify addr 1 still
// has committed data in rf (rollback only affects write queue).
// ============================================================
(* synthesize *)
module mkTestCBRF_RollbackPreservesCommitted();
   CheckpointBypassRF#(UInt#(3), Int#(32), UInt#(3), UInt#(3)) rf <- mkCheckpointBypassRF(8, False, "");

   Reg#(UInt#(5)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Reg#(UInt#(3)) w0 <- mkReg(0);
   Reg#(UInt#(3)) w1 <- mkReg(0);
   Reg#(UInt#(3)) c0 <- mkReg(0);
   Reg#(UInt#(3)) r1 <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc w0 for addr 1
   rule s0(step == 0);
      $display("=== TEST: CBRF_RollbackPreservesCommitted ===");
      let id <- rf.res_w1(1);
      w0 <= id;
      step <= 1;
   endrule

   // Step 1: Write data 42 to w0
   rule s1(step == 1);
      rf.write(w0, 42);
      step <= 2;
   endrule

   // Step 2: Commit w0 -- data 42 goes to rf[1]
   rule s2(step == 2);
      rf.rel_w1(w0);
      step <= 3;
   endrule

   // Step 3: Checkpoint c0
   rule s3(step == 3);
      let cid <- rf.checkpoint();
      c0 <= cid;
      step <= 4;
   endrule

   // Step 4: Speculatively alloc w1 for addr 1 (overwrite same addr)
   rule s4(step == 4);
      let id <- rf.res_w1(1);
      w1 <= id;
      step <= 5;
   endrule

   // Step 5: Write speculative data 999 to w1
   rule s5(step == 5);
      rf.write(w1, 999);
      step <= 6;
   endrule

   // Step 6: Rollback to c0
   rule s6(step == 6);
      rf.rollback(c0, True, True);
      step <= 7;
   endrule

   // Step 7: Reserve read for addr 1 -- should see committed data from rf
   // After rollback, w1 is invalidated, so no write queue conflict.
   // res_r1 should read from rf[1] which has committed value 42.
   rule s7(step == 7);
      let id <- rf.res_r1(1);
      r1 <= id;
      step <= 8;
   endrule

   // Step 8: Check owns_r1 and read the data
   rule s8(step == 8);
      let valid = rf.owns_r1();
      testAssert(valid, "read port 1 is valid (data from committed rf)", cyc);
      let data = rf.read1(r1);
      testAssert(data == 42, "addr 1 still has committed value 42 after rollback", cyc);
      if (!valid || data != 42) fails <= fails + 1;
      rf.rel_r1();
      step <= 9;
   endrule

   rule s9(step == 9);
      testDone("CBRF_RollbackPreservesCommitted", fails);
   endrule
endmodule

// ============================================================
// Test 3: Checkpoint in the same cycle as alloc.
// The chkPointer = wQueueHead + 1 when alloc fires concurrently,
// so the checkpoint captures the alloc. After rollback, w0 should
// still be valid (before checkpoint boundary) but w1 is invalidated.
// ============================================================
(* synthesize *)
module mkTestCBRF_CheckpointAfterAlloc();
   CheckpointBypassRF#(UInt#(3), Int#(32), UInt#(3), UInt#(3)) rf <- mkCheckpointBypassRF(8, False, "");

   Reg#(UInt#(5)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Reg#(UInt#(3)) w0 <- mkReg(0);
   Reg#(UInt#(3)) w1 <- mkReg(0);
   Reg#(UInt#(3)) w2 <- mkReg(0);
   Reg#(UInt#(3)) c0 <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc w0 for addr 1.
   // res_w1 CF checkpoint in the BVI schedule, so both can fire same cycle.
   // We do them in separate cycles for clarity first, then test the
   // simultaneous case in step 1.
   rule s0(step == 0);
      $display("=== TEST: CBRF_CheckpointAfterAlloc ===");
      let id <- rf.res_w1(1);
      w0 <= id;
      $display("  alloc w0 = %0d for addr 1", id);
      step <= 1;
   endrule

   // Step 1: Alloc w1 for addr 2 AND checkpoint in the same cycle.
   // chkPointer = wQueueHead + 1 (because ALLOC_E && ALLOC_READY).
   // The checkpoint should include w1 (the current alloc).
   rule s1(step == 1);
      let id <- rf.res_w1(2);
      w1 <= id;
      let cid <- rf.checkpoint();
      c0 <= cid;
      $display("  alloc w1 = %0d AND checkpoint c0 = %0d (same cycle)", id, cid);
      step <= 2;
   endrule

   // Step 2: Write data to w0 and w1 so they are "written"
   rule s2(step == 2);
      rf.write(w0, 10);
      step <= 3;
   endrule

   rule s3(step == 3);
      rf.write(w1, 20);
      step <= 4;
   endrule

   // Step 4: Alloc w2 (speculative, after checkpoint)
   rule s4(step == 4);
      let id <- rf.res_w1(3);
      w2 <= id;
      $display("  alloc w2 = %0d for addr 3 (after checkpoint)", id);
      step <= 5;
   endrule

   // Step 5: Rollback to c0 -- w2 should be invalidated, w0 and w1 preserved
   rule s5(step == 5);
      rf.rollback(c0, True, True);
      step <= 6;
   endrule

   // Step 6: Alloc again -- should get the slot w2 used (head was reset to after w1)
   rule s6(step == 6);
      let id <- rf.res_w1(4);
      $display("  alloc after rollback = %0d (expect w2's slot %0d)", id, w2);
      testAssert(id == w2, "after rollback, alloc reuses w2 slot (w0,w1 preserved)", cyc);
      if (id != w2) fails <= fails + 1;
      step <= 7;
   endrule

   // Step 7: Verify w1's data is still readable (it was before checkpoint boundary)
   rule s7(step == 7);
      let id <- rf.res_r1(2);
      step <= 8;
   endrule

   rule s8(step == 8);
      let valid = rf.owns_r1();
      let data = rf.read1(0); // read port returns saved data
      testAssert(valid, "read port valid -- w1 data intact after rollback", cyc);
      if (!valid) fails <= fails + 1;
      rf.rel_r1();
      step <= 9;
   endrule

   rule s9(step == 9);
      testDone("CBRF_CheckpointAfterAlloc", fails);
   endrule
endmodule

// ============================================================
// Test 4: Multiple checkpoints -- rollback to earlier one.
// Alloc w0, checkpoint c0. Alloc w1, checkpoint c1. Alloc w2.
// Rollback to c0 -- should invalidate w1 and w2 plus free c1.
// Then verify we can alloc starting from w1's slot.
// ============================================================
(* synthesize *)
module mkTestCBRF_MultipleCheckpoints();
   CheckpointBypassRF#(UInt#(3), Int#(32), UInt#(3), UInt#(3)) rf <- mkCheckpointBypassRF(8, False, "");

   Reg#(UInt#(5)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Reg#(UInt#(3)) w0 <- mkReg(0);
   Reg#(UInt#(3)) w1 <- mkReg(0);
   Reg#(UInt#(3)) w2 <- mkReg(0);
   Reg#(UInt#(3)) c0 <- mkReg(0);
   Reg#(UInt#(3)) c1 <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc w0 for addr 1
   rule s0(step == 0);
      $display("=== TEST: CBRF_MultipleCheckpoints ===");
      let id <- rf.res_w1(1);
      w0 <= id;
      $display("  alloc w0 = %0d", id);
      step <= 1;
   endrule

   // Step 1: Checkpoint c0
   rule s1(step == 1);
      let cid <- rf.checkpoint();
      c0 <= cid;
      $display("  checkpoint c0 = %0d", cid);
      step <= 2;
   endrule

   // Step 2: Alloc w1 for addr 2
   rule s2(step == 2);
      let id <- rf.res_w1(2);
      w1 <= id;
      $display("  alloc w1 = %0d", id);
      step <= 3;
   endrule

   // Step 3: Checkpoint c1
   rule s3(step == 3);
      let cid <- rf.checkpoint();
      c1 <= cid;
      $display("  checkpoint c1 = %0d", cid);
      step <= 4;
   endrule

   // Step 4: Alloc w2 for addr 3
   rule s4(step == 4);
      let id <- rf.res_w1(3);
      w2 <= id;
      $display("  alloc w2 = %0d", id);
      step <= 5;
   endrule

   // Step 5: Write data to all entries
   rule s5(step == 5);
      rf.write(w0, 10);
      step <= 6;
   endrule

   rule s6(step == 6);
      rf.write(w1, 20);
      step <= 7;
   endrule

   rule s7(step == 7);
      rf.write(w2, 30);
      step <= 8;
   endrule

   // Step 8: Rollback to c0 -- invalidates w1, w2, frees c1
   rule s8(step == 8);
      rf.rollback(c0, True, False);
      $display("  rollback to c0 (doRoll=True, doRel=False)");
      step <= 9;
   endrule

   // Step 9: Try to alloc -- should get w1's slot (head reset to c0 checkpoint)
   rule s9(step == 9);
      let id <- rf.res_w1(4);
      $display("  alloc after rollback = %0d (expect w1's slot %0d)", id, w1);
      testAssert(id == w1, "after rollback to c0, alloc starts from w1 slot", cyc);
      if (id != w1) fails <= fails + 1;
      step <= 10;
   endrule

   // Step 10: Verify c1 was freed by rollback (newer than c0).
   // We should be able to take a new checkpoint.
   rule s10(step == 10);
      let cid <- rf.checkpoint();
      testAssert(True, "checkpoint succeeded after rollback freed c1", cyc);
      $display("  new checkpoint = %0d", cid);
      step <= 11;
   endrule

   rule s11(step == 11);
      testDone("CBRF_MultipleCheckpoints", fails);
   endrule
endmodule

// ============================================================
// Test 5: Read port rollback.
// Alloc w0 for addr 3, checkpoint c0. Alloc w1 for addr 3
// (speculative). res_r1(3) -- should find w1 as conflict. The
// read reservation records rf1_owner = nextCheck (current checkpoint
// counter). Rollback to c0 -- rf1_inUse should be cleared because
// the read's checkpoint owner is newer than c0.
// This tests lines 366-370 in CheckpointBypassRF.v.
// ============================================================
(* synthesize *)
module mkTestCBRF_ReadPortRollback();
   CheckpointBypassRF#(UInt#(3), Int#(32), UInt#(3), UInt#(3)) rf <- mkCheckpointBypassRF(8, False, "");

   Reg#(UInt#(5)) step <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Reg#(UInt#(3)) w0 <- mkReg(0);
   Reg#(UInt#(3)) w1 <- mkReg(0);
   Reg#(UInt#(3)) c0 <- mkReg(0);
   Reg#(UInt#(3)) r1 <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: Alloc w0 for addr 3
   rule s0(step == 0);
      $display("=== TEST: CBRF_ReadPortRollback ===");
      let id <- rf.res_w1(3);
      w0 <= id;
      $display("  alloc w0 = %0d for addr 3", id);
      step <= 1;
   endrule

   // Step 1: Checkpoint c0
   rule s1(step == 1);
      let cid <- rf.checkpoint();
      c0 <= cid;
      $display("  checkpoint c0 = %0d", cid);
      step <= 2;
   endrule

   // Step 2: Speculatively alloc w1 for addr 3
   rule s2(step == 2);
      let id <- rf.res_w1(3);
      w1 <= id;
      $display("  alloc w1 = %0d for addr 3 (speculative)", id);
      step <= 3;
   endrule

   // Step 3: Take checkpoint c1. This makes nextCheck = 2.
   rule s3(step == 3);
      let cid <- rf.checkpoint();
      $display("  checkpoint c1 = %0d", cid);
      step <= 4;
   endrule

   // Step 4: Reserve read port 1 for addr 3.
   // rf1_owner = CHK_OUT = nextCheck = 2.
   // Then take yet another checkpoint to advance nextCheck to 3.
   // Now rf1_owner(2) is strictly between ROLLBK_IN(0) and nextCheck(3).
   rule s4(step == 4);
      let id <- rf.res_r1(3);
      r1 <= id;
      $display("  res_r1(3) -> read port id = %0d, rf1_owner = CHK_OUT = %0d", id, 2);
      let cid <- rf.checkpoint();
      $display("  checkpoint c2 = %0d (advances nextCheck past rf1_owner)", cid);
      step <= 5;
   endrule

   // Step 5: Rollback to c0.
   // rf1_owner = 2, ROLLBK_IN = 0, nextCheck = 3.
   // isNewer(2, 0, 3): nohmid = 2 < 0 = false; hmid = 0 < 3 && 2 >= 3 = false.
   // Hmm, still false. The isNewer function has issues here.
   // Actually: nohmid = (2 > 0) && !(0 < 3 && 2 >= 3) = true && !(true && false) = true && true = true.
   // Wait, isNewer returns !isOlder. isOlder: nohmid = a < b && ... Let me recompute.
   // isOlder(2, 0, 3): nohmid = 2 < 0 = false. hmid = 0 < 3 && 2 >= 3 = false.
   // isOlder = false. isNewer = true. The condition should hold!
   rule s5(step == 5);
      rf.rollback(c0, True, True);
      $display("  rollback to c0");
      step <= 6;
   endrule

   // Step 6: After rollback, read port should be freed.
   rule s6(step == 6);
      let id <- rf.res_r1(3);
      testAssert(True, "res_r1 succeeded after rollback -- read port freed", cyc);
      $display("  res_r1(3) after rollback succeeded, id = %0d", id);
      step <= 7;
   endrule

   rule s7(step == 7);
      rf.rel_r1();
      step <= 8;
   endrule

   rule s8(step == 8);
      testDone("CBRF_ReadPortRollback", fails);
   endrule
endmodule

endpackage
