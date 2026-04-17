package TestCheckpointLock;

import Locks :: *;
import ConfigReg :: *;
import TestHelper :: *;

// ============================================================
// Test 1: Basic checkpoint and rollback
// Reserve 2 IDs, checkpoint after the second. Reserve a third
// (speculative). Rollback to checkpoint. Verify only the first
// 2 reservations remain (third is undone).
// Models: decode reserves, branch checkpoint taken, speculative
//         instructions reserved, misprediction detected, rollback.
// ============================================================
(* synthesize *)
module mkTestCKL_BasicCheckpointRollback();
   CheckpointQueueLock#(LockId#(8), LockId#(8)) lock <- mkCheckpointQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(LockId#(8)) id1 <- mkReg(0);
   Reg#(LockId#(8)) chk <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: CKL_BasicCheckpointRollback ===");
      let i <- lock.res1();
      id0 <= i;
      step <= 1;
   endrule

   rule s1(step == 1);
      let i <- lock.res1();
      id1 <= i;
      step <= 2;
   endrule

   // Checkpoint after id1 (captures nextId after this cycle's reservations)
   rule s2(step == 2);
      let c <- lock.checkpoint();
      chk <= c;
      step <= 3;
   endrule

   // Reserve a speculative third ID
   rule s3(step == 3);
      let i <- lock.res1();
      // id2 is speculative, we don't need to save it
      step <= 4;
   endrule

   // Rollback to checkpoint (undoes the speculative reservation)
   // Cannot res/rel in the same cycle as rollback
   rule s4(step == 4);
      lock.rollback(chk, True, False);
      step <= 5;
   endrule

   // Verify: id0 still owns, lock is not empty, speculative is gone
   rule s5(step == 5);
      testAssert(lock.owns1(id0), "id0 still owns after rollback", cyc);
      testAssert(!lock.isEmpty(), "not empty after rollback", cyc);
      if (!lock.owns1(id0) || lock.isEmpty()) fails <= fails + 1;
      step <= 6;
   endrule

   // Release id0
   rule s6(step == 6);
      lock.rel1(id0);
      step <= 7;
   endrule

   // id1 should now own (it was before the checkpoint, not rolled back)
   rule s7(step == 7);
      testAssert(lock.owns1(id1), "id1 owns after id0 released", cyc);
      testAssert(!lock.isEmpty(), "not empty (id1 still held)", cyc);
      if (!lock.owns1(id1) || lock.isEmpty()) fails <= fails + 1;
      step <= 8;
   endrule

   // Release id1
   rule s8(step == 8);
      lock.rel1(id1);
      step <= 9;
   endrule

   // Should be empty: the speculative third was rolled back, id0 and id1 released
   rule s9(step == 9);
      testAssert(lock.isEmpty(), "empty after releasing pre-checkpoint IDs", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("CKL_BasicCheckpointRollback", fails);
   endrule
endmodule

// ============================================================
// Test 2: Checkpoint with no rollback (normal execution)
// Reserve, checkpoint, reserve more, then release all normally.
// Verify checkpoint doesn't interfere with normal operation.
// Models: branch predicted correctly, no rollback needed.
// ============================================================
(* synthesize *)
module mkTestCKL_CheckpointNoRollback();
   CheckpointQueueLock#(LockId#(8), LockId#(8)) lock <- mkCheckpointQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(LockId#(8)) id1 <- mkReg(0);
   Reg#(LockId#(8)) id2 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: CKL_CheckpointNoRollback ===");
      let i <- lock.res1();
      id0 <= i;
      step <= 1;
   endrule

   // Checkpoint (we won't use it)
   rule s1(step == 1);
      let c <- lock.checkpoint();
      // checkpoint value not used; just checking it doesn't break things
      let i <- lock.res1();
      id1 <= i;
      step <= 2;
   endrule

   rule s2(step == 2);
      let i <- lock.res1();
      id2 <= i;
      step <= 3;
   endrule

   // Verify all 3 exist and id0 owns
   rule s3(step == 3);
      testAssert(lock.owns1(id0), "id0 owns", cyc);
      testAssert(!lock.isEmpty(), "not empty with 3 reserved", cyc);
      if (!lock.owns1(id0) || lock.isEmpty()) fails <= fails + 1;
      step <= 4;
   endrule

   // Release in order
   rule s4(step == 4);
      lock.rel1(id0);
      step <= 5;
   endrule

   rule s5(step == 5);
      testAssert(lock.owns1(id1), "id1 owns after id0 released", cyc);
      if (!lock.owns1(id1)) fails <= fails + 1;
      lock.rel1(id1);
      step <= 6;
   endrule

   rule s6(step == 6);
      testAssert(lock.owns1(id2), "id2 owns after id1 released", cyc);
      if (!lock.owns1(id2)) fails <= fails + 1;
      lock.rel1(id2);
      step <= 7;
   endrule

   rule s7(step == 7);
      testAssert(lock.isEmpty(), "empty after normal release (no rollback)", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("CKL_CheckpointNoRollback", fails);
   endrule
endmodule

// ============================================================
// Test 3: Multiple checkpoints -- nested speculation
// Reserve id0, checkpoint (c1), reserve id1, checkpoint (c2),
// reserve id2. Rollback to c1. Verify only id0 remains.
// Models: nested branches where outer branch mispredicts.
// ============================================================
(* synthesize *)
module mkTestCKL_MultipleCheckpoints();
   CheckpointQueueLock#(LockId#(8), LockId#(8)) lock <- mkCheckpointQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(LockId#(8)) chk1 <- mkReg(0);
   Reg#(LockId#(8)) chk2 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: CKL_MultipleCheckpoints ===");
      let i <- lock.res1();
      id0 <= i;
      step <= 1;
   endrule

   // Checkpoint c1 (after id0)
   rule s1(step == 1);
      let c <- lock.checkpoint();
      chk1 <= c;
      step <= 2;
   endrule

   // Reserve id1
   rule s2(step == 2);
      let i <- lock.res1();
      step <= 3;
   endrule

   // Checkpoint c2 (after id1)
   rule s3(step == 3);
      let c <- lock.checkpoint();
      chk2 <= c;
      step <= 4;
   endrule

   // Reserve id2 (speculative after c2)
   rule s4(step == 4);
      let i <- lock.res1();
      step <= 5;
   endrule

   // Rollback to c1 (undoes id1 and id2)
   rule s5(step == 5);
      lock.rollback(chk1, True, False);
      step <= 6;
   endrule

   // Verify: id0 still owns, and nothing else
   rule s6(step == 6);
      testAssert(lock.owns1(id0), "id0 still owns after rollback to c1", cyc);
      testAssert(!lock.isEmpty(), "not empty (id0 still held)", cyc);
      if (!lock.owns1(id0) || lock.isEmpty()) fails <= fails + 1;
      step <= 7;
   endrule

   // Release id0 and verify empty
   rule s7(step == 7);
      lock.rel1(id0);
      step <= 8;
   endrule

   rule s8(step == 8);
      testAssert(lock.isEmpty(), "empty after releasing id0 post-rollback", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("CKL_MultipleCheckpoints", fails);
   endrule
endmodule

// ============================================================
// Test 4: Rollback to empty -- speculative then full rollback
// Reserve id0, checkpoint. Reserve more speculatively.
// Rollback to checkpoint, then release id0. Verify empty.
// Models: misprediction where the parent instruction also
//         completes normally after rollback.
// ============================================================
(* synthesize *)
module mkTestCKL_RollbackToEmpty();
   CheckpointQueueLock#(LockId#(8), LockId#(8)) lock <- mkCheckpointQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(LockId#(8)) chk <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: CKL_RollbackToEmpty ===");
      let i <- lock.res1();
      id0 <= i;
      step <= 1;
   endrule

   // Checkpoint after id0
   rule s1(step == 1);
      let c <- lock.checkpoint();
      chk <= c;
      step <= 2;
   endrule

   // Speculative reservations
   rule s2(step == 2);
      let i <- lock.res1();
      step <= 3;
   endrule

   rule s3(step == 3);
      let i <- lock.res1();
      step <= 4;
   endrule

   // Rollback to checkpoint (undoes speculative)
   rule s4(step == 4);
      lock.rollback(chk, True, False);
      step <= 5;
   endrule

   // Verify id0 still held, then release it
   rule s5(step == 5);
      testAssert(lock.owns1(id0), "id0 still owns after rollback", cyc);
      testAssert(!lock.isEmpty(), "not empty (id0 still held)", cyc);
      if (!lock.owns1(id0) || lock.isEmpty()) fails <= fails + 1;
      lock.rel1(id0);
      step <= 6;
   endrule

   // After releasing the only pre-checkpoint ID, should be empty
   rule s6(step == 6);
      testAssert(lock.isEmpty(), "empty after rollback and release", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("CKL_RollbackToEmpty", fails);
   endrule
endmodule

// ============================================================
// Test 5: Rollback and continue -- rollback, then reserve new
// Reserve id0, checkpoint, reserve speculative id1. Rollback.
// Then reserve new id2 (correct path). Verify new reservation
// works correctly after rollback.
// Models: misprediction recovery followed by new fetch.
// ============================================================
(* synthesize *)
module mkTestCKL_RollbackAndContinue();
   CheckpointQueueLock#(LockId#(8), LockId#(8)) lock <- mkCheckpointQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(LockId#(8)) id2 <- mkReg(0);
   Reg#(LockId#(8)) chk <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: CKL_RollbackAndContinue ===");
      let i <- lock.res1();
      id0 <= i;
      step <= 1;
   endrule

   // Checkpoint after id0
   rule s1(step == 1);
      let c <- lock.checkpoint();
      chk <= c;
      step <= 2;
   endrule

   // Speculative reserve (wrong path)
   rule s2(step == 2);
      let i <- lock.res1();
      step <= 3;
   endrule

   // Rollback to checkpoint
   rule s3(step == 3);
      lock.rollback(chk, True, False);
      step <= 4;
   endrule

   // Reserve on the correct path (after rollback)
   rule s4(step == 4);
      let i <- lock.res1();
      id2 <= i;
      step <= 5;
   endrule

   // Verify: id0 owns, id2 is also reserved
   rule s5(step == 5);
      testAssert(lock.owns1(id0), "id0 owns (correct path)", cyc);
      testAssert(!lock.owns1(id2), "id2 doesn't own yet", cyc);
      testAssert(!lock.isEmpty(), "not empty", cyc);
      if (!lock.owns1(id0) || lock.owns1(id2) || lock.isEmpty()) fails <= fails + 1;
      step <= 6;
   endrule

   // Release id0
   rule s6(step == 6);
      lock.rel1(id0);
      step <= 7;
   endrule

   // id2 should now own
   rule s7(step == 7);
      testAssert(lock.owns1(id2), "id2 owns after id0 released", cyc);
      if (!lock.owns1(id2)) fails <= fails + 1;
      lock.rel1(id2);
      step <= 8;
   endrule

   rule s8(step == 8);
      testAssert(lock.isEmpty(), "empty after all released post-rollback", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("CKL_RollbackAndContinue", fails);
   endrule
endmodule

endpackage
