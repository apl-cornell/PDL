package TestQueueLock;

import Locks :: *;
import ConfigReg :: *;
import TestHelper :: *;

// ============================================================
// Test 1: Basic lifecycle -- reserve, owns, release, empty
// Models the minimal path of a single instruction through
// the pipeline: decode reserves, writeback releases.
// ============================================================
(* synthesize *)
module mkTestQL_BasicLifecycle();
   QueueLock#(LockId#(4)) lock <- mkQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) id0 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: QL_BasicLifecycle ===");
      testAssert(lock.isEmpty(), "initially empty", cyc);
      testAssert(lock.canRes1(), "can reserve when empty", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      step <= 1;
   endrule

   // Reserve one ID (decode stage)
   rule s1(step == 1);
      let i <- lock.res1();
      id0 <= i;
      step <= 2;
   endrule

   // Verify ownership
   rule s2(step == 2);
      testAssert(!lock.isEmpty(), "not empty after reserve", cyc);
      testAssert(lock.owns1(id0), "id0 owns the lock", cyc);
      if (lock.isEmpty() || !lock.owns1(id0)) fails <= fails + 1;
      step <= 3;
   endrule

   // Release (writeback stage)
   rule s3(step == 3);
      lock.rel1(id0);
      step <= 4;
   endrule

   // Verify empty after release
   rule s4(step == 4);
      testAssert(lock.isEmpty(), "empty after release", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("QL_BasicLifecycle", fails);
   endrule
endmodule

// ============================================================
// Test 2: Pipeline stall -- 3 in-flight instructions
// Models a 3-deep pipeline where instructions are reserved
// in decode and released in writeback, in order.
// Only the head of the queue owns the lock.
// ============================================================
(* synthesize *)
module mkTestQL_PipelineStall();
   QueueLock#(LockId#(4)) lock <- mkQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) id0 <- mkReg(0);
   Reg#(LockId#(4)) id1 <- mkReg(0);
   Reg#(LockId#(4)) id2 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: QL_PipelineStall ===");
      let i <- lock.res1();
      id0 <= i;
      step <= 1;
   endrule

   rule s1(step == 1);
      let i <- lock.res1();
      id1 <= i;
      step <= 2;
   endrule

   rule s2(step == 2);
      let i <- lock.res1();
      id2 <= i;
      step <= 3;
   endrule

   // All 3 reserved. Only id0 (head) should own.
   rule s3(step == 3);
      testAssert(lock.owns1(id0), "id0 owns (head of queue)", cyc);
      testAssert(!lock.owns1(id1), "id1 does NOT own", cyc);
      testAssert(!lock.owns1(id2), "id2 does NOT own", cyc);
      if (!lock.owns1(id0) || lock.owns1(id1) || lock.owns1(id2)) fails <= fails + 1;
      step <= 4;
   endrule

   // Release id0 (first instruction completes writeback)
   rule s4(step == 4);
      lock.rel1(id0);
      step <= 5;
   endrule

   // id1 should now own
   rule s5(step == 5);
      testAssert(lock.owns1(id1), "id1 now owns after id0 released", cyc);
      testAssert(!lock.owns1(id2), "id2 still doesn't own", cyc);
      if (!lock.owns1(id1) || lock.owns1(id2)) fails <= fails + 1;
      lock.rel1(id1);
      step <= 6;
   endrule

   // id2 should now own
   rule s6(step == 6);
      testAssert(lock.owns1(id2), "id2 now owns after id1 released", cyc);
      if (!lock.owns1(id2)) fails <= fails + 1;
      lock.rel1(id2);
      step <= 7;
   endrule

   rule s7(step == 7);
      testAssert(lock.isEmpty(), "empty after all 3 released", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("QL_PipelineStall", fails);
   endrule
endmodule

// ============================================================
// Test 3: Full queue -- fill to capacity, verify backpressure
// Uses depth 4. Fills the queue, checks canRes1 is false,
// then drains one-by-one. Models pipeline saturation.
// ============================================================
(* synthesize *)
module mkTestQL_FullQueue();
   QueueLock#(LockId#(4)) lock <- mkQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) id0 <- mkReg(0);
   Reg#(LockId#(4)) id1 <- mkReg(0);
   Reg#(LockId#(4)) id2 <- mkReg(0);
   Reg#(LockId#(4)) id3 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: QL_FullQueue ===");
      let i <- lock.res1();
      id0 <= i;
      step <= 1;
   endrule

   rule s1(step == 1);
      let i <- lock.res1();
      id1 <= i;
      step <= 2;
   endrule

   rule s2(step == 2);
      let i <- lock.res1();
      id2 <= i;
      step <= 3;
   endrule

   rule s3(step == 3);
      let i <- lock.res1();
      id3 <= i;
      step <= 4;
   endrule

   // Queue is full (depth 4). canRes1 should be false.
   rule s4(step == 4);
      testAssert(!lock.canRes1(), "canRes1 false when full", cyc);
      testAssert(!lock.isEmpty(), "not empty when full", cyc);
      if (lock.canRes1() || lock.isEmpty()) fails <= fails + 1;
      step <= 5;
   endrule

   // Drain one: release head
   rule s5(step == 5);
      lock.rel1(id0);
      step <= 6;
   endrule

   // After draining one, canRes1 should be true again
   rule s6(step == 6);
      testAssert(lock.canRes1(), "canRes1 true after one released", cyc);
      testAssert(lock.owns1(id1), "id1 now owns", cyc);
      if (!lock.canRes1() || !lock.owns1(id1)) fails <= fails + 1;
      lock.rel1(id1);
      step <= 7;
   endrule

   rule s7(step == 7);
      lock.rel1(id2);
      step <= 8;
   endrule

   rule s8(step == 8);
      lock.rel1(id3);
      step <= 9;
   endrule

   rule s9(step == 9);
      testAssert(lock.isEmpty(), "empty after full drain", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("QL_FullQueue", fails);
   endrule
endmodule

// ============================================================
// Test 4: Rapid reserve/release -- steady-state pipeline
// Alternates reserve and release each cycle, modeling a
// pipeline processing one instruction per cycle. 6 iterations
// verify consistent ID advancement.
// ============================================================
(* synthesize *)
module mkTestQL_RapidReserveRelease();
   QueueLock#(LockId#(8)) lock <- mkQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) curId <- mkReg(0);
   Reg#(LockId#(8)) prevId <- mkReg(0);
   Reg#(UInt#(4)) iteration <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   // Step 0: reserve first ID to start the pipeline
   rule s0(step == 0);
      $display("=== TEST: QL_RapidReserveRelease ===");
      let i <- lock.res1();
      curId <= i;
      prevId <= i;
      iteration <= 0;
      step <= 1;
   endrule

   // Step 1: release current head
   rule s1(step == 1);
      lock.rel1(curId);
      step <= 2;
   endrule

   // Step 2: reserve new ID (the queue should be empty after release)
   rule s2(step == 2);
      let i <- lock.res1();
      prevId <= curId;
      curId <= i;
      step <= 3;
   endrule

   // Step 3: verify new ID is different from previous, and lock not empty
   rule s3(step == 3);
      testAssert(curId != prevId, "new ID differs from previous", cyc);
      testAssert(!lock.isEmpty(), "not empty after reserve", cyc);
      testAssert(lock.owns1(curId), "new ID owns lock", cyc);
      if (curId == prevId || lock.isEmpty() || !lock.owns1(curId)) fails <= fails + 1;
      if (iteration < 5)
         begin
            iteration <= iteration + 1;
            step <= 1;  // loop back to release/reserve cycle
         end
      else
         step <= 4;
   endrule

   // Step 4: final release and verify empty
   rule s4(step == 4);
      lock.rel1(curId);
      step <= 5;
   endrule

   rule s5(step == 5);
      testAssert(lock.isEmpty(), "empty after final release", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("QL_RapidReserveRelease", fails);
   endrule
endmodule

// ============================================================
// Test 5: Wrong release -- out-of-order commit attempt
// Reserve 2 IDs. Try releasing the second (non-head) first.
// Since QueueLock.rel1 checks owner == tid, only the head
// can be released. The non-owner release should be a no-op.
// ============================================================
(* synthesize *)
module mkTestQL_WrongRelease();
   QueueLock#(LockId#(4)) lock <- mkQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) id0 <- mkReg(0);
   Reg#(LockId#(4)) id1 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: QL_WrongRelease ===");
      let i <- lock.res1();
      id0 <= i;
      step <= 1;
   endrule

   rule s1(step == 1);
      let i <- lock.res1();
      id1 <= i;
      step <= 2;
   endrule

   // Try releasing id1 (not the head). Should be a no-op.
   rule s2(step == 2);
      lock.rel1(id1);
      step <= 3;
   endrule

   // Verify the queue is unchanged: id0 still owns, not empty
   rule s3(step == 3);
      testAssert(!lock.isEmpty(), "not empty after wrong release", cyc);
      testAssert(lock.owns1(id0), "id0 still owns after wrong release", cyc);
      testAssert(!lock.owns1(id1), "id1 still doesn't own", cyc);
      if (lock.isEmpty() || !lock.owns1(id0) || lock.owns1(id1)) fails <= fails + 1;
      step <= 4;
   endrule

   // Now release correctly: id0 first
   rule s4(step == 4);
      lock.rel1(id0);
      step <= 5;
   endrule

   // id1 should now own
   rule s5(step == 5);
      testAssert(lock.owns1(id1), "id1 owns after correct id0 release", cyc);
      if (!lock.owns1(id1)) fails <= fails + 1;
      lock.rel1(id1);
      step <= 6;
   endrule

   rule s6(step == 6);
      testAssert(lock.isEmpty(), "empty after both released correctly", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("QL_WrongRelease", fails);
   endrule
endmodule

endpackage
