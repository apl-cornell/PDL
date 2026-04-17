package TestCountingLock;

import Locks :: *;
import ConfigReg :: *;
import TestHelper :: *;

// ============================================================
// Test 1: Basic lifecycle -- same as QueueLock basic test
// Reserve, verify owns, release, verify empty.
// ============================================================
(* synthesize *)
module mkTestCL_BasicLifecycle();
   QueueLock#(LockId#(8)) lock <- mkCountingLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: CL_BasicLifecycle ===");
      testAssert(lock.isEmpty(), "initially empty", cyc);
      testAssert(lock.canRes1(), "can reserve when empty", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      step <= 1;
   endrule

   rule s1(step == 1);
      let i <- lock.res1();
      id0 <= i;
      step <= 2;
   endrule

   rule s2(step == 2);
      testAssert(!lock.isEmpty(), "not empty after reserve", cyc);
      testAssert(lock.owns1(id0), "id0 owns the lock", cyc);
      if (lock.isEmpty() || !lock.owns1(id0)) fails <= fails + 1;
      step <= 3;
   endrule

   rule s3(step == 3);
      lock.rel1(id0);
      step <= 4;
   endrule

   rule s4(step == 4);
      testAssert(lock.isEmpty(), "empty after release", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("CL_BasicLifecycle", fails);
   endrule
endmodule

// ============================================================
// Test 2: Same-cycle reserve and release (EHR port ordering)
// CountingLock's EHR allows res1 (port 0) and the updateEmpty
// rule to coexist properly. We reserve, then in the next cycle
// both release the first and reserve a new one (in separate
// rules), and verify the lock transitions correctly.
// Note: res1 writes doRes RWire and nextId[0]; rel1 writes
// doRel RWire and owner Reg. These are separate state so
// both can fire in the same cycle. The updateEmpty rule reads
// both RWires and updates empty.
// ============================================================
(* synthesize *)
module mkTestCL_SameCycleResRel();
   QueueLock#(LockId#(8)) lock <- mkCountingLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(LockId#(8)) id1 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: CL_SameCycleResRel ===");
      let i <- lock.res1();
      id0 <= i;
      step <= 1;
   endrule

   // Step 1: verify id0 owns
   rule s1(step == 1);
      testAssert(lock.owns1(id0), "id0 owns after reserve", cyc);
      if (!lock.owns1(id0)) fails <= fails + 1;
      step <= 2;
   endrule

   // Step 2: release id0 in this rule
   rule s2_rel(step == 2);
      lock.rel1(id0);
      step <= 3;
   endrule

   // Step 2 also: reserve a new ID in the same cycle
   // This fires alongside s2_rel because they write different state
   rule s2_res(step == 2);
      let i <- lock.res1();
      id1 <= i;
   endrule

   // Step 3: verify that the lock transitioned correctly
   // id1 should now own (id0 was released, id1 is the new head)
   rule s3(step == 3);
      testAssert(!lock.isEmpty(), "not empty (id1 still in lock)", cyc);
      testAssert(lock.owns1(id1), "id1 owns after same-cycle res/rel", cyc);
      if (lock.isEmpty() || !lock.owns1(id1)) fails <= fails + 1;
      step <= 4;
   endrule

   // Release id1
   rule s4(step == 4);
      lock.rel1(id1);
      step <= 5;
   endrule

   rule s5(step == 5);
      testAssert(lock.isEmpty(), "empty after all released", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("CL_SameCycleResRel", fails);
   endrule
endmodule

// ============================================================
// Test 3: Many reservations without releasing
// Reserve 6 IDs on a depth-8 lock without releasing.
// Verify isEmpty stays false. Then release all in order.
// Tests counter-based tracking with multiple outstanding.
// ============================================================
(* synthesize *)
module mkTestCL_ManyReservations();
   QueueLock#(LockId#(8)) lock <- mkCountingLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(LockId#(8)) id1 <- mkReg(0);
   Reg#(LockId#(8)) id2 <- mkReg(0);
   Reg#(LockId#(8)) id3 <- mkReg(0);
   Reg#(LockId#(8)) id4 <- mkReg(0);
   Reg#(LockId#(8)) id5 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: CL_ManyReservations ===");
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

   rule s4(step == 4);
      let i <- lock.res1();
      id4 <= i;
      step <= 5;
   endrule

   rule s5(step == 5);
      let i <- lock.res1();
      id5 <= i;
      step <= 6;
   endrule

   // All 6 reserved. Lock should not be empty.
   rule s6(step == 6);
      testAssert(!lock.isEmpty(), "not empty with 6 reserved", cyc);
      testAssert(lock.owns1(id0), "id0 owns (head)", cyc);
      if (lock.isEmpty() || !lock.owns1(id0)) fails <= fails + 1;
      step <= 7;
   endrule

   // Release all in order
   rule s7(step == 7);
      lock.rel1(id0);
      step <= 8;
   endrule

   rule s8(step == 8);
      lock.rel1(id1);
      step <= 9;
   endrule

   rule s9(step == 9);
      lock.rel1(id2);
      step <= 10;
   endrule

   rule s10(step == 10);
      lock.rel1(id3);
      step <= 11;
   endrule

   rule s11(step == 11);
      lock.rel1(id4);
      step <= 12;
   endrule

   rule s12(step == 12);
      lock.rel1(id5);
      step <= 13;
   endrule

   rule s13(step == 13);
      testAssert(lock.isEmpty(), "empty after releasing all 6", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("CL_ManyReservations", fails);
   endrule
endmodule

// ============================================================
// Test 4: Owner advancement -- release head, verify next owns
// Reserve 3 IDs. Release them one at a time. After each
// release, verify the next ID becomes the owner.
// Models thread-order commit in a pipeline.
// ============================================================
(* synthesize *)
module mkTestCL_OwnerAdvancement();
   QueueLock#(LockId#(8)) lock <- mkCountingLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(LockId#(8)) id1 <- mkReg(0);
   Reg#(LockId#(8)) id2 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: CL_OwnerAdvancement ===");
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

   // Verify initial ownership
   rule s3(step == 3);
      testAssert(lock.owns1(id0), "id0 owns initially", cyc);
      testAssert(!lock.owns1(id1), "id1 doesn't own yet", cyc);
      testAssert(!lock.owns1(id2), "id2 doesn't own yet", cyc);
      if (!lock.owns1(id0) || lock.owns1(id1) || lock.owns1(id2)) fails <= fails + 1;
      step <= 4;
   endrule

   // Release id0
   rule s4(step == 4);
      lock.rel1(id0);
      step <= 5;
   endrule

   // Verify id1 now owns
   rule s5(step == 5);
      testAssert(lock.owns1(id1), "id1 owns after id0 released", cyc);
      testAssert(!lock.owns1(id2), "id2 still doesn't own", cyc);
      if (!lock.owns1(id1) || lock.owns1(id2)) fails <= fails + 1;
      step <= 6;
   endrule

   // Release id1
   rule s6(step == 6);
      lock.rel1(id1);
      step <= 7;
   endrule

   // Verify id2 now owns
   rule s7(step == 7);
      testAssert(lock.owns1(id2), "id2 owns after id1 released", cyc);
      testAssert(!lock.isEmpty(), "not empty (id2 still held)", cyc);
      if (!lock.owns1(id2) || lock.isEmpty()) fails <= fails + 1;
      step <= 8;
   endrule

   // Release id2
   rule s8(step == 8);
      lock.rel1(id2);
      step <= 9;
   endrule

   rule s9(step == 9);
      testAssert(lock.isEmpty(), "empty after all released", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("CL_OwnerAdvancement", fails);
   endrule
endmodule

// ============================================================
// Test 5: Counter wraparound -- reserve/release past depth
// With depth 8 (3-bit counter), reserve and release 10 times
// to force the counter to wrap. Verify correct ownership
// after wraparound.
// ============================================================
(* synthesize *)
module mkTestCL_Wraparound();
   QueueLock#(LockId#(8)) lock <- mkCountingLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) curId <- mkReg(0);
   Reg#(UInt#(4)) iteration <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: CL_Wraparound ===");
      iteration <= 0;
      step <= 1;
   endrule

   // Reserve
   rule s1(step == 1);
      let i <- lock.res1();
      curId <= i;
      step <= 2;
   endrule

   // Check owns, then release
   rule s2(step == 2);
      testAssert(lock.owns1(curId), "curId owns in iteration", cyc);
      if (!lock.owns1(curId)) fails <= fails + 1;
      lock.rel1(curId);
      step <= 3;
   endrule

   // Check empty, loop or finish
   rule s3(step == 3);
      testAssert(lock.isEmpty(), "empty after release in iteration", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      if (iteration < 9)
         begin
            iteration <= iteration + 1;
            step <= 1;
         end
      else
         step <= 4;
   endrule

   // After 10 iterations (well past the 8-value counter range),
   // do one more reserve/check to confirm correct operation post-wrap.
   rule s4(step == 4);
      let i <- lock.res1();
      curId <= i;
      step <= 5;
   endrule

   rule s5(step == 5);
      testAssert(lock.owns1(curId), "owns after wraparound", cyc);
      testAssert(!lock.isEmpty(), "not empty after final reserve", cyc);
      if (!lock.owns1(curId) || lock.isEmpty()) fails <= fails + 1;
      lock.rel1(curId);
      step <= 6;
   endrule

   rule s6(step == 6);
      testAssert(lock.isEmpty(), "empty after final release post-wrap", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("CL_Wraparound", fails);
   endrule
endmodule

endpackage
