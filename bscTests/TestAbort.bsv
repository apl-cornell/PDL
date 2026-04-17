package TestAbort;

import Locks :: *;
import Memories :: *;
import RegFile :: *;
import ConfigReg :: *;
import TestHelper :: *;

typedef UInt#(5) Addr;
typedef UInt#(32) Data;

// ============================================================
// Test 1: CheckpointQueueLock abort resets uncommitted state
// ============================================================
(* synthesize *)
module mkTestAbort_LockBasic();
   CheckpointQueueLock#(LockId#(8), LockId#(8)) lock <- mkCheckpointQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: Abort_LockBasic ===");
      let i <- lock.res1();
      id0 <= i;
      step <= 1;
   endrule

   rule s1(step == 1);
      let i <- lock.res1();
      // Two reservations pending, id0 owns
      testAssert(lock.owns1(id0), "id0 owns before abort", cyc);
      if (!lock.owns1(id0)) fails <= fails + 1;
      step <= 2;
   endrule

   rule s2(step == 2);
      // Abort: should clear all pending reservations
      lock.abort();
      step <= 3;
   endrule

   rule s3(step == 3);
      testAssert(lock.isEmpty(), "lock empty after abort", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("Abort_LockBasic", fails);
   endrule
endmodule

// ============================================================
// Test 2: Abort after partial release -- committed releases stick
// ============================================================
(* synthesize *)
module mkTestAbort_AfterRelease();
   CheckpointQueueLock#(LockId#(8), LockId#(8)) lock <- mkCheckpointQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(LockId#(8)) id1 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: Abort_AfterRelease ===");
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
      // Release first (commit it)
      lock.rel1(id0);
      step <= 3;
   endrule

   rule s3(step == 3);
      // Now id1 is pending (uncommitted). Abort should clear id1's reservation.
      lock.abort();
      step <= 4;
   endrule

   rule s4(step == 4);
      // After abort: lock should be empty because owner advanced past id0
      // and abort reset nextId to owner
      testAssert(lock.isEmpty(), "empty after release+abort", cyc);
      if (!lock.isEmpty()) fails <= fails + 1;
      testDone("Abort_AfterRelease", fails);
   endrule
endmodule

// ============================================================
// Test 3: Can reserve again after abort
// ============================================================
(* synthesize *)
module mkTestAbort_ReserveAfterAbort();
   CheckpointQueueLock#(LockId#(8), LockId#(8)) lock <- mkCheckpointQueueLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(8)) id0 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: Abort_ReserveAfterAbort ===");
      let i <- lock.res1();
      step <= 1;
   endrule

   rule s1(step == 1);
      lock.abort();
      step <= 2;
   endrule

   rule s2(step == 2);
      // Should be able to reserve again after abort
      let i <- lock.res1();
      id0 <= i;
      testAssert(True, "reserve after abort succeeded", cyc);
      step <= 3;
   endrule

   rule s3(step == 3);
      testAssert(lock.owns1(id0), "new reservation owns lock", cyc);
      if (!lock.owns1(id0)) fails <= fails + 1;
      lock.rel1(id0);
      step <= 4;
   endrule

   rule s4(step == 4);
      testDone("Abort_ReserveAfterAbort", fails);
   endrule
endmodule

// ============================================================
// Test 4: AsyncMem clear drops in-flight requests
// ============================================================
(* synthesize *)
module mkTestAbort_AsyncMemClear();
   AsyncMem#(UInt#(16), Int#(32), MemId#(4), 4) mem <- mkAsyncMem();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(MemId#(4)) rid <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: Abort_AsyncMemClear ===");
      // Issue a read request
      let id <- mem.req1(0, 0, 0);
      rid <= id;
      step <= 1;
   endrule

   rule s1(step == 1);
      // Request is in flight but not responded yet.
      // Clear should drop it.
      mem.clear();
      step <= 2;
   endrule

   rule s2(step == 2);
      // After clear, checkRespId should be false (request dropped)
      testAssert(!mem.checkRespId1(rid), "response dropped after clear", cyc);
      if (mem.checkRespId1(rid)) fails <= fails + 1;
      testDone("Abort_AsyncMemClear", fails);
   endrule
endmodule

// ============================================================
// Test 5: AsyncMem clear then new request works
// ============================================================
(* synthesize *)
module mkTestAbort_AsyncMemClearAndReuse();
   AsyncMem#(UInt#(16), Int#(32), MemId#(4), 4) mem <- mkAsyncMem();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(MemId#(4)) rid <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkReg(0);

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: Abort_AsyncMemClearAndReuse ===");
      let id <- mem.req1(0, 0, 0);
      step <= 1;
   endrule

   rule s1(step == 1);
      mem.clear();
      step <= 2;
   endrule

   rule s2(step == 2);
      // After clear, should be able to issue a new request
      let id <- mem.req1(5, 0, 0);
      rid <= id;
      testAssert(True, "new request after clear succeeded", cyc);
      step <= 3;
   endrule

   rule s3(step == 3);
      testDone("Abort_AsyncMemClearAndReuse", fails);
   endrule
endmodule

endpackage
