package TestAddrLock;

import Locks :: *;
import ConfigReg :: *;
import TestHelper :: *;

// ============================================================
// Test 1: Independent addresses -- per-register lock isolation
// Reserve locks on 3 different addresses. Verify each address's
// lock is independent: owns1 correct for each, isEmpty true for
// unrelated addresses.
// Models: decode stage reserving write locks on different
//         destination registers (rd) for independent instructions.
// ============================================================
(* synthesize *)
module mkTestAL_IndependentAddrs();
   AddrLock#(LockId#(4), UInt#(8), 4) lock <- mkFAAddrLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) idA <- mkReg(0);
   Reg#(LockId#(4)) idB <- mkReg(0);
   Reg#(LockId#(4)) idC <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   UInt#(8) addrA = 10;
   UInt#(8) addrB = 20;
   UInt#(8) addrC = 30;
   UInt#(8) addrX = 99;  // unrelated address

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: AL_IndependentAddrs ===");
      testAssert(lock.isEmpty(addrA), "addrA initially empty", cyc);
      testAssert(lock.isEmpty(addrB), "addrB initially empty", cyc);
      testAssert(lock.isEmpty(addrC), "addrC initially empty", cyc);
      if (!lock.isEmpty(addrA)) fails <= fails + 1;
      step <= 1;
   endrule

   rule s1(step == 1);
      let i <- lock.res1(addrA);
      idA <= i;
      step <= 2;
   endrule

   rule s2(step == 2);
      let i <- lock.res1(addrB);
      idB <= i;
      step <= 3;
   endrule

   rule s3(step == 3);
      let i <- lock.res1(addrC);
      idC <= i;
      step <= 4;
   endrule

   // Verify independence: each address has its lock, unrelated addr is free
   rule s4(step == 4);
      testAssert(!lock.isEmpty(addrA), "addrA not empty", cyc);
      testAssert(!lock.isEmpty(addrB), "addrB not empty", cyc);
      testAssert(!lock.isEmpty(addrC), "addrC not empty", cyc);
      // addrX has no lock but the FA lock has 4 slots and 3 are used,
      // so 1 free slot remains => isEmpty(addrX) returns True (free slot available)
      testAssert(lock.isEmpty(addrX), "addrX empty (no lock, free slot exists)", cyc);
      if (lock.isEmpty(addrA) || lock.isEmpty(addrB) || lock.isEmpty(addrC) || !lock.isEmpty(addrX)) fails <= fails + 1;
      step <= 5;
   endrule

   // Verify ownership correctness
   rule s5(step == 5);
      testAssert(lock.owns1(idA, addrA), "idA owns addrA", cyc);
      testAssert(lock.owns1(idB, addrB), "idB owns addrB", cyc);
      testAssert(lock.owns1(idC, addrC), "idC owns addrC", cyc);
      if (!lock.owns1(idA, addrA) || !lock.owns1(idB, addrB) || !lock.owns1(idC, addrC)) fails <= fails + 1;
      step <= 6;
   endrule

   // Release all
   rule s6(step == 6);
      lock.rel1(idA, addrA);
      step <= 7;
   endrule

   rule s7(step == 7);
      lock.rel1(idB, addrB);
      step <= 8;
   endrule

   rule s8(step == 8);
      lock.rel1(idC, addrC);
      step <= 9;
   endrule

   // Wait one cycle for freelock rules to fire
   rule s9(step == 9);
      step <= 10;
   endrule

   rule s10(step == 10);
      testAssert(lock.isEmpty(addrA), "addrA empty after release", cyc);
      testAssert(lock.isEmpty(addrB), "addrB empty after release", cyc);
      if (!lock.isEmpty(addrA) || !lock.isEmpty(addrB)) fails <= fails + 1;
      testDone("AL_IndependentAddrs", fails);
   endrule
endmodule

// ============================================================
// Test 2: Same address conflict -- two writes to same register
// Reserve twice on the same address (like two instructions both
// writing to the same register, e.g., rd=x5). Verify the second
// gets a different ID, only the first owns, and ownership
// advances after release.
// Models: WAW hazard in pipeline -- second instruction stalls
//         until the first commits.
// ============================================================
(* synthesize *)
module mkTestAL_SameAddrConflict();
   AddrLock#(LockId#(4), UInt#(8), 4) lock <- mkFAAddrLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) id0 <- mkReg(0);
   Reg#(LockId#(4)) id1 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   UInt#(8) addr = 5;

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: AL_SameAddrConflict ===");
      let i <- lock.res1(addr);
      id0 <= i;
      step <= 1;
   endrule

   // Second reserve on the same address
   rule s1(step == 1);
      let i <- lock.res1(addr);
      id1 <= i;
      step <= 2;
   endrule

   // Verify: id0 owns, id1 does NOT own yet (queued behind id0)
   rule s2(step == 2);
      testAssert(id0 != id1, "id0 and id1 are different IDs", cyc);
      testAssert(lock.owns1(id0, addr), "id0 owns addr", cyc);
      testAssert(!lock.owns1(id1, addr), "id1 does NOT own addr yet", cyc);
      if (id0 == id1 || !lock.owns1(id0, addr) || lock.owns1(id1, addr)) fails <= fails + 1;
      step <= 3;
   endrule

   // Release id0 (first instruction commits)
   rule s3(step == 3);
      lock.rel1(id0, addr);
      step <= 4;
   endrule

   // id1 should now own
   rule s4(step == 4);
      testAssert(lock.owns1(id1, addr), "id1 now owns after id0 released", cyc);
      testAssert(!lock.isEmpty(addr), "addr not empty (id1 still held)", cyc);
      if (!lock.owns1(id1, addr) || lock.isEmpty(addr)) fails <= fails + 1;
      lock.rel1(id1, addr);
      step <= 5;
   endrule

   // Wait for freelock to free the slot
   rule s5(step == 5);
      step <= 6;
   endrule

   rule s6(step == 6);
      testAssert(lock.isEmpty(addr), "addr empty after both released", cyc);
      if (!lock.isEmpty(addr)) fails <= fails + 1;
      testDone("AL_SameAddrConflict", fails);
   endrule
endmodule

// ============================================================
// Test 3: Pool exhaustion -- FA lock with 4 slots, reserve 4
// different addresses. Try a 5th -- verify canRes1 returns false.
// Release one, verify the 5th can now be reserved.
// Models: all lock slots consumed by in-flight writes to
//         distinct registers; new decode must stall.
// ============================================================
(* synthesize *)
module mkTestAL_PoolExhaustion();
   AddrLock#(LockId#(4), UInt#(8), 4) lock <- mkFAAddrLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) id0 <- mkReg(0);
   Reg#(LockId#(4)) id1 <- mkReg(0);
   Reg#(LockId#(4)) id2 <- mkReg(0);
   Reg#(LockId#(4)) id3 <- mkReg(0);
   Reg#(LockId#(4)) id4 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   UInt#(8) a0 = 1;
   UInt#(8) a1 = 2;
   UInt#(8) a2 = 3;
   UInt#(8) a3 = 4;
   UInt#(8) a4 = 5;  // the 5th address that won't fit

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: AL_PoolExhaustion ===");
      let i <- lock.res1(a0);
      id0 <= i;
      step <= 1;
   endrule

   rule s1(step == 1);
      let i <- lock.res1(a1);
      id1 <= i;
      step <= 2;
   endrule

   rule s2(step == 2);
      let i <- lock.res1(a2);
      id2 <= i;
      step <= 3;
   endrule

   rule s3(step == 3);
      let i <- lock.res1(a3);
      id3 <= i;
      step <= 4;
   endrule

   // All 4 slots used. canRes1 for a new address should be false.
   rule s4(step == 4);
      testAssert(!lock.canRes1(a4), "canRes1 false for 5th addr (pool full)", cyc);
      // Existing addresses should still be reservable (they already have slots)
      testAssert(lock.canRes1(a0), "canRes1 true for existing addr a0", cyc);
      if (lock.canRes1(a4) || !lock.canRes1(a0)) fails <= fails + 1;
      step <= 5;
   endrule

   // Release one to free a slot
   rule s5(step == 5);
      lock.rel1(id0, a0);
      step <= 6;
   endrule

   // Wait for freelock rule to fire
   rule s6(step == 6);
      step <= 7;
   endrule

   // Now the 5th address should be reservable
   rule s7(step == 7);
      testAssert(lock.canRes1(a4), "canRes1 true after freeing a slot", cyc);
      if (!lock.canRes1(a4)) fails <= fails + 1;
      let i <- lock.res1(a4);
      id4 <= i;
      step <= 8;
   endrule

   // Verify the new reservation works
   rule s8(step == 8);
      testAssert(!lock.isEmpty(a4), "a4 not empty after reserve", cyc);
      testAssert(lock.owns1(id4, a4), "id4 owns a4", cyc);
      if (lock.isEmpty(a4) || !lock.owns1(id4, a4)) fails <= fails + 1;
      step <= 9;
   endrule

   // Clean up: release remaining
   rule s9(step == 9);
      lock.rel1(id1, a1);
      step <= 10;
   endrule

   rule s10(step == 10);
      lock.rel1(id2, a2);
      step <= 11;
   endrule

   rule s11(step == 11);
      lock.rel1(id3, a3);
      step <= 12;
   endrule

   rule s12(step == 12);
      lock.rel1(id4, a4);
      step <= 13;
   endrule

   // Wait for freelock
   rule s13(step == 13);
      step <= 14;
   endrule

   rule s14(step == 14);
      testDone("AL_PoolExhaustion", fails);
   endrule
endmodule

// ============================================================
// Test 4: Auto-free -- verify freelock rule clears slots
// Reserve an address, release it. Wait for the freelock rule
// to fire. Verify the address slot is freed (isEmpty returns
// true). Then reserve a different address on the same slot.
// Models: writeback completes, register lock freed, new
//         instruction can use the slot for a different register.
// ============================================================
(* synthesize *)
module mkTestAL_AutoFree();
   AddrLock#(LockId#(4), UInt#(8), 2) lock <- mkFAAddrLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) idA <- mkReg(0);
   Reg#(LockId#(4)) idB <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   UInt#(8) addrA = 42;
   UInt#(8) addrB = 99;

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: AL_AutoFree ===");
      let i <- lock.res1(addrA);
      idA <= i;
      step <= 1;
   endrule

   // Fill the second slot too so we can verify freeing
   rule s1(step == 1);
      let i <- lock.res1(addrB);
      idB <= i;
      step <= 2;
   endrule

   // Both slots full. Release addrA.
   rule s2(step == 2);
      testAssert(!lock.isEmpty(addrA), "addrA not empty before release", cyc);
      if (lock.isEmpty(addrA)) fails <= fails + 1;
      lock.rel1(idA, addrA);
      step <= 3;
   endrule

   // Wait one cycle for freelock rule to invalidate the entry
   rule s3(step == 3);
      step <= 4;
   endrule

   // addrA's slot should be freed (entryVec cleared by freelock)
   rule s4(step == 4);
      testAssert(lock.isEmpty(addrA), "addrA empty after freelock", cyc);
      if (!lock.isEmpty(addrA)) fails <= fails + 1;
      step <= 5;
   endrule

   // Reserve a completely new address on the freed slot
   rule s5(step == 5);
      UInt#(8) addrNew = 77;
      testAssert(lock.canRes1(addrNew), "can reserve new addr on freed slot", cyc);
      if (!lock.canRes1(addrNew)) fails <= fails + 1;
      step <= 6;
   endrule

   // Clean up: release addrB
   rule s6(step == 6);
      lock.rel1(idB, addrB);
      step <= 7;
   endrule

   rule s7(step == 7);
      step <= 8;
   endrule

   rule s8(step == 8);
      testDone("AL_AutoFree", fails);
   endrule
endmodule

// ============================================================
// Test 5: DM (direct-mapped) address lock basic test
// Reserve and release on 2 addresses, verify per-address
// independence. Unlike FA, DM always has capacity for any
// address (each address maps to its own counter lock).
// Models: register file with direct-mapped lock per register.
// ============================================================
(* synthesize *)
module mkTestAL_DMBasic();
   // DM lock: addr type UInt#(3) gives 2^3=8 lock entries
   // The third type parameter (unused) is set to 0
   AddrLock#(LockId#(4), UInt#(3), 0) lock <- mkDMAddrLock();

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) idA <- mkReg(0);
   Reg#(LockId#(4)) idB <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   UInt#(3) addrA = 2;
   UInt#(3) addrB = 5;

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: AL_DMBasic ===");
      testAssert(lock.isEmpty(addrA), "addrA initially empty", cyc);
      testAssert(lock.isEmpty(addrB), "addrB initially empty", cyc);
      // DM always has capacity
      testAssert(lock.canRes1(addrA), "canRes1 always true for DM", cyc);
      if (!lock.isEmpty(addrA) || !lock.isEmpty(addrB)) fails <= fails + 1;
      step <= 1;
   endrule

   rule s1(step == 1);
      let i <- lock.res1(addrA);
      idA <= i;
      step <= 2;
   endrule

   rule s2(step == 2);
      let i <- lock.res1(addrB);
      idB <= i;
      step <= 3;
   endrule

   // Verify: addrA and addrB independently locked, unreserved addr empty
   rule s3(step == 3);
      UInt#(3) addrC = 7;
      testAssert(!lock.isEmpty(addrA), "addrA not empty", cyc);
      testAssert(!lock.isEmpty(addrB), "addrB not empty", cyc);
      testAssert(lock.owns1(idA, addrA), "idA owns addrA", cyc);
      testAssert(lock.owns1(idB, addrB), "idB owns addrB", cyc);
      // An unreserved address should still be empty
      testAssert(lock.isEmpty(addrC), "unreserved addrC is empty", cyc);
      if (lock.isEmpty(addrA) || lock.isEmpty(addrB) || !lock.owns1(idA, addrA) || !lock.owns1(idB, addrB) || !lock.isEmpty(addrC)) fails <= fails + 1;
      step <= 4;
   endrule

   // Release addrA; addrB should be unaffected
   rule s4(step == 4);
      lock.rel1(idA, addrA);
      step <= 5;
   endrule

   rule s5(step == 5);
      testAssert(lock.isEmpty(addrA), "addrA empty after release", cyc);
      testAssert(!lock.isEmpty(addrB), "addrB still not empty", cyc);
      testAssert(lock.owns1(idB, addrB), "idB still owns addrB", cyc);
      if (!lock.isEmpty(addrA) || lock.isEmpty(addrB) || !lock.owns1(idB, addrB)) fails <= fails + 1;
      step <= 6;
   endrule

   // Release addrB
   rule s6(step == 6);
      lock.rel1(idB, addrB);
      step <= 7;
   endrule

   rule s7(step == 7);
      testAssert(lock.isEmpty(addrA), "addrA still empty", cyc);
      testAssert(lock.isEmpty(addrB), "addrB empty after release", cyc);
      if (!lock.isEmpty(addrA) || !lock.isEmpty(addrB)) fails <= fails + 1;
      testDone("AL_DMBasic", fails);
   endrule
endmodule

endpackage
