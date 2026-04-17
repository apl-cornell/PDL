package TestNewMemories;

import Locks :: *;
import Memories :: *;
import RegFile :: *;
import ConfigReg :: *;
import TestHelper :: *;

typedef UInt#(5) Addr;
typedef UInt#(32) Data;

// ============================================================
// Test 1: QueueLockCombMem -- basic read/write and lock lifecycle
// Write a value, read it back. Reserve lock, verify canAtom blocked.
// Release, verify canAtom restored. Simple lifecycle.
// ============================================================
(* synthesize *)
module mkTestMem_QLBasicReadWrite();
   RegFile#(Addr, Data) rf <- mkRegFileFull();
   QueueLockCombMem#(Addr, Data, LockId#(4)) mem <- mkQueueLockCombMem(rf);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) wid <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Addr target = 4;

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: Mem_QLBasicReadWrite ===");
      mem.write(target, 55);
      step <= 1;
   endrule

   // Read back the written value
   rule s1(step == 1);
      let v = mem.read(target);
      testAssert(v == 55, "read == 55 after write", cyc);
      if (v != 55) fails <= fails + 1;
      step <= 2;
   endrule

   // Verify canAtom is true when lock is empty
   rule s2(step == 2);
      testAssert(mem.canAtom_r1(target), "canAtom true when unlocked", cyc);
      if (!mem.canAtom_r1(target)) fails <= fails + 1;
      step <= 3;
   endrule

   // Reserve lock
   rule s3(step == 3);
      let id <- mem.lock.res1();
      wid <= id;
      step <= 4;
   endrule

   // canAtom should now be false (lock held)
   rule s4(step == 4);
      testAssert(!mem.canAtom_r1(target), "canAtom false when locked", cyc);
      if (mem.canAtom_r1(target)) fails <= fails + 1;
      step <= 5;
   endrule

   // Release lock
   rule s5(step == 5);
      mem.lock.rel1(wid);
      step <= 6;
   endrule

   // canAtom should be restored
   rule s6(step == 6);
      testAssert(mem.canAtom_r1(target), "canAtom true after release", cyc);
      if (!mem.canAtom_r1(target)) fails <= fails + 1;
      step <= 7;
   endrule

   // Verify atom_r still returns the written value
   rule s7(step == 7);
      let v = mem.atom_r(target);
      testAssert(v == 55, "atom_r == 55", cyc);
      if (v != 55) fails <= fails + 1;
      testDone("Mem_QLBasicReadWrite", fails);
   endrule
endmodule

// ============================================================
// Test 2: AddrLockCombMem -- RAW stall modeling
// Write addr 1 = 100. Reserve lock on addr 1 (simulating write-back targeting addr 1).
// While locked, canAtom_r1(addr 1) should be false but canAtom_r1(addr 2) should be true.
// Release, verify reads work. Models RAW stall in the pipeline.
// ============================================================
(* synthesize *)
module mkTestMem_ALReadAfterWrite();
   RegFile#(Addr, Data) rf <- mkRegFileFull();
   AddrLockCombMem#(Addr, Data, LockId#(4), 4) mem <- mkFAAddrLockCombMem(rf);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) lockId <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Addr addr1 = 1;
   Addr addr2 = 2;

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: Mem_ALReadAfterWrite ===");
      mem.write(addr1, 100);
      step <= 1;
   endrule

   // Verify the write took effect
   rule s1(step == 1);
      let v = mem.read(addr1);
      testAssert(v == 100, "read addr1 == 100", cyc);
      if (v != 100) fails <= fails + 1;
      step <= 2;
   endrule

   // Reserve lock on addr1 (simulating in-flight instruction writing to addr1)
   rule s2(step == 2);
      let id <- mem.lock.res1(addr1);
      lockId <= id;
      step <= 3;
   endrule

   // addr1 locked: canAtom_r1(addr1) should be false
   rule s3(step == 3);
      testAssert(!mem.canAtom_r1(addr1), "canAtom_r1(addr1) false when locked", cyc);
      if (mem.canAtom_r1(addr1)) fails <= fails + 1;
      step <= 4;
   endrule

   // addr2 not locked: canAtom_r1(addr2) should be true
   rule s4(step == 4);
      testAssert(mem.canAtom_r1(addr2), "canAtom_r1(addr2) true (different addr)", cyc);
      if (!mem.canAtom_r1(addr2)) fails <= fails + 1;
      step <= 5;
   endrule

   // Release the lock on addr1
   rule s5(step == 5);
      mem.lock.rel1(lockId, addr1);
      step <= 6;
   endrule

   // Wait a cycle for the lock auto-free rule to clear the entry
   rule s6(step == 6);
      step <= 7;
   endrule

   // After release, canAtom_r1(addr1) should be true
   rule s7(step == 7);
      testAssert(mem.canAtom_r1(addr1), "canAtom_r1(addr1) true after release", cyc);
      if (!mem.canAtom_r1(addr1)) fails <= fails + 1;
      step <= 8;
   endrule

   // Verify data is readable
   rule s8(step == 8);
      let v = mem.atom_r(addr1);
      testAssert(v == 100, "atom_r(addr1) == 100 after release", cyc);
      if (v != 100) fails <= fails + 1;
      testDone("Mem_ALReadAfterWrite", fails);
   endrule
endmodule

// ============================================================
// Test 3: AddrLockCombMem -- multiple readers with different destinations
// Reserve locks on addrs 3, 5, 7 (three different register destinations in flight).
// Verify addr 10 is still readable (no lock on it). Release all.
// Verify all addresses are readable. Models multiple in-flight instructions.
// ============================================================
(* synthesize *)
module mkTestMem_ALMultipleReaders();
   RegFile#(Addr, Data) rf <- mkRegFileFull();
   AddrLockCombMem#(Addr, Data, LockId#(4), 4) mem <- mkFAAddrLockCombMem(rf);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) id3 <- mkReg(0);
   Reg#(LockId#(4)) id5 <- mkReg(0);
   Reg#(LockId#(4)) id7 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Addr a3 = 3;
   Addr a5 = 5;
   Addr a7 = 7;
   Addr a10 = 10;

   rule tick; cyc <= cyc + 1; endrule

   // Pre-populate some values
   rule s0(step == 0);
      $display("=== TEST: Mem_ALMultipleReaders ===");
      mem.write(a3, 30);
      step <= 1;
   endrule

   rule s1(step == 1);
      mem.write(a5, 50);
      step <= 2;
   endrule

   rule s2(step == 2);
      mem.write(a7, 70);
      step <= 3;
   endrule

   rule s3(step == 3);
      mem.write(a10, 100);
      step <= 4;
   endrule

   // Reserve lock on addr 3
   rule s4(step == 4);
      let i <- mem.lock.res1(a3);
      id3 <= i;
      step <= 5;
   endrule

   // Reserve lock on addr 5
   rule s5(step == 5);
      let i <- mem.lock.res1(a5);
      id5 <= i;
      step <= 6;
   endrule

   // Reserve lock on addr 7
   rule s6(step == 6);
      let i <- mem.lock.res1(a7);
      id7 <= i;
      step <= 7;
   endrule

   // addr 10 has no lock -- canAtom should be true
   rule s7(step == 7);
      testAssert(mem.canAtom_r1(a10), "canAtom(10) true - no lock", cyc);
      if (!mem.canAtom_r1(a10)) fails <= fails + 1;
      step <= 8;
   endrule

   // Verify atom_r for addr 10
   rule s7b(step == 8);
      let v = mem.atom_r(a10);
      testAssert(v == 100, "atom_r(10) == 100", cyc);
      if (v != 100) fails <= fails + 1;
      step <= 9;
   endrule

   // Release addr 3
   rule s8(step == 9);
      mem.lock.rel1(id3, a3);
      step <= 10;
   endrule

   // Release addr 5
   rule s9(step == 10);
      mem.lock.rel1(id5, a5);
      step <= 11;
   endrule

   // Release addr 7
   rule s10(step == 11);
      mem.lock.rel1(id7, a7);
      step <= 12;
   endrule

   // Wait for lock auto-free
   rule s11(step == 12);
      step <= 13;
   endrule

   // Verify addr 3 readable
   rule s12(step == 13);
      testAssert(mem.canAtom_r1(a3), "canAtom(3) restored", cyc);
      if (!mem.canAtom_r1(a3)) fails <= fails + 1;
      step <= 14;
   endrule

   // Verify addr 5 readable
   rule s13(step == 14);
      testAssert(mem.canAtom_r1(a5), "canAtom(5) restored", cyc);
      if (!mem.canAtom_r1(a5)) fails <= fails + 1;
      step <= 15;
   endrule

   // Verify addr 7 readable
   rule s14(step == 15);
      testAssert(mem.canAtom_r1(a7), "canAtom(7) restored", cyc);
      if (!mem.canAtom_r1(a7)) fails <= fails + 1;
      testDone("Mem_ALMultipleReaders", fails);
   endrule
endmodule

// ============================================================
// Test 4: QueueLockCombMem -- atomic operations stall when locked
// Verify atom_r and atom_w work when lock is empty. Reserve lock,
// verify atom operations are blocked (canAtom returns false).
// Models the stall-based atomic access pattern.
// ============================================================
(* synthesize *)
module mkTestMem_QLAtomicOps();
   RegFile#(Addr, Data) rf <- mkRegFileFull();
   QueueLockCombMem#(Addr, Data, LockId#(4)) mem <- mkQueueLockCombMem(rf);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) wid0 <- mkReg(0);
   Reg#(LockId#(4)) wid1 <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Addr target = 8;

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: Mem_QLAtomicOps ===");
      mem.write(target, 77);
      step <= 1;
   endrule

   // When lock is empty, canAtom_r1 should be true
   rule s1(step == 1);
      testAssert(mem.canAtom_r1(target), "canAtom_r1 true when empty", cyc);
      if (!mem.canAtom_r1(target)) fails <= fails + 1;
      step <= 2;
   endrule

   // Verify atom_r returns correct value
   rule s1b(step == 2);
      let v = mem.atom_r(target);
      testAssert(v == 77, "atom_r == 77 when unlocked", cyc);
      if (v != 77) fails <= fails + 1;
      step <= 3;
   endrule

   // Verify canAtom_w1 also works when lock is empty
   rule s2(step == 3);
      testAssert(mem.canAtom_w1(target), "canAtom_w1 true when empty", cyc);
      if (!mem.canAtom_w1(target)) fails <= fails + 1;
      mem.atom_w(target, 88);
      step <= 4;
   endrule

   // Confirm atom_w wrote successfully
   rule s3(step == 4);
      let v = mem.atom_r(target);
      testAssert(v == 88, "atom_r == 88 after atom_w", cyc);
      if (v != 88) fails <= fails + 1;
      step <= 5;
   endrule

   // Reserve lock to make the queue non-empty
   rule s4(step == 5);
      let id <- mem.lock.res1();
      wid0 <= id;
      step <= 6;
   endrule

   // With lock held, canAtom_r1 should be false
   rule s5(step == 6);
      testAssert(!mem.canAtom_r1(target), "canAtom_r1 false when locked", cyc);
      if (mem.canAtom_r1(target)) fails <= fails + 1;
      step <= 7;
   endrule

   // Also verify canAtom_w1 is false
   rule s5b(step == 7);
      testAssert(!mem.canAtom_w1(target), "canAtom_w1 false when locked", cyc);
      if (mem.canAtom_w1(target)) fails <= fails + 1;
      step <= 8;
   endrule

   // Release lock and verify canAtom restored
   rule s6(step == 8);
      mem.lock.rel1(wid0);
      step <= 9;
   endrule

   rule s7(step == 9);
      testAssert(mem.canAtom_r1(target), "canAtom_r1 restored after release", cyc);
      if (!mem.canAtom_r1(target)) fails <= fails + 1;
      testDone("Mem_QLAtomicOps", fails);
   endrule
endmodule

// ============================================================
// Test 5: AddrLockCombMem -- write and release lifecycle
// Reserve on addr, write data to addr via write, release lock.
// Verify the written value persists.
// Models the writeback + commit sequence in Stage__57.
// ============================================================
(* synthesize *)
module mkTestMem_ALWriteAndRelease();
   RegFile#(Addr, Data) rf <- mkRegFileFull();
   AddrLockCombMem#(Addr, Data, LockId#(4), 4) mem <- mkFAAddrLockCombMem(rf);

   Reg#(UInt#(4)) step <- mkReg(0);
   Reg#(LockId#(4)) lockId <- mkReg(0);
   Reg#(UInt#(32)) cyc <- mkReg(0);
   Reg#(UInt#(32)) fails <- mkConfigReg(0);

   Addr target = 15;

   rule tick; cyc <= cyc + 1; endrule

   rule s0(step == 0);
      $display("=== TEST: Mem_ALWriteAndRelease ===");
      step <= 1;
   endrule

   // Reserve lock on target addr (simulating Stage__0 reserving rd)
   rule s1(step == 1);
      let id <- mem.lock.res1(target);
      lockId <= id;
      step <= 2;
   endrule

   // While locked, canAtom_r1 should be false
   rule s2(step == 2);
      testAssert(!mem.canAtom_r1(target), "canAtom false while locked", cyc);
      if (mem.canAtom_r1(target)) fails <= fails + 1;
      step <= 3;
   endrule

   // Write data to the address (simulating Stage__57 writeback)
   rule s3(step == 3);
      mem.write(target, 999);
      step <= 4;
   endrule

   // Release the lock (simulating Stage__57 commit)
   rule s4(step == 4);
      mem.lock.rel1(lockId, target);
      step <= 5;
   endrule

   // Wait a cycle for lock auto-free
   rule s5(step == 5);
      step <= 6;
   endrule

   // Verify addr is readable after release
   rule s6(step == 6);
      testAssert(mem.canAtom_r1(target), "canAtom true after release", cyc);
      if (!mem.canAtom_r1(target)) fails <= fails + 1;
      step <= 7;
   endrule

   // Verify data via atom_r
   rule s6b(step == 7);
      let v = mem.atom_r(target);
      testAssert(v == 999, "atom_r == 999 after writeback+commit", cyc);
      if (v != 999) fails <= fails + 1;
      step <= 8;
   endrule

   // Also verify via read
   rule s7(step == 8);
      let v = mem.read(target);
      testAssert(v == 999, "read == 999 persisted in rf", cyc);
      if (v != 999) fails <= fails + 1;
      testDone("Mem_ALWriteAndRelease", fails);
   endrule
endmodule

endpackage
