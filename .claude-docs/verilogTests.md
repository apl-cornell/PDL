# Verilog RF Module Tests

25 tests for the Verilog register file implementations in `bscRuntime/verilog/`. Located in `verilogTests/`.

## Running

```bash
cd verilogTests
export BLUESPECDIR=/opt/homebrew/opt/bsc/libexec
make test
```

## Results: 25/25 pass after 1 bug fix

### Confirmed Implementation Bug

**`CheckpointRenameRF.v` line 299 -- free list leak on rollback:**
```verilog
free <= free_copies[ROLLBK_IN] | (FE << oldName) | free;
```
The `| free` ORs the current free list into the restored snapshot. If a physical name was freed during speculation (via `rel_w1`), it stays free after rollback even though the rollback undoes the allocation that triggered the free. This causes **double-allocation**: a physical register can be both in the name map AND on the free list simultaneously.

**Reproduction** (`mkTestCRR_FreeListLeakOnRollback`):
1. Alloc r1 (phys 8, old mapping phys 1)
2. `rel_w1(8)` -- frees `old[8]` = phys 1
3. Checkpoint c0
4. Speculatively alloc r1 (phys 1 -- reused from free list), `rel_w1(1)` frees `old[1]` = phys 8
5. Rollback to c0: `free <= free_copies[c0] | free`
   - `free_copies[c0]` has phys 1 NOT free (it was in use at checkpoint)
   - Current `free` has phys 1 free (step 2 freed it, step 4 freed phys 8)
   - Result: phys 1 is on the free list AND in the name map (r1 -> phys 8 restored, but phys 1 freed)
6. Second alloc gets phys 1 -- now two arch regs map to it

**Fix applied** (line 299):
```verilog
// Before (bug):
free <= free_copies[ROLLBK_IN] | (FE << oldName) | free;
// After (fixed):
free <= free_copies[ROLLBK_IN] | (FE << oldName);
```

Verified: all 247 PDL compiler pipeline tests still pass after the fix.

### Test Issues Fixed During Development

**BypassRF combinational loop:** When two rules both call `rel_w1` with different arguments, BSV muxes the `W_F` port based on `WILL_FIRE`, but `F_READY` depends on `W_F`, creating a circular dependency. Fixed by using a `relTarget` register to decouple the mux.

**CheckpointBypassRF `isNewer` edge case:** The `isNewer(a, b, h)` function returns false when `a == h`, treating it as "oldest" rather than "newest". In the rollback logic for read ports (line 366), if `rf1_owner == nextCheck`, the read port is not cleared. Fixed by ensuring tests advance `nextCheck` past `rf1_owner` before rollback, matching real pipeline behavior where `res_r1` and `checkpoint` happen in the same stage.

## Test Cases

### TestRenameRF.bsv (5/5 pass)

| Test | What it tests |
|------|---------------|
| `mkTestRR_BasicAllocWriteRead` | Alloc, write, owns timing (next-cycle), read, release, realloc freed name |
| `mkTestRR_OwnsTimingNoForward` | Same-cycle write+owns returns false (no forwarding), next cycle true |
| `mkTestRR_NameRemapping` | Two allocs for same arch reg, name map tracks latest, WAW chain |
| `mkTestRR_FreeListExhaustion` | 8 allocs exhaust free list, ALLOC_READY false, release recovers |
| `mkTestRR_MultiRegPipeline` | 3-instruction data dependency chain (r1->r2->verify) |

### TestForwardRenameRF.bsv (5/5 pass)

| Test | What it tests |
|------|---------------|
| `mkTestFRR_BasicForward` | Same-cycle write+read+owns with combinational forwarding |
| `mkTestFRR_ForwardVsNoForward` | Forwarding active during write, data persists without forwarding |
| `mkTestFRR_TwoNameForward` | Two independent names forwarded in parallel |
| `mkTestFRR_WriteForwardPriority` | Port 1 write takes priority over port 2 in forwarding mux |
| `mkTestFRR_AllocAndImmediateRead` | res_r in same cycle as alloc sees OLD name map (posedge update) |

### TestBypassRF.bsv (5/5 pass)

| Test | What it tests |
|------|---------------|
| `mkTestBRF_BasicLifecycle` | Full write-then-read with bypass forwarding |
| `mkTestBRF_ReadBeforeWrite` | Stall until write, then forwarding |
| `mkTestBRF_NoConflictReadFromRF` | Direct RF read when no write queue conflict |
| `mkTestBRF_TwoWritesSameAddr` | Two writes to same addr, newest wins |
| `mkTestBRF_WriteQueueFull` | Queue exhaustion and recovery after release |

### TestCheckpointBypassRF.bsv (5/5 pass)

| Test | What it tests |
|------|---------------|
| `mkTestCBRF_BasicCheckpointRollback` | Write queue head reset on rollback |
| `mkTestCBRF_RollbackPreservesCommitted` | Committed RF data survives rollback |
| `mkTestCBRF_CheckpointAfterAlloc` | Same-cycle alloc+checkpoint captures alloc |
| `mkTestCBRF_MultipleCheckpoints` | Nested checkpoints, rollback to earlier |
| `mkTestCBRF_ReadPortRollback` | Read port invalidation on rollback |

### TestCheckpointRenameRF.bsv (5/5 pass after bug fix)

| Test | What it tests |
|------|---------------|
| `mkTestCRR_BasicCheckpointRollback` | Name map restored on rollback |
| `mkTestCRR_RollbackPreservesData` | Physical data untouched by rollback |
| `mkTestCRR_FreeListLeakOnRollback` | Free list leak regression test (was a bug, now fixed) |
| `mkTestCRR_MultipleReplicaSlots` | 4 replica slots, rollback frees newer replicas |
| `mkTestCRR_CheckpointIncludesCurrentAlloc` | Snapshot captures same-cycle alloc |
