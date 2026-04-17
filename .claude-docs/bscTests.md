# BSV Runtime Module Tests

40 hardware simulation tests for the modules in `bscRuntime/`. Located in `bscTests/`.

## Running

```bash
cd bscTests
export BLUESPECDIR=/opt/homebrew/opt/bsc/libexec
make test       # Run all 40 tests
make clean      # Remove build artifacts
make run_mkTestQL_BasicLifecycle   # Run a single test
```

Requires: `bsc`, `iverilog`, `vvp`, `timeout` or `gtimeout`. Build artifacts are cleaned up automatically after `make test`.

## Test Design

Tests model realistic pipeline behavior based on analysis of generated BSV from the RISC-V pipeline tests. Each test uses a step-counter FSM where each step corresponds to a pipeline stage's operation on the module.

### Harness (TestHelper.bsv)

Two standalone functions (no module state, no scheduling conflicts):
- `testAssert(Bool cond, String msg, UInt#(32) cycle)` -- prints `ok:` or `FAIL:`
- `testDone(String name, UInt#(32) fails)` -- prints `PASS` or `FAIL`, calls `$finish`

Each test module tracks its own `fails` counter and `cyc` register.

### BSV Scheduling Rules

These constraints shaped the test structure:
- **One write per register per rule** -- multiple `if (cond) fails <= fails + 1` in the same rule causes a parallel write conflict. Each rule has at most one conditional fail increment.
- **Method isolation** -- methods that read and write the same internal wires (e.g., `canAtom_r1` reads bypass wires, `write` sets them) cannot be called in the same rule. These are split into separate steps.
- **One `spec.free()` per rule** -- freeing multiple entries conflicts on `inUse[]`.
- **No non-ASCII in string literals** -- BSC 2025.07 crashes with "Internal Bluespec Compiler Error: quoting a character value" on em dashes or other non-ASCII. Use `--` not `--`.

## Test Files and Cases

### TestQueueLock.bsv (5 tests for mkQueueLock)

| Test | Scenario |
|------|----------|
| `mkTestQL_BasicLifecycle` | Reserve 1 ID, verify owns, release, verify empty |
| `mkTestQL_PipelineStall` | 3-deep pipeline: reserve 3 IDs, only head owns, release in order |
| `mkTestQL_FullQueue` | Fill depth-4 queue, verify `canRes1` backpressure, drain one by one |
| `mkTestQL_RapidReserveRelease` | Alternate reserve/release each cycle for 6 iterations (steady-state throughput) |
| `mkTestQL_WrongRelease` | Release non-owner is a no-op -- queue state preserved |

### TestCountingLock.bsv (5 tests for mkCountingLock)

| Test | Scenario |
|------|----------|
| `mkTestCL_BasicLifecycle` | Same basic reserve/owns/release lifecycle |
| `mkTestCL_SameCycleResRel` | EHR enables reserve and release in the same cycle via separate rules |
| `mkTestCL_ManyReservations` | 6 outstanding reservations on depth-8 lock, drain all |
| `mkTestCL_OwnerAdvancement` | Release head, verify next becomes owner, three-stage progression |
| `mkTestCL_Wraparound` | 10 reserve/release iterations wrapping the 3-bit counter |

### TestCheckpointLock.bsv (5 tests for mkCheckpointQueueLock)

| Test | Scenario |
|------|----------|
| `mkTestCKL_BasicCheckpointRollback` | Checkpoint after 2 reserves, speculative 3rd, rollback undoes it |
| `mkTestCKL_CheckpointNoRollback` | Checkpoint doesn't interfere with normal release flow |
| `mkTestCKL_MultipleCheckpoints` | Nested checkpoints (c1, c2), rollback to c1 undoes everything after c1 |
| `mkTestCKL_RollbackToEmpty` | Rollback speculative work, release original to reach empty |
| `mkTestCKL_RollbackAndContinue` | Rollback, then resume with new correct-path reservations |

### TestAddrLock.bsv (5 tests for mkFAAddrLock, mkDMAddrLock)

| Test | Scenario |
|------|----------|
| `mkTestAL_IndependentAddrs` | 3 addresses are independent, unrelated address reports empty |
| `mkTestAL_SameAddrConflict` | Two reservations on same address (WAW hazard), ownership advances on release |
| `mkTestAL_PoolExhaustion` | 4-slot FA lock full, `canRes1` false for new addr, release frees slot |
| `mkTestAL_AutoFree` | `freelock` rule auto-clears entry after release, freeing slot for reuse |
| `mkTestAL_DMBasic` | Direct-mapped lock: per-address independence, always has capacity |

### TestSpeculation.bsv (5 tests for mkSpecTable)

| Test | Scenario |
|------|----------|
| `mkTestSpec_AllocAndValidate` | Alloc 3 entries, validate first, check statuses, free all (correct prediction path) |
| `mkTestSpec_InvalidateCascade` | Invalidate s1 cascades to kill s2 (newer), s0 (older) unaffected |
| `mkTestSpec_FullTable` | Fill 4-entry table, verify alloc blocks, free one to resume |
| `mkTestSpec_ValidateThenInvalidate` | Invalidate overrides prior validate on same entry |
| `mkTestSpec_RapidAllocFree` | Alloc-validate-free loop for 6 rounds without running out of space |

### TestBypassLock.bsv (5 tests for mkBypassLockCombMem)

| Test | Scenario |
|------|----------|
| `mkTestBP_ReserveWriteReadRelease` | Full lifecycle with bypass forwarding, then RF commit |
| `mkTestBP_ReadBeforeWrite` | `canAtom_r1` false before write, true after (stall behavior) |
| `mkTestBP_TwoWritesSameAddr` | WAW: newest write (200) wins over older (100) in bypass |
| `mkTestBP_WriteReadDifferentAddrs` | Independent addresses return correct bypass data |
| `mkTestBP_CommitOrder` | Three writes released in order, each commit persists in RF |

### TestNewMemories.bsv (5 tests for mkQueueLockCombMem, mkFAAddrLockCombMem)

| Test | Scenario |
|------|----------|
| `mkTestMem_QLBasicReadWrite` | Write/read, lock blocks `canAtom`, release restores it |
| `mkTestMem_ALReadAfterWrite` | Lock on addr1 blocks only addr1, addr2 remains readable (RAW stall) |
| `mkTestMem_ALMultipleReaders` | Three locks, unrelated addr still readable, all restored after release |
| `mkTestMem_QLAtomicOps` | `atom_r`/`atom_w` work when unlocked, both blocked when locked |
| `mkTestMem_ALWriteAndRelease` | Reserve, write, release lifecycle modeling writeback stage |

### TestBHT.bsv (5 tests for mkBHT)

| Test | Scenario |
|------|----------|
| `mkTestBHT_StateMachine` | Full walk through all 4 counter states and back |
| `mkTestBHT_SaturationStrong` | 5 consecutive taken/not-taken verify saturation (no overflow) |
| `mkTestBHT_DifferentPCs` | 3 PCs trained independently, predictions don't interfere |
| `mkTestBHT_SameCycleReqUpd` | `req` and `upd` in same cycle -- `req` reads pre-update value (CF schedule) |
| `mkTestBHT_AliasingBehavior` | Two PCs aliasing same entry share counter, non-aliased is independent |
