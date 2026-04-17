# PDL BSV Runtime Module Reference

This document covers every hardware module in `bscRuntime/`. Modules are grouped by category.

---

## 1. Epoch History Registers (EHR)

**File:** `memories/Ehr.bsv` (MIT License, from MIT)

An EHR (Epoch History Register) is a register with multiple read/write "ports" that have a defined priority order within a single clock cycle.

```
type Ehr#(n, t) = Vector#(n, Reg#(t))
```

**Semantics:** Port `i` sees writes from all ports `j < i` (combinationally) and does not see writes from ports `j >= i`. The actual register updates with the last write at end of cycle.

**Scheduling constraints:**
- `read[i] < write[j]` for `j >= i` (read before later write)
- `read[i] > write[j]` for `j < i` (read after earlier write)
- `write[i] < write[j]` for `j > i` (lower port writes first)
- `write[i] conflicts with write[i]` (only one write per port per cycle)
- All reads are conflict-free with each other

**Example trace (2-port EHR, init=0):**

| Cycle | Port 0 read | Port 0 write | Port 1 read | Port 1 write | Register at end |
|-------|-------------|-------------|-------------|-------------|-----------------|
| 1     | 0           | 5           | 5 (sees p0) | 10          | 10 (last write) |
| 2     | 10          | -           | 10          | 7           | 7               |
| 3     | 7           | 3           | 3 (sees p0) | -           | 3               |

**Used by:** Locks, Speculation, SpecialQueues, Memories — anywhere within-cycle ordering between pipeline stages matters.

---

## 2. Lock Modules

**File:** `memories/Locks.bsv`

Locks enforce thread ordering for hazard prevention. All locks share a common lifecycle: **reserve -> block -> access -> release**. The lock ID type is `LockId#(d) = UInt#(TLog#(d))`.

### 2a. QueueLock (mkQueueLock)

The simplest lock. A FIFO queue of reservation IDs. No bypassing.

| Method | Signature | Behavior |
|--------|-----------|----------|
| `res1()` | `ActionValue#(id)` | Enqueues a new ID into the FIFO, returns it |
| `owns1(id)` | `Bool` | True if `id` is at the head of the queue |
| `rel1(id)` | `Action` | Dequeues the head if `id` matches |
| `isEmpty()` | `Bool` | True if queue is empty |
| `canRes1()` | `Bool` | True if queue is not full |

**Stall behavior:** A thread blocks (`owns1` returns false) until all prior reservations dequeue. No data forwarding.

**Example trace (depth=4):**

| Cycle | Action | Queue State | owns1(0) | owns1(1) |
|-------|--------|-------------|----------|----------|
| 1 | res1() -> id=0 | [0] | true | false |
| 2 | res1() -> id=1 | [0, 1] | true | false |
| 3 | rel1(0) | [1] | false | true |
| 4 | rel1(1) | [] (empty) | false | false |

### 2b. CountingLock (mkCountingLock)

Optimized lock using a monotonic counter pair instead of a FIFO. Uses EHR for same-cycle reserve+release.

| Method | Behavior |
|--------|----------|
| `res1()` | Increments `nextId[0]`, returns old value |
| `owns1(id)` | True if `id == owner` |
| `rel1(id)` | Advances `owner` to `owner + 1` |
| `isEmpty()` | True if `owner == nextId` (no outstanding reservations) |
| `canRes1()` | True unless `nextId == owner` and not empty (queue full — wrapped around) |

**Key difference from QueueLock:** No FIFO storage — just two counters. More area-efficient but same lack of bypassing.

### 2c. CheckpointQueueLock (mkCheckpointQueueLock)

CountingLock extended with checkpoint/rollback for speculation support.

| Method | Behavior |
|--------|----------|
| `checkpoint()` | Returns `nextId[1]` (captures reservation state after this cycle's reserves) |
| `rollback(i, doRoll, doRel)` | If `doRoll`: resets `nextId[0]` to `i`, sets `empty` if `i == owner` |

**Example trace with speculation:**

| Cycle | Action | nextId | owner | empty |
|-------|--------|--------|-------|-------|
| 1 | res1() -> 0 | 1 | 0 | false |
| 2 | checkpoint() -> 1; res1() -> 1 | 2 | 0 | false |
| 3 | rollback(1, true, false) | 1 | 0 | false |
| 4 | *(speculative res id=1 is gone)* | 1 | 0 | false |

### 2d. Fully-Associative Address Lock (mkFAAddrLock)

Per-address locking using a pool of CountingLocks. Each lock slot dynamically binds to an address.

| Method | Behavior |
|--------|----------|
| `res1(addr)` | Finds existing lock for `addr`, or allocates a free slot. Returns lock ID. |
| `owns1(id, addr)` | True if the lock for `addr` has `id` at head, or no lock is bound to `addr` and a slot is free |
| `rel1(id, addr)` | Releases the lock for `addr` |
| `isEmpty(addr)` | True if lock for `addr` is empty or no lock is bound |
| `canRes1(addr)` | True if lock exists for `addr` or a free slot is available |

**Auto-freeing:** A rule fires each cycle to invalidate lock slots whose CountingLock is empty and no reservation happened this cycle (via RWire guard).

**Stall scenario:** If all `numlocks` slots are in use and a new address is requested, `canRes1` returns false and the pipeline stalls.

### 2e. Direct-Mapped Address Lock (mkDMAddrLock)

One CountingLock per address (using address as direct index). Simpler but requires `2^szAddr` locks.

| Method | Behavior |
|--------|----------|
| `res1(addr)` | Reserves lock at index `addr` |
| `canRes1(addr)` | Always true (no capacity limit) |

---

## 3. Speculation Table

**File:** `memories/Speculation.bsv`

A circular buffer tracking speculative thread status. ID type: `SpecId#(n) = UInt#(TLog#(n))`.

| Method | Signature | Behavior |
|--------|-----------|----------|
| `alloc()` | `ActionValue#(sid)` | Allocates next entry, returns its ID. Blocks if full. |
| `check(s, i)` | `Maybe#(Bool)` | Returns `Invalid` if not in use, `Valid(True)` if correctly speculated, `Valid(False)` if mispredicted. Port `i` controls bypass timing. |
| `validate(s, i)` | `Action` | Marks entry `s` as correctly speculated (port `i`) |
| `invalidate(s, i)` | `Action` | Marks entry `s` AND all newer entries as mispredicted |
| `free(s)` | `Action` | Releases entry `s` |

**Bypass ports:** The `Integer i` parameter indexes into the EHR for each entry's status. Lower `i` = earlier in the cycle. The schedule is:
- Stages with `spec call` use the lowest index (allocate first)
- Stages with `update` use middle indices
- Stages with `verify` use the highest indices

This ensures that a verify in a later stage combinationally propagates to `spec_check`/`spec_barrier` in earlier stages within the same cycle.

**`isNewer` function:** Handles circular buffer wraparound — entry `a` is newer than `b` if `a > b` without the head being between them, or if wrap-around conditions hold.

**Example trace (4 entries):**

| Cycle | Action | head | Entry states |
|-------|--------|------|-------------|
| 1 | alloc() -> 0 | 1 | [0: Invalid] |
| 2 | alloc() -> 1 | 2 | [0: Invalid, 1: Invalid] |
| 3 | validate(0, 1) | 2 | [0: Valid(true), 1: Invalid] |
| 3 | check(1, 0) | 2 | returns Invalid (unknown) |
| 4 | invalidate(1, 1) | 2 | [0: Valid(true), 1: Valid(false)] |
| 4 | check(1, 0) | 2 | returns Valid(false) — mispredicted |
| 5 | free(0); free(1) | 2 | [0: unused, 1: unused] |

---

## 4. Special Queues

**File:** `memories/SpecialQueues.bsv`

### 4a. OutputQ (mkOutputFIFOF)

A tagged single-element FIFO that uses a monotonic tag counter to enforce read/write ordering across out-of-order pipeline stages.

| Method | Behavior |
|--------|----------|
| `canRead(tag)` | True if `nextTag[0] == tag` and data is valid |
| `first()` | Returns the stored data |
| `deq()` | Increments tag counter, invalidates data |
| `canWrite(tag)` | True if `nextTag[1] == tag` (EHR port 1 — ordered after reads) |
| `enq(d)` | Stores data |

**Purpose:** Coordinates the writeback (WB) stage in out-of-order pipelines. The dispatch stage enqueues tags indicating which branch each instruction took; WB reads from the correct output queue by matching tags.

### 4b. Non-Blocking FIFO (mkNBFIFOF)

A wrapper around a standard FIFO that allows multiple `enq` attempts per cycle — only the last one takes effect (via RWire).

**Purpose:** Used for pipeline stages where multiple rules might try to enqueue into the same FIFO in the same cycle (e.g., the recursive call and verify both trying to feed data back to the pipeline start).

---

## 5. Memory Modules

**File:** `memories/Memories.bsv`

### Memory Primitives

| Module | Latency | Description |
|--------|---------|-------------|
| `mkRegister(init)` | Combinational | Single register wrapped as `RegFile` interface |
| `mkRegFile(init, file)` | Combinational | Standard register file (with optional file init) |
| `mkBramPort(init, file)` | 1-cycle (sync) | Single-port BRAM with byte-enable, max 1M words |
| `mkBramPort2(init, file)` | 1-cycle (sync) | Dual-port BRAM with byte-enable |

### AsyncMem (mkAsyncMem, mkAsyncMem2)

Wraps a BRAM port with an in-flight request tracker. Supports out-of-order response consumption.

| Method | Behavior |
|--------|----------|
| `req1(addr, data, wmask)` | Sends request to BRAM, allocates slot in circular buffer, returns ID |
| `checkRespId1(id)` | True if response for `id` has arrived |
| `peekResp1(id)` | Returns the response data (must check first) |
| `resp1(id)` | Frees the slot |
| `bram_client` | Server-side connection to actual BRAM |

**Stall:** Blocks requests when all `inflight` slots are occupied.

### Locked Memory Compositions

These compose a memory primitive with a lock to create the complete PDL memory abstraction:

| Module | Memory | Lock | Notes |
|--------|--------|------|-------|
| `mkQueueLockCombMem` | RegFile | QueueLock | Simplest: stall-only, no bypass |
| `mkCheckpointQueueLockCombMem` | RegFile | CheckpointQueueLock | + speculation support |
| `mkQueueLockAsyncMem` | AsyncMem | QueueLock | For BRAM memories |
| `mkQueueLockAsyncMem2` | AsyncMem2 | QueueLock | Dual-port BRAM |
| `mkFAAddrLockCombMem` | RegFile | FA AddrLock | Per-address locking |
| `mkDMAddrLockCombMem` | RegFile | DM AddrLock | Direct-mapped per-address |
| `mkFAAddrLockAsyncMem(2)` | AsyncMem(2) | FA AddrLock | Per-address + BRAM |
| `mkDMAddrLockAsyncMem(2)` | AsyncMem(2) | DM AddrLock | Direct-mapped + BRAM |

All combinational (`Comb`) variants expose:
- `read(addr)` / `write(addr, data)` — direct memory access
- `atom_r(addr)` / `atom_w(addr, data)` — atomic access (lock must be held)
- `canAtom_r1` / `canAtom_r2` / `canAtom_w1` — ready signals (lock is empty for this addr)

### BypassLockCombMem (mkBypassLockCombMem)

Implements the **Bypass Queue** lock from the paper. Supports write-to-read data forwarding.

**Internal state:**
- `resVec[n]` — `Maybe#(addr)`: reserved address per slot
- `dataVec[n]` — `Maybe#(elem)`: written data per slot (Invalid until written)
- `bypassWire[n]` — `RWire#(elem)`: same-cycle bypass combinational path
- `head` — next slot to allocate
- `owner` — next slot to commit

| Method | Behavior |
|--------|----------|
| `res_w1(addr)` | Allocates slot at `head`, stores address, returns slot ID |
| `write(id, data)` | Stores data in slot, fires bypass wire for same-cycle forwarding |
| `rel_w1(id)` | Commits data to actual RegFile (`rf.upd`), frees slot, advances owner |
| `atom_r(addr)` | Returns bypassed data if a matching write exists, otherwise reads RegFile |
| `canAtom_r1(addr)` | True if no pending write for `addr`, OR the write's data is available |
| `owns_w1(id)` | Always true (write is non-blocking once reserved) |

**Bypass logic:** `readBypassData(ent)` checks the `bypassWire` first (same-cycle write), then `dataVec` (previous cycle write). `getMatchingEntry(addr)` finds the newest slot with this address.

**Example trace (3-slot bypass queue, register x1):**

| Cycle | Stage | Action | resVec | dataVec | rf[x1] |
|-------|-------|--------|--------|---------|--------|
| 1 | Decode | res_w1(x1) -> 0 | [0: x1] | [0: -] | old |
| 2 | Exec | write(0, 42) | [0: x1] | [0: 42] | old |
| 2 | Decode (next insn) | atom_r(x1) -> **42** (bypass!) | | | old |
| 3 | WB | rel_w1(0) | [0: -] | [0: -] | **42** |

### LSQ (mkLSQ)

A **Load-Store Queue** for out-of-order memory access. The most complex memory module.

**Internal structures:**
- Store Queue: `stQAddr`, `stQData`, `stQValid` — tracks pending stores
- Load Queue: `ldQAddr`, `ldQData`, `ldQStr` (store dependency), `ldQValid`, `ldQIssued`
- `stIssueQ` — FIFO of committed stores waiting to go to main memory

**Key operations:**

| Method | Behavior |
|--------|----------|
| `res_r1(addr)` | Allocates load entry. Searches store queue for matching address — if found with full data, forwards immediately; otherwise records dependency |
| `res_w1(addr)` | Allocates store entry |
| `write(name, data, wmask)` | Writes data into store entry, forwards to any dependent loads |
| `read(name)` | Returns load data (from forwarded or fetched result) |
| `owns_r1(name)` | True if load data is available |
| `rel_r1(name)` | Frees load entry |
| `rel_w1(name)` | Commits store — pushes to `stIssueQ` for memory write |

**Schedule rules:**
- `issueSt` — dequeues from `stIssueQ`, sends write to main memory
- `issueLd` — finds oldest un-issued load with no store dependency, sends read to main memory
- `moveLdData` — captures memory response into load queue

**Store-to-load forwarding:** When `write()` is called, it scans all loads and forwards data to any whose `ldQStr` matches this store, clearing the dependency.

---

## 6. Verilog Register File Implementations

**Files:** `verilog/*.v`, BSV wrappers in `verilog/VerilogLibs.bsv`

These implement the PDL hazard lock interface directly in Verilog for maximum performance.

### 6a. RenameRF (mkRenameRF)

Classic **register renaming** used in out-of-order processors.

**State:**
- `names[0:aregs-1]` — architectural-to-physical name mapping
- `phys[0:pregs-1]` — physical register file
- `busy[pregs]` — bit vector: 1 = data not yet written
- `free[pregs]` — bit vector: 1 = physical register available for allocation
- `old[pregs]` — previous name mapping (for freeing on commit)

| Method | Behavior |
|--------|----------|
| `res_w1(arch_addr)` | Allocates free physical register, updates name map, saves old mapping, clears busy bit, returns new name |
| `res_r1/r2(arch_addr)` | Looks up physical name from `names[]` |
| `owns_r1/r2(name)` | Returns `!busy[name]` — true when data is written |
| `write(name, data)` | Writes `phys[name]`, sets `busy[name] = 0` |
| `read(name)` | Returns `phys[name]` |
| `rel_w1(name)` | Frees `old[name]` (returns old physical register to free list) |

### 6b. ForwardRenameRF (mkForwardRenameRF)

Same as RenameRF but with **combinational write-to-read forwarding**: if `write` and `read` happen in the same cycle for the same name, the read sees the written data immediately. Also forwards the busy bit — `owns` returns true in the same cycle as `write`.

### 6c. CheckpointRenameRF (mkCheckpointRF)

RenameRF extended with **checkpoint/rollback** for speculation.

**Additional state:**
- `name_copies[0:num_replicas-1]` — snapshots of the name mapping
- `free_copies[0:num_replicas-1]` — snapshots of the free list
- `busy_copies[0:num_replicas-1]` — snapshots of the busy bits
- `nextCopy` / `copyFree` — circular buffer for checkpoint slots

| Method | Behavior |
|--------|----------|
| `checkpoint()` | Saves current `names`, `free`, `busy` into next replica slot. Returns checkpoint ID. |
| `rollback(cid, doRoll, doRel)` | If `doRoll`: restores `names`, `free`, `busy` from checkpoint. If `doRel`: frees the checkpoint slot. |

### 6d. BypassRF (mkBypassRF)

Implements the **Bypass Queue** lock in Verilog. Tracks pending reads and writes with 2 read slots and a circular write queue.

**State:**
- `rf[0:aregs-1]` — architectural register file
- `rf1, rf2` — pending read buffers (data, write dependency, valid, inUse)
- `wq_addr, wq_data, wq_valid` — circular write queue
- `head/owner` — write queue pointers

| Method | Behavior |
|--------|----------|
| `res_w1(addr)` | Allocates write queue slot, returns ID |
| `res_r1/r2(addr)` | Finds newest write to same address. If data valid, copies to read buffer. If not, records dependency. |
| `owns_r1/r2()` | True if read buffer has valid data |
| `write(id, data)` | Stores data in write queue. Forwards to dependent read buffers combinationally. |
| `read1/read2(id)` | Returns data from read buffer |
| `rel_w1(id)` | Commits write to `rf`, frees write queue slot |
| `rel_r1/r2()` | Frees read buffer |

### 6e. CheckpointBypassRF (mkCheckpointBypassRF)

BypassRF + checkpoint/rollback. On rollback, resets write queue `head` to checkpoint position and invalidates newer entries.

### 6f. Summary Table

| Module | Bypass? | OoO? | Checkpoint? | Use case |
|--------|---------|------|-------------|----------|
| QueueLock | No | No | No | Simple stall-only pipelines |
| CountingLock | No | No | No | Same, more area-efficient |
| CheckpointQueueLock | No | No | Yes | Simple + speculation |
| BypassRF | Yes | No | No | In-order with forwarding |
| CheckpointBypassRF | Yes | No | Yes | In-order + speculation |
| RenameRF | Yes | Yes | No | OoO (Tomasulo-style) |
| ForwardRenameRF | Yes (comb) | Yes | No | OoO with same-cycle forward |
| CheckpointRenameRF | Yes | Yes | Yes | OoO + speculation |
| FA/DM AddrLock | No | No | No | Per-address stalling |
| LSQ | Yes (store-to-load) | Yes | No | OoO memory access |

---

## 7. Branch History Table

**File:** `verilog/BHT.v`, BSV wrapper in `verilog/VerilogLibs.bsv`

A **2-bit saturating counter** branch predictor, indexed by PC bits.

**State:** `hist[0:num_entries-1]` — 2-bit counter per entry

**States:** `SKIP_S(00)` -> `SKIP_W(01)` -> `TAKE_W(10)` -> `TAKE_S(11)`

| Method | Behavior |
|--------|----------|
| `req(pc, skip_off, take_off)` | Returns `pc + skip_off` or `pc + take_off` based on prediction |
| `upd(pc, taken)` | Updates counter: taken moves toward TAKE_S, not-taken toward SKIP_S |

**State machine:**

```
         taken          taken          taken
SKIP_S --------> SKIP_W --------> TAKE_W --------> TAKE_S
       <--------        <--------        <--------
       not taken        not taken        not taken
```

**Init:** All entries start at `TAKE_W` (weakly predict taken).

**Example trace (entry for PC=0x100):**

| Cycle | Actual | Counter | Prediction |
|-------|--------|---------|------------|
| init  | -      | TAKE_W  | taken      |
| 1     | taken  | TAKE_S  | taken      |
| 2     | not    | TAKE_W  | taken      |
| 3     | not    | SKIP_W  | not taken  |
| 4     | taken  | TAKE_W  | taken      |
