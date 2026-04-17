# PDL Type Checking Specification

PDL's type checking consists of 14 passes executed sequentially by `Main.runPasses`. Each pass enforces a distinct set of correctness properties. Together they guarantee that the generated pipelined circuit behaves identically to a sequential one-instruction-at-a-time specification.

## Execution Order

```
1.  MarkNonRecursiveModulePass
2.  LockRegionInferencePass
3.  AddCheckpointHandlesPass + AddVerifyValuesPass
4.  CanonicalizePass
5.  TypeInference (Z3-based)
6.  BaseTypeChecker
7.  FunctionConstraintChecker
8.  BindModuleTypes
9.  SimplifyRecvPass
10. LockRegionChecker
11. LockWellformedChecker
12. LockOperationTypeChecker
13. PortChecker
14. PredicateGenerator (annotates AST with Z3 path predicates)
15. LockConstraintChecker (uses Z3)
16. LockReleaseChecker
17. LinearExecutionChecker (uses Z3)
18. SpeculationChecker (uses Z3)
19. LockOpTranslationPass
20. TimingTypeChecker
```

---

## 1. Type Infrastructure

### 1a. Subtypes (Subtypes.scala)

**`isSubtype(t1, t2): Boolean`** -- is t1 a subtype of t2?

| t1 | t2 | Rule |
|----|----|------|
| `TSizedInt(l1, u1)` | `TSizedInt(l2, u2)` | Exact match: `l1 == l2 && u1 == u2` (no width coercion) |
| `TRecType(_, f1)` | `TRecType(_, f2)` | Structural: f1 has all fields of f2, each field is a subtype |
| `TFun(arg1, r1)` | `TFun(arg2, r2)` | Contravariant args, covariant return. Same arity required. |
| `TLockedMemType(m1, id1, l1)` | `TLockedMemType(m2, id2, l2)` | `isSubtype(m1,m2) && l1==l2 && (id1==id2 or id2 is empty)` |
| `TMemType(...)` | `TMemType(...)` | Element types equal, address sizes equal, latencies equal. Port counts: t1 >= t2 (more ports is subtype), 0 is wildcard. |
| other | other | `areEqual(t1, t2)` |

**`areEqual(t1, t2)`** -- structural equality with special case: `TSizedInt(1, unsigned) == TBool`.

**`canCast(from, to)`** -- any `TSizedInt` can cast to any other `TSizedInt`. Otherwise must be `areEqual`.

**`isSpeculativeSubtype(t1, t2)`** -- `isSubtype(t1, t2)` AND (t2.maybeSpec OR !t1.maybeSpec). If the target context may be speculative, any type fits; otherwise both must be non-speculative.

### 1b. Environments (Environments.scala)

Six environment types, each with custom merge logic for control flow joins:

| Environment | Key | Value | intersect (if/split join) | union |
|-------------|-----|-------|---------------------------|-------|
| `TypeEnv` | `Id` | `Type` | Keep matching types only | Add non-conflicting bindings; error on mismatch |
| `LockEnv` | `Id` | `LockState` | Matching states kept; `Free+Released -> Released`; error on `Reserved+Free` etc. | Keep existing, error if other changed from Free |
| `IntEnv` | `Id` | `(Int,Int)` | Keep matching | `max(reads), max(writes)` per ID |
| `BoolEnv` | `Id` | `Boolean` | Set intersection | Set union |
| `ConditionalEnv` | `Id` | `Z3AST` | `mkAnd(v1, v2)` per key | Overwrite |
| `ConditionalLockEnv` | `LockArg` | `Z3AST` | `mkAnd(v1, v2)` per key | Overwrite |

**LockEnv state machine:**
```
Free -> Reserved -> Acquired -> Released
Free -> Acquired  (acquire = reserve + block)
```
Any other transition throws `IllegalLockModification`.

---

## 2. TypeInference (TypeInferenceWrapper.scala)

Z3-based Hindley-Milner-style type inference with bitwidth constraints. This is the largest checker (~900 lines).

### What it infers
- Bitwidths for integer types (e.g., `bit<?>` becomes `bit<32>`)
- Signedness
- Generic type parameter instantiation

### Key rules
- **Unification**: Types must unify; conflicting types throw `UnificationError`
- **Bitwidth constraints**: Generated as Z3 `ArithExpr` constraints (e.g., `len(a) + len(b) = len(concat)`)
- **Autocast mode**: When enabled, inserts `ECast` nodes to allow implicit narrowing/widening of integers
- **Generic functions**: Handles parametric polymorphism by substituting type variables
- **Z3 solving**: After collecting all constraints, calls Z3 to solve bitwidth equations. Unsatisfiable = type error.

### Corner cases
- `TBitWidthMax(a, b)`: When one arg is a variable and other is a literal, resolves to the literal
- `TBitWidthAdd/Sub`: Generates Z3 addition/subtraction constraints
- Recursive module calls: The call's argument types must match the module's input types after substitution
- `matchOrError` pattern: Used extensively -- extracts a type from a match or throws a descriptive error

---

## 3. BaseTypeChecker (BaseTypeChecker.scala)

Standard type checking for expressions, commands, and circuit declarations.

### Expression rules

| Expression | Rule |
|-----------|------|
| `EInt(v, base, bits)` | Type is `TSizedInt(bits, signed)` |
| `EBool(v)` | `TBool` |
| `EUop(BoolUOp, e)` | e must be `TBool`, result `TBool` |
| `EUop(NumUOp, e)` | e must be `TSizedInt`, result same type |
| `EBinop(BitOp("++"), e1, e2)` | Both `TSizedInt` with same signedness; result width = w1 + w2 |
| `EBinop(NumOp("*"), e1, e2)` | Both `TSizedInt` with same signedness; result width = w1 + w2 |
| `EBinop(BitOp("<<"/>>"), e1, e2)` | Both `TSizedInt`; result has e1's width |
| `EBinop(EqOp/CmpOp, e1, e2)` | Same types; result `TBool` |
| `EBinop(BoolOp, e1, e2)` | Both `TBool`; result `TBool` |
| `EBinop(NumOp, e1, e2)` | Both same `TSizedInt`; result same type |
| `EMemAccess(mem, idx, wm)` | idx must be `UInt` matching mem's address width. Write mask must be unsigned. Result is mem's element type. |
| `EBitExtract(num, start, end)` | num must be `TSizedInt` wide enough. Result width = start - end + 1. |
| `ETernary(c, t, f)` | c must be `TBool`; t and f must have equal types |
| `EApp(func, args)` | Looks up `TFun` in env. Checks arg count and subtypes. Returns function return type. Handles generic/templated bitwidths. |
| `ECall(mod, name, args)` | mod must be `TModType` or `TObject`. Checks arg count and subtypes. |
| `EVar(id)` | Looks up in env; adds to env if new with defaultType |
| `ECast(to, e)` | `canCast(from, to)` must be true |

### Command rules

| Command | Rule |
|---------|------|
| `CAssign(lhs, rhs)` | `isSubtype(rhs_type, lhs_type)` |
| `CRecv(lhs, rhs)` | Same as CAssign (for asynchronous receives) |
| `CIf(cond, cons, alt)` | cond must be `TBool`. Environments are intersected at join. |
| `CSplit(cases, default)` | Each case condition must be `TBool`. Environments intersected across all branches. |
| `CSpecCall(h, mod, args)` | mod must be `TModType`. Args checked against module inputs. Adds handle to env. |
| `CVerify(h, args, preds, upd)` | Handle must be `TRequestHandle(_, Speculation)`. Args and preds checked against module inputs. |
| `CLockOp(mem, op, lockType)` | mem must be `TLockedMemType` or `TModType`. If address-specific, index must be correct width. |
| `CCheckpoint(h, mod)` | mod must be `TLockedMemType` with checkpoint support. |
| `COutput(exp)` | Expression checked for type correctness |
| `CPrint(args)` | Each arg must be printable: `TSizedInt`, `TString`, or `TBool` |

### Module well-formedness
- No variable assigned more than once (SSA-like)
- No `return` statements (those are for functions only)
- No `CTBar`/`CSplit`/`COutput` in functions

### Circuit rules
- `CirMem`: max 2 ports, type is `TMemType(..., Async, Async)`
- `CirRegFile`: type is `TMemType(..., Combinational, Sequential)`
- `CirNew(mod, specialized, mods)`: checks module arg types via subtyping
- `CirCall(mod, inits)`: checks input types match module definition

---

## 4. FunctionConstraintChecker (FunctionConstraintChecker.scala)

Checks that combinational functions are well-formed and have correct return types.

### Rules
- Functions must have exactly one return statement on every execution path
- Return type must match declaration across all branches
- No `CTBar`, `CSplit`, or `COutput` inside functions (these are pipeline-only)
- If branches must both return or both not return; mismatched return types error
- Generates bitwidth constraints for generic parameters and solves with Z3

---

## 5. Lock Checkers

Five lock-related checkers enforce different aspects of the hazard lock discipline.

### 5a. LockWellformedChecker

**Invariant**: Each memory module uses exactly ONE lock granularity -- either `General` (whole-memory lock) or `Specific` (per-address lock). Mixing is forbidden.

**How it checks**: Traverses all `CLockOp` commands. If a `LockArg` has an `evar` (address expression), it's `Specific`; otherwise `General`. If the same memory ID appears with both granularities, throws `MalformedLockTypes`.

**Output**: Map from module -> (memory ID -> granularity). Used by subsequent lock checkers.

### 5b. LockOperationTypeChecker

**Invariant**: Memory reads require read locks, memory writes require write locks.

**Rules**:
- General locks cannot have a type annotation (they cover all operations)
- Per-address locks must have consistent type (Read or Write) per lock argument
- `CRecv(EMemAccess(mem, ...), _)` is a write: lock must NOT be `LockRead`
- `CRecv(_, EMemAccess(mem, ...))` is a read: lock must NOT be `LockWrite`
- Annotates `EMemAccess.memOpType` and `EMemAccess.granularity` for later passes

### 5c. LockRegionChecker

**Invariant**: Lock reservations only happen inside valid lock regions (`CLockStart`/`CLockEnd`).

**Lock state machine** (using `LockEnv`):
```
CLockStart(mod) -> Acquired
CLockEnd(mod)   -> Released
```

**Rules**:
- `CLockOp(mem, Reserved)` requires `env(mem.id) == Acquired` (inside lock region)
- `CCheckpoint(_, lock)` requires lock is `Acquired`
- At end of module: all locks must be `Free` or `Released` (no pending Acquired/Reserved)
- **Cross-branch rule**: A lock cannot be newly acquired in BOTH branches of an `if`/`split`. This prevents lock region ambiguity.
- Unlocked memory accesses and atomic operations must be inside lock regions

**Corner case**: The "cross-branch" check computes `envfree -- ltfree` for each branch to find newly-acquired locks, then checks the intersection is empty.

### 5d. LockConstraintChecker (Z3-based)

The most complex lock checker. Uses Z3 to verify lock state transitions under conditional paths.

**Invariant**: Under all possible execution paths, locks are used correctly.

**Rules**:
1. **State transitions**: At each `CLockOp`, verifies the lock is in the expected predecessor state:
   - `Reserve` requires `Free`
   - `Acquire` (block) requires `Reserved`
   - `Release` requires `Acquired`
   Uses Z3: checks if it's possible for the lock NOT to be in the expected state. If SAT (possible to violate), error.

2. **Final state**: At module end, all locks must be `Released` or `Free`. Z3 checks if any lock can be NOT Released/Free.

3. **Read-before-write ordering**: For per-address locks, all read lock operations (reserve/release) must happen before any write lock operations. Uses Z3 with a "lock mode" variable (READ=0, WRITE=1) and implications.

4. **Write lock usage**: Every write lock that is reserved must eventually be used for a write operation. Tracks `writeReserveMap` and `writeDoMap` with Z3 conditions, then checks `XOR(reserved_condition, write_condition)` is UNSAT (they must coincide).

5. **Write disjointness**: No two writes to the same memory can happen under overlapping conditions. Checks `mkAnd(old_write_conditions, new_write_condition)` is UNSAT.

**How conditional branches work**: At `CIf`/`CSplit`, each branch's lock states are wrapped with `mkImplies(branch_predicate, lock_state)`, then merged via `intersect` (which mkAnd's them). Z3 can then reason about which states are reachable.

### 5e. LockReleaseChecker

**Invariant**: Per-address lock releases happen in thread order -- no two conditional branches release the same lock.

**Rule**: Traverse the AST collecting released lock IDs per branch. If any two branches of an `if`/`split` release the same lock ID, throw `IllegalOOOLockRelease`. This ensures in-order commit.

---

## 6. SpeculationChecker (SpeculationChecker.scala)

Checks correctness of speculative execution using a typestate system and Z3.

### Typestate System

Three states: `Unknown`, `Speculative`, `NonSpeculative`

```
                    spec_check (non-blocking)
    Unknown  -------------------------------->  Speculative
       |                                             |
       |         spec_barrier (blocking)             |
       +-------------------------------------------->  NonSpeculative
                                                     |
    After stage separator (---):                     |
    Speculative  ---->  Unknown                      |
    NonSpeculative --->  NonSpeculative (stays)      |
```

### Rules per command

| Command | Required State | Transition |
|---------|---------------|------------|
| `CCheckSpec(blocking=false)` | `Unknown` | -> `Speculative` |
| `CCheckSpec(blocking=true)` | `Unknown` | -> `NonSpeculative` |
| `CSpecCall(...)` | NOT `Unknown` | no change |
| `CVerify(...)` | `NonSpeculative` | no change |
| `CUpdate(...)` | NOT `Unknown` | no change |
| `CInvalidate(...)` | any | no change |
| `COutput(...)` | `NonSpeculative` | no change |
| `CLockOp(_, Released, Write)` | `NonSpeculative` | no change |
| `CLockOp(_, _, _)` | NOT `Unknown` (if has checkpoint, relaxed) | no change |
| `CRecv(EMemAccess(unlocked), _)` | `NonSpeculative` | no change |
| `CRecv(EMemAccess(locked), _)` | NOT `Unknown` | no change |
| `CTBar(c1, c2)` | After c1: if NonSpec, stays; else resets to `Unknown` | -- |

**Cross-branch rule**: All branches of `if`/`split` must end in the same speculation state. Mismatched states throw `MismatchedSpeculationState`.

**Checkpoint relaxation**: Lock operations on memories that have checkpoints can be done speculatively (in `Speculative` state), because the checkpoint enables rollback on misprediction.

### Speculation handle resolution (Z3-based)

Each speculation handle has three states: `INIT`, `STARTED`, `RESOLVED`.

- `CSpecCall(h)`: checks h is `INIT`, transitions to `STARTED`
- `CVerify(h)`: checks h is `STARTED`, transitions to `RESOLVED`
- `CUpdate(nh, h)`: checks h is `STARTED` and nh is `INIT`; h -> `RESOLVED`, nh -> `STARTED`
- At module end: all handles must be `RESOLVED` or `INIT` (never `STARTED`)

Z3 is used to check these conditions under conditional paths, same pattern as LockConstraintChecker.

### Checkpoint checking
- `CCheckpoint(h, mem)`: records that this memory has a checkpoint
- `CVerify/CUpdate/CInvalidate` with checkpoint handles: verifies checkpoint handles exist for all relevant memories

---

## 7. LinearExecutionChecker (LinearExecutionChecker.scala)

**Invariant**: On every execution path, a pipeline module makes exactly one recursive call OR produces one output. No path may do both; no path may do neither.

### How it works
Maintains a stack of Z3 predicates representing conditions under which a call/output has been seen. For each `COutput`, `CVerify` (which redirects), or recursive `ECall`:

1. Check if the current path predicate can be true simultaneously with any existing predicate in the stack. If SAT -> `MultipleCall` error (two calls on overlapping paths).
2. If UNSAT (no overlap), push the predicate.

At module end, checks that the disjunction of all collected predicates is a tautology (covers all paths). If not -> `LonelyPaths` error (some path has no call/output).

---

## 8. TimingTypeChecker (TimingTypeChecker.scala)

**Invariant**: Variables are not used before they're available, and asynchronous operations happen in the right contexts.

### Availability tracking
Maintains `Available: Set[Id]` -- the set of variables whose values are ready in the current stage.

| Event | Effect |
|-------|--------|
| `CAssign(lhs, rhs)` | If rhs is combinational, lhs is immediately available. |
| `CRecv(lhs, rhs)` | lhs is NOT available until after `---` (asynchronous receive). |
| `CTBar(c1, c2)` | All non-available vars from c1 become available in c2 (stage boundary). |
| `CLockOp(_, Acquired, _)` | Lock handle becomes available based on lock implementation's latency. |
| Expression use | If var is not in `Available`, throws `UnavailableArgUse`. |

### Stage separator rules
- `CTBar` (---) cannot appear inside `if`/`split` branches (would create ambiguous pipeline structure)
- After a `CTBar`, all previously-unavailable receives become available

### Latency checking
- Combinational reads (e.g., register file) can be used in the same stage
- Sequential/Asynchronous reads (e.g., BRAM) must use `<-` (CRecv) and are available next stage
- Lock `block` operations have latency determined by `LockImplementation.getAccess`

---

## 9. PortChecker (PortChecker.scala)

**Invariant**: No pipeline stage exceeds the available read/write ports on any memory.

### How it works
Uses `IntEnv` which tracks `(read_count, write_count)` per memory ID. Within each stage (delimited by `CTBar`):

- Each `EMemAccess` in a read context: increments read count
- Each `EMemAccess` in a write context (`CRecv` LHS): increments write count
- At stage boundaries: resets counts

After each stage, checks:
- `read_count <= mem.readPorts` (or 0 = unlimited)
- `write_count <= mem.writePorts` (or 0 = unlimited)

**Cross-branch behavior**: Port counts from different `if` branches are merged with `max(reads), max(writes)` -- the worst case determines the count.

When `port_warn` is true, violations are warnings rather than errors.

---

## 10. Stub Checkers

### CheckpointChecker (CheckpointChecker.scala)
Skeleton only -- all methods throw `???`. Intended to verify checkpoint placement correctness but not implemented. Checkpoint checking is currently handled within `SpeculationChecker`.

### LatencyChecker (LatencyChecker.scala)
Commented out entirely. Was intended to track latency propagation through expressions. Superseded by `TimingTypeChecker`.

---

## Summary: What Each Checker Prevents

| Checker | Prevents |
|---------|----------|
| TypeInference | Bitwidth mismatches, type errors |
| BaseTypeChecker | Wrong argument types, undefined variables, illegal expressions |
| FunctionConstraintChecker | Missing returns, unreachable code in functions |
| LockWellformedChecker | Mixing per-address and whole-memory locks |
| LockOperationTypeChecker | Reading with write lock, writing with read lock |
| LockRegionChecker | Reserving locks outside lock regions, ambiguous lock scopes |
| LockConstraintChecker | Wrong lock state transitions, unreleased locks, overlapping writes |
| LockReleaseChecker | Out-of-order lock releases across branches |
| SpeculationChecker | Speculative writes to unlocked memory, unresolved speculation, mismatched states across branches |
| LinearExecutionChecker | Dead paths (no call/output), multiple calls on same path |
| TimingTypeChecker | Using async values before they arrive, stage separators inside branches |
| PortChecker | Exceeding memory port limits in a single stage |
