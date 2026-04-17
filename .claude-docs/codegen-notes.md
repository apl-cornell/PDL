# PDL Code Generation Notes

How the PDL compiler generates Bluespec System Verilog from the stage graph.

## Overall Flow

```
PDL Source
  -> Parse (Parser.scala -> Prog AST)
  -> Type Check + Passes (Main.runPasses -> annotated Prog)
  -> Stage Splitting (SplitStagesPass -> Map[Id, List[PStage]])
  -> Stage Optimization (ConvertAsync, AddEdgeValue, LockElimination, Collapse)
  -> BSV Generation (BluespecGeneration -> BProgram AST)
  -> BSV Pretty Print (BSVPrettyPrinter -> .bsv files)
  -> BSC Compiler (bsc -> Verilog)
```

## Key Classes

- **`BluespecProgramGenerator`** (BluespecGeneration.scala) -- top-level: takes a `Prog`, stage info, and config; produces `List[BProgram]`
- **`BluespecModuleGenerator`** (inner class) -- per-pipeline module: generates rules, declarations, interfaces
- **`Translations`** (Translations.scala) -- translates PDL expressions/types to BSV expressions/types
- **`BluespecInterfaces`** (BluespecInterfaces.scala) -- generates BSV module instantiation expressions, method calls
- **`BSVPrettyPrinter`** (BSVPrettyPrinter.scala) -- serializes the BSV AST to text

## BSV AST (BSVSyntax.scala)

The compiler builds a BSV AST before printing. Key nodes:

| Node | Represents |
|------|-----------|
| `BProgram(name, body)` | A BSV package |
| `BModuleDef(name, typ, params, body)` | A BSV module |
| `BRuleDef(name, conds, body)` | A BSV rule (conds = guard expressions) |
| `BMethodDef(sig, cond, body)` | A BSV method |
| `BModInst(name, module)` | Module/register instantiation |
| `BExprStmt(expr)` | Expression statement |
| `BAssign(lhs, rhs)` | Combinational assignment (`=`) |
| `BInvokeAssign(lhs, invoke)` | `let x <- invoke` |
| `BMethodInvoke(mod, method, args)` | Method call (`mod.method(args)`) |
| `BIf(cond, thenStmts, elseStmts)` | Conditional |
| `BStmtSeq(stmts)` | Statement sequence |
| `BBOp(op, l, r)` | Binary operation |
| `BUOp(op, e)` | Unary operation |

**Important**: `BAssign` produces `=` (combinational wire). For register writes, use `BExprStmt(BMethodInvoke(reg, "_write", List(value)))` which produces `reg <= value` in BSV.

## Per-Module Generation (BluespecModuleGenerator)

### Module Structure

Each PDL pipeline module becomes a BSV module containing:

1. **Instantiations**: FIFOs for pipeline edges, lock regions, registers
2. **Rules**: One execute rule + optional kill rule per pipeline stage
3. **Methods**: `req` (start pipeline), `peek`/`checkHandle`/`resp` (output), `busy` (backpressure)

### Key Data Structures

| Variable | Type | Purpose |
|----------|------|---------|
| `specTable` | `BVar` | Speculation table module instance |
| `busyReg` | `BVar` | Busy register (backpressure) |
| `globalExnFlag` | `BVar` | Global exception flag register |
| `threadIdVar` | `BVar` | Thread ID counter register |
| `outputQueue` | `BVar` | Output queue for pipeline results |
| `edgeParams` | `Map[PipelineEdge, BVar]` | FIFO variables for each pipeline edge |
| `modParams` | `Map[Id, BVar]` | Module parameter variables (memories, locks) |
| `lockRegions` | `Map[Id, BVar]` | Lock region registers |

### Stage Rule Generation (`getStageRule`)

Each `PStage` becomes a BSV rule:

```
rule <stage>_execute (<guards>);
    <declarations>     // request handle declarations
    <effects>          // memory ops, lock ops, sends/receives
    <queue ops>        // FIFO enqueues/dequeues
    <debug>            // optional $display
endrule
```

**Guards** come from two sources:
- `getBlockingConds(cmds)` -- lock ownership checks, output queue space, spec checks, exception flag checks
- `getRecvConds(cmds)` -- FIFO not-empty checks, memory response ready checks

The guards are AND'd together. The rule only fires when ALL guards are true.

### Kill Rule Generation (`getStageKillRule`)

Optional per-stage rule that fires when a speculated instruction is killed:

```
rule <stage>_kill (<kill_conds> && <recv_conds>);
    <dequeue input FIFOs>    // consume the dead instruction's data
    <free spec table entry>  // release speculation resources
endrule
```

Kill conditions check `isValid(specId) && !fromMaybe(True, specTable.check(specId))` -- the instruction was speculative AND is confirmed mispredicted.

### Guard Extraction

**`getBlockingConds(cmds)`** extracts guards from:
- `CLockStart(mod)` -> lock region start check
- `IReserveLock` -> lock reservation availability
- `ICheckLockOwned` -> lock ownership verification
- `IMemSend/IMemWrite` with `isAtomic` -> atomic access availability
- `COutput` -> output queue can write
- `CCheckSpec(blocking=true)` -> spec status must be True (non-speculative)
- `CCheckSpec(blocking=false)` -> spec status must not be False (not definitely killed)
- `ICheckExn` -> `!globalExnFlag` (not in exception handling mode)
- `ICondCommand` -> recursively extracts from conditional blocks

**`getKillConds(cmds)`** extracts kill triggers from:
- `CCheckSpec` -> spec status is definitely False (mispredicted)
- `ICondCommand` -> recursive extraction

### Effect Command Translation (`getEffectCmd`)

Translates PDL commands to BSV statements:

| PDL Command | BSV Output |
|-------------|-----------|
| `IMemSend(handle, ...)` | `let handle <- mem.req(addr, data, wmask)` |
| `IMemRecv(mem, handle, _)` | `mem.resp(handle)` |
| `IMemWrite(mem, addr, data, ...)` | `mem.write(addr, data)` or lock write |
| `ISend(handle, receiver, args)` | `fifo.enq(args)` or `let handle <- mod.req(args)` |
| `IRecv(_, sender, _)` | `sender.resp()` |
| `COutput(exp)` | `outputQueue.enq(value); threadId++` |
| `CSpecCall(handle, ...)` | `let specId <- specTable.alloc(); fifo.enq(args, specId)` |
| `CVerify(handle, args, preds)` | spec validate/invalidate + rollback |
| `IAbort(mem)` | `mem.lock.abort()` or `mem.clear()` |
| `ISetGlobalExnFlag(state)` | `globalExnFlag <= state` |
| `IFifoClear()` | `.clear()` on all edge FIFOs |
| `ISpecClear()` | `specTable.clear()` |
| `ICheckExn()` | *(guard condition, not a statement)* |

### FIFO / Edge Management

Pipeline edges are implemented as FIFOs. Each edge carries a struct with:
- All live variables needed by downstream stages
- Thread ID (`_threadID`)
- Speculation ID (`_specId`, if speculative)

**Edge struct names**: `E_<from>_TO_<to>` (generated by `getEdgeStructInfo`)

**FIFO variable names**: `fifo_<from>_TO_<to>` (generated by `genParamName`)

**Edge queue operations** (`getEdgeQueueStmts`):
- Input edges: `fifo.deq()` at start of rule
- Output edges: `fifo.enq(struct)` at end of rule
- Out-of-order coordination edges: tag-based routing

### Module Instantiation (`getTopModule`)

Assembles all pieces into a BSV module:

```bsv
module mkPipeline(PipelineInterface);
    // Instantiations
    FIFOF#(E_input_TO_Start) fifo_input_TO_Start <- mkNBFIFOF();
    FIFOF#(E_Start_TO_Stage0) fifo_Start_TO_Stage0 <- mkFIFOF();
    // ... more FIFOs, lock regions, module locks
    Reg#(Bool) busyReg <- mkReg(False);
    SpecTable#(...) specTable <- mkSpecTable();    // if speculative
    Reg#(Bool) globalExnFlag <- mkReg(False);      // if exception pipeline
    OutputQ#(...) outputQueue <- mkOutputFIFOF(0);
    Reg#(UInt#(N)) threadId <- mkReg(0);

    // Rules (one pair per stage)
    rule s_Start_execute (...);  ...  endrule
    rule s_Start_kill (...);     ...  endrule    // optional
    rule s_Stage0_execute (...); ...  endrule
    // ... etc

    // Interface methods
    method req(args) if (!busyReg); ...  endmethod
    method peek();      ...  endmethod
    method checkHandle(h); ... endmethod
    method resp();      ...  endmethod
endmodule
```

## Things to Watch Out For

1. **`BAssign` vs register write**: `BAssign(v, e)` produces `v = e` (combinational). For registers, use `BExprStmt(BMethodInvoke(reg, "_write", List(value)))` which produces `reg <= value`.

2. **Guard vs effect**: Some commands are guards (prevent rule firing) not effects (state changes). `ICheckExn` and `CCheckSpec` are guards extracted by `getBlockingConds`, not effects. If added to `getEffectCmd` they should return `None`.

3. **FIFO naming**: Edge FIFOs use generated names from `genEdgeName`. The `edgeParams` map stores the BVar for each edge. When generating `.clear()` calls, iterate `edgeParams.values`.

4. **Speculation table ports**: The `Integer i` parameter in `check(s, i)` and `validate/invalidate(s, i)` selects the EHR port. Lower = earlier in cycle. Stages with `spec_call` use port 0, stages with `verify` use higher ports. This is tracked by `specAnnotations` and `stgSpecOrder`.

5. **Module parameters**: Memories and locks passed to the pipeline are stored in `modParams: Map[Id, BVar]`. Lock methods are accessed as `modParams(mem).lock.method()` or directly via `LockImplementation.getXxxInfo()`.

6. **Non-blocking input FIFO**: The first stage's input FIFO uses `mkNBFIFOF` (non-blocking, last-writer-wins) because the recursive call, verify redirect, and external request can all enqueue in the same cycle.

7. **3-port EHR on AsyncMem**: We upgraded `AsyncMem`'s valid bits from 2-port to 3-port EHR to add `clear()` without breaking `fire_when_enabled` on existing rules. Port 0 = moveToOutFifo, port 1 = freeResp/checkResp/peekResp, port 2 = clear.

8. **Exception flag as guard**: `ICheckExn` becomes `!globalExnFlag._read()` in `getBlockingConds`. This prevents body stages from executing while the except block runs. The except block's own stages don't have `ICheckExn` so they execute normally.
