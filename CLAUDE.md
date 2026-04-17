# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

PDL (Pipeline Description Language) is a novel hardware description language and compiler for building pipelined processors, published at PLDI '22 (Zagieboylo, Sherk, Suh, Myers — Cornell). PDL provides **one-instruction-at-a-time semantics**: designers write imperative-style code that looks sequential, but the compiler generates a pipelined circuit (in Bluespec System Verilog) where multiple instructions execute concurrently across stages. The compiler statically guarantees that the pipelined implementation behaves identically to the sequential specification — no data hazards, no speculation bugs.

Key language abstractions:
- **Stage separators** (`---`): split combinational logic across clock cycles, defining pipeline structure
- **Hazard locks** (`reserve`, `block`, `acquire`, `release`): abstract stall/bypass/forwarding logic into modular, checkable hardware components. Lock implementations (Queue Lock, Bypass Queue, Renaming Register File) are interchangeable without changing the pipeline code.
- **Speculation API** (`spec call`, `verify`, `update`, `spec_check`, `spec_barrier`): explicit speculation with compiler-checked correctness. External RTL predictors can be integrated safely.
- **Out-of-order stages**: conditional branches with stage separators create DAG-shaped pipelines with compiler-generated coordination logic
- **Checkpoint/rollback**: compiler-inserted primitives for safely undoing speculative lock operations

The compiler uses Z3 SMT solving for path-sensitive type checking of lock usage and speculation correctness.

## Setup

```bash
./configure         # Detect toolchain, write config.env (run once)
make                # Build compiler JAR + BSV runtime libraries
```

**Requirements**: JDK (8+), SBT, Bluespec compiler (`bsc`), IVerilog, timeout/gtimeout.

On macOS: `brew install openjdk sbt bsc coreutils`
On Ubuntu: `apt install default-jdk sbt iverilog` + install `bsc` from https://github.com/B-Lang-org/bsc

`./configure` detects all tool paths and writes `config.env`, which is sourced by all Makefiles and `bin/runbsc`. Re-run if you update tools.

## Build and Test

```bash
make                # Full build: check setup, build compiler JAR, build BSV runtime libs
make compiler       # Build compiler JAR only (sbt assembly -> target/scala-3.3.6/pdl.jar)
make runtime        # Build BSV memory libraries only
make clean          # Clean compiler and BSV outputs
sbt test            # Run all 247 compiler tests (parse, typecheck, compile, simulate)
sbt "testOnly pipedsl.MainSuite"    # Run a single test suite
cd bscTests && make test             # Run 40 BSV runtime module tests
cd verilogTests && make test         # Run 25 Verilog RF module tests
```

## Running the Compiler

```bash
bin/pdl --mode parse -f input.pdl -o outdir/
bin/pdl --mode typecheck -f input.pdl -o outdir/
bin/pdl --mode gen -f input.pdl -o outdir/        # generates .bsv files
bin/pdl --mode interpret -f input.pdl -o outdir/ --mem key=file.mem
```

Generated BSV is then compiled to Verilog or simulated using `bin/runbsc` (modes: `c` compile, `v` verilog, `s` simulate).

## Compilation Pipeline (Main.scala)

1. **Parse** (`Parser.scala`) — Scala parser-combinators produce a `Prog` AST (defined in `common/Syntax.scala`)
2. **Passes & Type Checking** (`Main.runPasses`) — 14 sequential checker/transform phases:
   - `LockRegionInferencePass` → `AddCheckpointHandlesPass` → `AddVerifyValuesPass` → `CanonicalizePass`
   - `TypeInference` (Z3-based bitwidth inference) → `BaseTypeChecker` → `FunctionConstraintChecker`
   - `BindModuleTypes` → `SimplifyRecvPass`
   - `LockRegionChecker` → `LockWellformedChecker` → `LockOperationTypeChecker`
   - `PortChecker` → `PredicateGenerator` (SMT) → `LockConstraintChecker` → `LockReleaseChecker`
   - `LinearExecutionChecker` → `SpeculationChecker` (both use Z3 predicates)
   - `LockOpTranslationPass` → `TimingTypeChecker`
3. **Stage Extraction** (`Main.getStageInfo` → `SplitStagesPass`) — converts AST to `PStage` DAG (nodes = pipeline stages, edges = communication FIFOs), then runs: `ConvertAsyncPass` → `AddEdgeValuePass` → `LockEliminationPass` → `CollapseStagesPass` → `LockEliminationPass`
4. **Code Generation** (`codegen/bsv/`) — each `PStage` becomes a BSV rule; edges become FIFOs; live variable analysis determines inter-stage data; BSV scheduling directives are added for speculation bypass paths

## Package Structure

- `pipedsl` — `Main.scala` (entry point, orchestrates compilation), `Parser.scala`, `Interpreter.scala`
- `pipedsl.common` — Core AST (`Syntax.scala`: `Prog`, `Id`, `Type`, expressions, statements), stage DAG (`DAGSyntax.scala`: `PStage`), `Dataflow.scala`, lock models (`Locks.scala`, `LockImplementation.scala`), `PrettyPrinter.scala`
- `pipedsl.passes` — 17 transformation passes on both AST and stage representations
- `pipedsl.typechecker` — 18 type checking/constraint modules; `TypeInferenceWrapper` wraps Z3; `Environments.scala` defines type environments; speculation checking uses typestate (Unknown → Speculative → Nonspeculative)
- `pipedsl.codegen.bsv` — BSV syntax (`BSVSyntax.scala`), generation (`BluespecGeneration.scala`), interface generation (`BluespecInterfaces.scala`), pretty printing

## Type System

- Sized integers: `bit<N>` with optional sign
- Memory types: `T[size]<readLat, readPorts, writeLat, writePorts>`
- Module types with input/ref ports; request handles for async operations; Maybe types
- Latency model: Combinational (c), Sequential (s), Asynchronous (a) — lattice join for propagation
- Speculation typestate: `Unknown`, `Speculative`, `Nonspeculative` — tracks what operations a thread may perform

## Test Structure

Tests use ScalaTest FunSuite. Helpers in `src/test/scala/pipedsl/package.scala` provide `testParse`, `testTypecheck`, `testBlueSpecCompile`, `testBlueSpecSim` — each compares generated output against expected `.parsesol`/`.typechecksol`/`.simsol` files in `solutions/` subdirectories. Test programs are in `src/test/tests/` organized by feature (histogram, risc-pipe, lockTests, speculation, registerRenamingTests, etc.).

## BSV Runtime

`bscRuntime/` contains Bluespec libraries, lock implementations (Queue Lock in BSV, Bypass Queue and Renaming Register File in Verilog), memory modules, and support files. `bin/runbsc` wraps the Bluespec compiler.
