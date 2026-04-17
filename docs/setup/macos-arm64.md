# PDL Development Setup (macOS ARM64 / Apple Silicon)

## Prerequisites

Install via Homebrew:

```bash
brew install openjdk sbt bsc coreutils
```

This installs:
- **OpenJDK** — Java runtime for the Scala compiler
- **SBT** — Scala build tool
- **bsc** — Bluespec compiler (also installs IVerilog as a dependency)
- **coreutils** — Provides `gtimeout`, needed by `bin/runbsc` for simulation timeouts (macOS lacks GNU `timeout`)

## Environment Variables

Add to `~/.zshrc`:

```bash
export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"
export BLUESPECDIR=/opt/homebrew/opt/bsc/libexec
```

`BLUESPECDIR` must point to the Bluespec installation directory containing `lib/Libraries/` and `lib/Verilog/`. The `bin/check-setup.sh` script validates this.

## Build and Test

```bash
make            # Full build: compiler JAR + BSV runtime libraries
sbt test        # Run all 247 tests (parse, typecheck, compile, simulate)
```

## Dependency Updates for ARM64 Compatibility

The original project was developed on x86_64 Linux (Ubuntu 18.04, per CI). The following changes were needed to build and test on ARM64 macOS:

### project/build.properties
- SBT **1.4.4 → 1.10.11**: The old SBT bundled x86_64-only JNA natives, causing `UnsatisfiedLinkError` on ARM64.

### project/assembly.sbt
- sbt-assembly **0.14.10 → 2.3.1**: Required for SBT 1.10 compatibility.

### build.sbt
- Scala **2.13.2 → 2.13.16**: SBT 1.10 requires Scala >= 2.13.3 (SIP-51 binary compatibility enforcement).
- z3-turnkey **4.8.7.1** (`io.github.tudo-aqua`) → **4.13.0** (`tools.aqua`): The old JAR only bundled x86_64 Z3 natives. The maintainer moved to a new Maven group (`tools.aqua`) and added ARM64 support starting with 4.8.15.
- `in` syntax → slash syntax: `assemblyJarName in assembly` → `assembly / assemblyJarName` (deprecated in SBT 1.x).
- Added `Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat` to fix Z3 JNI class loading in tests.

### Z3 API changes (4.8.7 → 4.13.0)
Z3 4.8.13+ generified `Expr`, `ArithExpr`, and `IntExpr`:
- `Expr` → `Expr<R extends Sort>`
- `ArithExpr` → `ArithExpr<R extends ArithSort>`
- `BoolExpr` and `IntExpr` are **not** generic (they're leaf types)

Files changed:
- `src/main/scala/pipedsl/common/Constraints.scala` — `Z3ArithExpr` → `Z3ArithExpr[_]` in return types and casts
- `src/main/scala/pipedsl/passes/PredicateGenerator.scala` — `Z3Expr` → `Z3Expr[_]` in return types, added `asInstanceOf` casts where `Option[Z3Expr[_]]` pattern matching erases to `Any`
- `src/main/scala/pipedsl/typechecker/TypeInferenceWrapper.scala` — `Z3ArithExpr` → `Z3ArithExpr[_]` in return types

### bin/runbsc
Three macOS-specific issues:
1. **`timeout` command missing**: macOS lacks GNU `timeout`. Added detection logic to use `gtimeout` (from coreutils) as fallback.
2. **VVP shebang + gtimeout incompatibility**: iverilog produces `.bexe` files with a shebang pointing to `vvp`. Running these via `gtimeout ./mkTB.bexe` fails because gtimeout can't resolve the shebang-to-wrapper chain. Fixed by calling `vvp` explicitly: `gtimeout 10s vvp ./mkTB.bexe`.
3. **`$finish` output**: iverilog v13 prints `$finish(1) called at ...` to stdout, which the old version/Bluesim did not. Added `grep -v '\$finish'` filter to match expected test outputs.

## CI Configuration

The GitHub Actions workflow (`.github/workflows/scala.yml`) targets Ubuntu 18.04 with JDK 1.8 and downloads `bsc-2021.07`. This CI config is separate from the local macOS setup and does not need the above changes.
