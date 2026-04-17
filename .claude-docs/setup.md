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

## Dependency Updates

The original project was developed on x86_64 Linux (Ubuntu 18.04, per CI). All dependencies have been updated to latest stable versions.

| Dependency | Original | Current | Notes |
|---|---|---|---|
| **Scala** | **2.13.2** | **3.3.6 LTS** | **Major version migration** |
| SBT | 1.4.4 | 1.11.0 | ARM64 JNA natives |
| sbt-assembly | 0.14.10 | 2.3.1 | SBT 1.x compat |
| commons-io | 2.8.0 | 2.18.0 | |
| scala-parser-combinators | 1.1.2 | 2.4.0 | |
| pprint | 0.5.6 | 0.9.0 | |
| z3-turnkey | 4.8.7.1 (`io.github.tudo-aqua`) | 4.13.0 (`tools.aqua`) | ARM64 natives, generified API |
| scopt | 4.0.0-RC2 | 4.1.0 | Was pre-release, now stable |
| scala-logging | 3.9.2 | 3.9.5 | |
| logback-classic | 1.2.3 | 1.5.18 | Now uses SLF4J 2.x |
| scalatest | 3.2.2 | 3.2.19 | |
| scalactic | 3.2.2 | 3.2.19 | |

### build.sbt additional changes
- `in` syntax → slash syntax: `assemblyJarName in assembly` → `assembly / assemblyJarName` (deprecated in SBT 1.x)
- Added `Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat` to fix Z3 JNI class loading in tests
- Added `assembly / assemblyMergeStrategy` to discard `module-info.class` conflicts from newer Java dependencies

### Scala 3 migration changes
Migrated from Scala 2.13 to Scala 3.3.6 LTS. Key source changes:

**Syntax (all files):**
- `import foo._` → `import foo.*` (84 occurrences across 47 files)
- Varargs `: _*` → `*` (12 occurrences)
- Lambda params `{ x: Type => }` → `{ (x: Type) => }` (2 files)
- `.close` → `.close()` for side-effecting no-arg methods (Main.scala)
- `return` statements removed (test package.scala)

**Reserved keywords:**
- `export` variable renamed to `exportDecl` (BSVPrettyPrinter.scala) — `export` is a keyword in Scala 3

**Indentation-sensitive parsing (most common issue):**
- Multi-statement `case` bodies wrapped in explicit braces (CanonicalizePass.scala, TypeInferenceWrapper.scala, others)
- `matchOrError(...)` followed by `{ case ... }` on next line — moved `{` to same line (Syntax.scala, Utilities.scala, LockImplementation.scala, BaseTypeChecker.scala, FunctionConstraintChecker.scala)
- `if/else` reformatted for unambiguous indentation (PortChecker.scala)

**Stricter type inference:**
- Implicit conversions that auto-applied in Scala 2 need explicit calls in Scala 3 (TypeInferenceWrapper.scala: `1` → `TBitWidthLen(1)`)
- Ambiguous overload resolution needs type ascription (Utilities.scala, LockOpTranslationPass.scala: `.copyMeta(e: Expr)`)

**Removed APIs:**
- `scala.reflect.io.Directory` → `FileUtils.deleteDirectory` from commons-io (test package.scala)

**Test formatting:**
- Lambda body after `=>` with `{` on next line not parsed as lambda body in Scala 3 (TypeAutoCastSuite.scala)

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
