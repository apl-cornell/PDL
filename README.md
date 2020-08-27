# SpecLang

This language provides a high level, imperative-style programming model
for building pipelined processors and accelerators.
Designers use scheduling and optimization operators to define
when computations happen and to enable common hardware optimizations
like out-of-order execution and speculation.

## Set Up

### Dependencies

The compiler is built in [Scala](https://www.scala-lang.org/) and currently targets
[Bluespec System Verilog](https://github.com/B-Lang-org/bsc) (BSV) as the output language.

You will need a Java runtime, Scala, and [sbt](https://www.scala-sbt.org/) to build
and run the compiler.


- Install Java:
 - On Linux: 
 - On Mac: `brew tap AdoptOpenJDK/openjdk && brew cask install adoptopenjdk8`
- Install Scala and sbt:
 - Install an IDE like [intellij](https://www.jetbrains.com/idea/) and install the scala/sbt plugins (recommended)
 - OR
 - On Mac: `brew install scala sbt`
 - On Linux: TODO

You will need the Bluespec compiler and simulator to run end-to-end tests.
The instructions can be found on the bluespec github. Linux is currently
the only supported platform.

### Building the Compiler

In the top-level directory of this repo, run `sbt compile`.

TODO is to add an `assembly` directive to build an executable jar.

### Running the Compiler

The main class is `pipedsl.Main` and can be run via
the sbt command `sbt run pipedsl.Main <args>`.

At the moment, the compiler takes a single argument which is the
file path of the single source file to compile.

## Compiler Structure

Here we describe each of the packages and their contents and
then we will describe the high level architecture of compilation.

Packages:

- pipedsl
 - The toplevel package containing the main file and some of the other primary components,
 such as the parser.
-