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

In the top-level directory of this repo, simply  run `make`.

This will produce an executable jar, `pdsl.jar`, in the `target/scala-2.13/` directory
and will use BSV to compile the custom hardware module libraries.

You can also manually build the compiler by running `sbt compile`.
`sbt assembly` can be used to compile and then produce the executable jar.

### Running the Compiler

The main class is `pipedsl.Main` and can be run via
the sbt command `sbt "run pipedsl.Main <args>"`.
Alternatively, the executable jar can be used by 
running java with the command `java -jar pdsl.jar <args>`.

We also provide a script `pdl` in the `bin` directory that
runs the executable jar with the provided arguments.

## Compiler Structure

Here we describe each of the packages and their contents and
then we will describe the high level architecture of compilation.

Packages:

- pipedsl
 - The toplevel package containing the main file and some of the other primary components,
 such as the parser.
- pipedsl.common
 - This package contains a bunch of common utility files that are useful in many places,
 such as a Dataflow analysis framework.
 - It also contains most of the case class definitions used to build the AST for the parsed language
 and some of the intermediate representations.
 - Lastly it contains pretty printer files used to print out various ASTs.
- pipedsl.passes
 - A number of transformation passes that take in various parts of the AST (e.g. whole programs,
 single pipeline modules, other intermediate representations) and produce new versions of
 the input structure.
- pipedsl.typechecker
 - This contains a number of typechecking passes that may transform the existing AST, but only
 by modifying type (and potentially other) annotations. Various typecheckers check different
 subcomponents of the type system (e.g. base types, data availability, locking, speculation, etc.)
- pipedsl.codegen
 - The functions that transform our internal representations to ASTs in target languages.


### Compilation Strategy

The compiler works in the following steps:

1. Parse the input text file into a `Program` that contains:
 combinational function definitions; pipeline definitions; and, the full circuit instantiation.
2. Apply the various typechecker passes to annotate all of the `Id` and `EVar` expressions with
 the appropriate types.
3. Generate the internal `PStage` representation for each pipeline which is a dataflow graph
 representation of the original AST, where nodes in the graph represent distinct pipeline stages.
 These stages can execute concurrently with each other and primarily communicate with each other
 via channels represented by edges in the graph.
4. Run passes to annotate this graph with any extra information needed for code generation,
 such as live variable analysis that computes which data to send along the edges.
5. Generate the target AST (currently BSV) by applying transformations to each of the
 nodes in the stage graph.
6. Print the target AST to an output directory using the pretty printers.
