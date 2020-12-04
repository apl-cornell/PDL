
export SCALA_V := 2.13
export COMPILER_JAR := target/scala-$(SCALA_V)/pdsl.jar

all: compiler

compiler: $(COMPILER_JAR)

$(COMPILER_JAR):
	@echo "--- Building Compiler ---"
	@sbt assembly
	@echo

clean:
	@echo "Cleaning compiler"
	@sbt clean
	@echo

.PHONY: clean, compiler
