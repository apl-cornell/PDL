export SCALA_V := 2.13
export COMPILER_JAR := target/scala-$(SCALA_V)/pdl.jar

export BSV_MEMS := $(realpath bscRuntime/memories)

all: setup compiler runtime

setup:
	@echo "--- Checking setup ---"
	@./bin/check-setup.sh

compiler:
	@echo "--- Building Compiler ---"
	@sbt assembly
	@echo

runtime:
	@echo "--- Building BSV Libraries ---"
	@$(MAKE) -C $(BSV_MEMS)
	@echo
clean:
	@echo "Cleaning compiler"
	@sbt clean
	@echo
	@echo "Cleaning BSV Libraries"
	@$(MAKE) -C $(BSV_LOCKS) clean
	@$(MAKE) -C $(BSV_MEMS) clean
	@echo

.PHONY: setup, clean, compiler
