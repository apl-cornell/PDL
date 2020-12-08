
export SCALA_V := 2.13
export COMPILER_JAR := target/scala-$(SCALA_V)/pdsl.jar

export BSV_LOCKS := $(realpath bscRuntime/locks)
export BSV_MEMS := $(realpath bscRuntime/memories)

all: compiler runtime

compiler: $(COMPILER_JAR)

$(COMPILER_JAR):
	@echo "--- Building Compiler ---"
	@sbt assembly
	@echo

runtime:
	@echo "--- Building BSV Libraries ---"
	@$(MAKE) -C $(BSV_LOCKS)
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

.PHONY: clean, compiler
