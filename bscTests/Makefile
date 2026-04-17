# Source generated config if available (created by ./configure)
-include ../config.mk

BSC = bsc -no-show-timestamps -no-show-version --aggressive-conditions

# Absolute paths (resolved from this Makefile's location)
TESTDIR := $(shell pwd)
RTDIR := $(TESTDIR)/../bscRuntime/memories
VDIR := $(TESTDIR)/../bscRuntime/verilog
BPATH = -p $(TESTDIR):$(RTDIR):$(VDIR):$(BLUESPECDIR)/lib/Libraries/

# TIMEOUT_CMD set by config.env; fallback to auto-detect
TIMEOUT_CMD ?= $(shell command -v timeout 2>/dev/null || command -v gtimeout 2>/dev/null)

# All test modules to build and run
TESTS = \
	mkTestQL_BasicLifecycle \
	mkTestQL_PipelineStall \
	mkTestQL_FullQueue \
	mkTestQL_RapidReserveRelease \
	mkTestQL_WrongRelease \
	mkTestCL_BasicLifecycle \
	mkTestCL_SameCycleResRel \
	mkTestCL_ManyReservations \
	mkTestCL_OwnerAdvancement \
	mkTestCL_Wraparound \
	mkTestCKL_BasicCheckpointRollback \
	mkTestCKL_CheckpointNoRollback \
	mkTestCKL_MultipleCheckpoints \
	mkTestCKL_RollbackToEmpty \
	mkTestCKL_RollbackAndContinue \
	mkTestAL_IndependentAddrs \
	mkTestAL_SameAddrConflict \
	mkTestAL_PoolExhaustion \
	mkTestAL_AutoFree \
	mkTestAL_DMBasic \
	mkTestSpec_AllocAndValidate \
	mkTestSpec_InvalidateCascade \
	mkTestSpec_FullTable \
	mkTestSpec_ValidateThenInvalidate \
	mkTestSpec_RapidAllocFree \
	mkTestBP_ReserveWriteReadRelease \
	mkTestBP_ReadBeforeWrite \
	mkTestBP_TwoWritesSameAddr \
	mkTestBP_WriteReadDifferentAddrs \
	mkTestBP_CommitOrder \
	mkTestMem_QLBasicReadWrite \
	mkTestMem_ALReadAfterWrite \
	mkTestMem_ALMultipleReaders \
	mkTestMem_QLAtomicOps \
	mkTestMem_ALWriteAndRelease \
	mkTestBHT_StateMachine \
	mkTestBHT_SaturationStrong \
	mkTestBHT_DifferentPCs \
	mkTestBHT_SameCycleReqUpd \
	mkTestBHT_AliasingBehavior

# Source files per test module
mkTestQL_BasicLifecycle_SRC = TestQueueLock.bsv
mkTestQL_PipelineStall_SRC = TestQueueLock.bsv
mkTestQL_FullQueue_SRC = TestQueueLock.bsv
mkTestQL_RapidReserveRelease_SRC = TestQueueLock.bsv
mkTestQL_WrongRelease_SRC = TestQueueLock.bsv
mkTestCL_BasicLifecycle_SRC = TestCountingLock.bsv
mkTestCL_SameCycleResRel_SRC = TestCountingLock.bsv
mkTestCL_ManyReservations_SRC = TestCountingLock.bsv
mkTestCL_OwnerAdvancement_SRC = TestCountingLock.bsv
mkTestCL_Wraparound_SRC = TestCountingLock.bsv
mkTestCKL_BasicCheckpointRollback_SRC = TestCheckpointLock.bsv
mkTestCKL_CheckpointNoRollback_SRC = TestCheckpointLock.bsv
mkTestCKL_MultipleCheckpoints_SRC = TestCheckpointLock.bsv
mkTestCKL_RollbackToEmpty_SRC = TestCheckpointLock.bsv
mkTestCKL_RollbackAndContinue_SRC = TestCheckpointLock.bsv
mkTestAL_IndependentAddrs_SRC = TestAddrLock.bsv
mkTestAL_SameAddrConflict_SRC = TestAddrLock.bsv
mkTestAL_PoolExhaustion_SRC = TestAddrLock.bsv
mkTestAL_AutoFree_SRC = TestAddrLock.bsv
mkTestAL_DMBasic_SRC = TestAddrLock.bsv
mkTestSpec_AllocAndValidate_SRC = TestSpeculation.bsv
mkTestSpec_InvalidateCascade_SRC = TestSpeculation.bsv
mkTestSpec_FullTable_SRC = TestSpeculation.bsv
mkTestSpec_ValidateThenInvalidate_SRC = TestSpeculation.bsv
mkTestSpec_RapidAllocFree_SRC = TestSpeculation.bsv
mkTestBP_ReserveWriteReadRelease_SRC = TestBypassLock.bsv
mkTestBP_ReadBeforeWrite_SRC = TestBypassLock.bsv
mkTestBP_TwoWritesSameAddr_SRC = TestBypassLock.bsv
mkTestBP_WriteReadDifferentAddrs_SRC = TestBypassLock.bsv
mkTestBP_CommitOrder_SRC = TestBypassLock.bsv
mkTestMem_QLBasicReadWrite_SRC = TestNewMemories.bsv
mkTestMem_ALReadAfterWrite_SRC = TestNewMemories.bsv
mkTestMem_ALMultipleReaders_SRC = TestNewMemories.bsv
mkTestMem_QLAtomicOps_SRC = TestNewMemories.bsv
mkTestMem_ALWriteAndRelease_SRC = TestNewMemories.bsv
mkTestBHT_StateMachine_SRC = TestBHT.bsv
mkTestBHT_SaturationStrong_SRC = TestBHT.bsv
mkTestBHT_DifferentPCs_SRC = TestBHT.bsv
mkTestBHT_SameCycleReqUpd_SRC = TestBHT.bsv
mkTestBHT_AliasingBehavior_SRC = TestBHT.bsv

VSIM = -vsim iverilog
VPATH_FLAG = -vsearch $(BLUESPECDIR)/lib/Verilog:$(VDIR)

.PHONY: all clean test

all: test

# Compile runtime dependencies first
deps:
	@$(MAKE) -s -C $(RTDIR)

# Generic rule: compile BSV -> Verilog -> simulate
define make_test
.PHONY: run_$(1)
run_$(1): deps
	@mkdir -p $(TESTDIR)/build_$(1) $(TESTDIR)/results
	@cd $(TESTDIR)/build_$(1) && \
	  $(BSC) $(BPATH) $(VPATH_FLAG) $(VSIM) -vdir . -simdir . -u $(TESTDIR)/$($(1)_SRC) 2>&1 && \
	  $(BSC) $(VPATH_FLAG) $(VSIM) -verilog -vdir . -simdir . -o $(1).bexe -e $(1) $(1).v 2>&1 && \
	  $(TIMEOUT_CMD) 10s $(or $(SIM_RUNNER),vvp) ./$(1).bexe 2>&1 | grep -v WARNING | grep -v '\$$finish' > $(TESTDIR)/results/$(1).out 2>&1; \
	  true
endef

$(foreach t,$(TESTS),$(eval $(call make_test,$(t))))

test: $(foreach t,$(TESTS),run_$(t))
	@echo ""
	@echo "========================================"
	@echo "  BSV Runtime Test Results"
	@echo "========================================"
	@pass=0; fail=0; \
	for t in $(TESTS); do \
	  if [ -f $(TESTDIR)/results/$$t.out ] && grep -q "^PASS" $(TESTDIR)/results/$$t.out; then \
	    echo "  PASS  $$t"; \
	    pass=$$((pass+1)); \
	  else \
	    echo "  FAIL  $$t"; \
	    if [ -f $(TESTDIR)/results/$$t.out ]; then cat $(TESTDIR)/results/$$t.out; fi; \
	    fail=$$((fail+1)); \
	  fi; \
	done; \
	echo "========================================"; \
	echo "  $$pass passed, $$fail failed"; \
	echo "========================================"; \
	rm -rf $(TESTDIR)/build_* $(TESTDIR)/results $(TESTDIR)/*.bo $(TESTDIR)/*.bi; \
	test $$fail -eq 0

clean:
	rm -rf build_* results *.bo *.bi
