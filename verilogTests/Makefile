# Source generated config if available (created by ./configure)
-include ../config.mk

BSC = bsc -no-show-timestamps -no-show-version --aggressive-conditions

TESTDIR := $(shell pwd)
RTDIR := $(TESTDIR)/../bscRuntime/memories
VDIR := $(TESTDIR)/../bscRuntime/verilog
BPATH = -p $(TESTDIR):$(RTDIR):$(VDIR):$(BLUESPECDIR)/lib/Libraries/

# TIMEOUT_CMD set by config.env; fallback to auto-detect
TIMEOUT_CMD ?= $(shell command -v timeout 2>/dev/null || command -v gtimeout 2>/dev/null)

TESTS = \
	mkTestRR_BasicAllocWriteRead \
	mkTestRR_OwnsTimingNoForward \
	mkTestRR_NameRemapping \
	mkTestRR_FreeListExhaustion \
	mkTestRR_MultiRegPipeline \
	mkTestFRR_BasicForward \
	mkTestFRR_ForwardVsNoForward \
	mkTestFRR_TwoNameForward \
	mkTestFRR_WriteForwardPriority \
	mkTestFRR_AllocAndImmediateRead \
	mkTestBRF_BasicLifecycle \
	mkTestBRF_ReadBeforeWrite \
	mkTestBRF_NoConflictReadFromRF \
	mkTestBRF_TwoWritesSameAddr \
	mkTestBRF_WriteQueueFull \
	mkTestCBRF_BasicCheckpointRollback \
	mkTestCBRF_RollbackPreservesCommitted \
	mkTestCBRF_CheckpointAfterAlloc \
	mkTestCBRF_MultipleCheckpoints \
	mkTestCBRF_ReadPortRollback \
	mkTestCRR_BasicCheckpointRollback \
	mkTestCRR_RollbackPreservesData \
	mkTestCRR_FreeListLeakOnRollback \
	mkTestCRR_MultipleReplicaSlots \
	mkTestCRR_CheckpointIncludesCurrentAlloc

mkTestRR_BasicAllocWriteRead_SRC = TestRenameRF.bsv
mkTestRR_OwnsTimingNoForward_SRC = TestRenameRF.bsv
mkTestRR_NameRemapping_SRC = TestRenameRF.bsv
mkTestRR_FreeListExhaustion_SRC = TestRenameRF.bsv
mkTestRR_MultiRegPipeline_SRC = TestRenameRF.bsv
mkTestFRR_BasicForward_SRC = TestForwardRenameRF.bsv
mkTestFRR_ForwardVsNoForward_SRC = TestForwardRenameRF.bsv
mkTestFRR_TwoNameForward_SRC = TestForwardRenameRF.bsv
mkTestFRR_WriteForwardPriority_SRC = TestForwardRenameRF.bsv
mkTestFRR_AllocAndImmediateRead_SRC = TestForwardRenameRF.bsv
mkTestBRF_BasicLifecycle_SRC = TestBypassRF.bsv
mkTestBRF_ReadBeforeWrite_SRC = TestBypassRF.bsv
mkTestBRF_NoConflictReadFromRF_SRC = TestBypassRF.bsv
mkTestBRF_TwoWritesSameAddr_SRC = TestBypassRF.bsv
mkTestBRF_WriteQueueFull_SRC = TestBypassRF.bsv
mkTestCBRF_BasicCheckpointRollback_SRC = TestCheckpointBypassRF.bsv
mkTestCBRF_RollbackPreservesCommitted_SRC = TestCheckpointBypassRF.bsv
mkTestCBRF_CheckpointAfterAlloc_SRC = TestCheckpointBypassRF.bsv
mkTestCBRF_MultipleCheckpoints_SRC = TestCheckpointBypassRF.bsv
mkTestCBRF_ReadPortRollback_SRC = TestCheckpointBypassRF.bsv
mkTestCRR_BasicCheckpointRollback_SRC = TestCheckpointRenameRF.bsv
mkTestCRR_RollbackPreservesData_SRC = TestCheckpointRenameRF.bsv
mkTestCRR_FreeListLeakOnRollback_SRC = TestCheckpointRenameRF.bsv
mkTestCRR_MultipleReplicaSlots_SRC = TestCheckpointRenameRF.bsv
mkTestCRR_CheckpointIncludesCurrentAlloc_SRC = TestCheckpointRenameRF.bsv

VSIM = -vsim iverilog
VPATH_FLAG = -vsearch $(BLUESPECDIR)/lib/Verilog:$(VDIR)

.PHONY: all clean test

all: test

deps:
	@$(MAKE) -s -C $(RTDIR)

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
	@echo "  Verilog RF Test Results"
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
