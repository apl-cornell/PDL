BSC=bsc -no-show-timestamps -no-show-version --aggressive-conditions
TOBUILD=Ehr.bo Locks.bo Memories.bo Speculation.bo SpecialQueues.bo

## Default simulator is iverilog
VSIM = -vsim iverilog

all: $(TOBUILD)

Ehr.bo: Ehr.bsv
	$(BSC) -show-schedule $<
Locks.bo: Locks.bsv Ehr.bo
	$(BSC) -show-schedule $<
%.bo: %.bsv Ehr.bo Locks.bo
	$(BSC) -show-schedule $<

.PHONY: sim
sim: $(B_LIB) $(B_SIM)

	$(BSC) -show-schedule -sim $(B_SIM)
	$(BSC) -sim -o mk$(OUT).bexe -e mk$(OUT) mk$(OUT).ba
	./mk$(OUT).bexe > smoke_test_bluesim.out 

.PHONY: clean
clean:
	@rm -f *.bi *.bo *.ba 
	@rm -f *.cxx *.h *.o *.so *.bexe
	@rm -f *.v *.vexe
	@rm -f *.vcd *~ *.fsdb *.log
	@rm -f *.sched
	@rm -f smoke_test_verilog.out smoke_test_bluesim.out
	@rm -rf csrc INCA_libs simv.daidir vfastLog/  nWaveLog/ work_mkFibOne/

