LABNAME = Stages Stuff

BSC=bsc -no-show-timestamps -no-show-version --aggressive-conditions
BMOD=VerilogLibs
B_IN=$(BMOD).bsv
B_LIB=$(BMOD).bo
B_SIM=tb.bsv
OUT=Top

## Default simulator is iverilog
VSIM = -vsim iverilog

ifdef cvc
     VSIM = -vsim cvc
endif

ifdef vcs
    VSIM = -vsim vcs
endif

ifdef vcsi
    VSIM = -vsim vcsi
endif

ifdef ncverilog
    VSIM = -vsim ncverilog
endif

ifdef ncsim
    VSIM = -vsim ncsim
endif

ifdef modelsim
    VSIM = -vsim modelsim
endif


$(B_LIB): $(B_IN)
	$(BSC) -show-schedule $<

verilog: mk$O(UT).v

%.v: %.bsv
	$(BSC) -show-schedule -verilog $<

mk$(OUT).v: $(B_IN)
	$(BSC) -show-schedule -verilog $(B_IN)

.PHONY: sim
sim: $(B_LIB) $(B_SIM)

	$(BSC) -show-schedule $(DEBUG) $(VSIM) $(B_SIM)
	$(BSC) $(DEBUG) -verilog -e mk$(OUT) -o mk$(OUT).bexe
#	$(BSC) $(VSIM) -o mk$(OUT).bexe -e mk$(OUT) mk$(OUT).ba
	./mk$(OUT).bexe +bscvcd > smoke_test_bluesim.out 

.PHONY: clean
clean:
	@rm -f *.bi *.bo *.ba 
	@rm -f *.cxx *.h *.o *.so *.bexe
	@rm -f mkTop.v
	@rm -f *.vexe
	@rm -f *.vcd *~ *.fsdb *.log
	@rm -f *.sched
	@rm -f smoke_test_verilog.out smoke_test_bluesim.out
	@rm -rf csrc INCA_libs simv.daidir vfastLog/  nWaveLog/ work_mkFibOne/

