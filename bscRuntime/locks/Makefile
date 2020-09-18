LABNAME = Stages Stuff

BSC=bsc -no-show-timestamps -no-show-version --aggressive-conditions
B_IN=Locks.bsv

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


verilog: mk$(OUT).v

%.v: %.bsv
	$(BSC) -show-schedule -verilog $<

mk$(OUT).v: $(B_IN)
	$(BSC) -show-schedule -verilog $(B_IN)

.PHONY: sim
sim: $(B_IN).bsv

	$(BSC) -sim $(B_IN)
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

