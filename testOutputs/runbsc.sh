#!/bin/bash

TOP=Circuit
ARGS="-no-show-timestamps -no-show-version --aggressive-conditions"
BPATH="-p .:"$(realpath ~/volume/SpecLang/bscRuntime/locks)":"$(realpath ~/volume/SpecLang/bscRuntime/memories)":"$BLUESPECDIR/inst/lib/Libraries/
#Compiling Base Module
#bsc $ARGS $BPATH -show-schedule -verilog $FILE
#Compile test bench
bsc $ARGS $BPATH -show-schedule -verilog -u "$TOP".bsv
#Run simulation for like...a million cycles
bsc $ARGS $BPATH -sim -u "$TOP".bsv
bsc $ARGS -sim -o mk"$TOP".bexe -e mk"$TOP" mk"$TOP".ba
./mk"$TOP".bexe > top.sim.out
