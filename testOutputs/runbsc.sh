#!/bin/bash

ARGS="-no-show-timestamps -no-show-version --aggressive-conditions"
BPATH="-p .:"$(realpath ~/volume/SpecLang/bscRuntime/locks)":"$(realpath ~/volume/SpecLang/bscRuntime/memories)":"$BLUESPECDIR/inst/lib/Libraries/
#Compiling Base Module
bsc $ARGS $BPATH -show-schedule -verilog Cpu.bsv
#Compile test bench
bsc $ARGS $BPATH -show-schedule -verilog tb.bsv
#Run simulation for like...a million cycles
bsc $ARGS $BPATH -sim tb.bsv
bsc $ARGS -sim -o top.bexe -e top top.ba
./top.bexe > top.sim.out
