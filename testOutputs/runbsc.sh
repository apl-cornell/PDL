#!/bin/bash

ARGS="-no-show-timestamps -no-show-version --aggressive-conditions"
BPATH="-p .:"$(realpath ~/volume/SpecLang/bscRuntime/locks)":"$(realpath ~/volume/SpecLang/bscRuntime/memories)":"$BLUESPECDIR/inst/lib/Libraries/
#Compiling Base Module
bsc $ARGS $BPATH -show-schedule -verilog Cpu.bsv
#Compile test bench
bsc $ARGS $BPATH -show-schedule -verilog Circuit.bsv
#Run simulation for like...a million cycles
bsc $ARGS $BPATH -sim Circuit.bsv
bsc $ARGS -sim -o mkCircuit.bexe -e mkCircuit mkCircuit.ba
./mkCircuit.bexe > top.sim.out
