#!/bin/bash

TOP=Circuit
TB="mkTB"
ARGS="-no-show-timestamps -no-show-version --aggressive-conditions"
BPATH="-p  .:%/Prelude:%/Libraries:/Users/kevinzhang/Documents/SpecLang/bscRuntime/memories:/Users/kevinzhang/Documents/SpecLang/bscRuntime/locks:/Users/kevinzhang/Documents/SpecLang/bscRuntime/stages"
#Compiling Base Module
#bsc $ARGS $BPATH -show-schedule -verilog $FILE
#Compile test bench
VDIR="$TOP"_verilog
SDIR="$TOP"_sim
mkdir -p $VDIR
mkdir -p $SDIR
bsc $ARGS $BPATH -show-schedule -verilog -vdir $VDIR -u "$TOP".bsv
#Run simulation for like...a million cycles
bsc $ARGS $BPATH -sim -simdir $SDIR -u "$TOP".bsv
bsc $ARGS -sim -simdir $SDIR -o "$TB".bexe -e "$TB" "$TB".ba
./"$TB".bexe > top.sim.out
