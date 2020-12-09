#!/bin/bash

TOP=Circuit
TB="mkTB"
ARGS="-no-show-timestamps -no-show-version --aggressive-conditions"
BSC_LIB_DIR=$(realpath ../bscRuntime/locks)":"$(realpath ../bscRuntime/memories)
BPATH="-p .:"$BSC_LIB_DIR":"$BLUESPECDIR/inst/lib/Libraries/
VDIR="$TOP"_verilog
SDIR="$TOP"_sim
mkdir -p $VDIR
mkdir -p $SDIR
#Compile to Verilog
bsc $ARGS $BPATH -show-schedule -verilog -vdir $VDIR -u "$TOP".bsv
#Run simulation
bsc $ARGS $BPATH -sim -simdir $SDIR -u "$TOP".bsv
bsc $ARGS -sim -simdir $SDIR -o "$TB".bexe -e "$TB" "$TB".ba
./"$TB".bexe > top.sim.out
