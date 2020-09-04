#!/bin/bash

ARGS="-no-show-timestamps -no-show-version --aggressive-conditions"
BPATH="-p .:"$(realpath ~/volume/SpecLang/bscRuntime/locks)":"$(realpath ~/volume/SpecLang/bscRuntime/memories)":"$BLUESPECDIR/inst/lib/Libraries/
bsc $ARGS $BPATH -show-schedule -verilog one.bsv
