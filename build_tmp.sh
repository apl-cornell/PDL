#!/bin/sh
find tmp/Circuit_verilog/ -type f -not -name "mkTB.v" -exec cat {} \; |
    cat - bscRuntime/verilog/*.v |
    cat - ~/bsc/src/Verilog/RevertReg.v ~/bsc/src/Verilog/FIFO2.v ~/bsc/src/Verilog/SizedFIFO.v |
    sed 's/CLK/clk/g' - > tmp/mkCircuit__pickled.v
