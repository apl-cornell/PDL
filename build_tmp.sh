#!/bin/sh
find tmp/Circuit_verilog/ -type f -not -name "mkTB.v" -exec cat {} \; |
    cat - ~/bsc/src/Verilog/*.v |
    sed 's/CLK/clk/g' - > tmp/Circuit__pickled.v
