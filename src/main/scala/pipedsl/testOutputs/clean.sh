#!/bin/bash

rm -f *.bi *.bo *.ba
rm -f *.cxx *.h *.o *.so *.bexe
rm -f *.v *.vexe
rm -f *.vcd *~ *.fsdb *.log
rm -f *.sched 
find * -type d -exec rm -rf {} +

