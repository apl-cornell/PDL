#!/bin/bash
if ! which sbt &>/dev/null
then
    echo "- sbt not found. Is sbt installed?"
    exit 1
else
    echo "+ sbt found"
fi

if ! which bsc &>/dev/null
then
    echo "- bsc not found. Is bluespec system verilog installed?"
    exit 1
else
    echo "+ bsc found"
fi

if [[ -z "$BLUESPECDIR" ]]
then
    echo "- BLUESPECDIR environment variable must point to BSV installation"
    exit 1
fi
