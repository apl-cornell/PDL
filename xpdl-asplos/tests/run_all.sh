#!/bin/sh

# Stop on errors
set -e

# Create output directory
mkdir -p output

# List of PDL files and memInit options (manual pairing)
run_case() {
  file="$1"
  meminit="$2"
  outdir="output/$(basename "$file" .pdl)"

  echo "=== Generating and simulating: $file ==="
  pdl gen "./$file" -o "$outdir" $meminit

  cd "$outdir" || exit 1
  if runbsc s .; then
    echo "Output for $file:"
    cat top.sim.out
  else
    echo "Simulation failed for $file"
    exit 1
  fi
  cd - > /dev/null || exit 1
}

# Main list
run_case "exn-simple.pdl" ""
run_case "exn-recovery.pdl" ""
run_case "exn-long-final-block.pdl" ""
run_case "exn-simple-spec.pdl" "--memInit acc=./memInputs/acc"
run_case "exn-recovery-spec.pdl" "--memInit acc=./memInputs/acc"

echo "✅ All simulations finished successfully."

