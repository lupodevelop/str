Benchmarks
==========

This folder contains micro-benchmarks used to evaluate substring search
heuristics (sliding-match vs KMP) on representative inputs.

Usage:

    python3 scripts/bench_kmp.py --repeat 5

Notes:
- These benchmarks are implementation-agnostic: they exercise algorithmic
  characteristics (O(nm) vs O(n+m)) using Python implementations.
- They are intended to help choose heuristics (pattern length thresholds,
  repetitiveness heuristics) without modifying library code.

BEAM-native benchmark
---------------------

You can also run a BEAM-native micro-benchmark that invokes the compiled
Gleam modules directly on the Erlang VM. This is useful to measure the
actual runtime performance on the target platform.

Example invocation (from repository root):

```bash
erl -noshell \
  -pa build/dev/erlang/gleam_stdlib/ebin \
  -pa build/dev/erlang/str/ebin \
  -eval "bench_beam:run(), halt()."
```

This will write a CSV file under `scripts/bench_results/` with timing
results for several scenarios (repetitive, random, emoji). The script is
`scripts/bench_beam.erl` and does not modify the repository source; it
only reads the compiled `.beam` files.
