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
