#!/usr/bin/env python3
"""
Micro-benchmarks for substring search heuristics

Compares a non-allocating sliding-match vs KMP on lists of graphemes.
Designed to be run from the repository root:

    python3 scripts/bench_kmp.py [--repeat N]

Outputs CSV-like results to stdout.
"""
import time
import random
import argparse
import sys
from pathlib import Path

# Utilities

def to_graphemes(s):
    # Simple grapheme approximation: Python's default iteration on strings
    # yields codepoints, which is sufficient for our micro-benchmarks.
    # For emoji sequences, we treat the provided strings as single graphemes
    # when they are composed; tests will provide explicit sequences.
    return [c for c in s]

# Implement sliding-match (naive, but non-allocating)
def sliding_index_all(text, pat):
    n, m = len(text), len(pat)
    if m == 0:
        return []
    res = []
    for i in range(0, n - m + 1):
        j = 0
        while j < m and text[i + j] == pat[j]:
            j += 1
        if j == m:
            res.append(i)
    return res

# KMP implementation

def build_prefix(pat):
    m = len(pat)
    pi = [0] * m
    k = 0
    for q in range(1, m):
        while k > 0 and pat[k] != pat[q]:
            k = pi[k - 1]
        if pat[k] == pat[q]:
            k += 1
        pi[q] = k
    return pi


def kmp_search_all(text, pat):
    n, m = len(text), len(pat)
    if m == 0:
        return []
    pi = build_prefix(pat)
    q = 0
    res = []
    for i in range(n):
        while q > 0 and (q >= m or pat[q] != text[i]):
            q = pi[q - 1]
        if pat[q] == text[i]:
            q += 1
        if q == m:
            res.append(i - m + 1)
            q = pi[q - 1]
    return res

# Scenario generators

def gen_repetitive(char, n):
    return char * n


def gen_random(alphabet, n):
    return ''.join(random.choice(alphabet) for _ in range(n))


def run_case(name, text, pat, repeat=3):
    t_gr = to_graphemes(text)
    p_gr = to_graphemes(pat)

    # Warm-up
    sliding_index_all(t_gr, p_gr)
    kmp_search_all(t_gr, p_gr)

    t0 = time.perf_counter()
    for _ in range(repeat):
        sliding_index_all(t_gr, p_gr)
    t1 = time.perf_counter()

    for _ in range(repeat):
        kmp_search_all(t_gr, p_gr)
    t2 = time.perf_counter()

    sliding_time = (t1 - t0) / repeat
    kmp_time = (t2 - t1) / repeat

    print(
        f"{name},{len(t_gr)},{len(p_gr)},{len(sliding_index_all(t_gr,p_gr))},{sliding_time:.6f},{kmp_time:.6f}"
    )


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--repeat", type=int, default=3)
    parser.add_argument("--out", type=str, default=None, help="CSV output file path")
    parser.add_argument("--sweep", action="store_true", help="Run sweep of scenarios")
    args = parser.parse_args()

    header = "case,scenario_type,text_len,pat_len,matches,sliding_s,kmp_s,repeat\n"

    if args.out:
        out_path = args.out
        Path(out_path).parent.mkdir(parents=True, exist_ok=True)
        with open(out_path, "w", encoding="utf-8") as f:
            f.write(header)

        def run_case_and_write(name, scenario_type, text, pat, repeat):
            # run and append line
            t_gr = to_graphemes(text)
            p_gr = to_graphemes(pat)

            # Warm-up
            sliding_index_all(t_gr, p_gr)
            kmp_search_all(t_gr, p_gr)

            t0 = time.perf_counter()
            for _ in range(repeat):
                sliding_index_all(t_gr, p_gr)
            t1 = time.perf_counter()

            for _ in range(repeat):
                kmp_search_all(t_gr, p_gr)
            t2 = time.perf_counter()

            sliding_time = (t1 - t0) / repeat
            kmp_time = (t2 - t1) / repeat
            matches = len(sliding_index_all(t_gr, p_gr))

            line = f"{name},{scenario_type},{len(t_gr)},{len(p_gr)},{matches},{sliding_time:.6f},{kmp_time:.6f},{repeat}\n"
            with open(out_path, "a", encoding="utf-8") as f:
                f.write(line)
            print(line.strip())

    else:
        print(header.strip())

        def run_case_and_write(name, scenario_type, text, pat, repeat):
            run_case(name, text, pat, repeat)

    if args.sweep:
        # Sweep configuration
        text_sizes = [20000, 100000, 300000]
        pat_sizes = [1, 4, 16, 64, 256, 1000]

        for n in text_sizes:
            # Random text sweep
            for m in pat_sizes:
                text = gen_random('abcd', n)
                pat = gen_random('abcd', m)
                run_case_and_write(f"random_n{n}_m{m}", "random", text, pat, repeat=args.repeat)

            # Repetitive: no-match (long pattern + trailing char)
            for m in pat_sizes:
                text = gen_repetitive('a', n)
                pat = gen_repetitive('a', m) + 'b'
                run_case_and_write(f"repetitive_nomatch_n{n}_m{m}", "repetitive_nomatch", text, pat, repeat=max(1, args.repeat//2))

            # Repetitive: many matches
            for m in pat_sizes:
                text = gen_repetitive('a', n)
                pat = gen_repetitive('a', min(m, n))
                run_case_and_write(f"repetitive_many_n{n}_m{m}", "repetitive_many", text, pat, repeat=max(1, args.repeat//2))

        # Emoji tests
        emoji = "👨‍👩‍👧‍👦"
        text = (emoji + 'x') * 4000
        pat = emoji
        run_case_and_write("emoji_pat", "emoji", text, pat, repeat=args.repeat)

    else:
        # Original set
        run_case_and_write("worst_repetitive_no_match", "repetitive_nomatch", gen_repetitive('a', 20000), gen_repetitive('a', 1000) + 'b', repeat=args.repeat)
        run_case_and_write("repetitive_many_matches", "repetitive_many", gen_repetitive('a', 20000), gen_repetitive('a', 50), repeat=args.repeat)
        run_case_and_write("random_small_pat", "random", gen_random('abcd', 20000), gen_random('abcd', 20), repeat=args.repeat)
        run_case_and_write("large_text_small_pat", "random", gen_random('abcd', 200000), "abcdab", repeat=max(1, args.repeat//2))
        run_case_and_write("emoji_pat", "emoji", ("👨‍👩‍👧‍👦" + 'x') * 2000, "👨‍👩‍👧‍👦", repeat=args.repeat)


if __name__ == "__main__":
    main()
