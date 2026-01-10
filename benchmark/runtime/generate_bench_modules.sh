#!/usr/bin/env bash
set -euo pipefail
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
GEN=${ROOT}/scripts/generate_bitarray_tables.py
IN=${ROOT}/src/str/internal/generated_translit_pairs.gleam

# Generate a table-based bench module
python3 "$GEN" --mode table --out "$ROOT/src/str/internal/generated_translit_table_bench.gleam" --name translit_table_bench --input "$IN"

# Generate a pages-based bench module
python3 "$GEN" --mode pages --out "$ROOT/src/str/internal/generated_translit_pages_bench.gleam" --name translit_pages_bench --word-bits 64 --input "$IN"

echo "Generated bench modules: generated_translit_table_bench.gleam, generated_translit_pages_bench.gleam"