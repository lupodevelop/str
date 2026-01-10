#!/usr/bin/env bash
set -euo pipefail
HERE=$(cd "$(dirname "$0")" && pwd)
REPO_ROOT=$(cd "$HERE/.." && pwd)
OUT_DIR="$REPO_ROOT/benchmark/results"
mkdir -p "$OUT_DIR"
BENCH_ITER=200000

# Background execution support: handle --background / --bg flag
BG=false
NEWARGS=()
for arg in "$@"; do
  case "$arg" in
    --background|--bg)
      BG=true
      ;;
    *)
      NEWARGS+=("$arg")
      ;;
  esac
done
# Replace positional args with filtered args
set -- "${NEWARGS[@]}"

if [ "$BG" = true ]; then
  LOG="$OUT_DIR/bench.log"
  nohup "$0" "$@" > "$LOG" 2>&1 &
  BG_PID=$!
  # Save pid and report immediately
  printf "%s" "$BG_PID" > "$OUT_DIR/bench.pid"
  echo "Started benchmark in background (pid $BG_PID), logs: $LOG"
  exit 0
fi

usage() {
  cat <<EOF
Usage: $(basename "$0") <command> [options]

Commands:
  compile        Run compile measurements
  runtime        Run runtime benchmarks
  micro          Run micro benchmarks (word bits 32/64/128)
  export         Export results to CSV
  all            Run compile, runtime, micro, then export
  status         Show background benchmark status and log path
  stop           Stop background benchmark (using saved PID)
  help           Show this help

Options:
  --no-export    When used with 'all', skip CSV export
EOF
} 

current_ts() { date -u +"%Y-%m-%dT%H:%M:%SZ"; }

run_compile() {
  TARGET="$REPO_ROOT/src/str/internal/generated_translit_pages.gleam"
  OUT="$OUT_DIR/compile_results.txt"

  echo "===== baseline_table =====" >> "$OUT"
  python3 "$REPO_ROOT/scripts/generate_bitarray_tables.py" --mode table --out "$TARGET" --name translit_pages
  # Clean build to make compile time comparable
  rm -rf _build
  start=$(date +%s.%N)
  gleam test --target erlang >/dev/null || true
  end=$(date +%s.%N)
  dur=$(python3 - <<PY
s=float('$end')-float('$start')
print(f"{s:.3f}")
PY
)
  ts=$(current_ts)
  echo "baseline_table,compile_seconds,$dur" >> "$OUT"
  echo "baseline_table,unit,s" >> "$OUT"
  echo "baseline_table,timestamp,$ts" >> "$OUT"

  echo "===== optimized_pages =====" >> "$OUT"
  python3 "$REPO_ROOT/scripts/generate_bitarray_tables.py" --mode pages --out "$TARGET" --name translit_pages --word-bits 64
  rm -rf _build
  start=$(date +%s.%N)
  gleam test --target erlang >/dev/null || true
  end=$(date +%s.%N)
  dur=$(python3 - <<PY
s=float('$end')-float('$start')
print(f"{s:.3f}")
PY
)
  ts=$(current_ts)
  echo "optimized_pages,compile_seconds,$dur" >> "$OUT"
  echo "optimized_pages,unit,s" >> "$OUT"
  echo "optimized_pages,timestamp,$ts" >> "$OUT"

  echo "Compile measurements written to $OUT"
}

run_runtime() {
  OUT="$OUT_DIR/runtime_results.txt"
  BENCH_ERL="$REPO_ROOT/benchmark/runtime/bench.erl"
  TRANSLIT="$REPO_ROOT/src/str/internal/translit_pure.gleam"
  FALLBACK="$REPO_ROOT/benchmark/runtime/translit_pure_fold.gleam"
  GEN="$REPO_ROOT/scripts/generate_bitarray_tables.py"

  : > "$OUT"

  compile_bench() {
    mkdir -p "$REPO_ROOT/benchmark/runtime/bench_beam"
    erlc -o "$REPO_ROOT/benchmark/runtime/bench_beam" "$BENCH_ERL"
  }

  run_bench() {
    label="$1"
    iter="$2"
    ts=$(current_ts)
    echo "===== $label =====" >> "$OUT"
    gleam build --target erlang >/dev/null
    result_ms=$(erl -noshell -pa build/dev/erlang/str/ebin -pa build/dev/erlang/gleam_stdlib/ebin -pa "$REPO_ROOT/benchmark/runtime/bench_beam" -eval "bench:run($iter), init:stop()." | tail -n1)
    echo "$label,iterations,$iter" >> "$OUT"
    echo "$label,time_ms,$result_ms" >> "$OUT"
    echo "$label,unit,ms" >> "$OUT"
    echo "$label,timestamp,$ts" >> "$OUT"
    echo "$label runtime: $result_ms ms"
  }

  # Backup current translit_pure
  cp "$TRANSLIT" "$TRANSLIT.bak"

  echo "Switching to fallback transliterate (fold-based)"
  cp "$FALLBACK" "$TRANSLIT"
  compile_bench
  run_bench "fallback_fold" 20000

  echo "Restoring optimized transliterate (pages-based)"
  mv "$TRANSLIT.bak" "$TRANSLIT"

  echo "Generating bench modules"
  bash "$REPO_ROOT/benchmark/runtime/generate_bench_modules.sh"
  gleam build --target erlang >/dev/null

  erlc -o "$REPO_ROOT/benchmark/runtime/bench_beam" "$REPO_ROOT/benchmark/runtime/bench.erl"
  erlc -o "$REPO_ROOT/benchmark/runtime/bench_beam" "$REPO_ROOT/benchmark/runtime/bench_bench.erl"

  ts=$(current_ts)
  result_table=$(erl -noshell -pa build/dev/erlang/str/ebin -pa build/dev/erlang/gleam_stdlib/ebin -pa "$REPO_ROOT/benchmark/runtime/bench_beam" -eval "bench_bench:run_table($BENCH_ITER), init:stop()." | tail -n1)
  echo "table_lookup,iterations,$BENCH_ITER" >> "$OUT"
  echo "table_lookup,time_ms,$result_table" >> "$OUT"
  echo "table_lookup,unit,ms" >> "$OUT"
  echo "table_lookup,timestamp,$ts" >> "$OUT"
  echo "table lookup runtime: $result_table ms"

  ts=$(current_ts)
  result_pages=$(erl -noshell -pa build/dev/erlang/str/ebin -pa build/dev/erlang/gleam_stdlib/ebin -pa "$REPO_ROOT/benchmark/runtime/bench_beam" -eval "bench_bench:run_pages($BENCH_ITER), init:stop()." | tail -n1)
  echo "pages_lookup,iterations,$BENCH_ITER" >> "$OUT"
  echo "pages_lookup,time_ms,$result_pages" >> "$OUT"
  echo "pages_lookup,unit,ms" >> "$OUT"
  echo "pages_lookup,timestamp,$ts" >> "$OUT"
  echo "pages lookup runtime: $result_pages ms"

  # Clean bench beam
  rm -rf "$REPO_ROOT/benchmark/runtime/bench_beam"

  echo "Runtime results written to $OUT"
}

run_micro() {
  OUT="$OUT_DIR/microbench_results.csv"
  GEN="$REPO_ROOT/scripts/generate_bitarray_tables.py"
  PAIRS_IN="$REPO_ROOT/src/str/internal/generated_translit_pairs.gleam"
  TARGET="$REPO_ROOT/src/str/internal/generated_translit_pages.gleam"

  echo "mode,word_bits,metric,iterations,value,unit,timestamp" > "$OUT"

  # backup
  if [ -f "$TARGET" ]; then cp "$TARGET" "$TARGET.bak"; fi

  for wb in 32 64 128; do
    echo "Running micro-bench for word_bits=$wb"
    ts=$(current_ts)
    python3 "$GEN" --mode pages --out "$TARGET" --name translit_pages --word-bits "$wb"
    # compile measure
    rm -rf _build
    start=$(date +%s.%N)
    gleam test --target erlang >/dev/null || true
    end=$(date +%s.%N)
    dur=$(python3 - <<PY
s=float('$end')-float('$start')
print(f"{s:.3f}")
PY
)
    echo "pages,$wb,compile_seconds,,$dur,s,$ts" >> "$OUT"

    python3 "$GEN" --mode pages --out "$REPO_ROOT/src/str/internal/generated_translit_pages_bench.gleam" --name translit_pages_bench --word-bits "$wb" --input "$PAIRS_IN"
    gleam build --target erlang >/dev/null
    mkdir -p "$REPO_ROOT/benchmark/runtime/bench_beam"
    erlc -o "$REPO_ROOT/benchmark/runtime/bench_beam" "$REPO_ROOT/benchmark/runtime/bench.erl"
    erlc -o "$REPO_ROOT/benchmark/runtime/bench_beam" "$REPO_ROOT/benchmark/runtime/bench_bench.erl"
    ts2=$(current_ts)
    result_pages=$(erl -noshell -pa build/dev/erlang/str/ebin -pa build/dev/erlang/gleam_stdlib/ebin -pa "$REPO_ROOT/benchmark/runtime/bench_beam" -eval "bench_bench:run_pages($BENCH_ITER), init:stop()." | tail -n1)
    echo "pages,$wb,time_ms,$BENCH_ITER,${result_pages},ms,$ts2" >> "$OUT"

    # table baseline
    python3 "$GEN" --mode table --out "$REPO_ROOT/src/str/internal/generated_translit_table_bench.gleam" --name translit_table_bench --input "$PAIRS_IN"
    gleam build --target erlang >/dev/null
    ts3=$(current_ts)
    result_table=$(erl -noshell -pa build/dev/erlang/str/ebin -pa build/dev/erlang/gleam_stdlib/ebin -pa "$REPO_ROOT/benchmark/runtime/bench_beam" -eval "bench_bench:run_table($BENCH_ITER), init:stop()." | tail -n1)
    echo "table,${wb},time_ms,$BENCH_ITER,${result_table},ms,${ts3}" >> "$OUT"

    rm -rf "$REPO_ROOT/benchmark/runtime/bench_beam"
  done

  if [ -f "$TARGET.bak" ]; then mv "$TARGET.bak" "$TARGET"; fi

  echo "Microbench results written to $OUT"
}

export_csv() {
  OUT="$OUT_DIR/bench_results_flat.csv"
  : > "$OUT"
  echo "label,iterations,time_ms,unit,timestamp,compile_seconds,source,file" > "$OUT"

  process_file() {
    file="$1"
    src=$(basename "$file" .txt)
    label=""
    iterations=""
    time_ms=""
    unit=""
    timestamp=""
    compile_seconds=""

    while IFS= read -r line || [[ -n "$line" ]]; do
      if [[ "$line" =~ ^=====\ (.*)\ =====$ ]]; then
        if [[ -n "$label" ]]; then
          printf '%s,%s,%s,%s,%s,%s,%s,%s\n' "$label" "$iterations" "$time_ms" "$unit" "$timestamp" "$compile_seconds" "$src" "$file" >> "$OUT"
        fi
        label="${BASH_REMATCH[1]}"
        iterations=""
        time_ms=""
        unit=""
        timestamp=""
        compile_seconds=""
      elif [[ "$line" =~ ^([^,]+),([^,]+),(.+)$ ]]; then
        key="${BASH_REMATCH[2]}"
        val="${BASH_REMATCH[3]}"
        case "$key" in
          iterations) iterations="$val" ;;
          time_ms) time_ms="$val" ;;
          unit) unit="$val" ;;
          timestamp) timestamp="$val" ;;
          compile_seconds) compile_seconds="$val" ;;
        esac
      fi
    done < "$file"
    if [[ -n "$label" ]]; then
      printf '%s,%s,%s,%s,%s,%s,%s,%s\n' "$label" "$iterations" "$time_ms" "$unit" "$timestamp" "$compile_seconds" "$src" "$file" >> "$OUT"
    fi
  }

  process_file "$OUT_DIR/compile_results.txt"
  process_file "$OUT_DIR/runtime_results.txt"
  echo "CSV export written to $OUT"
}

run_status() {
  PID_FILE="$OUT_DIR/bench.pid"
  LOG="$OUT_DIR/bench.log"
  if [ ! -f "$PID_FILE" ]; then
    echo "No background benchmark running (no pid file at $PID_FILE)"
    return 0
  fi
  PID=$(cat "$PID_FILE")
  if kill -0 "$PID" >/dev/null 2>&1; then
    echo "Benchmark running (pid $PID)"
  else
    echo "No process with pid $PID. It may have exited."
  fi
  echo "Log file: $LOG"
}

run_stop() {
  PID_FILE="$OUT_DIR/bench.pid"
  if [ ! -f "$PID_FILE" ]; then
    echo "No background PID file at $PID_FILE"
    return 0
  fi
  PID=$(cat "$PID_FILE")
  if kill -0 "$PID" >/dev/null 2>&1; then
    echo "Stopping benchmark (pid $PID)..."
    kill "$PID" >/dev/null 2>&1 || true
    # wait a few seconds for graceful stop
    for i in 1 2 3 4 5; do
      if kill -0 "$PID" >/dev/null 2>&1; then
        sleep 1
      else
        break
      fi
    done
    if kill -0 "$PID" >/dev/null 2>&1; then
      echo "Process did not exit; sending SIGKILL"
      kill -9 "$PID" >/dev/null 2>&1 || true
    fi
    echo "Stopped."
  else
    echo "Process $PID not running."
  fi
  rm -f "$PID_FILE"
}

# Dispatch
case "${1:-help}" in
  compile) run_compile ;;
  runtime) run_runtime ;;
  micro) run_micro ;;
  export) export_csv ;;
  status) run_status ;;
  stop) run_stop ;;
  all)
    NO_EXPORT=false
    shift || true
    while [ "$#" -gt 0 ]; do case "$1" in --no-export) NO_EXPORT=true; shift;; *) shift;; esac; done
    run_compile
    run_runtime
    run_micro
    if [ "$NO_EXPORT" = false ]; then export_csv; fi ;;
  help|*) usage ;;
esac
