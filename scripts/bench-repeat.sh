#!/usr/bin/env zsh
set -euo pipefail

repeats=5
iters=1000

targets=(
  "program/10k/eachm"
  "program/10k+1/eachm"
)

print_help() {
  cat <<'EOF'
Usage: ./scripts/bench-repeat.sh [options]

Options:
  --repeats N   Number of runs per benchmark (default: 5)
  --iters N     Benchmark iterations passed to criterion (default: 1000)
  --name NAME   Benchmark glob name; can be passed multiple times
  --help        Show this help
EOF
}

custom_targets=()
while (( $# > 0 )); do
  case "$1" in
    --repeats)
      shift
      repeats="${1:?missing value for --repeats}"
      ;;
    --iters)
      shift
      iters="${1:?missing value for --iters}"
      ;;
    --name)
      shift
      custom_targets+=("${1:?missing value for --name}")
      ;;
    --help)
      print_help
      exit 0
      ;;
    *)
      echo "unknown option: $1" >&2
      print_help >&2
      exit 1
      ;;
  esac
  shift
done

if (( ${#custom_targets[@]} > 0 )); then
  targets=("${custom_targets[@]}")
fi

median_lower() {
  if (( $# == 0 )); then
    echo "n/a"
    return
  fi
  printf '%s\n' "$@" | sort -n | awk '
    { a[++n] = $1 }
    END {
      if (n == 0) {
        print "n/a"
      } else {
        idx = int((n + 1) / 2)
        print a[idx]
      }
    }
  '
}

run_target() {
  local name="$1"
  local -a allocs=()
  local -a elapseds=()
  local i out alloc elapsed

  echo "== ${name} =="
  for (( i = 1; i <= repeats; i++ )); do
    out="$(
      cabal bench corn-bench \
        --ghc-options=-O2 \
        --benchmark-options="-m glob ${name} --iters ${iters} +RTS -N -s -RTS" \
        2>&1 || true
    )"

    alloc="$(printf '%s\n' "$out" \
      | rg -m1 'bytes allocated in the heap' \
      | sed -E 's/^[[:space:]]*([0-9,]+) bytes allocated in the heap.*$/\1/' \
      | tr -d ',')"

    elapsed="$(printf '%s\n' "$out" \
      | rg -m1 'Total[[:space:]]+time' \
      | sed -E 's/.*\(([[:space:]]*[0-9.]+)s elapsed\).*/\1/' \
      | tr -d ' ')"

    if [[ -z "$alloc" || -z "$elapsed" ]]; then
      echo "run ${i}: failed to parse benchmark output" >&2
      exit 1
    fi

    allocs+=("$alloc")
    elapseds+=("$elapsed")
    echo "run ${i}: alloc=${alloc} elapsed=${elapsed}s"
  done

  echo "median: alloc=$(median_lower "${allocs[@]}") elapsed=$(median_lower "${elapseds[@]}")s"
  echo
}

for name in "${targets[@]}"; do
  run_target "$name"
done
