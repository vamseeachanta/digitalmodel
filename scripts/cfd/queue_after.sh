#!/usr/bin/env bash
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/cfd_help.sh"
# Queue a detached command behind marker/ledger readiness and host solver idleness.
# Usage: queue_after.sh [status|cancel] --root DIR --name NAME [queue options]
set -euo pipefail
set -o pipefail

die() { echo "queue_after: FATAL: $*" >&2; exit 1; }
stamp() { date -u +%FT%TZ; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/cfd_chain.sh"
ROOT="$(cfd_campaign_root)"
action=start
case "${1:-}" in status|cancel|_wait) action=$1; shift;; esac
name= wait_files= wait_regex= ledger= run= cwd= no_solver="interFoam,simpleFoam"
while [ "$#" -gt 0 ]; do
  case "$1" in
    --root) ROOT=${2:?}; shift 2;; --name) name=${2:?}; shift 2;;
    --wait) wait_files=${2:?}; shift 2;; --wait-ledger-line) wait_regex=${2:?}; shift 2;;
    --ledger) ledger=${2:?}; shift 2;; --no-solver) no_solver=${2:?}; shift 2;;
    --run) run=${2:?}; shift 2;; --cwd) cwd=${2:?}; shift 2;;
    *) die "unknown argument: $1";;
  esac
done
status_dir="$ROOT/status"; mkdir -p "$status_dir"
safe_name=${name//[^A-Za-z0-9_.-]/_}

if [ "$action" = status ]; then
  printf 'QUEUE\tSTATE\tPID\n'
  for f in "$status_dir"/QUEUE_*_WAITING; do
    [ -f "$f" ] || continue
    q=${f##*/QUEUE_}; q=${q%_WAITING}
    pid=$(sed -n 's/.*pid=\([0-9][0-9]*\).*/\1/p' "$f" | tail -1)
    state=waiting
    if [ -f "$status_dir/QUEUE_${q}_LAUNCHED" ]; then
      state=launched
      pid=$(sed -n 's/.*pid=\([0-9][0-9]*\).*/\1/p' "$status_dir/QUEUE_${q}_LAUNCHED" | tail -1)
    fi
    [ -f "$status_dir/QUEUE_${q}_CANCELLED" ] && state=cancelled
    printf '%s\t%s\t%s\n' "$q" "$state" "${pid:--}"
  done
  exit 0
fi

[ -n "$name" ] || die "--name is required"
waiting="$status_dir/QUEUE_${safe_name}_WAITING"
released="$status_dir/QUEUE_${safe_name}_RELEASED"
launched="$status_dir/QUEUE_${safe_name}_LAUNCHED"
cancelled="$status_dir/QUEUE_${safe_name}_CANCELLED"

if [ "$action" = cancel ]; then
  [ -f "$waiting" ] || die "queue $name has no WAITING marker"
  [ ! -f "$launched" ] || die "queue $name has already launched"
  pid=$(sed -n 's/.*pid=\([0-9][0-9]*\).*/\1/p' "$waiting" | tail -1)
  [ -n "$pid" ] || die "queue $name WAITING marker has no pid"
  if [ -d "/proc/$pid" ]; then
    cmd=$(tr '\0' ' ' < "/proc/$pid/cmdline" 2>/dev/null || true)
    case "$cmd" in *queue_after.sh*"_wait"*"--name $name"*) kill "$pid";;
      *) die "recorded pid $pid is not the waiting queue $name";; esac
  fi
  printf '%s pid=%s queue=%s\n' "$(stamp)" "$pid" "$name" > "$cancelled"
  exit 0
fi

if [ "$action" = start ]; then
  [ -n "$run" ] || die "--run is required"
  [ -n "$wait_files$wait_regex" ] || die "--wait or --wait-ledger-line is required"
  [ -z "$wait_regex" ] || [ -n "$ledger" ] || die "--ledger is required with --wait-ledger-line"
  if [ -f "$waiting" ] && [ ! -f "$launched" ] && [ ! -f "$cancelled" ]; then
    old=$(sed -n 's/.*pid=\([0-9][0-9]*\).*/\1/p' "$waiting" | tail -1)
    [ -z "$old" ] || [ ! -d "/proc/$old" ] || die "queue $name is already waiting (pid $old)"
  fi
  rm -f "$waiting" "$released" "$launched" "$cancelled"
  args=( _wait --root "$ROOT" --name "$name" --run "$run" --no-solver "$no_solver" )
  [ -z "$wait_files" ] || args+=(--wait "$wait_files")
  [ -z "$wait_regex" ] || args+=(--wait-ledger-line "$wait_regex")
  [ -z "$ledger" ] || args+=(--ledger "$ledger")
  [ -z "$cwd" ] || args+=(--cwd "$cwd")
  setsid nohup "$0" "${args[@]}" > "$status_dir/QUEUE_${safe_name}.out" 2>&1 < /dev/null &
  pid=$!
  printf '%s pid=%s queue=%s\n' "$(stamp)" "$pid" "$name" > "$waiting"
  queue_ledger=${ledger:-$ROOT/$safe_name.log}
  printf '%s QUEUE %s WAITING pid=%s\n' "$(stamp)" "$name" "$pid" >> "$queue_ledger"
  echo "queue_after: $name waiting as pid $pid"
  exit 0
fi

# _wait worker. Readiness is ANY marker or a matching ledger line.
trap 'exit 143' TERM INT
[ -n "$ledger" ] || ledger="$ROOT/$safe_name.log"
[ -n "$cwd" ] || cwd="$ROOT"
ready=false
while [ "$ready" = false ]; do
  IFS=, read -r -a markers <<< "$wait_files"
  for marker in "${markers[@]}"; do [ -n "$marker" ] && [ -e "$marker" ] && ready=true; done
  [ "$ready" = true ] || [ -z "$wait_regex" ] || ! [ -f "$ledger" ] || ! grep -Eq "$wait_regex" "$ledger" || ready=true
  [ "$ready" = true ] || sleep 1
done
while :; do
  busy=false
  IFS=, read -r -a solvers <<< "$no_solver"
  for solver in "${solvers[@]}"; do
    [ -n "$solver" ] || continue
    pgrep -x "$solver" >/dev/null 2>&1 && busy=true
  done
  [ "$busy" = false ] && break
  sleep 2
done
printf '%s queue=%s\n' "$(stamp)" "$name" > "$released"
printf '%s QUEUE %s RELEASED\n' "$(stamp)" "$name" >> "$ledger"
mkdir -p "$cwd"
(cd "$cwd" && setsid nohup bash -c "$run" > "$status_dir/QUEUE_${safe_name}.run.out" 2>&1 < /dev/null) &
child=$!
printf '%s pid=%s queue=%s\n' "$(stamp)" "$child" "$name" > "$launched"
printf '%s QUEUE %s LAUNCHED pid=%s\n' "$(stamp)" "$name" "$child" >> "$ledger"
