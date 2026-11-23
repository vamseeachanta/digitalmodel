#!/usr/bin/env bash
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/cfd_help.sh"
# Stage 5: solve the configured KCS levels, in registry order.
#
# Sequential by design. The box has 8 physical cores and the committed
# benchmark shows efficiency collapsing above 8 ranks, so running two levels
# concurrently at 4 ranks each would be slower in total AND would make each
# level's timing uninterpretable.
#
# Each solve is supervised by a host-side poller so budget enforcement does not
# depend on the flapping link being up.
#
#   usage: solve_chain.sh [case ...]
#
# With no arguments it solves every case in DM_CFD_CONFIG. The original ran a
# literal `for case in kcs_production kcs_companion`, which silently ignored
# any level added later; the registry now carries four.
#
# NO `set -e`: each driver's exit status is captured and logged before the
# chain decides what to do with it, and `wait` legitimately returns non-zero.
set -uo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/cfd_chain.sh disable=SC1090,SC1091
. "$SELF_DIR/lib/cfd_chain.sh"

# Validate the root HERE, in this shell: inside a command substitution cfd_die
# would only kill the subshell and leave the path empty.
cfd_root > /dev/null
ROOT="$(cfd_root)"

CFD_LOG="${CFD_LOG:-$ROOT/solve_chain.log}"
CFD_MARKER="${CFD_MARKER:-$ROOT/solve_chain.marker}"
export CFD_LOG CFD_MARKER

DRIVER="${CFD_DRIVER:-$SELF_DIR/stage45_driver.sh}"
POLLER="${CFD_POLLER:-$SELF_DIR/poller.sh}"

# Seconds between releasing the driver and attaching the poller: long enough
# for the driver to have written detached_run.json, which is the only thing the
# poller can find the run by.
LAUNCH_GRACE_SECONDS="${CFD_LAUNCH_GRACE_SECONDS:-30}"

# Terminal marker on BOTH outcomes -- see stage45_driver.sh for why silence is
# not evidence.
_marked=0
_on_exit() {
  local rc=$?
  [ "$_marked" = "1" ] && return 0
  cfd_marker_fail "solve chain exited rc=$rc without completing"
  return 0
}
trap _on_exit EXIT

[ -x "$DRIVER" ] || cfd_die "no executable driver at $DRIVER (set CFD_DRIVER)"
# Checked before the first solve rather than per case: the budget is the only
# bound on a runaway, so starting an unsupervised 60 h solve is worse than not
# starting one.
[ -x "$POLLER" ] || cfd_die "no executable poller at $POLLER (set CFD_POLLER); refusing to start an unsupervised solve"

if [ "$#" -gt 0 ]; then
  cases=("$@")
else
  mapfile -t cases < <(cfd_cases)
fi
[ "${#cases[@]}" -gt 0 ] || cfd_die "no cases in ${DM_CFD_CONFIG:-<unset>}"

cfd_say "SOLVE CHAIN START (${#cases[@]} level(s): ${cases[*]})"

for case_name in "${cases[@]}"; do
  case_dir="$(cfd_case_dir "$case_name")"
  [ -d "$case_dir" ] || cfd_die "no such case directory: $case_dir"

  # Budgets come from the registry. The budget is a RUNAWAY BOUND, not a
  # schedule: it should never fire on a healthy run, and if it does, something
  # is wrong and stopping is correct. The original kept them in a bash
  # associative array that lived only on the solve host, where the numbers
  # behind a 60 h bound could not be reviewed or reproduced.
  budget="$(cfd_case_get "$case_name" budget_hours)"
  [ -n "$budget" ] || cfd_die "no budget_hours for $case_name"

  cells="$(cfd_mesh_cells "$case_dir")" || cells=""
  cfd_say "=== $case_name: ${cells:-unknown} cells, budget ${budget}h ==="

  cfd_say "$case_name solve begin"
  "$DRIVER" solve "$case_name" "$budget" >> "$CFD_LOG" 2>&1 &
  DRIVER_PID=$!

  # Give the driver a moment to write its launch record, then supervise.
  sleep "$LAUNCH_GRACE_SECONDS"
  # setsid + nohup so the poller outlives this chain and any ssh that started
  # it; `< /dev/null` so a detached child cannot consume the stdin this script
  # is still being fed from.
  setsid nohup "$POLLER" "$case_dir" loop > /dev/null 2>&1 < /dev/null &

  wait "$DRIVER_PID"
  rc=$?
  cfd_say "$case_name solve end rc=$rc"

  iters="$(grep -c "^Time = " "$case_dir/log.interFoam" 2>/dev/null)" || iters=0
  cfd_say "$case_name iterations reached: $iters"
  if [ -f "$case_dir/detached_run.terminated.json" ]; then
    cfd_say "$case_name WAS TERMINATED BY THE POLLER -- budget exceeded"
  fi
  if [ "$rc" -ne 0 ]; then
    cfd_say "FATAL $case_name rc=$rc -- stopping the chain rather than solving the next level"
    cfd_marker_fail "$case_name failed rc=$rc after $iters iterations"
    _marked=1
    exit "$rc"
  fi
done

cfd_say "SOLVE CHAIN COMPLETE -- ${#cases[@]} level(s) solved"
cfd_marker_ok "solved ${cases[*]}"
_marked=1
