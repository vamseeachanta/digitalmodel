#!/usr/bin/env bash
# Unattended solver launcher for one case (#1173, #2023).
#
# This is ONE generalised launcher folded from two host-side originals that
# were the same job on two machines: kcs_cases/kcs_fine/runsolve.sh on
# lane-A (foreground, mapped initial field, 8 ranks) and a launcher on
# lane-B (detached, cold start, 16 ranks). They differed only in case,
# rank count and whether they blocked -- all three are now arguments.
#
#   usage: solve_case.sh <case-name> [--foreground]
#   env:   DM_CFD_ROOT     case root (required; no default, see cfd_root)
#          DM_CFD_CONFIG   case registry (required) -- supplies ranks, budget
#          DM_CFD_SOLVER   solver executable name (default interFoam)
#          DM_CFD_ARM_WATCHER  arm the ITTC watcher (default 1)
#          DM_CFD_ARM_POLLER   arm the budget poller  (default 1)
#          MIN_ITER ...    passed through to the watcher (see ittc_watch.sh)
#
# DELIVER THIS AS A FILE, never piped to `bash -s`. mpirun reads and closes
# stdin: when the lane-B original was piped over ssh, an mpirun swallowed
# the remainder of the script -- the solver launch never executed, ssh
# returned 0, and the lane reported OK while nothing was running. Every mpirun
# below also gets an explicit `< /dev/null` so it cannot consume anything.
#
# It runs interFoam DIRECTLY rather than through the stage driver: the driver
# re-runs restore0Dir/setFields before solving, which would overwrite a mapped
# initial field. On the original run that step was skipped only because
# runApplication refuses to repeat a stage whose log exists -- an accident,
# not a guarantee.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/cfd/lib/cfd_chain.sh
source "$SCRIPT_DIR/lib/cfd_chain.sh"

CASE_NAME="${1:?usage: solve_case.sh <case-name> [--foreground]}"
shift
DETACH=1
for arg in "$@"; do
  case "$arg" in
    --foreground) DETACH=0 ;;
    --detach)     DETACH=1 ;;
    *) cfd_die "unknown option '$arg' (expected --foreground or --detach)" ;;
  esac
done

CASE="$(cfd_case_dir "$CASE_NAME")"
[ -d "$CASE" ] || cfd_die "case directory does not exist: $CASE"
SOLVER="${DM_CFD_SOLVER:-interFoam}"
SOLVER_LOG="$CASE/log.$SOLVER"
CFD_LOG="${CFD_LOG:-$CASE/solve_case.log}"
CFD_MARKER="${CFD_MARKER:-$CASE/solve_case.marker}"
export CFD_LOG CFD_MARKER

_terminal_ok()   { CFD_TERMINAL=1; cfd_marker_ok "$*"; cfd_say "OK $*"; }
_terminal_fail() { CFD_TERMINAL=1; cfd_marker_fail "$*"; cfd_say "FAILED $*"; }
trap 'rc=$?; [ "$rc" -eq 0 ] || [ -n "${CFD_TERMINAL:-}" ] || cfd_marker_fail "launcher exited rc=$rc"' EXIT

# --------------------------------------------------------------------------- #
# The ITTC watcher's stop is INERT without a matching abort function object.
#
# The watcher only touches a file. Something in the case has to read it, and
# that something is an `abort` functionObject in system/controlDict. A case on
# #1173 shipped without the block, so the trigger did nothing and the run went
# all the way to endTime -- days of compute past the point the criterion had
# been met. Verify the wiring BEFORE launching, so a mis-wired case costs one
# second instead of a week.
_require_abort_wiring() {
  local cd_file="$CASE/system/controlDict"
  [ -f "$cd_file" ] || cfd_die "no controlDict at $cd_file: cannot verify the abort wiring"

  local missing=""
  grep -Eq 'type[[:space:]]+abort;' "$cd_file" || missing="$missing 'type abort;'"
  grep -q 'ITTC_CONVERGED' "$cd_file"          || missing="$missing 'file .../ITTC_CONVERGED'"
  grep -Eq 'action[[:space:]]+writeNow;' "$cd_file" || missing="$missing 'action writeNow;'"

  if [ -n "$missing" ]; then
    cfd_say "controlDict is missing:$missing"
    cfd_say "add to system/controlDict functions {}:"
    # shellcheck disable=SC2016  # $FOAM_CASE must reach the user UNexpanded:
    # it is OpenFOAM's own variable, resolved by the dictionary reader.
    cfd_say '    ittcStop { type abort; file "$FOAM_CASE/ITTC_CONVERGED"; action writeNow; }'
    _terminal_fail "case '$CASE_NAME' has no abort function object wired to ITTC_CONVERGED"
    cfd_die "refusing to arm the ITTC watcher against a case whose abort wiring is missing:$missing"
  fi
  cfd_say "abort wiring verified in $cd_file"
}

# --------------------------------------------------------------------------- #
RANKS="$(cfd_case_get "$CASE_NAME" ranks)"
BUDGET_HOURS="$(cfd_case_get "$CASE_NAME" budget_hours)"
ARM_WATCHER="${DM_CFD_ARM_WATCHER:-1}"
ARM_POLLER="${DM_CFD_ARM_POLLER:-1}"

cfd_say "launching '$CASE_NAME': solver=$SOLVER ranks=$RANKS budget=${BUDGET_HOURS}h detach=$DETACH"

# cfd_load_openfoam saves and restores the caller's `set -e`/`set -u` around
# the OpenFOAM bashrc, which dereferences unset variables and calls
# pop_var_context. Sourcing it directly under either flag aborts the shell --
# both have bitten this chain, separately.
cfd_load_openfoam

if [ "$ARM_WATCHER" = "1" ]; then
  _require_abort_wiring
fi

# A leftover trigger from the PREVIOUS run stops the new solve at its first
# write. Clear it before anything starts.
rm -f "$CASE/ITTC_CONVERGED"

SUBDOMAINS="$(find "$CASE" -maxdepth 1 -type d -name 'processor*' | wc -l)"
cfd_say "subdomains present: $SUBDOMAINS (ranks requested: $RANKS)"
[ "$SUBDOMAINS" -eq "$RANKS" ] || cfd_die "case is decomposed into $SUBDOMAINS subdomains but $RANKS ranks were requested; decomposePar first"

cd "$CASE"

# --------------------------------------------------------------------------- #
# Launch. Both forms carry `< /dev/null` because mpirun reads and closes
# stdin; the detached form additionally survives the ssh session that started
# it, which is how a multi-day solve outlives a flapping link.
STARTED_EPOCH="$(date +%s)"
if [ "$DETACH" = "1" ]; then
  setsid nohup mpirun -np "$RANKS" "$SOLVER" -parallel < /dev/null > "$SOLVER_LOG" 2>&1 &
else
  mpirun -np "$RANKS" "$SOLVER" -parallel < /dev/null > "$SOLVER_LOG" 2>&1 &
fi
SOLVER_PID=$!

# --------------------------------------------------------------------------- #
# Confirm it actually started. cfd_solver_ranks is `pgrep -xc` on the
# EXECUTABLE name; never `pgrep -f "$SOLVER"`, whose pattern also matches the
# ssh command line carrying it, so this check would pass on a dead solve.
sleep "${DM_CFD_START_GRACE_SECONDS:-30}"
# `| tail -1` is not cosmetic. cfd_solver_ranks is `pgrep -xc NAME || echo 0`,
# and `pgrep -xc` PRINTS "0" and ALSO exits 1 when nothing matches, so the
# fallback appends a second line and the helper returns "0\n0". Unnormalised,
# `[ "$NRANKS" -lt 1 ]` errors with "integer expression expected", and inside
# an `if` that error reads as FALSE -- i.e. as a solver that started.
NRANKS="$(cfd_solver_ranks "$SOLVER" | tail -1)"
if [ "$NRANKS" -lt 1 ]; then
  cfd_say "--- tail of $SOLVER_LOG"
  tail -25 "$SOLVER_LOG" 2>/dev/null >> "$CFD_LOG" || true
  _terminal_fail "solver did not start (0 ranks after the start grace)"
  exit 1
fi
cfd_say "solver running: $NRANKS ranks, pid $SOLVER_PID"

# The REAL process group of the launched job, read from ps.
#
# NOT `$$`. A job started with `&` from a non-interactive shell does not
# become a group leader -- it inherits this script's group -- so a record that
# writes `$$` names the LAUNCHER's group, and a budget kill against it
# decapitates the supervisor instead of the job. The original record did
# exactly that, on every run. Under setsid the group is genuinely the job's;
# recording what ps reports makes the distinction explicit either way, and
# poller.sh refuses the target if it turns out to be its own or an ancestor's.
SOLVER_PGID="$(ps -o pgid= -p "$SOLVER_PID" 2>/dev/null | tr -d ' ')"
[ -n "$SOLVER_PGID" ] || cfd_die "cannot read the process group of pid $SOLVER_PID; the budget could not be enforced"
cfd_say "solver process group: $SOLVER_PGID (this launcher's own: $(ps -o pgid= -p $$ | tr -d ' '))"

# Record the run so poller.sh can enforce the budget from the registry.
python3 - "$CASE/detached_run.json" "$SOLVER_PID" "$SOLVER_PGID" "$STARTED_EPOCH" "$BUDGET_HOURS" "$CASE_NAME" "$RANKS" "$SOLVER" <<'PY'
import json, sys
out, pid, pgid, started, budget, case, ranks, solver = sys.argv[1:9]
json.dump({
    "pid": int(pid),
    "pgid": int(pgid),
    "started_epoch": int(started),
    "wallclock_budget_hours": float(budget),
    "case": case,
    "ranks": int(ranks),
    "solver": solver,
}, open(out, "w"), indent=2)
PY

# --------------------------------------------------------------------------- #
# Arm the supervisors. Both are detached with `< /dev/null` so neither can
# consume this script's remaining input, and both write their own terminal
# markers, so their silence is never ambiguous either.
if [ "$ARM_WATCHER" = "1" ]; then
  setsid nohup "$SCRIPT_DIR/ittc_watch.sh" "$CASE_NAME" < /dev/null >> "$CFD_LOG" 2>&1 &
  cfd_say "ITTC watcher armed (pid $!)"
else
  cfd_say "ITTC watcher NOT armed (DM_CFD_ARM_WATCHER=$ARM_WATCHER); the run will go to endTime"
fi

if [ "$ARM_POLLER" = "1" ]; then
  setsid nohup "$SCRIPT_DIR/poller.sh" "$CASE_NAME" loop < /dev/null >> "$CFD_LOG" 2>&1 &
  cfd_say "budget poller armed (pid $!), bound ${BUDGET_HOURS}h"
else
  cfd_say "budget poller NOT armed (DM_CFD_ARM_POLLER=$ARM_POLLER); nothing bounds this run"
fi

if [ "$DETACH" = "1" ]; then
  _terminal_ok "solve launched detached: $NRANKS ranks, pid $SOLVER_PID, budget ${BUDGET_HOURS}h"
  exit 0
fi

# Foreground mode: block on the solver and report its real exit status.
rc=0
wait "$SOLVER_PID" || rc=$?
if [ "$rc" -ne 0 ]; then
  tail -25 "$SOLVER_LOG" 2>/dev/null >> "$CFD_LOG" || true
  _terminal_fail "solver exited rc=$rc (see $SOLVER_LOG)"
  exit "$rc"
fi
_terminal_ok "solve complete rc=0 ($NRANKS ranks)"
