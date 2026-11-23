#!/usr/bin/env bash
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/cfd_help.sh"
# Host-side budget enforcement for a detached OpenFOAM run (#1173, #2023).
#
# Mirrors digitalmodel.solvers.openfoam.runner.poll_detached_run for a host
# with no repo checkout. Runs ON the execution host, so enforcement does not
# depend on the flapping link being up -- which is the failure mode the
# in-repo poller's docstring names as its own honest limitation.
#
#   usage: poller.sh <case-name>          one observation, enforce budget
#          poller.sh <case-name> loop     supervise until the run ends
#
#   env:   DM_CFD_ROOT     case root (required; no default, see cfd_root)
#          DM_CFD_CONFIG   case registry (required)
#          DM_CFD_SOLVER   solver executable name (default interFoam)
#          DM_CFD_POLL_SECONDS  observation interval in loop mode (default 300)
#
#   exit:  0  run still alive within budget (once) / run ended (loop)
#          1  fatal -- could not observe
#          2  OVER BUDGET, run terminated
#          3  run is not running (once mode only)
#
# WHAT THIS SCRIPT IS NOT ALLOWED TO DO. It is the one place in the chain
# where a mistake destroys days of compute, so:
#   - it never matches a process by command line. `pgrep -f`/`pkill -f` match
#     the ssh command line carrying the pattern, so a supervisor sees itself
#     and never exits (a 13.5 h zombie on this fleet) and a pattern kill took
#     out the operator's own session. Liveness is `kill -0 <pid>` on the pid
#     the launcher recorded, and rank counting is cfd_solver_ranks (pgrep -x,
#     executable name);
#   - it never kills by name. It resolves the process GROUP of the recorded
#     pid and refuses to signal its own group.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/cfd/lib/cfd_chain.sh
source "$SCRIPT_DIR/lib/cfd_chain.sh"

CASE_NAME="${1:?usage: poller.sh <case-name> [loop]}"
MODE="${2:-once}"
SOLVER="${DM_CFD_SOLVER:-interFoam}"
POLL="${DM_CFD_POLL_SECONDS:-300}"

# cfd_case_dir fails closed when DM_CFD_ROOT is unset. Anything that dies
# before the marker path below exists still exits non-zero and prints FATAL
# to stderr; there is simply no case directory to write a marker into yet.
CASE="$(cfd_case_dir "$CASE_NAME")"
REC="$CASE/detached_run.json"
CFD_LOG="${CFD_LOG:-$CASE/poller.log}"
CFD_MARKER="${CFD_MARKER:-$CASE/poller.marker}"
export CFD_LOG CFD_MARKER

# Terminal markers on BOTH paths: silence must never be readable as success
# or as failure. CFD_TERMINAL says "a marker was already written
# deliberately", so the trap does not overwrite a considered verdict with a
# generic one.
_terminal_ok()   { CFD_TERMINAL=1; cfd_marker_ok "$*"; cfd_say "OK $*"; }
_terminal_fail() { CFD_TERMINAL=1; cfd_marker_fail "$*"; cfd_say "FAILED $*"; }
trap 'rc=$?; [ "$rc" -eq 0 ] || [ -n "${CFD_TERMINAL:-}" ] || cfd_marker_fail "poller exited rc=$rc"' EXIT

# --------------------------------------------------------------------------- #
# The launcher's record. Parsed as JSON, not with `grep -oP`: a record whose
# formatting changed must fail loudly rather than yield an empty pid, and an
# empty pid in a kill line is how a supervisor signals the wrong thing.
_rec_summary() {
  python3 - "$REC" <<'PY'
import json, sys
rec = json.load(open(sys.argv[1]))
missing = [k for k in ("pid", "started_epoch") if rec.get(k) is None]
if missing:
    sys.exit("detached_run.json is missing %s" % ", ".join(missing))
# "-" for absent, never "": the fields are read positionally, and an empty
# field would silently shift the next one into its place.
print(rec["pid"], rec["started_epoch"],
      rec.get("wallclock_budget_hours") or "-",
      rec.get("pgid") or "-")
PY
}

_hours_to_seconds() {
  python3 -c 'import sys; print(int(float(sys.argv[1]) * 3600))' "$1"
}

# --------------------------------------------------------------------------- #
# One observation. Enforces the budget as a side effect.
observe() {
  [ -f "$REC" ] || { _terminal_fail "no detached_run.json in $CASE"; exit 1; }

  local pid started declared rec_pgid
  read -r pid started declared rec_pgid <<<"$(_rec_summary)"

  # `set -e` is suppressed inside a function invoked in a condition context,
  # so a failed reader here would yield an EMPTY pid, `kill -0 ""` would fail,
  # and the poller would report "run ended" over a live solve. Validate
  # explicitly instead of trusting the flag.
  case "$pid" in ''|*[!0-9]*) _terminal_fail "detached_run.json has no usable pid"; exit 1 ;; esac
  case "${started%%.*}" in ''|*[!0-9]*) _terminal_fail "detached_run.json has no usable started_epoch"; exit 1 ;; esac

  # The registry is the single source of truth for the budget (the host-side
  # original kept it in a bash associative array that existed on one machine).
  # If the launcher declared something different, take the SMALLER: a runaway
  # bound that disagrees with itself should bind tighter, not looser.
  local budget
  budget="$(cfd_case_get "$CASE_NAME" budget_hours)"
  if [ "$declared" != "-" ] && [ -n "$declared" ] && [ "$declared" != "$budget" ]; then
    cfd_say "NOTE record declares ${declared}h, registry says ${budget}h -- using the smaller"
    budget="$(python3 -c 'import sys; print(min(float(sys.argv[1]), float(sys.argv[2])))' "$declared" "$budget")"
  fi

  local now elapsed budget_s iters ranks alive=0
  now="$(date +%s)"
  elapsed=$(( now - ${started%%.*} ))
  budget_s="$(_hours_to_seconds "$budget")"
  iters="$(grep -c "^Time = " "$CASE/log.$SOLVER" 2>/dev/null || echo 0)"
  # `| tail -1` is not cosmetic. cfd_solver_ranks is
  # `pgrep -xc NAME || echo 0`, and `pgrep -xc` PRINTS "0" and ALSO exits 1
  # when nothing matches -- so the fallback appends a second line and the
  # helper returns "0\n0". Left unnormalised, `[ "$ranks" -gt 0 ]` errors out
  # with "integer expression expected", and in an `if` condition that error
  # reads as FALSE, i.e. as a healthy rank count.
  ranks="$(cfd_solver_ranks "$SOLVER" | tail -1)"
  # `kill -0 <pid>` asks the kernel about ONE recorded pid. A bare
  # `cmd && var=1` statement would return non-zero when the process is gone
  # and trip `set -e`, so it is written as a conditional.
  if kill -0 "$pid" 2>/dev/null; then alive=1; fi

  # The recorded pid may be a wrapper (setsid/nohup) that exec'd or forked. A
  # dead pid with LIVE ranks is not an ended run -- retiring the supervisor
  # there would leave N solvers holding the cores with nothing bounding them.
  # Fall back to a live rank's own pid, found by EXECUTABLE name (pgrep -x,
  # which cannot self-match), so the budget stays enforceable.
  if [ "$alive" = "0" ] && [ "$ranks" -gt 0 ]; then
    local rank_pid
    rank_pid="$(pgrep -x "$SOLVER" 2>/dev/null | head -1 || true)"
    if [ -n "$rank_pid" ]; then
      cfd_say "WARN recorded pid $pid is gone but $ranks rank(s) still run; supervising rank pid $rank_pid"
      pid="$rank_pid"
      alive=1
    fi
  fi

  cfd_say "pid=$pid alive=$alive ranks=$ranks elapsed=$((elapsed / 60))min budget=${budget}h iters=$iters"

  if [ "$alive" = "1" ] && [ "$elapsed" -gt "$budget_s" ]; then
    _enforce "$pid" "$elapsed" "$budget_s" "$iters" "$rec_pgid"
    return 2
  fi
  [ "$alive" = "1" ] && return 0
  return 3
}

# --------------------------------------------------------------------------- #
# Process-group helpers.
_pgid_of() { ps -o pgid= -p "$1" 2>/dev/null | tr -d ' '; }
_ppid_of() { ps -o ppid= -p "$1" 2>/dev/null | tr -d ' '; }

# Every process group this poller must NEVER signal: its own, and every one of
# its ancestors'.
#
# WHY THE ANCESTOR WALK AND NOT JUST "OWN GROUP". The solve driver is started
# with `&` from the chain script, so it does not become a group leader -- it
# INHERITS the chain's group, and the launch record's pgid is therefore the
# CHAIN's group, not the job's. If the poller is started from that same chain
# without setsid, `kill -TERM -<pgid>` decapitates the supervisor that is
# meant to observe the kill. A self-only check does not catch that, because
# the group belongs to the parent rather than to this process. Verified live
# while building this port: an "own group" check passed and the signal still
# terminated the shell that launched the poller.
_forbidden_pgids() {
  local p="$$" pg
  while [ -n "$p" ] && [ "$p" -gt 1 ] 2>/dev/null; do
    pg="$(_pgid_of "$p")"
    if [ -n "$pg" ]; then printf '%s\n' "$pg"; fi
    p="$(_ppid_of "$p")"
    case "$p" in ''|*[!0-9]*) break ;; esac
  done
}

# Positive proof that a group is the JOB and not a supervisor: at least one
# process in it must be the solver executable. `pgrep -x` matches the
# executable NAME, so it cannot match the ssh command line carrying it and
# cannot self-match.
_group_holds_solver() {
  local target="$1" rp
  for rp in $(pgrep -x "$SOLVER" 2>/dev/null || true); do
    if [ "$(_pgid_of "$rp")" = "$target" ]; then return 0; fi
  done
  return 1
}

# --------------------------------------------------------------------------- #
# Budget kill.
#
# The kill target is EXPLICIT AND VERIFIED, never inherited:
#   - it comes from the launch record's `pgid` (the launcher captures the real
#     group from ps; the original record wrote `$$`, which was the wrong value
#     on every run);
#   - it must still agree with the live group of the recorded pid;
#   - it must not be this poller's group or any ancestor's;
#   - it must demonstrably contain a solver process.
# Any of those failing means STOP, loudly, with a marker. Killing the
# supervisor instead of the job is worse than not enforcing the budget.
#
# Signalling is STAGED, and the two stages have different safety profiles:
#
#   stage 1  TERM each solver RANK by its own pid. This can never hit a
#            supervisor -- every pid signalled was matched by executable name
#            as the solver -- so it is always allowed. It is also the stage
#            that lets the chain observe the kill: the driver's `wait` returns
#            normally and the chain can log its own termination line, which a
#            group kill would prevent by taking the driver out first.
#
#   stage 2  signal the whole GROUP. Only reached if ranks survive stage 1,
#            and GATED on the group not being this poller's own or an
#            ancestor's, because that group is the supervisor/chain itself.
#
# Refusing stage 2 therefore costs nothing that stage 1 already achieved: the
# ranks are dead either way. Refusing BOTH -- which an up-front group check
# would do -- would abandon enforcement entirely over a risk that only ever
# applied to stage 2.
_enforce() {
  local pid="$1" elapsed="$2" budget_s="$3" iters="$4" rec_pgid="$5"
  local target live_pgid f rp
  live_pgid="$(_pgid_of "$pid")"

  target="$rec_pgid"
  if [ "$target" = "-" ] || [ -z "$target" ]; then
    target="$live_pgid"
    cfd_say "NOTE launch record carries no pgid; falling back to the live group of pid $pid"
  elif [ -n "$live_pgid" ] && [ "$target" != "$live_pgid" ]; then
    _terminal_fail "record pgid $target disagrees with the live group $live_pgid of pid $pid; refusing to signal"
    exit 1
  fi

  # An empty target turns `kill -TERM -$target` into a malformed signal, and
  # group 1 is init's.
  case "$target" in
    ''|*[!0-9]*) _terminal_fail "cannot resolve a numeric process group for pid $pid (got '$target'); refusing to signal"; exit 1 ;;
  esac
  if [ "$target" -le 1 ]; then
    _terminal_fail "implausible process group '$target'; refusing to signal"
    exit 1
  fi

  if ! _group_holds_solver "$target"; then
    _terminal_fail "no '$SOLVER' process is in group $target; refusing to signal a group that does not hold the job"
    exit 1
  fi

  # Stage 1: the ranks, by their own pids. Always allowed.
  cfd_say "OVER BUDGET -- terminating $SOLVER ranks in verified group $target (recorded pid $pid)"
  for rp in $(pgrep -x "$SOLVER" 2>/dev/null || true); do
    if [ "$(_pgid_of "$rp")" = "$target" ]; then kill -TERM "$rp" 2>/dev/null || true; fi
  done
  sleep "${DM_CFD_TERM_GRACE:-10}"

  # Stage 2: escalation to the group -- gated. Written as an if/else rather
  # than an early return so the terminated-run record and the terminal marker
  # below are written on BOTH paths; a budget kill that leaves no record is
  # indistinguishable from a run that stopped on its own.
  if _group_holds_solver "$target"; then
    for f in $(_forbidden_pgids); do
      if [ "$f" = "$target" ]; then
        _terminal_fail "ranks survived TERM but group $target is this poller's own or an ancestor's (the supervisor/chain group); refusing to escalate -- signalling it would kill the supervisor instead of the job"
        exit 1
      fi
    done
    cfd_say "ranks survived TERM -- escalating to process group $target"
    kill -TERM -"$target" 2>/dev/null || true
    sleep "${DM_CFD_TERM_GRACE:-10}"
    kill -KILL -"$target" 2>/dev/null || true
  else
    cfd_say "all $SOLVER ranks stopped; the driver's own wait can observe the termination"
  fi

  python3 - "$CASE/detached_run.terminated.json" "$pid" "$elapsed" "$budget_s" "$iters" <<'PY'
import json, sys, time
out, pid, elapsed, budget_s, iters = sys.argv[1:6]
json.dump({
    "pid": int(pid),
    "elapsed_seconds": int(elapsed),
    "budget_seconds": int(budget_s),
    "iterations": int(iters),
    "reason": "elapsed exceeded the declared wall-clock budget",
    "terminated_epoch": int(time.time()),
}, open(out, "w"), indent=2)
PY
  _terminal_fail "terminated over budget after $((elapsed / 3600))h (${iters} iterations written)"
}

# --------------------------------------------------------------------------- #
if [ "$MODE" = "loop" ]; then
  cfd_say "poller armed for '$CASE_NAME' (mode=loop, interval=${POLL}s)"
  while true; do
    # A bare `[ ... ] && exit` statement returns non-zero when the test fails,
    # which under `set -e` would end the supervisor on its first healthy
    # observation. Every dispatch here is a real conditional.
    rc=0; observe || rc=$?
    if [ "$rc" -eq 2 ]; then exit 2; fi
    if [ "$rc" -eq 3 ]; then _terminal_ok "run ended within budget"; exit 0; fi
    sleep "$POLL"
  done
else
  rc=0; observe || rc=$?
  if [ "$rc" -eq 2 ]; then exit 2; fi
  if [ "$rc" -eq 3 ]; then _terminal_ok "run is not running"; exit 3; fi
  _terminal_ok "run alive within budget"
  exit 0
fi
