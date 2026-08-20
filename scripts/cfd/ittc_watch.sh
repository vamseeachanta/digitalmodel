#!/usr/bin/env bash
# ITTC 7.5-03-01-01 oscillatory convergence watcher (#1173, #2023).
#
#   usage: ittc_watch.sh <case-name>
#   env:   DM_CFD_ROOT   case root (required; no default, see cfd_root)
#          MIN_ITER WINDOW WINDOWS SPREAD_PCT DRIFT_PCT HOLD  criterion params
#          DM_CFD_SOLVER solver executable name (default interFoam)
#          DM_CFD_POLL_SECONDS  check interval (default 300)
#
# WHY THIS EXISTS. OpenFOAM v2312's runTimeControl offers no condition for
# "the window MEAN has stopped moving". Its `average` condition compares the
# INSTANTANEOUS value to the running mean -- it fires when the signal stops
# fluctuating, not when the mean settles. That is what stopped the previous
# fine-grid run at iteration 9,011 with its mean still descending 35%.
#
# THE CRITERION, assembled from ITTC text rather than invented:
#   7.5-03-01-01 §4.1, oscillatory convergence:  U_I = 1/2 (S_U - S_L)
#   and the governing requirement: "Iterative errors must be accurately
#   estimated or negligible in comparison to errors due to input parameters".
# So: take trailing window means, and stop only when their spread is an order
# below the grid-to-grid difference this run exists to measure (5.58%).
#
# Stop requires ALL of:
#   - at least MIN_ITER iterations done (the start transient must wash out)
#   - WINDOWS consecutive window means available
#   - spread of those means <= SPREAD_PCT of their mean   (U_I <= half that)
#   - no monotone drift: |last - first| <= DRIFT_PCT of the mean
# Held for HOLD consecutive checks, so a momentary quiet patch cannot trigger it.
#
# THIS WATCHER ONLY TOUCHES A FILE. It is inert unless the case's controlDict
# carries a matching `abort` function object reading the same file:
#
#     ittcStop { type abort; file "$FOAM_CASE/ITTC_CONVERGED"; action writeNow; }
#
# A case shipped without that block, so the trigger did nothing and the run
# went to endTime. solve_case.sh verifies the wiring before arming this
# watcher; if you arm it by hand, verify it yourself.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/cfd/lib/cfd_chain.sh
source "$SCRIPT_DIR/lib/cfd_chain.sh"

CASE_NAME="${1:?usage: ittc_watch.sh <case-name>}"
CASE="$(cfd_case_dir "$CASE_NAME")"
TRIG="$CASE/ITTC_CONVERGED"
SOLVER="${DM_CFD_SOLVER:-interFoam}"
POLL="${DM_CFD_POLL_SECONDS:-300}"
CFD_LOG="${CFD_LOG:-$CASE/ittc_watch.log}"
CFD_MARKER="${CFD_MARKER:-$CASE/ittc_watch.marker}"
export CFD_LOG CFD_MARKER

_terminal_ok()   { CFD_TERMINAL=1; cfd_marker_ok "$*"; cfd_say "OK $*"; }
_terminal_fail() { CFD_TERMINAL=1; cfd_marker_fail "$*"; cfd_say "FAILED $*"; }
# shellcheck disable=SC2154  # rc is assigned by the trap body itself
trap 'rc=$?; [ "$rc" -eq 0 ] || [ -n "${CFD_TERMINAL:-}" ] || cfd_marker_fail "watcher exited rc=$rc"' EXIT

# --------------------------------------------------------------------------- #
# Criterion parameters. Named, commented and overridable because they were
# retuned per run, and a literal buried in the criterion cannot be retuned
# without editing the criterion.
#
# MIN_ITER is the one that actually differs between runs: a COLD START needs a
# larger MIN_ITER than a mapFieldsPar-prolonged start, because a mapped field
# begins near the converged solution while a cold field has the whole start
# transient to wash out. 10000 (cold) and 6000 (mapped) were the values used
# on #1173. The default below is the mapped one; export MIN_ITER=10000 for a
# cold start.
MIN_ITER="${MIN_ITER:-6000}"       # iterations before the criterion may fire
WINDOW="${WINDOW:-1500}"           # iterations per window
WINDOWS="${WINDOWS:-5}"            # trailing windows compared
SPREAD_PCT="${SPREAD_PCT:-0.60}"   # max-min of the window means, % of mean
DRIFT_PCT="${DRIFT_PCT:-0.35}"     # end-to-end drift across the windows, % of mean
HOLD="${HOLD:-3}"                  # consecutive passing checks required

# --------------------------------------------------------------------------- #
# NEWEST coefficient file, not a fixed name. OpenFOAM VERSIONS the output
# when a case is re-run (coefficient.dat -> coefficient_0.dat) rather than
# clobbering it, so a fixed name silently reads the PREVIOUS run. Caught
# live on #1173: this watcher was reading a dead 9,011-iteration history
# while the live run sat at 455 -- the stop was effectively not armed.
#
# shellcheck disable=SC2012  # `ls -t` is the point: mtime ordering is the
# selection criterion, and OpenFOAM's own filenames carry no whitespace.
coef_file() { ls -t "$CASE"/postProcessing/forceCoeffs1/*/coefficient*.dat 2>/dev/null | head -1; }

cfd_say "ITTC watcher armed for '$CASE_NAME': min_iter=$MIN_ITER window=$WINDOW x$WINDOWS spread<=${SPREAD_PCT}% drift<=${DRIFT_PCT}% hold=$HOLD"

pass=0
while true; do
  sleep "$POLL"

  # Stop watching if the solver is gone -- endTime, crash or budget kill.
  # cfd_solver_running is `pgrep -x` on the EXECUTABLE name. Never `pgrep -f`
  # here: the pattern would match the ssh command line carrying it, so the
  # watcher would see itself and never exit. That produced a 13.5 h zombie on
  # this fleet.
  if ! cfd_solver_running "$SOLVER"; then
    _terminal_ok "solver no longer running; watcher exiting without a stop"
    exit 0
  fi

  COEF="$(coef_file)"
  [ -n "$COEF" ] && [ -f "$COEF" ] || { cfd_say "WAIT no coefficient file yet"; continue; }

  line="$(python3 - "$COEF" "$MIN_ITER" "$WINDOW" "$WINDOWS" "$SPREAD_PCT" "$DRIFT_PCT" <<'PY'
import sys
path, min_iter, win, nwin, spread_pct, drift_pct = sys.argv[1:7]
min_iter, win, nwin = int(min_iter), int(win), int(nwin)
spread_pct, drift_pct = float(spread_pct), float(drift_pct)
rows = []
for line in open(path, errors="replace"):
    if line.startswith("#"):
        continue
    p = line.split()
    if len(p) < 2:
        continue
    try:
        rows.append((int(float(p[0])), abs(float(p[1]))))
    except ValueError:
        continue
if not rows:
    print("WAIT no-rows"); raise SystemExit
last = rows[-1][0]
if last < min_iter:
    print(f"WAIT iter={last}<{min_iter}"); raise SystemExit
means = []
for k in range(nwin, 0, -1):
    hi, lo = last - (k - 1) * win, last - k * win
    seg = [v for it, v in rows if lo < it <= hi]
    if not seg:
        print(f"WAIT window-{k}-empty"); raise SystemExit
    means.append(sum(seg) / len(seg))
m = sum(means) / len(means)
spread = 100 * (max(means) - min(means)) / m
drift = 100 * abs(means[-1] - means[0]) / m
ok = spread <= spread_pct and drift <= drift_pct
print(f"{'PASS' if ok else 'WAIT'} iter={last} mean={m:.6e} "
      f"spread={spread:.3f}% drift={drift:.3f}% U_I={spread/2:.3f}%")
PY
)"

  # An empty verdict means the criterion produced nothing, which is not a
  # PASS and must not be read as one. `read` would exit non-zero on an empty
  # line and kill the watcher mid-run, so parse by expansion.
  [ -n "$line" ] || { cfd_say "WAIT criterion produced no output"; continue; }
  verdict="${line%% *}"
  msg="${line#* }"

  if [ "$verdict" = "PASS" ]; then
    pass=$((pass + 1))
    cfd_say "PASS $pass/$HOLD  $msg"
    if [ "$pass" -ge "$HOLD" ]; then
      cfd_say "ITTC CRITERION MET -- creating trigger $TRIG"
      touch "$TRIG"
      _terminal_ok "ITTC criterion met; trigger created, solver will writeNow and stop -- $msg"
      exit 0
    fi
  else
    # `[ ... ] && cmd` as a bare statement returns non-zero when the test
    # fails, which under `set -e` kills the watcher on the first WAIT. Use a
    # real conditional.
    if [ "$pass" -gt 0 ]; then cfd_say "reset after $pass pass(es)"; fi
    pass=0
    cfd_say "$msg"
  fi
done
