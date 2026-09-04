#!/usr/bin/env bash
# Unattended solve launcher: wait for meshing, sanity-gate the cell counts,
# then start the solve stage (#1173, #2023).
#
#   usage: auto_solve.sh <fine-case> <coarse-case>
#   env:   DM_CFD_ROOT             case root (required; no default)
#          DM_CFD_CONFIG           case registry (required)
#          DM_CFD_SOLVE_ENTRY      solve-stage entry point
#                                  (default: solve_chain.sh beside this script)
#          DM_CFD_FINE_CELLS_MIN/MAX     gate bounds for the fine grid
#          DM_CFD_COARSE_CELLS_MIN/MAX   gate bounds for the coarse grid
#          DM_CFD_MESH_WAIT_HOURS  deadline on the meshing wait (default 12)
#          DM_CFD_POLL_SECONDS     wait interval (default 60)
#
# WHY THE GATE EXISTS. The pipeline should be autonomous WITHOUT being
# reckless. Mesh density is set by a calculated multiplier that has never been
# run on a given hull, so the cell count is a PREDICTION. If it lands far from
# target the right move is to stop and recalibrate, not to spend days solving
# a mesh nobody looked at. A HALT here is a success of the gate.
#
# The two cases are the grid-convergence pair: the ratio of their cell counts
# gives the linear refinement ratio r = (N_fine/N_coarse)^(1/3), which should
# land near sqrt(2) = 1.4142 for a standard two-level study. It is reported,
# not enforced, because the gate on absolute counts already bounds the damage.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/cfd/lib/cfd_chain.sh
source "$SCRIPT_DIR/lib/cfd_chain.sh"

FINE_CASE="${1:?usage: auto_solve.sh <fine-case> <coarse-case>}"
COARSE_CASE="${2:?usage: auto_solve.sh <fine-case> <coarse-case>}"

ROOT="$(cfd_root)"
CHAIN_LOG="${DM_CFD_CHAIN_LOG:-$ROOT/chain.log}"
CFD_LOG="${CFD_LOG:-$ROOT/auto_solve.log}"
CFD_MARKER="${CFD_MARKER:-$ROOT/auto_solve.marker}"
export CFD_LOG CFD_MARKER

_terminal_ok()   { CFD_TERMINAL=1; cfd_marker_ok "$*"; cfd_say "OK $*"; }
_terminal_fail() { CFD_TERMINAL=1; cfd_marker_fail "$*"; cfd_say "FAILED $*"; }
# shellcheck disable=SC2154  # rc is assigned by the trap body itself
trap 'rc=$?; [ "$rc" -eq 0 ] || [ -n "${CFD_TERMINAL:-}" ] || cfd_marker_fail "auto_solve exited rc=$rc"' EXIT

# Gate bounds. Defaults are the values calibrated for the #1173 KCS grids;
# they are env-overridable because they are hull- and study-specific, not
# properties of the chain.
FINE_MIN="${DM_CFD_FINE_CELLS_MIN:-1000000}"
FINE_MAX="${DM_CFD_FINE_CELLS_MAX:-2500000}"
COARSE_MIN="${DM_CFD_COARSE_CELLS_MIN:-350000}"
COARSE_MAX="${DM_CFD_COARSE_CELLS_MAX:-1000000}"
POLL="${DM_CFD_POLL_SECONDS:-60}"
WAIT_HOURS="${DM_CFD_MESH_WAIT_HOURS:-12}"

# Validate the entry point NOW, before a multi-hour wait. Discovering at hour
# 11 that the thing we waited for cannot be launched is the same wasted
# compute the gate exists to prevent.
SOLVE_ENTRY="${DM_CFD_SOLVE_ENTRY:-$SCRIPT_DIR/solve_chain.sh}"
[ -x "$SOLVE_ENTRY" ] || cfd_die "solve entry point is not executable: $SOLVE_ENTRY (set DM_CFD_SOLVE_ENTRY)"

# --------------------------------------------------------------------------- #
# Wait for meshing -- WITH A DEADLINE. An unattended `while ! grep; do sleep;
# done` with no bound is exactly the shape that produced a 13.5 h zombie
# supervisor on this fleet: it cannot distinguish "not finished yet" from
# "will never finish".
case "$WAIT_HOURS" in ''|*[!0-9]*) cfd_die "DM_CFD_MESH_WAIT_HOURS must be whole hours, got '$WAIT_HOURS'" ;; esac
DEADLINE=$(( $(date +%s) + WAIT_HOURS * 3600 ))
cfd_say "AUTO-SOLVE ARMED: waiting up to ${WAIT_HOURS}h for meshing ($CHAIN_LOG)"
while ! grep -q "CHAIN COMPLETE" "$CHAIN_LOG" 2>/dev/null; do
  if grep -q "FATAL" "$CHAIN_LOG" 2>/dev/null; then
    _terminal_fail "meshing reported FATAL in $CHAIN_LOG"
    exit 1
  fi
  if [ "$(date +%s)" -ge "$DEADLINE" ]; then
    _terminal_fail "meshing did not complete within ${WAIT_HOURS}h; not launching"
    exit 1
  fi
  sleep "$POLL"
done
cfd_say "meshing complete"

# --------------------------------------------------------------------------- #
# Cell counts. cfd_mesh_cells returns EMPTY (not 0) when it cannot read the
# checkMesh log, so "could not read" can never be mistaken for "an empty mesh"
# and slip through a numeric comparison.
FINE_CELLS="$(cfd_mesh_cells "$(cfd_case_dir "$FINE_CASE")" || true)"
COARSE_CELLS="$(cfd_mesh_cells "$(cfd_case_dir "$COARSE_CASE")" || true)"
cfd_say "cell counts: $FINE_CASE=${FINE_CELLS:-?} $COARSE_CASE=${COARSE_CELLS:-?}"

if [ -z "$FINE_CELLS" ] || [ -z "$COARSE_CELLS" ]; then
  _terminal_fail "could not read cell counts (fine='$FINE_CELLS' coarse='$COARSE_CELLS')"
  exit 1
fi

if [ "$FINE_CELLS" -lt "$FINE_MIN" ] || [ "$FINE_CELLS" -gt "$FINE_MAX" ]; then
  _terminal_fail "HALT: $FINE_CASE $FINE_CELLS cells outside [$FINE_MIN, $FINE_MAX] -- recalibrate mesh_scale"
  exit 2
fi
if [ "$COARSE_CELLS" -lt "$COARSE_MIN" ] || [ "$COARSE_CELLS" -gt "$COARSE_MAX" ]; then
  _terminal_fail "HALT: $COARSE_CASE $COARSE_CELLS cells outside [$COARSE_MIN, $COARSE_MAX] -- recalibrate mesh_scale"
  exit 2
fi

RATIO_LINE="$(python3 -c 'import sys
f, c = float(sys.argv[1]), float(sys.argv[2])
print("cell ratio %.4f -> linear refinement ratio %.4f (target 1.4142)"
      % (f / c, (f / c) ** (1.0 / 3.0)))' "$FINE_CELLS" "$COARSE_CELLS")"
cfd_say "$RATIO_LINE"

# --------------------------------------------------------------------------- #
# `< /dev/null` on the launch too: the solve stage runs mpirun, and mpirun
# reads and closes stdin. Inheriting this script's stdin is how a launch line
# got swallowed while the wrapper reported success.
cfd_say "GATE PASSED -- launching the solve stage: $SOLVE_ENTRY"
setsid nohup "$SOLVE_ENTRY" "$FINE_CASE" "$COARSE_CASE" < /dev/null > "$ROOT/solve_chain.out" 2>&1 &
_terminal_ok "gate passed; solve stage launched (pid $!)"
