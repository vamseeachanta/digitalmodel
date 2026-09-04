#!/usr/bin/env bash
# Stage 4: wait for Stage 1 to finish cleanly, then mesh the configured KCS
# levels.
#
# Deliberately serialised: meshing concurrently with Stage 1 would contend for
# the same 8 cores and corrupt the per-cell-iteration rate measurement, which
# is the whole point of Stage 1.
#
#   usage: chain_stage45.sh [case ...]
#
# With no arguments it meshes every case in DM_CFD_CONFIG. The original ran a
# literal `for case in kcs_production kcs_companion` against a case root
# hard-coded to one account of one host.
#
# NO `set -e`: the driver's exit status is captured and logged before this
# stage decides what to do with it.
set -uo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/cfd_chain.sh disable=SC1090,SC1091
. "$SELF_DIR/lib/cfd_chain.sh"

# Validate the root HERE, in this shell: inside a command substitution cfd_die
# would only kill the subshell and leave the path empty.
cfd_root > /dev/null
ROOT="$(cfd_root)"

CFD_LOG="${CFD_LOG:-$ROOT/chain.log}"
CFD_MARKER="${CFD_MARKER:-$ROOT/chain_stage45.marker}"
export CFD_LOG CFD_MARKER

DRIVER="${CFD_DRIVER:-$SELF_DIR/stage45_driver.sh}"
STAGE1_PROGRESS="${CFD_STAGE1_PROGRESS:-$ROOT/stage1-dtchull/PROGRESS}"
POLL_SECONDS="${CFD_POLL_SECONDS:-60}"
SETTLE_SECONDS="${CFD_SETTLE_SECONDS:-20}"

# Terminal marker on BOTH outcomes -- see stage45_driver.sh for why silence is
# not evidence.
_marked=0
_on_exit() {
  local rc=$?
  [ "$_marked" = "1" ] && return 0
  cfd_marker_fail "mesh chain exited rc=$rc without completing"
  return 0
}
trap _on_exit EXIT

[ -x "$DRIVER" ] || cfd_die "no executable driver at $DRIVER (set CFD_DRIVER)"

cfd_say "CHAIN START: waiting for Stage 1 to complete"
while ! grep -q "DRIVER COMPLETE" "$STAGE1_PROGRESS" 2>/dev/null; do
  # cfd_solver_running matches the EXECUTABLE NAME (pgrep -x). The original
  # used `pgrep -f "interFoam -parallel"`, which also matches the ssh command
  # line carrying the solver and the supervisor's own command line -- a waiter
  # that sees itself waits forever, and one on this fleet ran 13.5 h past its
  # job for exactly this reason.
  if ! cfd_solver_running interFoam \
     && ! grep -q "SOLVER END" "$STAGE1_PROGRESS" 2>/dev/null; then
    cfd_say "WARNING: no solver running and no SOLVER END mark -- Stage 1 may have died"
  fi
  sleep "$POLL_SECONDS"
done
cfd_say "Stage 1 complete"

# Let the box settle before timing anything else.
sleep "$SETTLE_SECONDS"

if [ "$#" -gt 0 ]; then
  cases=("$@")
else
  mapfile -t cases < <(cfd_cases)
fi
[ "${#cases[@]}" -gt 0 ] || cfd_die "no cases in ${DM_CFD_CONFIG:-<unset>}"

for case_name in "${cases[@]}"; do
  cfd_say "MESH $case_name begin"
  "$DRIVER" mesh "$case_name" >> "$CFD_LOG" 2>&1
  rc=$?
  cfd_say "MESH $case_name end rc=$rc"
  if [ "$rc" -ne 0 ]; then
    cfd_say "FATAL meshing $case_name failed"
    cfd_marker_fail "meshing $case_name failed rc=$rc"
    _marked=1
    exit "$rc"
  fi
  case_dir="$(cfd_case_dir "$case_name")"
  cells="$(cfd_mesh_cells "$case_dir")" || cells=""
  cfd_say "MESH $case_name cells=${cells:-unknown}"
done

cfd_say "CHAIN COMPLETE -- ${#cases[@]} mesh(es) built, awaiting solve authorisation"
cfd_marker_ok "meshed ${cases[*]}"
_marked=1
