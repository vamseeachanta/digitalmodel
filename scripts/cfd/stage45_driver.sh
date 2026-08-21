#!/usr/bin/env bash
# Stage 4 (mesh) and Stage 5 (solve) driver for the KCS chain (#1173, #2023).
#
#   usage: stage45_driver.sh mesh  <case>
#          stage45_driver.sh solve <case> [budget_hours]
#
# Meshing is attached and quick; solving is long and is launched detached by
# the caller, with the budget enforced out of band by the poller.
#
# The case root comes from DM_CFD_ROOT and every per-case number (ranks,
# budget, decomposition) from DM_CFD_CONFIG. The host-side original hard-coded
# ROOT="$HOME/cfd/dm1173" and defaulted the budget to 48 h, so it ran on one
# account of one host and its numbers were unreviewable.
#
# NO `set -e`. Every stage's exit code is captured and written to TIMING.csv
# before the driver decides what to do with it; errexit would abort at the
# failing command and lose the one row that says which stage failed and how
# long it took. `set -u` IS safe here now: the OpenFOAM etc/bashrc that used to
# abort under it is loaded through cfd_load_openfoam, which saves and restores
# the caller's flags around the source.
set -uo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/cfd_chain.sh disable=SC1090,SC1091
. "$SELF_DIR/lib/cfd_chain.sh"

PHASE="${1:-}"
CASE_NAME="${2:-}"
case "$PHASE" in
  mesh|solve) ;;
  *) echo "usage: $0 mesh|solve <case> [budget_hours]" >&2; exit 2 ;;
esac
[ -n "$CASE_NAME" ] || { echo "usage: $0 $PHASE <case> [budget_hours]" >&2; exit 2; }

# Validate the root HERE, in this shell. cfd_case_dir resolves it inside a
# command substitution, where cfd_die can only kill the subshell -- the caller
# would get an empty path and cd into the wrong place.
cfd_root > /dev/null
CASE="$(cfd_case_dir "$CASE_NAME")"
[ -d "$CASE" ] || cfd_die "no such case directory: $CASE"

PROG="$CASE/PROGRESS"
TIMING="$CASE/TIMING.csv"
CFD_LOG="${CFD_LOG:-$PROG}"
CFD_MARKER="${CFD_MARKER:-$CASE/stage45_driver.$PHASE.marker}"
export CFD_LOG CFD_MARKER

# "mark" is the vocabulary the rest of the chain greps for (SOLVER END,
# CHECKMESH VERDICT, DRIVER COMPLETE). Keep the name; the mechanism is now the
# shared timestamped logger.
mark() { cfd_say "$@"; }

# Terminal marker on BOTH outcomes. The explicit calls below cover the failures
# this driver knows how to have; the trap covers the ones that killed the
# untracked original -- an unhandled signal, or the poller's group-kill when a
# run went over budget. A lane that marks only success cannot be told apart
# from one still running, and in practice gets read as success.
_marked=0
_on_exit() {
  local rc=$?
  [ "$_marked" = "1" ] && return 0
  cfd_marker_fail "$PHASE $CASE_NAME exited rc=$rc without reaching a verdict"
  return 0
}
trap _on_exit EXIT

# Per-case numbers from the registry, never from a literal in this file.
RANKS="$(cfd_case_get "$CASE_NAME" ranks)"
[ -n "$RANKS" ] || cfd_die "no ranks for $CASE_NAME in $DM_CFD_CONFIG"
DECOMPOSE_N="$(cfd_decompose_n "$RANKS")"
[ -n "$DECOMPOSE_N" ] || cfd_die "no decomposition vector for $RANKS ranks"

BUDGET="${3:-}"
if [ "$PHASE" = "solve" ] && [ -z "$BUDGET" ]; then
  BUDGET="$(cfd_case_get "$CASE_NAME" budget_hours)"
  [ -n "$BUDGET" ] || cfd_die "no budget_hours for $CASE_NAME in $DM_CFD_CONFIG"
fi

cfd_load_openfoam
cd "$CASE" || cfd_die "cannot enter $CASE"

# RunFunctions has the same unset-variable habit as the bashrc that ships with
# it, so it gets the same save/restore treatment cfd_load_openfoam applies.
_saved_flags="$-"
set +u
# shellcheck disable=SC1090,SC1091
. "${WM_PROJECT_DIR}/bin/tools/RunFunctions"
# NOT restored, deliberately. The save/restore protected the SOURCING, but
# RunFunctions' functions dereference unset variables when CALLED too --
# runApplication dies on `appRun: unbound variable` at its first use. So the
# whole remainder of this script runs without -u once RunFunctions is loaded.
#
# This was a live defect: the port added `set -uo pipefail` as hardening, and
# that hardening broke every mesh and solve invocation. The original host
# script had no -u, which is why it worked and this did not. Nothing caught it
# because the guard tests assert on this file's TEXT; a script that cannot run
# still reads correctly. Only executing it finds this class of fault.
case "$_saved_flags" in *u*) : ;; esac

# ---------------------------------------------------------------------------
# hierarchical decomposition requires numberOfSubdomains == the rank count we
# are about to launch, else decomposePar exits fatally with "Wrong number of
# domain divisions" -- after the mesh has already been built. Read-only: the
# dict belongs to the setup stage, and writing into a case from here is how a
# live solve gets aborted.
_check_decomposition() {
  local dict="system/decomposeParDict" n
  [ -f "$dict" ] || cfd_die "no $dict in $CASE; the setup stage must write it"
  n="$(awk '/^[[:space:]]*numberOfSubdomains/ { gsub(/[^0-9]/, "", $0); print; exit }' "$dict")"
  [ -n "$n" ] || cfd_die "cannot read numberOfSubdomains from $CASE/$dict"
  [ "$n" = "$RANKS" ] || cfd_die \
    "$dict declares numberOfSubdomains=$n but the registry gives $CASE_NAME $RANKS ranks (expected n = $DECOMPOSE_N)"
}

# ---------------------------------------------------------------------------
# Stage timing. Split into begin/end rather than one wrapper so that a parallel
# stage can be written out as a literal mpirun line with its own redirects --
# see the WHY on the mpirun calls below.
T_NAME=""
T0=""
tbegin() {
  T_NAME="$1"
  T0="$(date +%s.%N)"
  mark "BEGIN $T_NAME"
}
tend() {
  local rc="$1" t1 elapsed
  t1="$(date +%s.%N)"
  elapsed="$(echo "$t1 - $T0" | bc)"
  echo "$T_NAME,$elapsed,$rc" >> "$TIMING"
  mark "END   $T_NAME rc=$rc elapsed=${elapsed}s"
  if [ "$rc" -ne 0 ]; then
    mark "FATAL $T_NAME rc=$rc"
    cfd_marker_fail "$PHASE $CASE_NAME failed at $T_NAME rc=$rc"
    _marked=1
    exit "$rc"
  fi
  return 0
}
tstage() {
  local name="$1"; shift
  tbegin "$name"
  "$@" >> "$CASE/driver.log" 2>&1
  tend "$?"
}

if [ "$PHASE" = "mesh" ]; then
  : > "$PROG"
  echo "stage,seconds,rc" > "$TIMING"
  mark "MESH PHASE START $CASE_NAME pid=$$ ranks=$RANKS decompose='$DECOMPOSE_N'"

  tstage surfaceFeatureExtract runApplication surfaceFeatureExtract
  tstage blockMesh             runApplication blockMesh

  # Refinement passes are discovered from the dicts the setup stage wrote
  # rather than hard-coded to the six the two original levels happened to
  # use -- the registry now carries four levels and they do not share a pass
  # count.
  topo_dicts=(system/topoSetDict.*)
  if [ ! -e "${topo_dicts[0]}" ]; then
    # Not fatal: a deliberately coarse level may have none. It is loud because
    # an unrefined mesh still solves, and the resulting number looks like a
    # result. The downstream cell-count gate is what must catch it.
    mark "WARNING no system/topoSetDict.N present -- mesh will NOT be refined"
    topo_dicts=()
  fi
  for dict in "${topo_dicts[@]}"; do
    i="${dict##*.}"
    tstage "topoSet.$i"    runApplication -s "$i" topoSet -dict "$dict"
    tstage "refineMesh.$i" runApplication -s "$i" refineMesh -dict system/refineMeshDict -overwrite
  done

  tstage snappyHexMesh runApplication snappyHexMesh -overwrite
  tstage checkMesh     runApplication checkMesh

  # STRICT verdict rule: read the OUTPUT TEXT, not the exit code. checkMesh
  # returns 0 even when it reports failed checks, so an exit-code gate would
  # certify a mesh it had just been told was bad.
  if grep -q "^Mesh OK" log.checkMesh && ! grep -q "Failed .* mesh checks" log.checkMesh; then
    mark "CHECKMESH VERDICT: PASS (Mesh OK, zero failed checks)"
  else
    mark "CHECKMESH VERDICT: FAIL -- $(grep -c 'Failed' log.checkMesh) failure line(s)"
    grep -E "\*\*\*|Failed" log.checkMesh >> "$PROG"
  fi
  mark "CELLS: $(cfd_mesh_cells "$CASE")"

  restore0Dir >> "$CASE/driver.log" 2>&1
  mark "restore0Dir done"
  # setFields initialises a VOLUME FRACTION. A single-phase case has none to
  # initialise -- no alpha field, no setFieldsDict -- so running it there is
  # not a stage that fails, it is a stage that should never have run. The
  # chain was written when every case was two-phase, so "two-phase" was baked
  # in as an assumption rather than tested as a property; the double-body case
  # is what exposed it.
  #
  # Tested POSITIVELY and structurally, never by case name: a name check is a
  # backdoor, and the same reasoning governs the forces density-source guard.
  if [ -f system/setFieldsDict ] && compgen -G "0.orig/alpha.*" > /dev/null; then
    tstage setFields runApplication setFields
  else
    mark "SKIP setFields -- single-phase case (no setFieldsDict, no alpha field)"
  fi

  _check_decomposition

  # WHY these two are literal mpirun lines and not `runParallel`:
  #   - runParallel builds the mpirun command itself and supplies no stdin
  #     redirect. mpirun READS AND CLOSES STDIN; without `< /dev/null` it eats
  #     the rest of whatever stream is feeding this script, which once caused a
  #     solver launch to be skipped entirely while the wrapper reported success;
  #   - runParallel also returns 0 without doing anything when log.<app>
  #     already exists, so a rerun after a partial mesh silently no-ops and
  #     looks like a pass.
  # Neither is visible from the caller, so the invocation is written out.
  tbegin redistributePar.decompose
  mpirun -np "$RANKS" redistributePar -decompose -parallel > "$CASE/log.redistributePar.decompose" 2>&1 < /dev/null
  tend "$?"

  tbegin renumberMesh
  mpirun -np "$RANKS" renumberMesh -overwrite -parallel > "$CASE/log.renumberMesh" 2>&1 < /dev/null
  tend "$?"

  mark "MESH PHASE COMPLETE -- ready to solve"
  cfd_marker_ok "mesh $CASE_NAME complete, $(cfd_mesh_cells "$CASE") cells"
  _marked=1

else
  _check_decomposition
  mark "SOLVE PHASE START $CASE_NAME pid=$$ ranks=$RANKS budget=${BUDGET}h"

  # Launch record for the out-of-band poller. It is written
  # BEFORE the solver is released, so that a poller reconnecting after a link
  # drop can find the run without having witnessed the launch.
  #
  # pgid is read from ps rather than assumed to be $$: a script started with
  # `&` from another script inherits its caller's process group and is NOT a
  # group leader, so the original's `"pgid": $$` was wrong on every run. The
  # poller kills the group, so this value decides what dies. Resolved into a
  # variable first: an empty substitution inside the heredoc would emit invalid
  # JSON, and the poller would then read nothing at all rather than read it
  # wrong -- an unsupervised 60 h solve.
  SOLVE_PGID="$(ps -o pgid= $$ 2>/dev/null | tr -d ' ')"
  [ -n "$SOLVE_PGID" ] || SOLVE_PGID=$$

  # The solver is READ FROM THE CASE, never assumed. It was hardcoded to
  # interFoam, which is right for every two-phase case and silently wrong for
  # a single-phase one: the double-body case declares simpleFoam and got
  # interFoam anyway, failing one second into a solve phase that had just
  # spent 35 minutes meshing. Fourth defect of the same shape -- the chain was
  # written when there was one kind of case, so "one kind of case" was baked
  # in as an assumption rather than read as a property.
  SOLVER="$(awk '/^application/ {gsub(/;/,"",$2); print $2; exit}' \
            "$CASE/system/controlDict")"
  [ -n "$SOLVER" ] || cfd_die "no application entry in $CASE/system/controlDict"
  mark "SOLVER: $SOLVER (read from controlDict)"
  cat > "$CASE/detached_run.json" <<JSON
{
  "case": "$CASE_NAME",
  "pid": $$,
  "pgid": $SOLVE_PGID,
  "argv": ["$SOLVER", "-parallel"],
  "ranks": $RANKS,
  "started_epoch": $(date +%s),
  "wallclock_budget_hours": $BUDGET,
  "log_file": "log.$SOLVER"
}
JSON

  tbegin "$SOLVER"
  mpirun -np "$RANKS" "$SOLVER" -parallel > "$CASE/log.$SOLVER" 2>&1 < /dev/null
  SOLVE_RC=$?
  # "SOLVER END" is the token the mesh stage greps for when deciding whether a
  # silent Stage 1 died or merely finished. Keep it whatever tend does next.
  mark "SOLVER END rc=$SOLVE_RC"
  # Unlike the original, a failed solve now leaves this driver with a non-zero
  # exit status. The original always ended on a successful `mark`, so the
  # caller's "stop the chain rather than solve the next level" branch was
  # dead code and a failed level was followed by another 24 h of solving.
  tend "$SOLVE_RC"

  mark "SOLVE PHASE COMPLETE"
  cfd_marker_ok "solve $CASE_NAME complete rc=$SOLVE_RC"
  _marked=1
fi
