#!/bin/bash
# Stage 1 of the #1173 ship-resistance chain: reproduce the OpenFOAM DTCHull
# tutorial UNMODIFIED, with per-stage timing.
#
# WHY A BASELINE STAGE AT ALL. Every later case (production, companion, fine,
# the near-wall variants) is a derivative of this tutorial's dictionaries. If
# the tutorial itself does not run to completion on this host, at this OpenFOAM
# version, with this decomposition, then nothing measured downstream can be
# attributed to the KCS hull rather than to the environment. The value of this
# stage is entirely in being byte-identical to what upstream ships -- which is
# why this script COPIES and CHECKSUMS the tutorial and never edits it. A
# baseline that has been "improved" measures nothing, and the edit is invisible
# six months later.
#
# WHAT WAS GENERALISED FROM THE HOST-SIDE ORIGINAL (#2023): the case root was
# hard-coded to one account on one machine, the tutorial path and the geometry
# were literals, the refinement-pass count was a bare `for i in 1 2 3 4 5 6`,
# and a previous baseline was destroyed unconditionally by `rm -rf`.
#
# NOTE: no `set -e` and no `set -u` in this script. The OpenFOAM etc/bashrc and
# bin/tools/RunFunctions both dereference unset variables; under either flag the
# shell aborts mid-source and leaves a half-built environment that fails much
# later and much less legibly. Failure is handled explicitly instead: every
# stage checks its own return code and the EXIT trap writes the FAILED marker.

CFD_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/lib" && pwd)" || exit 1
# shellcheck source-path=SCRIPTDIR source=lib/cfd_chain.sh
. "$CFD_LIB_DIR/cfd_chain.sh"

set -o pipefail

# --------------------------------------------------------------------------- #
# Parameters. Every one of these was a literal in the original.
# --------------------------------------------------------------------------- #

# Tutorial case, relative to $FOAM_TUTORIALS. This is the provenance of the
# whole chain: the KCS cases are this tutorial's dictionaries with the hull,
# the domain and the base grid changed.
TUTORIAL_CASE="${DM_CFD_TUTORIAL_CASE:-multiphase/interFoam/RAS/DTCHull}"

# Hull geometry shipped with the tutorials, staged into constant/triSurface.
# The tutorial's Allrun does this too; it is not a modification.
TUTORIAL_GEOMETRY="${DM_CFD_TUTORIAL_GEOMETRY:-resources/geometry/DTC-scaled.stl.gz}"

# The tutorial refines the free-surface band in N paired topoSet/refineMesh
# passes driven by system/topoSetDict.1 ... .N. The count is a property of the
# tutorial, not a tuning knob: raising it without adding the matching
# topoSetDict silently refines nothing (topoSet fails, the pass is fatal).
REFINE_PASSES="${DM_CFD_REFINE_PASSES:-6}"

# Sub-directory of the case root that holds this stage. Kept outside
# kcs_cases/ because it is not a KCS case and must not appear in the registry.
STAGE_DIR="${DM_CFD_STAGE1_DIR:-stage1-dtchull}"

# --------------------------------------------------------------------------- #

ROOT="$(cfd_root)" || exit 1
RUNROOT="$ROOT/$STAGE_DIR"
CASE="$RUNROOT/$(basename "$TUTORIAL_CASE")"
TIMING="$RUNROOT/TIMING.csv"
DRIVER_LOG="$RUNROOT/driver.log"

mkdir -p "$RUNROOT" || { echo "FATAL cannot create $RUNROOT" >&2; exit 1; }

# Append-only, and NOT truncated on entry: a re-run must not erase the record
# of the attempt that preceded it.
: "${CFD_LOG:=$RUNROOT/PROGRESS}"
: "${CFD_MARKER:=$RUNROOT/stage1_dtchull.marker}"
export CFD_LOG CFD_MARKER

# Terminal marker on BOTH paths. A lane that marks only success makes silence
# mean "still running" and "died" simultaneously, and in practice it reads as
# success.
on_exit() {
  local rc=$?
  [ "$rc" -eq 0 ] || cfd_marker_fail "stage1 $TUTORIAL_CASE rc=$rc"
}
trap on_exit EXIT

[ -f "$TIMING" ] || echo "stage,seconds,rc" > "$TIMING"
cfd_say "DRIVER START pid=$$ host=$(hostname 2>/dev/null || echo unknown)"

cfd_load_openfoam
cfd_say "OpenFOAM ${WM_PROJECT_VERSION:-unknown} loaded"
[ -n "${FOAM_TUTORIALS:-}" ] || cfd_die "FOAM_TUTORIALS unset after loading OpenFOAM"

# --------------------------------------------------------------------------- #
# Stage timing. `< /dev/null` on every stage: the parallel stages end in
# mpirun, which reads and closes stdin, and an inherited stdin detaches the
# driver from its caller or hangs the run outright.
# --------------------------------------------------------------------------- #
tstage() {
  local name="$1"; shift
  local t0 t1 rc
  t0=$(date +%s.%N)
  cfd_say "BEGIN $name"
  "$@" >> "$DRIVER_LOG" 2>&1 < /dev/null
  rc=$?
  t1=$(date +%s.%N)
  echo "$name,$(echo "$t1 - $t0" | bc),$rc" >> "$TIMING"
  cfd_say "END   $name rc=$rc elapsed=$(echo "$t1 - $t0" | bc)s"
  if [ "$rc" -ne 0 ]; then cfd_die "$name failed rc=$rc (see $DRIVER_LOG)"; fi
}

# --------------------------------------------------------------------------- #
# Copy the tutorial. Refuse to destroy a previous baseline: it is evidence, and
# the original removed it unconditionally.
# --------------------------------------------------------------------------- #
if [ -e "$CASE" ]; then
  if [ "${DM_CFD_FORCE:-0}" = "1" ]; then
    cfd_say "DM_CFD_FORCE=1 -- replacing the previous baseline at $CASE"
    rm -rf -- "$CASE"
  else
    cfd_die "$CASE exists; refusing to overwrite a previous baseline (DM_CFD_FORCE=1 to replace)"
  fi
fi

SRC_TUTORIAL="$FOAM_TUTORIALS/$TUTORIAL_CASE"
[ -d "$SRC_TUTORIAL" ] || cfd_die "no tutorial at $SRC_TUTORIAL"
cp -r "$SRC_TUTORIAL" "$CASE" || cfd_die "cannot copy $SRC_TUTORIAL"
cfd_say "tutorial copied UNMODIFIED from \$FOAM_TUTORIALS/$TUTORIAL_CASE"

# The checksums are the proof of "unmodified". Without them the claim rests on
# this script never having been edited, which is not a property anyone can
# verify after the fact.
( cd "$CASE" && md5sum system/* constant/* 0.orig/* 2>/dev/null ) \
  > "$RUNROOT/case-checksums.txt"
cfd_say "case checksums written to case-checksums.txt"

cd "$CASE" || cfd_die "cannot enter $CASE"
# shellcheck disable=SC1090,SC1091
. "${WM_PROJECT_DIR:?}/bin/tools/RunFunctions"

mkdir -p constant/triSurface
cp -f "$FOAM_TUTORIALS/$TUTORIAL_GEOMETRY" constant/triSurface/ \
  || cfd_die "cannot stage geometry $TUTORIAL_GEOMETRY"
cfd_say "geometry staged: $TUTORIAL_GEOMETRY"

# --------------------------------------------------------------------------- #
# Decomposition is READ, never rewritten: the tutorial's own decomposeParDict
# is part of what "unmodified" means, and runParallel takes its rank count from
# it. Validate it up front anyway -- `hierarchical` requires
# prod(n) == numberOfSubdomains, and decomposePar exits fatally with "Wrong
# number of domain divisions" if it does not. Discovering that AFTER the mesh
# has been built wastes the whole mesh phase.
# --------------------------------------------------------------------------- #
check_decomposition() {
  local dict="system/decomposeParDict" nsub prod
  [ -f "$dict" ] || { cfd_say "NOTE no decomposeParDict in the tutorial"; return 0; }
  nsub=$(awk '/^[[:space:]]*numberOfSubdomains/ {v=$NF; gsub(/[^0-9]/,"",v); print v; exit}' "$dict")
  cfd_say "decomposition: numberOfSubdomains=${nsub:-unknown} (tutorial's own, unmodified)"
  grep -qE '^[[:space:]]*method[[:space:]]+hierarchical' "$dict" || return 0
  prod=$(awk '/^[[:space:]]*n[[:space:]]+\(/ {gsub(/[();]/," "); p=1;
              for (i = 1; i <= NF; i++) if ($i ~ /^[0-9]+$/) p *= $i; print p; exit}' "$dict")
  [ -n "$nsub" ] && [ -n "$prod" ] || return 0
  [ "$prod" -eq "$nsub" ] || cfd_die \
    "hierarchical n multiplies to $prod but numberOfSubdomains is $nsub; decomposePar would exit fatally"
}
check_decomposition

# --------------------------------------------------------------------------- #
# The tutorial's own Allrun sequence, stage for stage.
# --------------------------------------------------------------------------- #
tstage surfaceFeatureExtract runApplication surfaceFeatureExtract
tstage blockMesh             runApplication blockMesh

for i in $(seq 1 "$REFINE_PASSES"); do
  tstage "topoSet.$i"    runApplication -s "$i" topoSet -dict "system/topoSetDict.$i"
  tstage "refineMesh.$i" runApplication -s "$i" refineMesh -dict system/refineMeshDict -overwrite
done

tstage snappyHexMesh runApplication snappyHexMesh -overwrite

# checkMesh is NOT in the tutorial Allrun; added as a Stage-1 acceptance probe
# only. It does not gate the run -- the point of the baseline is to reproduce
# the tutorial, including any mesh the tutorial happens to produce.
cfd_say "BEGIN checkMesh (acceptance probe, not part of Allrun)"
runApplication checkMesh >> "$DRIVER_LOG" 2>&1 < /dev/null
# STRICT verdict rule: read the OUTPUT TEXT, not the exit code. checkMesh
# returns 0 even when it reports failed checks, so an exit-code gate would
# certify a mesh it had just been told was bad.
if grep -q "^Mesh OK" log.checkMesh 2>/dev/null \
   && ! grep -q "Failed .* mesh checks" log.checkMesh; then
  cfd_say "CHECKMESH VERDICT: PASS (Mesh OK, zero failed checks)"
else
  cfd_say "CHECKMESH VERDICT: FAIL -- $(grep -c 'Failed' log.checkMesh 2>/dev/null || echo 0) failure line(s)"
  grep -E "\*\*\*|Failed" log.checkMesh >> "$CFD_LOG" 2>/dev/null
fi
cfd_say "CELLS: $(cfd_mesh_cells "$CASE" || echo unknown)"
cfd_say "END   checkMesh"

restore0Dir >> "$DRIVER_LOG" 2>&1 < /dev/null
cfd_say "restore0Dir done"

# setFields is CORRECT here: this case is initialised from 0.orig, not from a
# mapped solution. (The fine grid is the opposite case -- see setup_fine.sh.)
tstage setFields                  runApplication setFields
tstage redistributePar_decompose  runParallel -s decompose redistributePar -decompose
tstage renumberMesh               runParallel renumberMesh -overwrite

SOLVER="$(getApplication)"
cfd_say "SOLVER START ($SOLVER, endTime from the tutorial's own controlDict)"
SOLVE_T0=$(date +%s.%N)
runParallel "$SOLVER" >> "$DRIVER_LOG" 2>&1 < /dev/null
SOLVE_RC=$?
SOLVE_T1=$(date +%s.%N)
echo "$SOLVER,$(echo "$SOLVE_T1 - $SOLVE_T0" | bc),$SOLVE_RC" >> "$TIMING"
cfd_say "SOLVER END rc=$SOLVE_RC elapsed=$(echo "$SOLVE_T1 - $SOLVE_T0" | bc)s"
[ "$SOLVE_RC" -eq 0 ] || cfd_die "$SOLVER failed rc=$SOLVE_RC"

tstage redistributePar_reconstruct runParallel -s reconstruct redistributePar -reconstruct

cfd_say "DRIVER COMPLETE"
cfd_marker_ok "stage1 $TUTORIAL_CASE complete"
