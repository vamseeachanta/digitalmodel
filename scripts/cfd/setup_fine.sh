#!/bin/bash
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/cfd_help.sh"
# Build the fine grid: the third Richardson level, sqrt(2) finer than the
# source (production) level, prolonged from the source solution with
# mapFieldsPar.
#
#   usage: setup_fine.sh [build|map] [src_case] [dst_case]
#          setup_fine.sh                       # build kcs_production -> kcs_fine
#          setup_fine.sh map                   # after the mesh phase has run
#
# TWO PHASES, BECAUSE THE MAPPING CANNOT HAPPEN AT BUILD TIME. `build` is pure
# file manipulation: it writes the destination case's dictionaries and needs no
# OpenFOAM and no cores. `map` requires the destination to be MESHED and
# DECOMPOSED, so it runs after the mesh phase, and it is the step that
# prolongs the coarse solution onto the fine grid.
#
# RICHARDSON DISCIPLINE. Only the base blockMesh divisions change. Every
# snappyHexMesh refinement level, every fvSchemes entry, every fvSolution
# setting, the turbulence model and all LTS parameters are copied unchanged
# from the source case and verified byte-identical below. LTS violates temporal
# conservation, so changing maxCo/maxAlphaCo between levels would move the
# answer rather than the path to it.
#
# The grid family, measured from the two existing levels:
#   companion (36 16 43...) -> production (51 23 60...)   x1.417 y1.438 z~1.41
# fine continues it at the same ratios, which is why the refinement ratio is
# sqrt(2) and is applied to EVERY division of EVERY block rather than being
# four hand-computed literals in a sed expression (as in the original).
#
# THE CONVERGENCE STOP IS AN ADDITION, NOT A CHANGE. runTimeControl only
# decides WHEN to stop; it cannot alter the solution path. Production is
# verified converged (window means flat to 0.02% from iteration ~9,000), so
# stopping the fine grid at verified convergence keeps the pair comparable.
# The controlDict endTime is retained as a backstop -- runTimeControl only
# shortens.
set -eo pipefail

CFD_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/lib" && pwd)"
# shellcheck source-path=SCRIPTDIR source=lib/cfd_chain.sh
. "$CFD_LIB_DIR/cfd_chain.sh"

PHASE="${1:-build}"
SRC_CASE="${2:-kcs_production}"
DST_CASE="${3:-kcs_fine}"

# --------------------------------------------------------------------------- #
# Grid parameters
# --------------------------------------------------------------------------- #

# Linear refinement ratio between Richardson levels. sqrt(2) is the measured
# family ratio (see header); it is applied to x, y and z alike, so the cell
# aspect ratios of the source level are preserved exactly.
REFINEMENT_RATIO="${DM_CFD_REFINEMENT_RATIO:-1.4142135623730951}"

# The source level's base blockMesh divisions in x and y. Every block in the
# source blockMeshDict shares them (the z division varies per block), so they
# are what identifies a block to rescale. Change these only if the source
# case's base grid changes.
SRC_BASE_NX="${DM_CFD_SRC_BASE_NX:-51}"
SRC_BASE_NY="${DM_CFD_SRC_BASE_NY:-23}"

# --------------------------------------------------------------------------- #
# Force-coefficient parameters (model scale, from the KCS benchmark condition)
# --------------------------------------------------------------------------- #
FORCE_PATCH="${DM_CFD_FORCE_PATCH:-hull}"   # the wetted hull patch to integrate
RHO_INF="${DM_CFD_RHO_INF:-998.8}"          # kg/m3, water at test temperature
MAG_U_INF="${DM_CFD_MAG_U_INF:-2.1962}"     # m/s, Fn 0.26 at Lpp 7.2786 m
L_REF="${DM_CFD_L_REF:-7.2786}"             # m, Lpp (model scale)

# REFERENCE AREA -- THE FACTOR-OF-TWO TRAP. 4.71895 is HALF the published
# wetted surface S = 9.4379 m2. The domain is cut at the centreplane and
# `forces` integrates the hull patch only, so the reported force is half-body.
# Halving the area is what makes the reported Cd numerically equal to the true
# full-hull Ct. Getting this wrong is invisible: every coefficient stays
# plausible and is exactly 2x wrong. It cost a day on this issue.
A_REF="${DM_CFD_A_REF:-4.71895}"

COFR="${DM_CFD_COFR:-(3.70591 0 0.298993)}"  # centre of rotation, m
LIFT_DIR="${DM_CFD_LIFT_DIR:-(0 0 1)}"
DRAG_DIR="${DM_CFD_DRAG_DIR:-(1 0 0)}"
PITCH_AXIS="${DM_CFD_PITCH_AXIS:-(0 1 0)}"

# --------------------------------------------------------------------------- #
# Convergence-stop parameters (runTimeControl on the running mean of Cd)
# --------------------------------------------------------------------------- #
CD_TOLERANCE="${DM_CFD_CD_TOLERANCE:-4e-5}"        # absolute, on the window mean
CD_WINDOW="${DM_CFD_CD_WINDOW:-4000}"              # iterations in the mean
CD_N_ITER_STARTUP="${DM_CFD_CD_N_ITER_STARTUP:-9000}"  # ignore the transient

# --------------------------------------------------------------------------- #
# Prolongation parameters
# --------------------------------------------------------------------------- #

# Which source time is mapped. NEVER leave this to the default: mapFieldsPar
# would map time 0, i.e. prolong the initial condition, and the fine grid would
# start from nothing while looking exactly like a warm start.
MAP_SOURCE_TIME="${DM_CFD_MAP_SOURCE_TIME:-latestTime}"

# -consistent: source and target occupy the SAME domain and differ only in
# mesh. That is true by construction here (only blockMesh divisions changed),
# and it lets mapFieldsPar skip the patch-mapping heuristics.
MAP_CONSISTENT="${DM_CFD_MAP_CONSISTENT:-1}"

# --------------------------------------------------------------------------- #

ROOT="$(cfd_root)"
SRC="$(cfd_case_dir "$SRC_CASE")"
DST="$(cfd_case_dir "$DST_CASE")"

: "${CFD_LOG:=$ROOT/setup_fine.log}"
: "${CFD_MARKER:=$ROOT/setup_fine.marker}"
export CFD_LOG CFD_MARKER

on_exit() {
  local rc=$?
  [ "$rc" -eq 0 ] || cfd_marker_fail "setup_fine $PHASE $DST_CASE rc=$rc"
}
trap on_exit EXIT

# Files whose content must be identical to the source level for the Richardson
# comparison to mean anything. decomposeParDict is handled separately: it may
# differ intentionally when the destination is registered at a different rank
# count (partitioning changes nothing about the discretisation).
HELD_CONSTANT_SYSTEM=(fvSchemes fvSolution snappyHexMeshDict meshQualityDict
                      surfaceFeatureExtractDict setFieldsDict refineMeshDict)
HELD_CONSTANT_CONSTANT=(turbulenceProperties transportProperties g)

# --------------------------------------------------------------------------- #
# Match the destination's decomposition to its registered rank count.
#
# `hierarchical` requires prod(n) == numberOfSubdomains or decomposePar exits
# fatally with "Wrong number of domain divisions" -- after the mesh has been
# built. cfd_decompose_n is the single source of that vector and refuses to
# return one that does not multiply out.
# --------------------------------------------------------------------------- #
DECOMP_CHANGED=0
sync_decomposition() {
  local dict="$DST/system/decomposeParDict" ranks nsub vec n
  [ -f "$dict" ] || return 0
  if [ -z "${DM_CFD_CONFIG:-}" ]; then
    echo "    decomposeParDict: no registry configured; left as copied"
    return 0
  fi
  if ! cfd_cases | grep -qx "$DST_CASE"; then
    echo "    decomposeParDict: $DST_CASE is not in the registry; left as copied"
    return 0
  fi
  ranks="$(cfd_case_get "$DST_CASE" ranks)"
  nsub=$(awk '/^[[:space:]]*numberOfSubdomains/ {v=$NF; gsub(/[^0-9]/,"",v); print v; exit}' "$dict")
  if [ "$nsub" = "$ranks" ]; then
    echo "    decomposeParDict: already $ranks ranks"
    return 0
  fi
  n=$(grep -cE '^[[:space:]]*numberOfSubdomains[[:space:]]' "$dict" || true)
  [ "$n" -eq 1 ] || cfd_die "numberOfSubdomains appears $n times; refusing blind sed"
  sed -i -E "s/^([[:space:]]*)numberOfSubdomains[[:space:]]+[0-9]+;/\\1numberOfSubdomains $ranks;/" "$dict"
  if grep -qE '^[[:space:]]*method[[:space:]]+hierarchical' "$dict"; then
    vec="$(cfd_decompose_n "$ranks")"
    n=$(grep -cE '^[[:space:]]*n[[:space:]]+\(' "$dict" || true)
    [ "$n" -eq 1 ] || cfd_die "the hierarchical n vector appears $n times; refusing blind sed"
    sed -i -E "s/^([[:space:]]*)n[[:space:]]+\\([0-9 ]+\\);/\\1n               ($vec);/" "$dict"
    echo "    decomposeParDict: $nsub -> $ranks ranks, hierarchical n = ($vec)"
  else
    echo "    decomposeParDict: $nsub -> $ranks ranks (non-hierarchical method keeps its own partitioning)"
  fi
  DECOMP_CHANGED=1
}

verify_held_constant() {
  local f fail=0
  echo "--- files that MUST be identical to $SRC_CASE"
  for f in "${HELD_CONSTANT_SYSTEM[@]}"; do
    [ -f "$SRC/system/$f" ] || continue
    if diff -q "$SRC/system/$f" "$DST/system/$f" >/dev/null; then
      echo "    $f identical"
    else
      echo "    !! $f DIFFERS"; fail=1
    fi
  done
  for f in "${HELD_CONSTANT_CONSTANT[@]}"; do
    [ -f "$SRC/constant/$f" ] || continue
    if diff -q "$SRC/constant/$f" "$DST/constant/$f" >/dev/null; then
      echo "    constant/$f identical"
    else
      echo "    !! constant/$f DIFFERS"; fail=1
    fi
  done
  if [ "$DECOMP_CHANGED" -eq 1 ]; then
    echo "    decomposeParDict INTENTIONALLY differs (rank count; partitioning only)"
  elif [ -f "$SRC/system/decomposeParDict" ]; then
    if diff -q "$SRC/system/decomposeParDict" "$DST/system/decomposeParDict" >/dev/null; then
      echo "    decomposeParDict identical"
    else
      echo "    !! decomposeParDict DIFFERS"; fail=1
    fi
  fi
  [ "$fail" -eq 0 ] || cfd_die "a held-constant file changed; the Richardson pair is not comparable"
}

# --------------------------------------------------------------------------- #
phase_build() {
  [ -d "$SRC" ] || cfd_die "source case missing: $SRC"
  [ -e "$DST" ] && cfd_die "$DST exists; refusing to overwrite"

  mkdir -p "$DST"
  local d
  for d in system constant 0.orig geom; do
    [ -e "$SRC/$d" ] && cp -r "$SRC/$d" "$DST/$d"
  done
  # The reconstructed mesh belongs to the source level, not this one. Leaving
  # it behind would let blockMesh/snappyHexMesh appear to succeed against a
  # mesh nobody rebuilt.
  rm -rf -- "$DST/constant/polyMesh"
  echo "copied: $(cd "$DST" && printf '%s ' *)"

  # --- base grid: source x REFINEMENT_RATIO, every block, every direction ---
  REFINEMENT_RATIO="$REFINEMENT_RATIO" SRC_BASE_NX="$SRC_BASE_NX" \
  SRC_BASE_NY="$SRC_BASE_NY" python3 - "$DST/system/blockMeshDict" <<'PY'
import os, pathlib, re, sys

ratio = float(os.environ["REFINEMENT_RATIO"])
nx = int(os.environ["SRC_BASE_NX"])
ny = int(os.environ["SRC_BASE_NY"])
path = pathlib.Path(sys.argv[1])
text = path.read_text()

# Every division is scaled by the same ratio and rounded to the nearest cell,
# so the refinement is uniform and the cell aspect ratios are preserved.
new_nx, new_ny = round(nx * ratio), round(ny * ratio)
pattern = re.compile(r"\(\s*%d\s+%d\s+(\d+)\s*\)" % (nx, ny))


def scale(match: "re.Match") -> str:
    return "(%d %d %d)" % (new_nx, new_ny, round(int(match.group(1)) * ratio))


scaled, n = pattern.subn(scale, text)
if n == 0:
    sys.exit("FATAL no (%d %d N) blocks in blockMeshDict; "
             "SRC_BASE_NX/NY do not describe the source grid" % (nx, ny))
path.write_text(scaled)
print("blockMeshDict: %d block(s) scaled by %.6f -> base (%d %d N)"
      % (n, ratio, new_nx, new_ny))
PY

  echo "--- blockMesh divisions after scaling"
  grep -E "hex \(" "$DST/system/blockMeshDict" | sed 's/^/    /' || true
  if grep -qE "\($SRC_BASE_NX $SRC_BASE_NY [0-9]+\)" "$DST/system/blockMeshDict"; then
    cfd_die "an unscaled ($SRC_BASE_NX $SRC_BASE_NY N) block survives"
  fi

  # --- force coefficients + convergence stop -------------------------------
  FORCE_PATCH="$FORCE_PATCH" RHO_INF="$RHO_INF" MAG_U_INF="$MAG_U_INF" \
  L_REF="$L_REF" A_REF="$A_REF" COFR="$COFR" LIFT_DIR="$LIFT_DIR" \
  DRAG_DIR="$DRAG_DIR" PITCH_AXIS="$PITCH_AXIS" CD_TOLERANCE="$CD_TOLERANCE" \
  CD_WINDOW="$CD_WINDOW" CD_N_ITER_STARTUP="$CD_N_ITER_STARTUP" \
  python3 - "$DST/system/controlDict" <<'PY'
import os, pathlib, sys

env = os.environ
path = pathlib.Path(sys.argv[1])
text = path.read_text()

if "functions" not in text:
    sys.exit("FATAL controlDict has no functions block to extend")
if "forceCoeffs1" in text:
    sys.exit("FATAL controlDict already carries forceCoeffs1")

block = """
    forceCoeffs1
    {{
        type            forceCoeffs;
        libs            (forces);
        patches         ({patch});
        rho             rhoInf;
        rhoInf          {rho};
        magUInf         {u};
        lRef            {lref};
        Aref            {aref};
        liftDir         {lift};
        dragDir         {drag};
        CofR            {cofr};
        pitchAxis       {pitch};
        log             on;
        writeControl    timeStep;
        writeInterval   1;
    }}

    converged
    {{
        type            runTimeControl;
        libs            (utilityFunctionObjects);
        conditions
        {{
            CdConverged
            {{
                type            average;
                functionObject  forceCoeffs1;
                fields          (Cd);
                tolerance       {tol};
                window          {window};
                windowType      exact;
                nIterStartUp    {startup};
            }}
        }}
        satisfiedAction end;
        nWriteStep      1;
    }}
""".format(patch=env["FORCE_PATCH"], rho=env["RHO_INF"], u=env["MAG_U_INF"],
           lref=env["L_REF"], aref=env["A_REF"], lift=env["LIFT_DIR"],
           drag=env["DRAG_DIR"], cofr=env["COFR"], pitch=env["PITCH_AXIS"],
           tol=env["CD_TOLERANCE"], window=env["CD_WINDOW"],
           startup=env["CD_N_ITER_STARTUP"])

# Insert before the closing brace of the functions block.
i = text.rindex("}")
path.write_text(text[:i] + block + text[i:])
print("controlDict: forceCoeffs1 + runTimeControl inserted")
PY

  sync_decomposition
  verify_held_constant

  echo "--- controlDict: only the added function objects should differ"
  diff "$SRC/system/controlDict" "$DST/system/controlDict" | grep -E "^[<>]" \
    | grep -vE "forceCoeffs1|runTimeControl|conditions|converged|CdConverged|tolerance|window|nIterStartUp|satisfiedAction|nWriteStep|magUInf|lRef|Aref|liftDir|dragDir|pitchAxis|type|libs|patches|rho|log|writeControl|writeInterval|CofR|fields|functionObject|^[<>] *[{}]|^[<>] *$" \
    && echo "    (any lines above are UNEXPECTED controlDict changes)" \
    || echo "    only the added blocks differ"

  echo "BUILD COMPLETE -- next: mesh $DST_CASE, then '$0 map $SRC_CASE $DST_CASE'"
  cfd_marker_ok "setup_fine build $DST_CASE"
}

# --------------------------------------------------------------------------- #
# Prolong the source solution onto the fine grid.
# --------------------------------------------------------------------------- #
count_processor_dirs() {
  local d n=0
  for d in "$1"/processor*; do [ -d "$d" ] && n=$((n + 1)); done
  echo "$n"
}

phase_map() {
  [ -d "$SRC" ] || cfd_die "source case missing: $SRC"
  [ -d "$DST" ] || cfd_die "destination case missing: $DST (run the build phase first)"

  local np_src np_dst ntimes t flags
  np_dst="$(count_processor_dirs "$DST")"
  np_src="$(count_processor_dirs "$SRC")"
  [ "$np_dst" -gt 0 ] || cfd_die "$DST_CASE is not decomposed; run the mesh phase first"
  # A parallel map reads the source case in parallel too, so the two
  # decompositions must agree. Mismatched counts fail deep inside the utility
  # with a message about missing processor directories.
  [ "$np_src" -eq "$np_dst" ] \
    || cfd_die "decomposition mismatch: $SRC_CASE has $np_src processor dirs, $DST_CASE has $np_dst"

  # A source that never wrote a time beyond 0 has nothing to prolong; mapping
  # its initial condition produces a "warm start" that is entirely fictional.
  ntimes=0
  for t in "$SRC"/processor0/[1-9]*; do [ -d "$t" ] && ntimes=$((ntimes + 1)); done
  [ "$ntimes" -gt 0 ] || cfd_die "$SRC_CASE has no written time directories; nothing to map"

  cfd_load_openfoam

  flags=(-sourceTime "$MAP_SOURCE_TIME")
  [ "$MAP_CONSISTENT" = "1" ] && flags+=(-consistent)

  cfd_say "mapFieldsPar $SRC_CASE($MAP_SOURCE_TIME) -> $DST_CASE on $np_dst ranks"
  # `< /dev/null`: mpirun reads and closes stdin.
  ( cd "$DST" && mpirun -np "$np_dst" mapFieldsPar "$SRC" "${flags[@]}" -parallel \
      > "$DST/log.mapFieldsPar" 2>&1 < /dev/null ) \
    || cfd_die "mapFieldsPar failed (see $DST/log.mapFieldsPar)"
  grep -q "FOAM FATAL" "$DST/log.mapFieldsPar" \
    && cfd_die "mapFieldsPar reported a FOAM FATAL error despite exiting 0"

  write_runsolve "$np_dst"
  echo "MAP COMPLETE -- solve with $DST/runsolve.sh"
  cfd_marker_ok "setup_fine map $DST_CASE from $SRC_CASE@$MAP_SOURCE_TIME"
}

# --------------------------------------------------------------------------- #
# Emit the case's own solve script.
#
# THIS IS THE PROTECTION, AND IT IS THE POINT OF THE WHOLE PHASE. A case whose
# initial field came from mapFieldsPar must be solved by invoking the solver
# DIRECTLY. The mesh/solve driver re-runs restore0Dir and setFields before
# solving, either of which overwrites the mapped field with the uniform initial
# condition -- and the run then looks completely normal, just started from
# nothing. On the host-side original this was avoided only by accident:
# runApplication refuses to repeat a stage whose log already exists. An
# accident is not a guarantee, so the mapped case gets its own entry point that
# cannot reach those stages.
# --------------------------------------------------------------------------- #
write_runsolve() {
  local np="$1"
  cat > "$DST/runsolve.sh" <<RUNSOLVE
#!/bin/bash
# Solve $DST_CASE, whose initial field was MAPPED from $SRC_CASE.
#
# interFoam is invoked directly, NOT through the mesh/solve driver: that driver
# re-runs restore0Dir/setFields before solving, which would overwrite the
# mapped initial field. Do not "simplify" this into a driver call.
#
# Generated by scripts/cfd/setup_fine.sh -- edit that, not this.

# The OpenFOAM bashrc dereferences unset variables and calls pop_var_context,
# so it must never be sourced under set -e/-u.
set +eu
# shellcheck disable=SC1090
source "\${WM_BASHRC:-/usr/lib/openfoam/openfoam2312/etc/bashrc}" >/dev/null 2>&1
set -eo pipefail

cd "\$(cd "\$(dirname "\${BASH_SOURCE[0]}")" && pwd)"
echo "\$(date -u +%Y-%m-%dT%H:%M:%SZ) SOLVE START (mapped IC, $np ranks)" >> PROGRESS
# \`< /dev/null\`: mpirun reads and closes stdin.
mpirun -np $np interFoam -parallel > log.interFoam 2>&1 < /dev/null
rc=\$?
echo "\$(date -u +%Y-%m-%dT%H:%M:%SZ) SOLVE END rc=\$rc" >> PROGRESS
exit \$rc
RUNSOLVE
  chmod +x "$DST/runsolve.sh"
  echo "wrote $DST/runsolve.sh (direct solver invocation, $np ranks)"
}

case "$PHASE" in
  build) phase_build ;;
  map)   phase_map ;;
  *)     cfd_die "usage: $0 [build|map] [src_case] [dst_case]" ;;
esac
