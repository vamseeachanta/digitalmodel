#!/bin/bash
# Build the y+-targeted case: the source case's grid, with the near-wall layer
# designed from a TARGET y+ instead of a fraction of the local cell size.
#
#   usage: setup_yplus.sh [src_case] [dst_case]
#          setup_yplus.sh              # kcs_production -> kcs_prod_yplus
#
# THE DEFECT THIS EXISTS TO FIX. addLayersControls used `relativeSizes true`,
# so the first-layer height was a fraction of whatever the LOCAL cell size
# happened to be. That inherits every bit of mesh-size variation across the
# hull, and y+ came out spanning 4.39 to 1075.67 -- a factor of 245. A wall
# function is valid to roughly y+ 300, so most of the hull was outside the
# model's range and C_v came out 2.4x the flat-plate line.
#
# THE FIX, DESIGNED FROM MEASUREMENT. mag(wallShearStress) was measured on the
# source case's own converged solution: max 0.059242 m^2/s^2, hence
# u_tau_max = sqrt(0.059242) = 0.24340 m/s. Since y+ = (t/2)*u_tau/nu, an
# ABSOLUTE first-layer thickness pins the worst-case y+ directly:
#
#   t = 2 * y+_target * nu / u_tau_max = 2 * 200 * 1.1418e-6 / 0.24340
#     = 1.8764e-03 m
#
# That value is DERIVED at run time from the three parameters below, never
# pasted: at another speed, another fluid or another grid the same literal is
# silently wrong, and the target y+ it came from would be unrecoverable.
#
# Predicted y+: mean 67.9, max 200 -- against 82.6 / 1075.7 today. Note the
# MEAN barely moves; it is the MAX that collapses 5.4x, which is the whole
# point. The wall-refinement experiment (setup_wallref.sh) moved the mean 2.5x
# and the max only 4.6%, and C_v stayed 274% high.
#
# nSurfaceLayers 6 at expansionRatio 1.3 grows 1.876e-3 back to the existing
# outer layer size; the stack totals 2.394e-2 m against ~2.0e-2 today, only
# 1.20x, so cell count and mesh quality should not move much.
#
# RELATIVE vs ABSOLUTE IS THE ENTIRE POINT -- DO NOT COLLAPSE THE TWO.
# snappyHexMesh reads `finalLayerThickness` and `firstLayerThickness` in units
# decided by `relativeSizes`. Flipping the flag without replacing the key (or
# the reverse) still parses, still meshes, and changes the near-wall spacing by
# orders of magnitude. This script flips both together and then verifies that
# no relative key survived. setup_wallref.sh is the relative variant and must
# stay that way.
#
# ONLY addLayersControls CHANGES. Same base grid as the source, same refinement
# levels, same numerics, same models -- verified byte-identical below. Any
# shift in C_v is then attributable to near-wall resolution alone.
set -euo pipefail

CFD_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/lib" && pwd)"
# shellcheck source-path=SCRIPTDIR source=lib/cfd_chain.sh
. "$CFD_LIB_DIR/cfd_chain.sh"

SRC_CASE="${1:-kcs_production}"
DST_CASE="${2:-kcs_prod_yplus}"

# --------------------------------------------------------------------------- #
# Wall-resolution design parameters
# --------------------------------------------------------------------------- #

# Worst-case y+ the first cell is sized to hit. 200 sits inside the wall
# function's validity (~300) with margin for the fact that u_tau on the refined
# mesh will not be identical to the measured one.
YPLUS_TARGET="${DM_CFD_YPLUS_TARGET:-${YPLUS_TARGET:-200}}"

# Kinematic viscosity, m^2/s. Must match constant/transportProperties for the
# water phase, or the thickness targets a y+ the solver will never see.
NU_KINEMATIC="${DM_CFD_NU_KINEMATIC:-${NU_KINEMATIC:-1.1418e-6}}"

# MEASURED, not assumed: max mag(wallShearStress) on the hull patch from the
# source case's own converged solution, in m^2/s^2 (interFoam reports the
# kinematic form, tau/rho). u_tau_max = sqrt of it. Re-measure this whenever
# the source case changes -- it is the only input here that comes from a
# solution rather than from the physics.
WALL_SHEAR_STRESS_MAX="${DM_CFD_WALL_SHEAR_STRESS_MAX:-${WALL_SHEAR_STRESS_MAX:-0.059242}}"

# Layer stack. 6 at 1.3 grows the first cell back to the existing outer layer
# size, so the total stack height barely changes and mesh quality holds.
YPLUS_N_SURFACE_LAYERS="${DM_CFD_YPLUS_N_SURFACE_LAYERS:-6}"
YPLUS_EXPANSION_RATIO="${DM_CFD_YPLUS_EXPANSION_RATIO:-1.3}"

# ABSOLUTE, in metres (relativeSizes is false in this case): layers thinner
# than this are collapsed. ~0.1x the first layer, so a slightly squeezed layer
# survives instead of being discarded.
YPLUS_MIN_THICKNESS="${DM_CFD_YPLUS_MIN_THICKNESS:-2.0e-04}"

# Escape hatch for a sensitivity study. Empty means "derive it", which is the
# only mode that keeps the target y+ recoverable from the case.
FIRST_LAYER_THICKNESS="${DM_CFD_FIRST_LAYER_THICKNESS:-}"

# --------------------------------------------------------------------------- #

ROOT="$(cfd_root)"
SRC="$(cfd_case_dir "$SRC_CASE")"
DST="$(cfd_case_dir "$DST_CASE")"

: "${CFD_LOG:=$ROOT/setup_yplus.log}"
: "${CFD_MARKER:=$ROOT/setup_yplus.marker}"
export CFD_LOG CFD_MARKER

on_exit() {
  local rc=$?
  [ "$rc" -eq 0 ] || cfd_marker_fail "setup_yplus $DST_CASE rc=$rc"
}
trap on_exit EXIT

# --------------------------------------------------------------------------- #
# Derive the first-layer thickness:  t = 2 * y+_target * nu / u_tau_max
# --------------------------------------------------------------------------- #
if [ -n "$FIRST_LAYER_THICKNESS" ]; then
  echo "WARNING first-layer thickness overridden to $FIRST_LAYER_THICKNESS m;" \
       "the target y+ is no longer recoverable from this case"
else
  FIRST_LAYER_THICKNESS="$(awk -v y="$YPLUS_TARGET" -v nu="$NU_KINEMATIC" \
      -v tau="$WALL_SHEAR_STRESS_MAX" 'BEGIN {
        utau = sqrt(tau);
        if (utau <= 0) exit 1;
        printf("%.4e", 2 * y * nu / utau);
      }')" || cfd_die "cannot derive the first-layer thickness from y+=$YPLUS_TARGET, nu=$NU_KINEMATIC, tau=$WALL_SHEAR_STRESS_MAX"
fi

awk -v y="$YPLUS_TARGET" -v nu="$NU_KINEMATIC" -v tau="$WALL_SHEAR_STRESS_MAX" \
    -v t="$FIRST_LAYER_THICKNESS" -v n="$YPLUS_N_SURFACE_LAYERS" \
    -v e="$YPLUS_EXPANSION_RATIO" 'BEGIN {
      utau = sqrt(tau);
      printf("--- near-wall design\n");
      printf("    u_tau_max      %.5f m/s   = sqrt(%s)\n", utau, tau);
      printf("    first layer    %s m  = 2 * %s * %s / u_tau_max\n", t, y, nu);
      printf("    stack height   %.4e m  (%d layers at %s)\n",
             t * (e ^ n - 1) / (e - 1), n, e);
    }'

# --------------------------------------------------------------------------- #

[ -d "$SRC" ] || cfd_die "source case missing: $SRC"
[ -e "$DST" ] && cfd_die "$DST exists; refusing to overwrite"

mkdir -p "$DST"
for d in system constant 0.orig geom; do
  [ -e "$SRC/$d" ] && cp -r "$SRC/$d" "$DST/$d"
done
# The inherited mesh belongs to the source case; this one is re-meshed with a
# different layer stack, which is the whole reason it exists.
rm -rf -- "$DST/constant/polyMesh"
echo "copied: $(cd "$DST" && printf '%s ' *)"

DICT="$DST/system/snappyHexMeshDict"
[ -f "$DICT" ] || cfd_die "no snappyHexMeshDict in $DST_CASE"

# relativeSizes must be PRESENT and unique: it is the key that decides whether
# every other thickness is a fraction or a length. An absent key would leave
# the dictionary on OpenFOAM's relative default while this script writes
# absolute metres into it -- the exact silent unit flip this case exists to fix.
n=$(grep -cE '^[[:space:]]*relativeSizes[[:space:]]' "$DICT" || true)
[ "$n" -eq 1 ] || cfd_die "relativeSizes appears $n times in snappyHexMeshDict; it must be present exactly once before switching to absolute sizing"
for k in nSurfaceLayers expansionRatio finalLayerThickness minThickness; do
  n=$(grep -cE "^[[:space:]]*${k}[[:space:]]" "$DICT" || true)
  [ "$n" -eq 1 ] || cfd_die "$k appears $n times in snappyHexMeshDict; refusing blind sed"
done

# The flip is one change, not two: the flag and the key move together.
sed -i -E \
  -e "s/^([[:space:]]*)relativeSizes[[:space:]]+.*/\\1relativeSizes       false;/" \
  -e "s/^([[:space:]]*)nSurfaceLayers[[:space:]]+[0-9.]+;/\\1nSurfaceLayers $YPLUS_N_SURFACE_LAYERS;/" \
  -e "s/^([[:space:]]*)expansionRatio[[:space:]]+[0-9.]+;/\\1expansionRatio      $YPLUS_EXPANSION_RATIO;/" \
  -e "s/^([[:space:]]*)finalLayerThickness[[:space:]]+[0-9.]+;/\\1firstLayerThickness $FIRST_LAYER_THICKNESS;/" \
  -e "s/^([[:space:]]*)minThickness[[:space:]]+[0-9.eE+-]+;/\\1minThickness        $YPLUS_MIN_THICKNESS;/" \
  "$DICT"

# --------------------------------------------------------------------------- #
# Verify the flip completed. A half-applied conversion parses and meshes:
#   - a relative key left under relativeSizes false is read as metres;
#   - an absolute key left under relativeSizes true is read as a fraction.
# Both produce a mesh, a y+ and a C_v that look like results.
# --------------------------------------------------------------------------- #
grep -qE '^[[:space:]]*relativeSizes[[:space:]]+false;' "$DICT" \
  || cfd_die "relativeSizes is not false after the edit; the absolute thicknesses would be read as fractions of the local cell"
grep -qE '^[[:space:]]*firstLayerThickness[[:space:]]' "$DICT" \
  || cfd_die "firstLayerThickness is absent after the edit"
if grep -qE '^[[:space:]]*finalLayerThickness[[:space:]]' "$DICT"; then
  cfd_die "finalLayerThickness survived the switch to absolute sizing; a relative fraction would be read as metres"
fi

echo "--- addLayersControls after edit"
sed -n '/addLayersControls/,/^}/p' "$DICT" \
  | grep -E "relativeSizes|nSurfaceLayers|expansionRatio|firstLayerThickness|minThickness" \
  | sed 's/^/    /' || true

# --------------------------------------------------------------------------- #
# Match the destination's decomposition to its registered rank count.
#
# The near-wall study is registered at a higher rank count than its source
# case, and `hierarchical` requires prod(n) == numberOfSubdomains: raising the
# rank count without rewriting the vector makes decomposePar exit fatally with
# "Wrong number of domain divisions", after the mesh has been built.
# cfd_decompose_n is the single source of that vector and refuses to return one
# that does not multiply out. (Deliberately duplicated from setup_fine.sh
# rather than added to the shared library, which is a separate contract.)
# --------------------------------------------------------------------------- #
DECOMP_CHANGED=0
sync_decomposition() {
  local dict="$DST/system/decomposeParDict" ranks nsub vec count
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
  count=$(grep -cE '^[[:space:]]*numberOfSubdomains[[:space:]]' "$dict" || true)
  [ "$count" -eq 1 ] || cfd_die "numberOfSubdomains appears $count times; refusing blind sed"
  sed -i -E "s/^([[:space:]]*)numberOfSubdomains[[:space:]]+[0-9]+;/\\1numberOfSubdomains $ranks;/" "$dict"
  if grep -qE '^[[:space:]]*method[[:space:]]+hierarchical' "$dict"; then
    vec="$(cfd_decompose_n "$ranks")"
    count=$(grep -cE '^[[:space:]]*n[[:space:]]+\(' "$dict" || true)
    [ "$count" -eq 1 ] || cfd_die "the hierarchical n vector appears $count times; refusing blind sed"
    sed -i -E "s/^([[:space:]]*)n[[:space:]]+\\([0-9 ]+\\);/\\1n               ($vec);/" "$dict"
    echo "    decomposeParDict: $nsub -> $ranks ranks, hierarchical n = ($vec)"
  else
    echo "    decomposeParDict: $nsub -> $ranks ranks (non-hierarchical method keeps its own partitioning)"
  fi
  DECOMP_CHANGED=1
}
sync_decomposition

# --------------------------------------------------------------------------- #
echo "--- everything else must be identical to $SRC_CASE"
fail=0
for f in fvSchemes fvSolution meshQualityDict blockMeshDict controlDict \
         surfaceFeatureExtractDict setFieldsDict refineMeshDict; do
  [ -f "$SRC/system/$f" ] || continue
  if diff -q "$SRC/system/$f" "$DST/system/$f" >/dev/null; then
    echo "    $f identical"
  else
    echo "    !! $f DIFFERS"; fail=1
  fi
done
for f in turbulenceProperties transportProperties g; do
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

echo "--- snappyHexMeshDict differs ONLY in the layer keys"
LAYER_KEYS="relativeSizes|nSurfaceLayers|expansionRatio|finalLayerThickness|firstLayerThickness|minThickness"
if diff <(grep -vE "$LAYER_KEYS" "$SRC/system/snappyHexMeshDict") \
        <(grep -vE "$LAYER_KEYS" "$DICT") >/dev/null; then
  echo "    confirmed"
else
  echo "    !! snappyHexMeshDict differs elsewhere"; fail=1
fi
[ "$fail" -eq 0 ] || cfd_die "a held-constant file changed; the C_v shift would not be attributable to near-wall resolution"

cfd_say "setup_yplus built $DST_CASE from $SRC_CASE at y+=$YPLUS_TARGET (t=$FIRST_LAYER_THICKNESS m)"
echo "SETUP COMPLETE"
cfd_marker_ok "setup_yplus $DST_CASE y+target=$YPLUS_TARGET t=$FIRST_LAYER_THICKNESS"
