#!/bin/bash
# Build the wall-refined companion case: the companion base grid with a
# near-wall mesh refined enough to bring y+ into the wall-function band.
#
#   usage: setup_wallref.sh [src_case] [dst_case]
#          setup_wallref.sh            # kcs_companion -> kcs_companion_wallref
#
# WHY A NEW CASE. The source case holds a converged multi-hour result whose
# mean is stationary to +/-1.4% across five windows. It is the V3 input and it
# is not to be overwritten. Everything here writes into the DESTINATION only.
#
# WHAT CHANGES, AND ONLY THIS. The base grid, every refinement level, every
# numerical setting and every physical model stay identical. Only
# addLayersControls changes, so any shift in Cv is attributable to near-wall
# resolution and nothing else.
#
#   nSurfaceLayers      3    -> 8
#   expansionRatio      1.5  -> 1.3
#   finalLayerThickness 0.7  -> 0.4
#   minThickness        0.25 -> 0.05
#
# First-cell height, relative to local cell size:
#   before  0.7 / 1.5^2 = 0.311
#   after   0.4 / 1.3^7 = 0.0637      -> 4.88x thinner
# y+ scales linearly with it, so the measured companion range 11.75 - 1715.6
# (avg 154.9) should land near 2.4 - 352 (avg 31.7). The upper end is the one
# that matters: it is currently 5.7x outside the wall function's validity.
#
# THESE THICKNESSES ARE RELATIVE, AND THAT IS THE WHOLE DIFFERENCE FROM
# setup_yplus.sh. Under `relativeSizes true` a thickness is a FRACTION of the
# local cell size; the same number written into a dictionary with
# `relativeSizes false` is a length in metres -- 0.4 m on a 7.3 m hull. Both
# dictionaries parse and both mesh. This script therefore never introduces an
# absolute thickness key, never touches relativeSizes, and refuses to run
# against a source that is not relatively sized.
set -euo pipefail

CFD_LIB_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/lib" && pwd)"
# shellcheck source-path=SCRIPTDIR source=lib/cfd_chain.sh
. "$CFD_LIB_DIR/cfd_chain.sh"

SRC_CASE="${1:-kcs_companion}"
DST_CASE="${2:-kcs_companion_wallref}"

# --------------------------------------------------------------------------- #
# Layer parameters. ALL FOUR ARE RELATIVE (fractions of the local cell size).
# --------------------------------------------------------------------------- #

# More layers at a gentler expansion is what thins the first cell without
# changing the outer extent of the layer stack much.
WALLREF_N_SURFACE_LAYERS="${DM_CFD_WALLREF_N_SURFACE_LAYERS:-8}"
WALLREF_EXPANSION_RATIO="${DM_CFD_WALLREF_EXPANSION_RATIO:-1.3}"

# Thickness of the OUTERMOST layer, as a fraction of the local cell size.
# The first-cell height follows from it:
#     first = final / expansion^(nLayers - 1)
WALLREF_FINAL_LAYER_THICKNESS="${DM_CFD_WALLREF_FINAL_LAYER_THICKNESS:-0.4}"

# Layers thinner than this fraction are collapsed by snappyHexMesh. It has to
# drop with the first-cell height or the new layers are simply discarded and
# the mesh silently reverts to the old near-wall resolution.
WALLREF_MIN_THICKNESS="${DM_CFD_WALLREF_MIN_THICKNESS:-0.05}"

# --------------------------------------------------------------------------- #

ROOT="$(cfd_root)"
SRC="$(cfd_case_dir "$SRC_CASE")"
DST="$(cfd_case_dir "$DST_CASE")"

: "${CFD_LOG:=$ROOT/setup_wallref.log}"
: "${CFD_MARKER:=$ROOT/setup_wallref.marker}"
export CFD_LOG CFD_MARKER

on_exit() {
  local rc=$?
  [ "$rc" -eq 0 ] || cfd_marker_fail "setup_wallref $DST_CASE rc=$rc"
}
trap on_exit EXIT

[ -d "$SRC" ] || cfd_die "source case missing: $SRC"
[ -e "$DST" ] && cfd_die "$DST exists; refusing to overwrite"

mkdir -p "$DST"
# Configuration only. The processor*/ directories hold tens of thousands of
# iterations of results and are both enormous and irrelevant to a fresh mesh.
for d in system constant 0.orig geom; do
  [ -e "$SRC/$d" ] && cp -r "$SRC/$d" "$DST/$d"
done
# Any inherited mesh belongs to the source level; leaving it would let a solve
# start on the un-refined mesh this case exists to replace.
rm -rf -- "$DST/constant/polyMesh"
echo "copied: $(cd "$DST" && printf '%s ' *)"

DICT="$DST/system/snappyHexMeshDict"
[ -f "$DICT" ] || cfd_die "no snappyHexMeshDict in $DST_CASE"

# --------------------------------------------------------------------------- #
# Refuse an absolutely-sized source. 0.4 as a fraction is a near-wall cell;
# 0.4 as a length is a quarter of the hull. Nothing downstream would complain.
# --------------------------------------------------------------------------- #
if grep -qE '^[[:space:]]*relativeSizes[[:space:]]' "$DICT"; then
  grep -qE '^[[:space:]]*relativeSizes[[:space:]]+true;' "$DICT" \
    || cfd_die "$SRC_CASE sizes its layers absolutely; the relative thicknesses in this script would be read as metres (use setup_yplus.sh for absolute sizing)"
else
  # OpenFOAM's default is relativeSizes true, but an unstated default is not
  # something to bet a 24 h mesh-and-solve on.
  cfd_die "$SRC_CASE does not state relativeSizes; refusing to assume the default"
fi

# Each key must be unique in the file, or a blind sed edits the wrong block.
for k in nSurfaceLayers expansionRatio finalLayerThickness minThickness; do
  n=$(grep -cE "^[[:space:]]*${k}[[:space:]]" "$DICT" || true)
  [ "$n" -eq 1 ] || cfd_die "$k appears $n times in snappyHexMeshDict; refusing blind sed"
done

sed -i -E \
  -e "s/^([[:space:]]*)nSurfaceLayers[[:space:]]+[0-9.]+;/\\1nSurfaceLayers $WALLREF_N_SURFACE_LAYERS;/" \
  -e "s/^([[:space:]]*)expansionRatio[[:space:]]+[0-9.]+;/\\1expansionRatio      $WALLREF_EXPANSION_RATIO;/" \
  -e "s/^([[:space:]]*)finalLayerThickness[[:space:]]+[0-9.]+;/\\1finalLayerThickness $WALLREF_FINAL_LAYER_THICKNESS;/" \
  -e "s/^([[:space:]]*)minThickness[[:space:]]+[0-9.]+;/\\1minThickness        $WALLREF_MIN_THICKNESS;/" \
  "$DICT"

echo "--- addLayersControls after edit"
sed -n '/addLayersControls/,/^}/p' "$DICT" \
  | grep -E "relativeSizes|nSurfaceLayers|expansionRatio|finalLayerThickness|minThickness" \
  | sed 's/^/    /' || true

# --------------------------------------------------------------------------- #
# Report the derived first-cell height. The "before" numbers are read back from
# the source dictionary rather than assumed, so the printed ratio is a
# measurement of what actually changed.
# --------------------------------------------------------------------------- #
read_key() {
  awk -v k="$2" '$1 == k {v=$2; gsub(/[^0-9.eE+-]/,"",v); print v; exit}' "$1"
}
SRC_N="$(read_key "$SRC/system/snappyHexMeshDict" nSurfaceLayers)"
SRC_E="$(read_key "$SRC/system/snappyHexMeshDict" expansionRatio)"
SRC_F="$(read_key "$SRC/system/snappyHexMeshDict" finalLayerThickness)"
awk -v n0="$SRC_N" -v e0="$SRC_E" -v f0="$SRC_F" \
    -v n1="$WALLREF_N_SURFACE_LAYERS" -v e1="$WALLREF_EXPANSION_RATIO" \
    -v f1="$WALLREF_FINAL_LAYER_THICKNESS" 'BEGIN {
      # first-cell height = finalLayerThickness / expansionRatio^(nLayers - 1),
      # expressed as a fraction of the local cell size.
      b = f0 / (e0 ^ (n0 - 1)); a = f1 / (e1 ^ (n1 - 1));
      printf("--- first cell (fraction of local cell size)\n");
      printf("    before %.4f = %s / %s^%d\n", b, f0, e0, n0 - 1);
      printf("    after  %.4f = %s / %s^%d\n", a, f1, e1, n1 - 1);
      if (a > 0) printf("    %.2fx thinner; y+ scales linearly with it\n", b / a);
    }'

echo "--- confirming nothing else moved"
KEYS="nSurfaceLayers|expansionRatio|finalLayerThickness|minThickness"
if diff <(grep -vE "$KEYS" "$SRC/system/snappyHexMeshDict") \
        <(grep -vE "$KEYS" "$DICT") >/dev/null; then
  echo "    snappyHexMeshDict differs ONLY in the four layer keys"
else
  cfd_die "snappyHexMeshDict differs outside the four layer keys"
fi

fail=0
for f in controlDict fvSolution fvSchemes decomposeParDict blockMeshDict \
         meshQualityDict surfaceFeatureExtractDict setFieldsDict refineMeshDict; do
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
[ "$fail" -eq 0 ] || cfd_die "a held-constant file changed; the Cv shift would not be attributable to the near-wall mesh"

# The decomposition is inherited unchanged: this is a one-off experiment rather
# than a registry case, and it is compared against a source solved at the same
# rank count.
cfd_say "setup_wallref built $DST_CASE from $SRC_CASE"
echo "SETUP COMPLETE"
cfd_marker_ok "setup_wallref $DST_CASE"
