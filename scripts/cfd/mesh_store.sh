#!/usr/bin/env bash
# Master mesh store for a CFD campaign: one serial mesh per mesh IDENTITY,
# cases link to it instead of carrying a private copy.
#
# The OrcaFlex analogue: a master model + includes/ that variations reference,
# never copy. Here the "master" is constant/polyMesh and the "variation" is a
# case whose system/ and 0.orig/ differ.
#
#   mesh_store.sh id      <case>              print the identity hash of the case's mesh INPUTS
#   mesh_store.sh find    <case>              print the store dir whose identity matches, rc=1 if none
#   mesh_store.sh promote <case> <tag>        move the case's built serial mesh into the store, link back
#   mesh_store.sh link    <case> <store-dir>  replace the case's serial polyMesh with a link to the store
#   mesh_store.sh dedupe  <case> <store-dir>  replace a byte-identical private mesh with a link
#   mesh_store.sh verify  <case>              case inputs hash == provenance of the store it links to
#   mesh_store.sh status                      table of masters and the cases that link to each
#   mesh_store.sh drop    <store-dir>         remove a master nothing links to
#
# WHAT IS SHARED AND WHAT IS NOT. Only constant/polyMesh (serial) is shared.
# processor*/constant/polyMesh is NOT: redistributePar -decompose on lane-A
# is not reproducible run to run (sibling cases with byte-identical serial
# meshes and identical decomposeParDicts came out with different processor
# cell counts), and a case's processor time directories are numbered in ITS
# processor cell order. So a decomposition belongs to the results it holds;
# it is rebuilt per case in ~15 s, against 12-75 min for snappyHexMesh.
#
# IDENTITY. sha256 over every input that determines the serial mesh: the
# surfaces in constant/triSurface/*.stl and the meshing dicts in system/
# (blockMesh, surfaceFeatureExtract, snappyHexMesh, meshQuality, refineMesh,
# topoSetDict.N). Comments and whitespace are normalised so a re-commented
# dict does not fork the identity. decomposeParDict, controlDict, fvSchemes,
# fvSolution, 0.orig and the transport/turbulence dicts are NOT inputs: they
# change the solve, not the mesh.
#
# READ-ONLY. Master polyMesh files are chmod a-w. Anything that tries to write
# through the link (snappyHexMesh -overwrite, refineMesh -overwrite, topoSet,
# renumberMesh without -parallel, createPatch ...) fails loudly instead of
# silently rewriting every sibling. To re-mesh a case, unlink first (promote
# and the driver both do this).
#
# LINKS ARE RELATIVE (../../../meshes/<id>/polyMesh) so the campaign tree can
# be moved or rsynced as a unit.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT="${DM_CFD_ROOT:-$(cd "$SCRIPT_DIR/.." && pwd)}"
STORE="${DM_CFD_MESH_STORE:-$ROOT/meshes}"
CASES_DIR="$ROOT/${DM_CFD_CASES_DIR:-cases}"

die() { echo "mesh_store: FATAL: $*" >&2; exit 1; }
say() { echo "mesh_store: $*"; }

# Resolve a case argument (name or path) to an absolute case directory.
case_dir() {
  local c="$1"
  if [ -d "$c" ]; then (cd "$c" && pwd)
  elif [ -d "$CASES_DIR/$c" ]; then (cd "$CASES_DIR/$c" && pwd)
  else die "no case dir: $c"; fi
}

# Resolve a store argument (id, id-tag, or path) to an absolute store dir.
store_dir() {
  local s="$1" m
  if [ -d "$s" ]; then (cd "$s" && pwd); return; fi
  if [ -d "$STORE/$s" ]; then (cd "$STORE/$s" && pwd); return; fi
  m=$(ls -d "$STORE/$s"-* 2>/dev/null | head -1 || true)
  [ -n "$m" ] || die "no store entry: $s"
  (cd "$m" && pwd)
}

# Strip // and /* */ comments, blank lines, and collapse whitespace so the hash
# tracks meaning, not formatting.
norm_dict() {
  sed -e 's://.*$::' "$1" | sed -e ':a;s:/\*[^*]*\*/::g;ta' \
    | tr -s ' \t' ' ' | sed -e 's/^ //;s/ $//' | grep -v '^$' || true
}

# The list of input files, in a fixed order, relative to the case dir.
mesh_inputs() {
  local case="$1" f
  (
    cd "$case"
    for f in system/blockMeshDict system/surfaceFeatureExtractDict \
             system/snappyHexMeshDict system/meshQualityDict system/refineMeshDict; do
      [ -f "$f" ] && echo "$f"
    done
    ls system/topoSetDict.* 2>/dev/null | sort -t. -k2,2n || true
    ls constant/triSurface/*.stl 2>/dev/null | sort || true
  )
}

# Per-file sha256 (dicts normalised, STLs raw), as "sha  path" lines.
input_shas() {
  local case="$1" f
  mesh_inputs "$case" | while read -r f; do
    case "$f" in
      *.stl) printf '%s  %s\n' "$(sha256sum "$case/$f" | cut -d' ' -f1)" "$f" ;;
      *)     printf '%s  %s\n' "$(norm_dict "$case/$f" | sha256sum | cut -d' ' -f1)" "$f" ;;
    esac
  done
}

identity() {
  local case="$1" n
  n=$(mesh_inputs "$case" | wc -l)
  [ "$n" -ge 3 ] || die "$case has only $n mesh inputs; not a meshable case"
  input_shas "$case" | sha256sum | cut -c1-12
}

# Which store dir does a case's constant/polyMesh link into, if any.
linked_store() {
  local case="$1" t
  [ -L "$case/constant/polyMesh" ] || return 1
  t=$(readlink -f "$case/constant/polyMesh") || return 1
  [ -d "$t" ] || return 1
  dirname "$t"
}

mesh_cells() {
  grep -ao 'nCells:[0-9]*' "$1/owner" 2>/dev/null | head -1 | cut -d: -f2 || echo '?'
}

# ---------------------------------------------------------------------------
cmd_id()   { identity "$(case_dir "$1")"; }

cmd_find() {
  local case id m
  case=$(case_dir "$1"); id=$(identity "$case")
  m=$(ls -d "$STORE/$id"-* 2>/dev/null | head -1 || true)
  [ -n "$m" ] || { say "no master for identity $id ($1)" >&2; return 1; }
  echo "$m"
}

cmd_link() {
  local case store rel pm
  case=$(case_dir "$1"); store=$(store_dir "$2")
  [ -f "$store/polyMesh/owner" ] || die "$store has no polyMesh/owner"
  pm="$case/constant/polyMesh"
  if [ -e "$pm" ] && [ ! -L "$pm" ]; then
    die "$pm is a real directory; promote or remove it before linking"
  fi
  mkdir -p "$case/constant"
  rel=$(realpath --relative-to="$case/constant" "$store/polyMesh")
  rm -f "$pm"
  ln -s "$rel" "$pm"
  # Downstream gates read the mesh logs (db_job.sh greps hull layer coverage
  # from log.snappyHexMesh; the driver greps log.checkMesh). Give the case
  # copies so a reused mesh passes the same gates the original did.
  for l in "$store"/logs/log.*; do
    [ -f "$l" ] && [ ! -e "$case/$(basename "$l")" ] && cp -p "$l" "$case/"
  done
  say "linked $(basename "$case")/constant/polyMesh -> $rel ($(mesh_cells "$store/polyMesh") cells)"
}

cmd_promote() {
  local case tag id dest pm cov verdict cells
  case=$(case_dir "$1"); tag="${2:?usage: promote <case> <tag>}"
  pm="$case/constant/polyMesh"
  [ -L "$pm" ] && die "$pm is already a link ($(readlink "$pm"))"
  [ -f "$pm/owner" ] || die "$pm has no built mesh"
  id=$(identity "$case")
  dest="$STORE/$id-$tag"
  if ls -d "$STORE/$id"-* >/dev/null 2>&1; then
    die "identity $id already in store: $(ls -d "$STORE/$id"-*). Link to it, or drop it first."
  fi
  mkdir -p "$dest/inputs" "$dest/logs"
  mv "$pm" "$dest/polyMesh"
  # Keep the inputs beside the mesh so a future identity mismatch can be
  # diffed against what actually built it.
  mesh_inputs "$case" | while read -r f; do
    case "$f" in *.stl) ;; *) mkdir -p "$dest/inputs/$(dirname "$f")"; cp -p "$case/$f" "$dest/inputs/$f" ;; esac
  done
  for l in log.blockMesh log.surfaceFeatureExtract log.snappyHexMesh log.checkMesh log.topoSet.* log.refineMesh.*; do
    for f in "$case"/$l; do [ -f "$f" ] && cp -p "$f" "$dest/logs/"; done
  done
  cells=$(mesh_cells "$dest/polyMesh")
  cov=$(awk '/^hull /{c=$6} END{print c}' "$case/log.snappyHexMesh" 2>/dev/null || true)
  if [ -f "$case/log.checkMesh" ]; then
    if grep -q '^Mesh OK' "$case/log.checkMesh" && ! grep -q 'Failed .* mesh checks' "$case/log.checkMesh"; then verdict=PASS; else verdict=FAIL; fi
  else verdict=unknown; fi
  {
    echo "{"
    echo "  \"identity\": \"$id\","
    echo "  \"tag\": \"$tag\","
    echo "  \"built_from_case\": \"$(basename "$case")\","
    echo "  \"promoted_at\": \"$(date -u +%FT%TZ)\","
    echo "  \"host\": \"$(hostname -s)\","
    echo "  \"openfoam\": \"${WM_PROJECT_VERSION:-$(basename "$(ls -d /usr/lib/openfoam/openfoam* 2>/dev/null | head -1)")}\","
    echo "  \"cells\": ${cells:-null},"
    echo "  \"checkMesh\": \"$verdict\","
    echo "  \"hull_layer_coverage_pct\": ${cov:-null},"
    echo "  \"inputs\": {"
    input_shas "$case" | awk '{printf "%s    \"%s\": \"%s\"", (NR>1?",\n":""), $2, $1} END{print ""}'
    echo "  }"
    echo "}"
  } > "$dest/mesh_provenance.json"
  chmod -R a-w "$dest/polyMesh"
  cmd_link "$case" "$dest"
  say "promoted $(basename "$case") -> $dest ($cells cells, checkMesh $verdict, coverage ${cov:-?}%)"
}

cmd_dedupe() {
  # Replace a case's PRIVATE serial polyMesh with a link to a store entry,
  # only after proving the two are byte-identical (checkMesh's sets/ output
  # is diagnostic, not mesh, and is ignored). Refuses otherwise.
  local case store pm
  case=$(case_dir "$1"); store=$(store_dir "$2"); pm="$case/constant/polyMesh"
  [ -L "$pm" ] && { say "$(basename "$case") already links to $(readlink "$pm")"; return 0; }
  [ -f "$pm/owner" ] || die "$pm has no mesh to dedupe"
  if diff -rq -x sets "$store/polyMesh" "$pm" >/dev/null 2>&1; then
    rm -rf "$pm"
    cmd_link "$case" "$store"
    say "deduped $(basename "$case"): private copy removed, byte-identical to $(basename "$store")"
  else
    die "$(basename "$case") polyMesh differs from $store/polyMesh -- not touched"
  fi
}

cmd_verify() {
  local case store id pid
  case=$(case_dir "$1")
  store=$(linked_store "$case") || die "$(basename "$case") does not link into the store"
  id=$(identity "$case")
  pid=$(basename "$store" | cut -d- -f1)
  if [ "$id" = "$pid" ]; then
    say "OK $(basename "$case") inputs $id == $(basename "$store")"
  else
    say "MISMATCH $(basename "$case") inputs hash $id but links to $(basename "$store")" >&2
    say "  diff the case's mesh dicts against $store/inputs/ to see what moved" >&2
    return 2
  fi
}

cmd_status() {
  local m c t n
  [ -d "$STORE" ] || { say "no store at $STORE"; return 0; }
  printf '%-32s %10s %7s %6s  %s\n' MASTER CELLS CHECK SIZE LINKED_CASES
  for m in "$STORE"/*/; do
    [ -f "$m/polyMesh/owner" ] || continue
    n=""
    for c in "$CASES_DIR"/*/; do
      t=$(linked_store "$c" 2>/dev/null || true)
      [ "$t" = "${m%/}" ] && n="$n $(basename "$c")"
    done
    printf '%-32s %10s %7s %6s %s\n' "$(basename "$m")" "$(mesh_cells "$m/polyMesh")" \
      "$(grep -o '"checkMesh": "[A-Za-z]*"' "$m/mesh_provenance.json" 2>/dev/null | cut -d'"' -f4)" \
      "$(du -sh "$m/polyMesh" | cut -f1)" "${n:- (none)}"
  done
}

cmd_drop() {
  local store c t
  store=$(store_dir "$1")
  for c in "$CASES_DIR"/*/; do
    t=$(linked_store "$c" 2>/dev/null || true)
    [ "$t" = "$store" ] && die "$(basename "$c") still links to $store"
  done
  chmod -R u+w "$store"
  rm -rf "$store"
  say "dropped $store"
}

cmd="${1:-}"; shift || true
case "$cmd" in
  id|find|link|promote|dedupe|verify|status|drop) "cmd_$cmd" "$@" ;;
  *) sed -n '2,20p' "$0" >&2; exit 64 ;;
esac
