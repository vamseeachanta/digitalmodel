#!/usr/bin/env bash
# Prune the PROGRAM ARTEFACTS of an assessed case and keep everything needed
# to rebuild or audit it. Dry-run unless --apply is given.
#
#   usage: prune_case.sh [--apply] [--reason "text"] <case> [<case> ...]
#
# KEPT (inputs + evidence):
#   system/                      all dicts (mesh identity inputs + solve settings)
#   constant/*Properties, constant/triSurface/*.stl
#   0.orig/                      initial fields
#   log.*, driver.log, TIMING.csv, PROGRESS, *.json, *.md, *.sh, *.csv
#   postProcessing/              forces / coefficients: the harvested result record
#
# REMOVED (rebuildable from the above):
#   constant/polyMesh            (a link is removed as a link, target untouched)
#   constant/extendedFeatureEdgeMesh, constant/triSurface/*.eMesh
#   processor*/                  decomposition + parallel results
#   <numeric time dirs>/         serial fields (0/, 1500/, ...); 0.orig is NOT one
#   dynamicCode/, VTK/
#
# Writes PRUNED.md into the case with what was removed and why, so a later
# reader does not mistake a pruned case for an unfinished one.
set -euo pipefail

APPLY=0; REASON="assessed; conditions re-run with free surface"
CASES=()
while [ $# -gt 0 ]; do
  case "$1" in
    --apply)  APPLY=1 ;;
    --reason) REASON="$2"; shift ;;
    -h|--help) sed -n '2,22p' "$0"; exit 0 ;;
    *) CASES+=("$1") ;;
  esac; shift
done
[ ${#CASES[@]} -gt 0 ] || { sed -n '2,22p' "$0" >&2; exit 64; }

ROOT="${DM_CFD_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
CASES_DIR="$ROOT/${DM_CFD_CASES_DIR:-cases}"

total_before=0; total_after=0
for name in "${CASES[@]}"; do
  case_dir="$CASES_DIR/$name"; [ -d "$case_dir" ] || case_dir="$name"
  [ -d "$case_dir" ] || { echo "SKIP $name: no such case"; continue; }
  name=$(basename "$case_dir")

  # Refuse to prune a case with a live process in it. Checked by the CWD of
  # real OpenFOAM/MPI processes, never by `pgrep -f <path>`: that matches the
  # command line of whoever invoked this script with the path (lane rule:
  # poll by PID, never by name -- it reported a dead run alive for 13.5 h).
  live=0
  for p in $(pgrep -x 'interFoam|simpleFoam|snappyHexMesh|mpirun|redistributePar|renumberMesh|decomposePar|setFields|blockMesh|refineMesh|topoSet|checkMesh|reconstructPar' 2>/dev/null); do
    case "$(readlink /proc/$p/cwd 2>/dev/null)/" in "$case_dir"/*) live=1 ;; esac
  done
  for pf in mesh.pid run.pid; do
    [ -f "$case_dir/$pf" ] && kill -0 "$(cat "$case_dir/$pf" 2>/dev/null)" 2>/dev/null && live=1
  done
  if [ "$live" = 1 ]; then echo "SKIP $name: a process is running in it"; continue; fi

  before=$(du -sb "$case_dir" | cut -f1)
  targets=()
  [ -e "$case_dir/constant/polyMesh" ] || [ -L "$case_dir/constant/polyMesh" ] && targets+=("constant/polyMesh")
  [ -d "$case_dir/constant/extendedFeatureEdgeMesh" ] && targets+=("constant/extendedFeatureEdgeMesh")
  for f in "$case_dir"/constant/triSurface/*.eMesh; do [ -e "$f" ] && targets+=("constant/triSurface/$(basename "$f")"); done
  for d in "$case_dir"/processor*; do [ -d "$d" ] && targets+=("$(basename "$d")"); done
  for d in "$case_dir"/*/; do
    b=$(basename "$d")
    printf '%s' "$b" | grep -qE '^[0-9]+(\.[0-9]+)?([eE][-+]?[0-9]+)?$' && targets+=("$b")
  done
  for d in dynamicCode VTK; do [ -d "$case_dir/$d" ] && targets+=("$d"); done

  if [ ${#targets[@]} -eq 0 ]; then
    printf '%-22s nothing to prune (%s)\n' "$name" "$(du -sh "$case_dir" | cut -f1)"; continue
  fi
  removable=0
  for t in "${targets[@]}"; do
    [ -L "$case_dir/$t" ] && continue
    removable=$(( removable + $(du -sb "$case_dir/$t" | cut -f1) ))
  done
  printf '%-22s %7s -> %7s  removing: %s\n' "$name" \
    "$(numfmt --to=iec "$before")" "$(numfmt --to=iec $(( before - removable )))" "${targets[*]}"
  total_before=$(( total_before + before )); total_after=$(( total_after + before - removable ))

  [ "$APPLY" = 1 ] || continue
  {
    echo "# Pruned $(date -u +%FT%TZ)"
    echo
    echo "Reason: $REASON"
    echo
    echo "Removed (rebuildable from system/, constant/, 0.orig/):"
    for t in "${targets[@]}"; do
      if [ -L "$case_dir/$t" ]; then echo "- $t (link -> $(readlink "$case_dir/$t"); target untouched)"
      else echo "- $t ($(du -sh "$case_dir/$t" | cut -f1))"; fi
    done
    echo
    echo "Kept: system/, constant/ dicts and STL surfaces, 0.orig/, logs, TIMING.csv, postProcessing/, provenance."
    echo "Size before: $(numfmt --to=iec "$before"); after: $(numfmt --to=iec $(( before - removable )))."
    echo "Rebuild: mesh via scripts/stage45_driver.sh mesh <case> (store reuse applies), then solve."
  } > "$case_dir/PRUNED.md"
  for t in "${targets[@]}"; do
    if [ -L "$case_dir/$t" ]; then rm -f "$case_dir/$t"; else rm -rf "${case_dir:?}/$t"; fi
  done
done
printf '\nTOTAL %s -> %s  (reclaim %s)%s\n' "$(numfmt --to=iec "$total_before")" \
  "$(numfmt --to=iec "$total_after")" "$(numfmt --to=iec $(( total_before - total_after )))" \
  "$([ "$APPLY" = 1 ] && echo '  APPLIED' || echo '  DRY RUN -- add --apply')"
