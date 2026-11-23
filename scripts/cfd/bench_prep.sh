#!/usr/bin/env bash
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/cfd_help.sh"
# Prepare identical-state OpenFOAM benchmark variants.
# Usage: bench_prep.sh --source CASE --dest DIR --iterations N --ranks LIST --variants FILE
set -euo pipefail
set -o pipefail
die() { echo "bench_prep: FATAL: $*" >&2; exit 1; }
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/cfd_chain.sh"
ROOT="$(cfd_campaign_root)"
source_case= dest= iterations= ranks_csv= variants_file=
while [ "$#" -gt 0 ]; do
  case "$1" in --source) source_case=$2; shift 2;; --dest) dest=$2; shift 2;;
    --iterations) iterations=$2; shift 2;; --ranks) ranks_csv=$2; shift 2;;
    --variants) variants_file=$2; shift 2;; *) die "unknown argument: $1";; esac
done
[ -z "$source_case" ] || [ -d "$source_case" ] || source_case="$ROOT/${DM_CFD_CASES_DIR:-cases}/$source_case"
if [ -n "$dest" ]; then case "$dest" in /*) ;; *) [ -e "$dest" ] || dest="$ROOT/$dest";; esac; fi
[ -d "$source_case" ] && [ -n "$dest" ] && [ -n "$iterations" ] && [ -f "$variants_file" ] || die "required: --source CASE --dest DIR --iterations N --ranks LIST --variants FILE"
mkdir -p "$dest"; : > "$dest/PREP_STATUS"; rm -f "$dest/PREP_DONE"
set +e
set +u
# shellcheck disable=SC1091
source "${WM_BASHRC:-/usr/lib/openfoam/openfoam2312/etc/bashrc}" >/dev/null 2>&1
set -u
set -e
for cmd in rsync foamDictionary setFields decomposePar renumberMesh; do command -v "$cmd" >/dev/null || die "$cmd not found"; done
printf 'name\tranks\tmpirun_flags\tcorrectors\tsmoother\n' > "$dest/variants.tsv"
declare -A wanted=()
IFS=, read -r -a requested <<< "$ranks_csv"; for r in "${requested[@]}"; do wanted[$r]=1; done
while IFS='|' read -r name ranks flags corr smoother; do
  [[ -z "$name" || "$name" = \#* ]] && continue
  [ -n "${wanted[$ranks]:-}" ] || die "variant $name uses rank count $ranks not present in --ranks"
  printf '%s\t%s\t%s\t%s\t%s\n' "$name" "$ranks" "$flags" "$corr" "$smoother" >> "$dest/variants.tsv"
done < "$variants_file"
for r in "${requested[@]}"; do
  state="$dest/states/ranks-$r"; mkdir -p "$state"
  rsync -a --delete --exclude='processor*' "$source_case/system" "$source_case/constant" "$source_case/0.orig" "$state/"
  (
    cd "$state"
    foamDictionary -entry startFrom -set startTime system/controlDict >/dev/null
    foamDictionary -entry startTime -set 0 system/controlDict >/dev/null
    foamDictionary -entry endTime -set "$iterations" system/controlDict >/dev/null
    foamDictionary -entry writeInterval -set "$iterations" system/controlDict >/dev/null
    foamDictionary -entry purgeWrite -set 1 system/controlDict >/dev/null
    foamDictionary -entry numberOfSubdomains -set "$r" system/decomposeParDict >/dev/null
    cp -a 0.orig 0
    setFields > log.setFields 2>&1
    decomposePar -force > log.decomposePar 2>&1
    renumberMesh -parallel -overwrite > log.renumberMesh 2>&1
  )
  printf '%s state ranks=%s ready\n' "$(date -u +%FT%TZ)" "$r" | tee -a "$dest/PREP_STATUS"
done
while IFS= read -r row; do
  name=$(printf '%s\n' "$row" | cut -f1); r=$(printf '%s\n' "$row" | cut -f2)
  flags=$(printf '%s\n' "$row" | cut -f3); corr=$(printf '%s\n' "$row" | cut -f4); smoother=$(printf '%s\n' "$row" | cut -f5)
  [ "$name" = name ] && continue
  v="$dest/variants/$name"; mkdir -p "$v"; rsync -a "$dest/states/ranks-$r/" "$v/"
  IFS=/ read -r outer ncorr nonorth <<< "$corr"
  foamDictionary -entry PIMPLE/nOuterCorrectors -set "$outer" "$v/system/fvSolution" >/dev/null
  foamDictionary -entry PIMPLE/nCorrectors -set "$ncorr" "$v/system/fvSolution" >/dev/null
  foamDictionary -entry PIMPLE/nNonOrthogonalCorrectors -set "$nonorth" "$v/system/fvSolution" >/dev/null
  foamDictionary -entry 'solvers/p_rgh/smoother' -set "$smoother" "$v/system/fvSolution" >/dev/null
  printf '%s variant=%s ready\n' "$(date -u +%FT%TZ)" "$name" | tee -a "$dest/PREP_STATUS"
done < "$dest/variants.tsv"
printf 'iterations=%s\ninitial_time=0\n' "$iterations" > "$dest/BENCH_META"
date -u +%FT%TZ > "$dest/PREP_DONE"
