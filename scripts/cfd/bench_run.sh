#!/usr/bin/env bash
# Run prepared benchmark variants serially and summarise their solver logs.
set -euo pipefail
set -o pipefail
die() { echo "bench_run: FATAL: $*" >&2; exit 1; }
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/cfd_chain.sh"
ROOT="$(cfd_campaign_root)"
dest= after= max_load= epilogue=
while [ "$#" -gt 0 ]; do case "$1" in --dest) dest=$2; shift 2;; --after-marker) after=$2; shift 2;;
  --max-load) max_load=$2; shift 2;; --epilogue) epilogue=$2; shift 2;; *) die "unknown argument: $1";; esac; done
if [ -n "$dest" ]; then case "$dest" in /*) ;; *) [ -d "$dest" ] || dest="$ROOT/$dest";; esac; fi
[ -d "$dest" ] && [ -f "$dest/variants.tsv" ] || die "prepared --dest is required"
[ -z "$after" ] || { while [ ! -e "$after" ]; do sleep 2; done; }
for s in interFoam simpleFoam; do ! pgrep -x "$s" >/dev/null 2>&1 || die "$s is already running"; done
if [ -n "$max_load" ]; then
  load=$(awk '{print $1}' /proc/loadavg); awk -v a="$load" -v b="$max_load" 'BEGIN{exit !(a<=b)}' || die "1-minute load $load exceeds $max_load"
fi
set +e
set +u
# shellcheck disable=SC1091
source "${WM_BASHRC:-/usr/lib/openfoam/openfoam2312/etc/bashrc}" >/dev/null 2>&1
set -u
set -e
initial_time=$(sed -n 's/^initial_time=//p' "$dest/BENCH_META" 2>/dev/null | head -1); initial_time=${initial_time:-0}
header=$'variant\tranks\titerations\ts_per_it\tus_per_cell_iteration\ts_per_it_1_50\ts_per_it_last_50\tCd_last_quarter\tpressure_share_pct\tviscous_share_pct\tfinal_p_rgh_initial_residual'
printf '%s\n' "$header" > "$dest/bench_results.tsv"; : > "$dest/BENCH_STATUS"; rm -f "$dest/BENCH_DONE"
while IFS= read -r row; do
  name=$(printf '%s\n' "$row" | cut -f1); ranks=$(printf '%s\n' "$row" | cut -f2)
  flags=$(printf '%s\n' "$row" | cut -f3); corr=$(printf '%s\n' "$row" | cut -f4); smoother=$(printf '%s\n' "$row" | cut -f5)
  [ "$name" = name ] && continue
  v="$dest/variants/$name"; solver=$(awk '/^[[:space:]]*application[[:space:]]/{gsub(/;/,"",$2); print $2; exit}' "$v/system/controlDict")
  [ -n "$solver" ] || die "$name has no solver application"
  printf '%s variant=%s RUNNING\n' "$(date -u +%FT%TZ)" "$name" | tee -a "$dest/BENCH_STATUS"
  (cd "$v" && mpirun -np "$ranks" $flags "$solver" -parallel > "log.$solver" 2>&1)
  log="$v/log.$solver"
  metrics=$(awk '
    /^Time = / {n++; t[n]=$3}
    /^ExecutionTime = / {e[n]=$3}
    /Solving for p_rgh/ {if (match($0,/Initial residual = [^,]+/)) pr=substr($0,RSTART+19,RLENGTH-19)}
    END { if (!n) exit 2; total=e[n]/n; first=(n>=50?(e[50]-e[1])/49:total); k=(n>50?n-50:1); last=(e[n]-e[k])/(n-k?n-k:1); printf "%d\t%.6g\t%.6g\t%.6g\t%s",n,total,first,last,pr }
  ' "$log") || die "$name produced no iterations"
  IFS=$'\t' read -r its spi first50 last50 pr <<< "$metrics"
  cells=$(find "$v" -path '*/constant/polyMesh/owner' -type f -print0 2>/dev/null \
    | xargs -0 grep -aho 'nCells:[0-9]*' 2>/dev/null | awk -F: '{s+=$2} END{print s+0}' || true)
  cells=${cells:-0}
  us=$(awk -v s="$spi" -v c="$cells" 'BEGIN{if(c) printf "%.6g",s*1e6/c; else print "nan"}')
  force=$(find "$v/postProcessing" -path '*/force.dat' -type f 2>/dev/null | head -1 || true)
  read -r force_mean pshare vshare < <(awk '
    !/^#/ && NF {n++; p[n]=$5+0; v[n]=$8+0}
    END {start=int(n*0.75)+1; for(i=start;i<=n;i++){ps+=p[i];vs+=v[i];k++}; total=ps+vs; if(!k||total==0) print "nan nan nan"; else printf "%.6g %.6g %.6g\n",total/k,100*ps/total,100*vs/total}
  ' "$force" 2>/dev/null || echo 'nan nan nan')
  coeff=$(find "$v/postProcessing" -path '*/coefficient.dat' -type f 2>/dev/null | head -1 || true)
  cd=$(awk '!/^#/ && NF {n++; x[n]=$2+0} END{s=int(n*.75)+1; for(i=s;i<=n;i++){a+=x[i];k++}; if(k) printf "%.6g",a/k; else print "nan"}' "$coeff" 2>/dev/null || echo nan)
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$name" "$ranks" "$its" "$spi" "$us" "$first50" "$last50" "$cd" "$pshare" "$vshare" "$pr" >> "$dest/bench_results.tsv"
  find "$v" -maxdepth 1 -type d -regextype posix-extended -regex '.*/[0-9]+([.][0-9]+)?' ! -name 0 -exec rm -rf {} +
  for proc in "$v"/processor*/; do
    [ -d "$proc" ] || continue
    find "$proc" -maxdepth 1 -type d -regextype posix-extended -regex '.*/[0-9]+([.][0-9]+)?' ! -name "$initial_time" -exec rm -rf {} +
  done
  printf '%s variant=%s DONE\n' "$(date -u +%FT%TZ)" "$name" | tee -a "$dest/BENCH_STATUS"
done < "$dest/variants.tsv"
date -u +%FT%TZ > "$dest/BENCH_DONE"
[ -z "$epilogue" ] || (cd "$dest" && bash -c "$epilogue")
