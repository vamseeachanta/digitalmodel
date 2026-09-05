#!/bin/bash
# lane_probe.sh <lane> <case-dir> [force-unit-divisor]   -> one pipe-separated row
lane=$1; c=$2; div=${3:-1000}; unit=$([ "$div" = 1000 ] && echo kN || echo N)
name=$(basename "$c"); l=$c/log.interFoam
if [ ! -f "$l" ]; then st=queued; [ -f $c/MESH_DONE ] && st="queued (mesh done)"; printf '%s|%s|%s|-|-|-|-|-|-|-|-|-|-|-\n' "$lane" "$name" "$st"; exit 0; fi
st=running; [ -f $c/RUN_DONE ] || [ -f $c/CONT_DONE ] && st=done; [ -f $c/RUN_FAILED ] || [ -f $c/CONT_FAILED ] && st=FAILED
n=$(grep -ac '^Time = ' "$l"); last=$(grep -a '^Time = ' "$l" | tail -1 | awk '{print $3}'); e=$(grep -aE '^endTime' $c/system/controlDict | tr -dc '0-9')
ex=$(grep -a '^ExecutionTime' "$l" | tail -1 | awk '{print $3}')
r=$(grep -a '^ExecutionTime' "$l" | awk '{print $3}' | awk -v n=$n 'NR==n-50{a=$1} END{if(n>50) printf "%.1f",($1-a)/50; else printf "%.1f", $1/n}')
wall=$(awk -v x="$ex" 'BEGIN{printf "%.1f", x/3600}')
raw=$(grep -a '^Flow time scale min/max' "$l" | tail -1 | awk '{printf "%.1e/%s", $(NF-1), $NF}' | tr -d ',')
sm=$(grep -a '^Smoothed flow time scale' "$l" | tail -1 | awk '{printf "%.1e/%s", $(NF-1), $NF}' | tr -d ',')
co="$(grep -aE '^\s*maxCo' $c/system/fvSolution | tr -dc '0-9.' )/$(grep -aE '^\s*maxAlphaCo' $c/system/fvSolution | tr -dc '0-9.')"
ph=$(grep -a 'Phase-1 volume fraction' "$l" | awk 'NR==1{a=$5} END{printf "%+.4f", ($5-a)/a*100}')
umax=$(grep -a 'max(U)' "$l" | tail -1 | sed -n 's/.*max(U) = (\([^)]*\)).*/\1/p' | awk '{printf "%.2f", sqrt($1*$1+$2*$2+$3*$3)}')
w=$(ls -d $c/processor0/[0-9]* 2>/dev/null | xargs -n1 basename | sort -g | tail -1)
f=$(ls -t $c/postProcessing/forces_hull/*/force.dat $c/postProcessing/forces/*/force.dat 2>/dev/null | head -1)
fr=$(grep -v '^#' "$f" 2>/dev/null | tail -1 | awk -v d=$div -v u=$unit '{if(d==1) printf "%+.2f/%+.2f %s", $5/d, $8/d, u; else printf "%+.0f/%+.0f %s", $5/d, $8/d, u}')
pr=$(grep -a 'Solving for p_rgh' "$l" | tail -1 | sed -n 's/.*Initial residual = \([^,]*\),.*/\1/p' | awk '{printf "%.1e", $1}')
printf '%s|%s|%s|%s/%s|%s|%s h|%s|%s|%s|%s|%s|%s|%s|%s\n' "$lane" "$name" "$st" "$last" "$e" "$r" "$wall" "$w" "$raw" "$sm" "$co" "$ph" "$umax" "$fr" "$pr"
