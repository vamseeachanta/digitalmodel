#!/bin/bash
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/cfd_help.sh"
# Local variant of the primary lane's stage4 gate (fetched 2026-09-04): the same
# gates, run ON the solve host without ssh.  usage: stage4_gate_local.sh <case>
# Gate 1 mass balance |drift| <= 0.5 %; Gate 2 settling: two 400-iteration windows of
# forces_hull: POWER GATE = total force within 1 % between two 400-it windows and pressure wobble
# < 2 % of the total (decision 2026-09-06; the 0.2 % per-component gate is reported for information);
# plausibility 0.6 < Cf/ITTC < 1.3.
set -o pipefail
CASE=$1
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/cfd_chain.sh"
ROOT="$(cfd_campaign_root)"
source /usr/lib/openfoam/openfoam2312/etc/bashrc >/dev/null 2>&1
cd "$ROOT/${DM_CFD_CASES_DIR:-cases}/$CASE" || { echo "no case $CASE"; exit 2; }
echo "=== $CASE on $(hostname -s) $(date -u +%FT%TZ) ==="
fail=0

# ---- gate 1: mass balance ---------------------------------------------------
FIRST=$(grep -a -m1 '^Phase-1 volume fraction' log.interFoam 2>/dev/null | awk '{print $5}')
LAST=$(grep -a '^Phase-1 volume fraction' log.interFoam 2>/dev/null | tail -1 | awk '{print $5}')
NIT=$(grep -ac '^Time = ' log.interFoam 2>/dev/null)
if [ -z "$FIRST" ] || [ -z "$LAST" ]; then
  echo "  MASS BALANCE        : FAIL (no Phase-1 volume fraction line in log.interFoam)"; fail=1
else
  awk -v a="$FIRST" -v b="$LAST" -v n="$NIT" 'BEGIN{
    d=100*(b-a)/a; ok=(d<0.5 && d>-0.5)
    printf "  Phase-1 fraction    : first %.6f  last %.6f  over %d iterations\n", a, b, n
    printf "  mass drift          : %+.4f %%   (gate 0.5 %%)\n", d
    printf "  MASS BALANCE        : %s\n", ok?"PASS":"FAIL"
    exit !ok }' || fail=1
fi
BEACH=$(grep -ac '^beachDamping:' log.interFoam 2>/dev/null)
echo "  beach report lines  : ${BEACH:-0}  $(grep -a -m1 '^beachDamping:' log.interFoam 2>/dev/null | cut -c1-70)"

# ---- gate 2: settling + plausibility -----------------------------------------
seg=$(ls -d postProcessing/forces_hull/*/ 2>/dev/null | awk -F/ '{print $(NF-1)}' | sort -g | tail -1)
f="postProcessing/forces_hull/$seg/force.dat"
if [ ! -f "$f" ]; then
  echo "  SETTLING GATE       : FAIL (no $f)"; fail=1
else
  R=$(grep -vc '^#' "$f")
  echo "  force segment       : $f ($R rows)"
  if [ "$R" -lt 800 ]; then
    echo "  SETTLING GATE       : FAIL (fewer than 800 rows)"; fail=1
  else
    U=$(foamDictionary -entry functions/forceCoeffs/magUInf -value system/controlDict)
    A=$(foamDictionary -entry functions/forceCoeffs/Aref    -value system/controlDict)
    L=$(foamDictionary -entry functions/forceCoeffs/lRef    -value system/controlDict)
    D=$(foamDictionary -entry functions/forceCoeffs/rhoInf  -value system/controlDict)
    NU=$(foamDictionary -entry water/nu -value constant/transportProperties)
    echo "  rhoInf / nu(water)  : $D / $NU   (read from the case)"
    grep -v '^#' "$f" | awk -v U="$U" -v A="$A" -v D="$D" -v L="$L" -v NU="$NU" '
     {n++; P[n]=($5<0?-$5:$5); V[n]=($8<0?-$8:$8); T[n]=($5+$8<0?-($5+$8):$5+$8)}
     END{q=0.5*D*U*U*A; Re=U*L/NU; cf=0.075/((log(Re)/log(10))-2)^2
         for(i=n-399;i<=n;i++){bp+=P[i];bv+=V[i]}
         for(i=n-799;i<=n-400;i++){ap+=P[i];av+=V[i]}
         bp/=400;bv/=400;ap/=400;av/=400
         dv=100*(bv-av)/av; dp=100*(bp-ap)/ap
         printf "  Re / ITTC Cf        : %.3e / %.6f\n", Re, cf
         printf "  window A (%d-%d) : Cf %.4f  Cp %.4f\n", n-799, n-400, av/q/cf, ap/q/cf
         printf "  window B (%d-%d) : Cf %.4f  Cp %.4f\n", n-399, n,     bv/q/cf, bp/q/cf
         printf "  final 400           : Cf %.4f  Cp %.4f  C_T/ITTC %.4f\n", bv/q/cf, bp/q/cf, (bv+bp)/q/cf
         printf "  two-window drift    : visc %+.3f %%  press %+.3f %%   (component gate 0.2 %%, informational)\n", dv, dp
         # total resistance = pressure + viscous with sign (resistance-negative convention in force.dat);
         # delivered power at fixed speed follows the total, so the convergence gate is 1 %% on the
         # TOTAL between two consecutive 400-iteration windows, plus a bound on the pressure wobble
         # (|window change of the pressure force| < 2 %% of the total). Decision 2026-09-06.
         at=0; bt=0
         for(i=n-799;i<=n-400;i++){at+=T[i]}; for(i=n-399;i<=n;i++){bt+=T[i]}; at/=400; bt/=400
         dt=100*(bt-at)/(at==0?1e-30:at); if(dt<0)dt=-dt
         wob=100*(bp-ap)/(bt==0?1e-30:bt); if(wob<0)wob=-wob
         printf "  total window A / B  : %.1f / %.1f kN   drift %.3f %%   (POWER GATE 1 %%)\n", at/1000, bt/1000, dt
         printf "  pressure wobble     : %.2f %% of the total   (bound 2 %%)\n", wob
         okc=(dv<0.2 && dv>-0.2 && dp<0.2 && dp>-0.2); ok=(dt<1.0 && wob<2.0); pl=(bv/q/cf>0.6 && bv/q/cf<1.3)
         printf "  COMPONENT GATE      : %s   (0.2 %% each, informational)\n", okc?"PASS":"FAIL"
         printf "  POWER GATE          : %s   (total within 1 %%, pressure wobble < 2 %% of total)\n", ok?"PASS":"FAIL"
         printf "  PLAUSIBILITY (Cf)   : %s   (0.6 < Cf/ITTC < 1.3)\n", pl?"PASS":"FAIL"
         exit !(ok && pl) }' || fail=1
  fi
fi
echo "  VERDICT             : $([ $fail -eq 0 ] && echo USABLE || echo 'NOT USABLE - do not quote')"
exit $fail
