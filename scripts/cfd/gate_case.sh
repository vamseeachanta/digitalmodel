#!/bin/bash
# Terminal waiter + settling gate for one calm-water resistance case.
#
# Runs ON the solve host, detached and reparented to PID 1, so a control-surface
# restart cannot take it with the session. Replaces wait_lam.sh, which polled
# from the primary lane over SSH and therefore died with it.
#
# Two rules this encodes, both learned the hard way:
#   - Poll by PID, never by process name. `pgrep -f X` matches the ssh command
#     carrying X and has reported a dead run as alive for 13.5 hours.
#   - Write a terminal marker on EVERY exit path, success and failure alike, so
#     the absence of a marker is unambiguous rather than readable as either.
#
# The reduction and gate are lifted verbatim from wait_lam.sh: two-window mean
# over the final 400 iterations against the preceding 400, 0.2 % per component,
# plus a plausibility band on Cf/ITTC. rhoInf and nu are read from the case.
#
# Usage: gate_case.sh <case> <driver-pid> [deadline-hours]
#
# NOT `set -u`. The OpenFOAM bashrc dereferences unset variables, so `set -u`
# aborts this script the moment it is sourced -- silently, before any reduction
# runs. That is digitalmodel 99ed224c, "the ported driver could never run --
# set -u killed every stage" (#2023), rediscovered here.
set -o pipefail

CASE=${1:?usage: gate_case.sh <case> <driver-pid> [deadline-hours]}
DRIVER_PID=${2:?usage: gate_case.sh <case> <driver-pid> [deadline-hours]}
DEADLINE_H=${3:-12}

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/cfd_chain.sh"
ROOT="$(cfd_campaign_root)"
CASEDIR=$ROOT/${DM_CFD_CASES_DIR:-cases}/$CASE
MARKER=$ROOT/$CASE.gate.marker
REPORT=$ROOT/$CASE.gate.log

stamp() { date -u +%FT%TZ; }
mark()  { printf '%s %s gate %s %s\n' "$1" "$(stamp)" "$CASE" "$2" > "$MARKER"; }
die()   { mark FAILED "reason=$1"; echo "GATE ABORTED: $1" >> "$REPORT"; exit 1; }

: > "$REPORT"
echo "=== $(stamp) waiter armed on $(hostname -s), watching driver PID $DRIVER_PID" >> "$REPORT"

END=$(( $(date +%s) + DEADLINE_H * 3600 ))
while kill -0 "$DRIVER_PID" 2>/dev/null; do
  [ "$(date +%s)" -ge "$END" ] && die "deadline-${DEADLINE_H}h-exceeded-driver-still-alive"
  sleep 60
done
echo "=== $(stamp) driver PID $DRIVER_PID gone -- reducing" >> "$REPORT"

source /usr/lib/openfoam/openfoam2312/etc/bashrc >/dev/null 2>&1
cd "$CASEDIR" || die "case-absent"

{
  echo "=== $CASE terminal on $(hostname -s) ==="
  tail -3 "$ROOT/$CASE.log" 2>/dev/null | sed 's/^/  /'
  echo -n "  turbulence          : "
  foamDictionary -entry simulationType -value constant/momentumTransport 2>/dev/null \
    || foamDictionary -entry simulationType -value constant/turbulenceProperties 2>/dev/null
  echo -n "  outlet BC           : "
  awk '/^    outlet/{f=1} f&&/type/{print $2; exit}' 0.orig/U | tr -d ';'
  echo -n "  last Time           : "; grep -h '^Time = ' log.interFoam | tail -1
  echo -n "  solver end line     : "; tail -20 log.interFoam | grep -E '^(End|Finalising)' | tail -1
} >> "$REPORT" 2>&1

# A restarted case writes a NEW postProcessing time directory and leaves the
# pre-restart one in place. Hardcoding .../0/ would gate the stale segment and
# return a confident verdict about a run that is no longer the run -- so take
# the highest-numbered start time, not the first and not the newest by mtime.
f=$(ls -d postProcessing/forces_hull/*/ 2>/dev/null \
      | awk -F/ '{print $(NF-1)}' | sort -g | tail -1)
f="postProcessing/forces_hull/$f/force.dat"
[ -f "$f" ] || die "no-forces_hull-force.dat"
R=$(grep -vc '^#' "$f")
echo "  force segment       : $f" >> "$REPORT"
echo "  force rows          : $R" >> "$REPORT"
[ "$R" -lt 800 ] && die "too-few-rows-$R-in-$f"

U=$(foamDictionary -entry functions/forceCoeffs/magUInf -value system/controlDict)
A=$(foamDictionary -entry functions/forceCoeffs/Aref    -value system/controlDict)
L=$(foamDictionary -entry functions/forceCoeffs/lRef    -value system/controlDict)
D=$(foamDictionary -entry functions/forceCoeffs/rhoInf  -value system/controlDict)
NU=$(foamDictionary -entry water/nu -value constant/transportProperties)
echo "  rhoInf / nu(water)  : $D / $NU   (read from the case, never assumed)" >> "$REPORT"

grep -v '^#' "$f" | awk -v U="$U" -v A="$A" -v D="$D" -v L="$L" -v NU="$NU" '
 {n++; P[n]=($5<0?-$5:$5); V[n]=($8<0?-$8:$8)}
 END{q=0.5*D*U*U*A; Re=U*L/NU; cf=0.075/((log(Re)/log(10))-2)^2
     printf "  Re / ITTC Cf        : %.3e / %.6f\n", Re, cf
     printf "  500-iteration window trend of C/(ITTC Cf):\n"
     for(w=0; w*500+500<=n; w++){sp=0;sv=0
       for(i=w*500+1;i<=w*500+500;i++){sp+=P[i];sv+=V[i]}
       printf "    %5d-%5d  Cf %7.4f  Cp %7.4f  TOT %7.4f\n", w*500+1,w*500+500, sv/500/q/cf, sp/500/q/cf, (sv+sp)/500/q/cf}
     if(n<800){print "  fewer than 800 rows"; exit}
     for(i=n-399;i<=n;i++){bp+=P[i];bv+=V[i]}
     for(i=n-799;i<=n-400;i++){ap+=P[i];av+=V[i]}
     bp/=400;bv/=400;ap/=400;av/=400
     dv=100*(bv-av)/av; dp=100*(bp-ap)/ap
     printf "  window A (%d-%d) : Cf %.4f  Cp %.4f\n", n-799, n-400, av/q/cf, ap/q/cf
     printf "  window B (%d-%d) : Cf %.4f  Cp %.4f\n", n-399, n,     bv/q/cf, bp/q/cf
     printf "  final 400           : Cf %.4f  Cp %.4f  C_T/ITTC %.4f\n", bv/q/cf, bp/q/cf, (bv+bp)/q/cf
     printf "  two-window drift    : visc %+.2f %%  press %+.2f %%   (gate 0.2 %%)\n", dv, dp
     ok = (dv<0.2 && dv>-0.2 && dp<0.2 && dp>-0.2)
     plaus = (bv/q/cf > 0.6 && bv/q/cf < 1.3)
     printf "  SETTLING GATE       : %s\n", ok ? "PASS" : "FAIL"
     printf "  PLAUSIBILITY (Cf)   : %s   double-body gives 0.857 at the same Re\n", plaus ? "PASS" : "FAIL"
     printf "  VERDICT             : %s\n", (ok && plaus) ? "USABLE" : "NOT USABLE - do not quote"
     printf "  COMPARISON          : the reference case (k-omega SST) gave Cf 0.9673, press +35.15 %%\n"
     printf "  READ                : if press drift is still large here, turbulence is NOT the cause\n" }' >> "$REPORT" 2>&1

VERDICT=$(awk -F: '/VERDICT/{gsub(/^ +| +$/,"",$2); print $2}' "$REPORT" | tail -1)
DRIFT=$(awk '/two-window drift/{sub(/.*: /,""); print}' "$REPORT" | tail -1)
[ -z "$VERDICT" ] && die "reduction-produced-no-verdict"

case "$VERDICT" in
  USABLE) mark OK "verdict=USABLE  $DRIFT" ;;
  *)      mark OK "verdict=NOT_USABLE  $DRIFT" ;;
esac
echo "=== $(stamp) gate complete: $VERDICT" >> "$REPORT"
