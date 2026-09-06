#!/usr/bin/env bash
# Continue a finished or stopped parallel solve from its latest write.
#
#   continue_case.sh <case> <new endTime> [ranks]
#   env DM_CFD_MPIRUN_FLAGS   extra mpirun flags, e.g. "--map-by numa --bind-to core"
#                             (binding to physical cores; Open MPI 4.1 binds by socket
#                             for np > 2 and lets ranks wander otherwise)
#
# What the stage driver and solve_case.sh cannot do: they are FRESH starts
# (restore0Dir, setFields, decomposePar -force) and discard every processor
# time directory. This script is the codified form of the RESTART.md recipe
# the r3 case carries: set startFrom latestTime, raise endTime, launch the
# solver detached on the existing decomposition, and write a terminal marker
# on every exit path. The previous solver log is kept beside the new one.
#
# Refuses if the case has no processor time directory to continue from, if a
# solver is already running in it, or if endTime is not above the latest
# write. The rank count defaults to the number of processor* directories,
# which is the only count the decomposition can run on.
set -euo pipefail
CASE_ARG="${1:?usage: continue_case.sh <case> <new endTime> [ranks]}"
END="${2:?usage: continue_case.sh <case> <new endTime> [ranks]}"
ROOT="${DM_CFD_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"   # deployed at <campaign>/scripts
CASE="$CASE_ARG"; [ -d "$CASE" ] || CASE="$ROOT/${DM_CFD_CASES_DIR:-cases}/$CASE_ARG"
[ -d "$CASE" ] || { echo "continue_case: no case dir $CASE_ARG" >&2; exit 1; }
cd "$CASE"
NAME=$(basename "$CASE")
RANKS="${3:-$(ls -d processor* 2>/dev/null | wc -l)}"
[ "$RANKS" -gt 0 ] || { echo "continue_case: $NAME has no processor* directories" >&2; exit 1; }
LATEST=$(ls -d processor0/[0-9]* 2>/dev/null | xargs -n1 basename | sort -g | tail -1)
[ -n "$LATEST" ] || { echo "continue_case: $NAME has no processor time directory to continue from" >&2; exit 1; }
awk -v e="$END" -v l="$LATEST" 'BEGIN{exit !(e+0 > l+0)}' || { echo "continue_case: endTime $END is not above the latest write $LATEST" >&2; exit 1; }
for p in $(pgrep -x interFoam -x simpleFoam 2>/dev/null); do
  case "$(readlink /proc/$p/cwd 2>/dev/null)/" in "$CASE"/*) echo "continue_case: a solver is already running in $NAME (pid $p)" >&2; exit 1;; esac
done
if [ -f run.pid ] && kill -0 "$(cat run.pid)" 2>/dev/null; then echo "continue_case: run.pid $(cat run.pid) is alive" >&2; exit 1; fi

# The OpenFOAM bashrc dereferences unset variables: source it with -u and -e
# off, as lib/cfd_chain.sh does, or the script dies here silently (#2023).
set +eu
# shellcheck disable=SC1091
source "${WM_BASHRC:-/usr/lib/openfoam/openfoam2312/etc/bashrc}" >/dev/null 2>&1
set -eu
command -v foamDictionary >/dev/null || { echo "continue_case: OpenFOAM env did not load" >&2; exit 1; }
SOLVER=$(awk '/^application/ {gsub(/;/,"",$2); print $2; exit}' system/controlDict)
[ -n "$SOLVER" ] || { echo "continue_case: no application in system/controlDict" >&2; exit 1; }
foamDictionary -entry startFrom -set latestTime system/controlDict >/dev/null
foamDictionary -entry endTime -set "$END" system/controlDict >/dev/null
# A pause is done with `stopAt writeNow` or `nextWrite`; left in place it stops the
# resumed solver after one iteration and writes a false CONT_DONE (ace-linux-1,
# 2026-09-06). Always resume with stopAt endTime.
foamDictionary -entry stopAt -set endTime system/controlDict >/dev/null
rm -f PAUSED
STAMP=$(date -u +%FT%TZ)
[ -f "log.$SOLVER" ] && mv "log.$SOLVER" "log.$SOLVER.to${LATEST}.$STAMP"
rm -f CONT_DONE CONT_FAILED
LEDGER="$ROOT/$NAME.log"
echo "=== $STAMP continue $NAME from $LATEST to $END on $RANKS ranks ($SOLVER)" >> "$LEDGER"
setsid nohup bash -c "
  echo \$\$ > run.pid
  mpirun -np $RANKS ${DM_CFD_MPIRUN_FLAGS:-} $SOLVER -parallel > log.$SOLVER 2>&1 < /dev/null; rc=\$?
  last=\$(grep -a '^Time = ' log.$SOLVER | tail -1 | awk '{print \$3}')
  if [ \$rc -eq 0 ]; then echo \"\$(date -u +%FT%TZ) continued $NAME to Time=\$last rc=0\" | tee -a '$LEDGER' > CONT_DONE
  else echo \"\$(date -u +%FT%TZ) $SOLVER rc=\$rc at Time=\$last\" | tee -a '$LEDGER' > CONT_FAILED; fi
  rm -f run.pid
" > continue.out 2>&1 < /dev/null & disown
sleep 2
echo "continue_case: $NAME from $LATEST to $END on $RANKS ranks, pid $(cat run.pid 2>/dev/null), markers CONT_DONE | CONT_FAILED, ledger $LEDGER"
