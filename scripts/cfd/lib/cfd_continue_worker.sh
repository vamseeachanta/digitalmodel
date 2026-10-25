#!/usr/bin/env bash
# Detached continuation worker; arguments are data, never interpolated shell code.
SOLVER="${1:?solver required}"
RANKS="${2:?rank count required}"
END="${3:?endTime required}"
LEDGER="${4:?ledger required}"
NAME="${5:?case name required}"
echo $$ > run.pid
# Intentional word splitting preserves the launcher's extra MPI flags contract.
# shellcheck disable=SC2086
mpirun -np "$RANKS" ${DM_CFD_MPIRUN_FLAGS:-} "$SOLVER" -parallel \
  > "log.$SOLVER" 2>&1 < /dev/null
rc=$?
last=$(grep -a '^Time = ' "log.$SOLVER" | tail -1 | awk '{print $3}')
# Completion contract: rc=0 AND finite last >= requested endTime, no tolerance.
# Time-control basis: OpenFOAM v13 User Guide (2025), section 4.4.2.
# https://doc.cfd.direct/openfoam/user-guide-v13/controldict
# This is a wrapper completion criterion, not evidence of engineering convergence.
# ENVIRON preserves literal backslashes; awk -v assignments decode escapes.
if [ "$rc" -eq 0 ] && DM_CFD_LAST="$last" DM_CFD_END="$END" awk '
  function numeric(value) {
    return value ~ scalar && sprintf("%g", value+0) ~ scalar
  }
  BEGIN {
    last = ENVIRON["DM_CFD_LAST"]
    end = ENVIRON["DM_CFD_END"]
    scalar = "^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?$"
    exit !(numeric(last) && numeric(end) && last+0 >= end+0)
  }'; then
  echo "$(date -u +%FT%TZ) continued $NAME to Time=$last rc=$rc" \
    | tee -a "$LEDGER" > CONT_DONE
else
  echo "$(date -u +%FT%TZ) $SOLVER rc=$rc at Time=${last:-unknown} target=$END; completion not established" \
    | tee -a "$LEDGER" > CONT_FAILED
fi
rm -f run.pid
