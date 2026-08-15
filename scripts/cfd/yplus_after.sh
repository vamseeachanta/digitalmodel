#!/bin/bash
# Post-solve wall-resolution diagnostic for #1173.
#
# WHY THIS IS A SEPARATE, LATER-RUNNING SCRIPT rather than a functionObject
# added to the live case:
#   - editing a running case's controlDict risks aborting a solve that is
#     ~15 h from completion, for a diagnostic that loses nothing by waiting;
#   - editing solve_chain.sh while bash is executing it can corrupt execution,
#     because bash reads scripts incrementally;
#   - postProcess wants all 8 cores, which interFoam is currently using.
# So: wait for the chain to stop, then run. Purely additive.
#
# THIS IS A DIAGNOSTIC, NOT A GATE. It is being added AFTER the running
# coefficients were seen. A criterion invented after looking at the answer is
# not a criterion. y+ is reported as evidence; V1/V2a/V2b/V3 stand unchanged.

ROOT="$HOME/cfd/dm1173"
LOG="$ROOT/yplus.log"
say() { echo "$(date -u +%Y-%m-%dT%H:%M:%SZ) $*" >> "$LOG"; }

say "yPlus collector armed; waiting for the solve chain to stop"

# Wait for the chain to finish, however it finishes. Four exits are possible:
# completion, solver failure, budget kill, or the chain process dying. All of
# them stop solve_chain.sh, so waiting on the process covers every case.
while pgrep -f "solve_chain.sh" >/dev/null 2>&1; do
  sleep 300
done
say "solve chain no longer running"
tail -5 "$ROOT/solve_chain.log" >> "$LOG" 2>/dev/null

# shellcheck disable=SC1091
source /usr/lib/openfoam/openfoam2312/etc/bashrc || { say "FATAL cannot source OpenFOAM"; exit 1; }

for case in kcs_production kcs_companion; do
  CASE="$ROOT/kcs_cases/$case"
  if [ ! -d "$CASE" ]; then
    say "SKIP $case -- case directory absent"
    continue
  fi
  # A case that never solved has no written time directories beyond 0, so
  # postProcess would produce a y+ field for the initial condition and report
  # it as though it meant something. Require a real written time.
  ntimes=$(ls -d "$CASE"/processor0/[1-9]* 2>/dev/null | wc -l)
  if [ "$ntimes" -eq 0 ]; then
    say "SKIP $case -- no written time directories; nothing solved to measure"
    continue
  fi
  say "=== $case: $ntimes written times, running yPlus at latest"
  ( cd "$CASE" && mpirun -np 8 postProcess -func yPlus -parallel -latestTime \
      > "$CASE/log.yPlus" 2>&1 )
  rc=$?
  if [ $rc -ne 0 ]; then
    say "FAIL $case yPlus rc=$rc (see log.yPlus)"
    continue
  fi
  # v2312 prints one summary line per patch: "patch <name> y+ : min = .. max = .. average = .."
  hits=$(grep -cE "y\+ *:" "$CASE/log.yPlus" 2>/dev/null || echo 0)
  if [ "$hits" -eq 0 ]; then
    say "WARN $case: yPlus rc=0 but no summary lines parsed -- log format may differ"
  else
    say "OK $case: $hits patch summaries"
    grep -E "y\+ *:" "$CASE/log.yPlus" | sed 's/^/    /' >> "$LOG"
  fi
done

say "YPLUS COLLECTION COMPLETE"
