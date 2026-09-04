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

# shellcheck source=lib/cfd_chain.sh
source "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/cfd_chain.sh"

ROOT="$(cfd_root)"
LOG="$ROOT/yplus.log"
CFD_LOG="$LOG"
CFD_MARKER="${CFD_MARKER:-$ROOT/yplus_after.marker}"
export CFD_LOG CFD_MARKER
say() { echo "$(date -u +%Y-%m-%dT%H:%M:%SZ) $*" >> "$LOG"; }

say "yPlus collector armed; waiting for the solve chain to stop"

# Wait for the chain's TERMINAL MARKER, not for its process name.
#
# This previously polled `pgrep -f "solve_chain.sh"`, which is wrong twice
# over. `-f` matches any command line CONTAINING the string -- including the
# ssh invocation that launches this collector -- so the wait can self-match
# and never end; that exact pattern produced a 13.5 h zombie elsewhere on this
# fleet. And process absence conflates completion, solver failure, budget kill
# and process death, so the collector could not tell a finished solve from a
# dead one.
#
# solve_chain.sh now writes a marker on BOTH outcomes under an EXIT trap, so
# the outcome is knowable rather than merely "it stopped".
CHAIN_MARKER="${DM_CFD_CHAIN_MARKER:-$ROOT/solve_chain.marker}"
while [ ! -f "$CHAIN_MARKER" ]; do
  sleep 300
done
CHAIN_VERDICT="$(head -1 "$CHAIN_MARKER")"
say "solve chain terminal marker: $CHAIN_VERDICT"
tail -5 "$ROOT/solve_chain.log" >> "$LOG" 2>/dev/null

cfd_load_openfoam

# Case list from the registry rather than two hard-coded names. Absent cases
# are already skipped below, so a registry with more levels than this host
# built is harmless.
for case in $(cfd_cases); do
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
  # MUST be `interFoam -postProcess`, NOT bare `postProcess`. The bare form
  # does not construct the turbulence model, so yPlus writes a field of zeros
  # and still exits 0 -- OpenFOAM says so itself:
  #   "Unable to find turbulence model in the database: yPlus will not be
  #    calculated. Please try to use the solver option -postProcess"
  # The first version of this script used the bare form and reported OK
  # against min=0 max=0 average=0.
  # Ranks from the registry, not a hard-coded 8. `< /dev/null` because mpirun
  # reads and closes stdin -- without it, an mpirun inside a piped script
  # swallows the remainder of the script and the rest silently never runs.
  ranks="$(cfd_case_get "$case" ranks)"
  ( cd "$CASE" && mpirun -np "$ranks" interFoam -postProcess -func yPlus -parallel \
      -latestTime > "$CASE/log.yPlus" 2>&1 < /dev/null )
  rc=$?
  if [ $rc -ne 0 ]; then
    say "FAIL $case yPlus rc=$rc (see log.yPlus)"
    continue
  fi

  # v2312 prints one summary line per patch:
  #   "patch <name> y+ : min = .. max = .. average = .."
  hits=$(grep -cE "y\+ *:" "$CASE/log.yPlus" 2>/dev/null || echo 0)
  if [ "$hits" -eq 0 ]; then
    say "WARN $case: yPlus rc=0 but no summary lines parsed -- log format may differ"
    continue
  fi

  # A parsed line is not a measured line. Three ways rc=0 still means nothing,
  # each of which the first version of this script reported as OK:
  if grep -q "Unable to find turbulence model" "$CASE/log.yPlus"; then
    say "FAIL $case: turbulence model absent -- y+ was NOT calculated"
    continue
  fi
  if grep -qE "y\+ *: *min = 0,? +max = 0" "$CASE/log.yPlus"; then
    say "FAIL $case: y+ identically zero -- physically impossible on a wetted wall"
    continue
  fi
  if grep -c "FOAM Warning" "$CASE/log.yPlus" > /dev/null 2>&1; then
    nwarn=$(grep -c "FOAM Warning" "$CASE/log.yPlus")
    [ "$nwarn" -gt 0 ] && say "NOTE $case: $nwarn FOAM Warning(s) in log.yPlus"
  fi

  say "OK $case: $hits patch summaries"
  grep -E "y\+ *:" "$CASE/log.yPlus" | sed 's/^/    /' >> "$LOG"
done

say "YPLUS COLLECTION COMPLETE"
cfd_marker_ok "yplus collected after chain verdict: $CHAIN_VERDICT"
