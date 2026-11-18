#!/usr/bin/env bash
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/lib/cfd_help.sh"
# Deploy the committed solve-host chain and verify destination parity.
# Copies are sourced from HEAD, including when the working tree is dirty.
# DEPLOYED.txt records a point-in-time check, not continuing parity.
#
#   usage: deploy_lane.sh [--dry-run] <user@host> [campaign]
#   campaign defaults to DM_CFD_CAMPAIGN, then "campaign"
#
# Repo -> lane is the ONLY path for chain scripts. A lane-local edit is a bug
# until it is committed here and redeployed; the two lanes ran different
# drivers for two weeks before this existed (docs/domains/openfoam/
# mesh_store_case_layout.md, "Lanes and deployment").
#
# What it does NOT touch: registries (db_chain.yml carries lane-specific rank
# counts), cases/, meshes/, the run ledger at the campaign root, and the lane
# entry points db_job.sh / db_job_matrix.sh at the root (deploy those by hand
# and on purpose -- they are what a queue is running).
#
# Refuses if a CHAIN DRIVER (stage45_driver.sh, db_job.sh, db_job_matrix.sh,
# solve_case.sh, run_queue.sh) is running on the lane: swapping a script under
# a bash that is executing it makes bash resume at a stale byte offset in the
# new file (the 2026-09-04 stage4_build.sh incident wiped a finished mesh).
# A bare solver launched by a case-local chain (mesh_chain.sh / solve_chain.sh
# inside the case) does not read these files, so it only earns a warning.
#
# The driver check reads /proc/<pid>/cmdline of every bash on the host and
# excludes the shell running this very check: `pgrep -f <pattern>` would match
# the ssh command carrying the pattern and report an idle host busy.
set -euo pipefail

DRY=""
[ "${1:-}" = "--dry-run" ] && { DRY="--dry-run"; shift; }
HOST="${1:?usage: deploy_lane.sh [--dry-run] <user@host> [campaign]}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/cfd_chain.sh"
CAMPAIGN="${2:-$(cfd_campaign)}"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SSH="ssh -o BatchMode=yes -o ConnectTimeout=15"
DEST="cfd/$CAMPAIGN/scripts/"

# shellcheck disable=SC2016
state=$($SSH "$HOST" '
  # Scan EVERY process by command line, not only those named bash: a script
  # started through its shebang carries the script name as its process name
  # ("stage45_driver." for stage45_driver.sh), so `pgrep -x bash` misses it.
  drivers=0
  for d in /proc/[0-9]*; do
    p=${d#/proc/}
    [ "$p" = "$$" ] || [ "$p" = "$PPID" ] && continue
    if tr "\0" " " < $d/cmdline 2>/dev/null | grep -qE "stage45_driver\.sh|db_job(_matrix)?\.sh|solve_case\.sh|run_queue\.sh|wigley_job\.sh"; then
      drivers=$((drivers + 1))
    fi
  done
  solvers=0
  for p in interFoam simpleFoam snappyHexMesh mpirun redistributePar renumberMesh decomposePar; do
    solvers=$((solvers + $(pgrep -x -c "$p")))
  done
  echo "$drivers $solvers"' </dev/null)
drivers=${state%% *}; solvers=${state##* }
if [ "${drivers:-1}" != 0 ]; then
  echo "deploy_lane: $HOST has $drivers chain driver process(es) running; scripts must not be swapped under it" >&2
  exit 3
fi
[ "${solvers:-0}" = 0 ] || echo "deploy_lane: note: $HOST has $solvers solver/mesher process(es) (case-local chain); these do not read scripts/, proceeding"

# The face-resolution gate imports digitalmodel; ship the package source beside
# the chain so the gate can run on a lane that has no checkout (fail-closed
# otherwise: "cannot import digitalmodel" reads as a FAILED mesh).
# Freeze both payloads to the same repository object, not mutable worktree files.
# Source: git-archive(1), DESCRIPTION and <tree-ish>, online edition:
# https://git-scm.com/docs/git-archive
repo=$(git -C "$HERE" rev-parse --show-toplevel)
rev=$(git -C "$repo" rev-parse HEAD)
snapshot=$(mktemp -d "${TMPDIR:?TMPDIR must name the deployment scratch directory}/cfd-deploy.XXXXXXXX")
trap 'rm -rf -- "$snapshot"' EXIT
git -C "$repo" archive "$rev" scripts/cfd src/digitalmodel | tar -xf - -C "$snapshot"
HERE="$snapshot/scripts/cfd"
SRC="$snapshot/src"
package_filters=(--exclude '__pycache__' --exclude '*.pyc')
script_filters=(--exclude '__pycache__' --exclude '*.bak' --exclude '*.stale-bak' --exclude '*.upstream.sh' --exclude 'README.md')
echo "deploy_lane: $SRC/digitalmodel -> $HOST:cfd/dm_src/digitalmodel/ ${DRY:+(dry run)}"
[ -n "$DRY" ] || $SSH "$HOST" "mkdir -p cfd/dm_src cfd/$CAMPAIGN/scripts" </dev/null
rsync -a --checksum $DRY --delete "${package_filters[@]}" "$SRC/digitalmodel/" "$HOST:cfd/dm_src/digitalmodel/"
echo "deploy_lane: $HERE -> $HOST:$DEST ${DRY:+(dry run)}"
rsync -a --checksum $DRY --itemize-changes \
  "${script_filters[@]}" \
  "$HERE/" "$HOST:$DEST"
[ -n "$DRY" ] && exit 0
$SSH "$HOST" "cd cfd/$CAMPAIGN &&
  for script in scripts/stage45_driver.sh scripts/mesh_store.sh scripts/prune_case.sh scripts/solve_case.sh; do
    bash -n \"\$script\" || exit
  done && echo 'deploy_lane: remote scripts parse OK'" </dev/null
# rsync(1), --checksum / --dry-run / --itemize-changes: compare file
# contents rather than trusting size and mtime; any pending change denies parity.
# Source: https://download.samba.org/pub/rsync/rsync.1 (online edition, option sections).
verify_parity() {
  local changes
  if ! changes=$(rsync -a --checksum --dry-run --itemize-changes "$@"); then
    echo "deploy_lane: parity check could not complete; no stamp written" >&2
    return 1
  fi
  if [ -n "$changes" ]; then
    echo "deploy_lane: parity mismatch; no stamp written" >&2
    printf '%s\n' "$changes" >&2
    return 1
  fi
}
verify_parity --delete "${package_filters[@]}" "$SRC/digitalmodel/" "$HOST:cfd/dm_src/digitalmodel/"
verify_parity "${script_filters[@]}" "$HERE/" "$HOST:$DEST"
$SSH "$HOST" "echo '$(date -u +%FT%TZ) digitalmodel@$rev scripts/cfd src/digitalmodel parity=verified' >> cfd/$CAMPAIGN/scripts/DEPLOYED.txt" </dev/null
echo "deploy_lane: recorded digitalmodel@$rev in $HOST:cfd/$CAMPAIGN/scripts/DEPLOYED.txt"
