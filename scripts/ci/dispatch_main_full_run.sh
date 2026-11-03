#!/usr/bin/env bash
# Dispatch "Main full run" (.github/workflows/full-matrix-sweep.yml) on demand.
#
# The full suite runs daily on main from a GitHub schedule at 03:17 UTC.
# External dispatch through `gh workflow run` is an optional supplement.
# Pull requests run only the fast gate (lint + touched-module tests).
#
# Keep this script for on-demand use; retire any external daily cron to avoid
# a second full run after the scheduled run completes.
#
# Skips when a Main full run is already queued or in progress, so a slow run is
# never stacked behind another.
set -euo pipefail

if [ -n "${MAIN_FULL_RUN_REPO:-}" ]; then
  repo="$MAIN_FULL_RUN_REPO"
else
  repo="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." &&
    gh repo view --json nameWithOwner --jq .nameWithOwner)"
fi
workflow="full-matrix-sweep.yml"
stamp="$(date -u +%Y-%m-%dT%H:%MZ)"

active="$(gh run list -R "$repo" --workflow "$workflow" --branch main --limit 20 \
  --json status --jq '[.[] | select(.status != "completed")] | length')"
if [ "${active:-0}" -gt 0 ]; then
  echo "$stamp skip: $active Main full run(s) still queued or running"
  exit 0
fi

gh workflow run "$workflow" -R "$repo" --ref main
echo "$stamp dispatched $workflow on main for $repo"
