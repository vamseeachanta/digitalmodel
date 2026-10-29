#!/usr/bin/env bash
# Dispatch the daily "Main full run" (.github/workflows/full-matrix-sweep.yml) on main.
#
# Maintainer ruling, 28 Oct 2026: the full suite runs once a day on main, triggered by
# our own cron through `gh workflow run`, not by a GitHub `schedule:` trigger.
# Pull requests run only the fast gate (lint + touched-module tests).
#
# Crontab line (one host only; needs an authenticated `gh`):
#   17 7 * * * bash <checkout>/scripts/ci/dispatch_main_full_run.sh >> <log> 2>&1
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
