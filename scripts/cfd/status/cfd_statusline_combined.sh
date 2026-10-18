#!/usr/bin/env bash
# Prepend an optional workspace status segment to the cached CFD status line.
# Usage: cfd_statusline_combined.sh [--width COLUMNS]
# Set WORKSPACE_STATUSLINE to an executable workspace-hub statusline command.
set -uo pipefail

case "${1:-}" in -h|--help) sed -n '2,/^[^#]/s/^# *//p' "$0"; exit 0;; esac
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
input=$(cat)
# Pass valid Claude Code JSON through to the workspace command; invalid input is ignored.
valid=""
if printf '%s' "$input" | python3 -c 'import json,sys; json.load(sys.stdin)' 2>/dev/null; then valid="$input"; fi
left=""
if [ -n "${WORKSPACE_STATUSLINE:-}" ] && [ -x "$WORKSPACE_STATUSLINE" ]; then
  left=$(printf '%s' "$valid" | timeout 8 "$WORKSPACE_STATUSLINE" 2>/dev/null | head -1) || left=""
fi
cfd=$(printf '%s' "$valid" | "$HERE/cfd_statusline.sh" "$@")
[ -n "$left" ] && printf '%s │ %s\n' "$left" "$cfd" || printf '%s\n' "$cfd"
