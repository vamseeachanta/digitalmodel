#!/usr/bin/env bash
# Render the cached multi-lane CFD state as exactly one compact status line.
# Usage: cfd_statusline.sh [--width COLUMNS]
# Environment: DM_CFD_STATUS_CACHE, DM_CFD_STATUS_STALE_SECONDS (default 2400).
set -uo pipefail

WIDTH=160
case "${1:-}" in
  -h|--help) sed -n '2,/^[^#]/s/^# *//p' "$0"; exit 0 ;;
  --width) WIDTH="${2:?--width requires columns}" ;;
  "") ;;
  *) echo "cfd_statusline: unknown argument: $1" >&2; exit 64 ;;
esac
case "$WIDTH" in ''|*[!0-9]*) echo "cfd_statusline: width must be a positive integer" >&2; exit 64;; esac
[ "$WIDTH" -gt 0 ] || exit 0

# Claude Code supplies JSON on stdin. The CFD segment deliberately ignores it.
if [ ! -t 0 ]; then input=$(cat); case "$input" in \{*\}) : ;; *) : ;; esac; fi
CACHE="${DM_CFD_STATUS_CACHE:-$HOME/.cache/digitalmodel/cfd-status.cache}"
if [ ! -s "$CACHE" ]; then
  line="CFD no cache"
else
  IFS='|' read -r tag stamp iso < "$CACHE"
  now="${DM_CFD_STATUS_NOW:-$(date +%s)}"
  case "$stamp" in ''|*[!0-9]*) stamp=$now;; esac
  age=$((now - stamp)); [ "$age" -ge 0 ] || age=0
  parts=()
  while IFS='|' read -r tag lane case_name state progress rate wall write raw smooth co mass umax force residual; do
    [ "$tag" = probe ] || continue
    if [ "$state" = unreachable ]; then part="$lane:$case_name unreachable"
    else part="$lane:$case_name $progress ${rate}s w$write m${mass}%"; fi
    parts+=("$part")
  done < "$CACHE"
  line=""
  for part in "${parts[@]}"; do [ -z "$line" ] && line="$part" || line="$line │ $part"; done
  if [ "$age" -gt "${DM_CFD_STATUS_STALE_SECONDS:-2400}" ]; then
    age_text=$((age / 3600)); [ "$age_text" -gt 0 ] && age_text="${age_text}h" || age_text="$((age / 60))m"
    line="${line:-CFD no rows} │ STALE $age_text"
  else
    [ "$age" -lt 60 ] && age_text="${age}s" || age_text="$((age / 60))m"
    line="${line:-CFD no rows} │ age $age_text"
  fi
fi

if [ "${#line}" -gt "$WIDTH" ]; then
  [ "$WIDTH" -eq 1 ] && line="…" || line="${line:0:WIDTH-1}…"
fi
printf '%s\n' "$line"
