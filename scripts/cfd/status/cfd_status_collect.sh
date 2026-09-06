#!/usr/bin/env bash
# Collect one lane_probe row for every configured CFD case into an atomic cache.
# Usage: cfd_status_collect.sh [--dry-run]
# Environment: DM_CFD_STATUS_CONFIG (required), DM_CFD_STATUS_CACHE (optional).
set -uo pipefail

DRY_RUN=false
case "${1:-}" in
  -h|--help) sed -n '2,/^[^#]/s/^# *//p' "$0"; exit 0 ;;
  --dry-run) DRY_RUN=true ;;
  "") ;;
  *) echo "cfd_status_collect: unknown argument: $1" >&2; exit 64 ;;
esac

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROBE="$HERE/../lane_probe.sh"
CONFIG="${DM_CFD_STATUS_CONFIG:?set DM_CFD_STATUS_CONFIG to a status YAML file}"
[ -f "$CONFIG" ] || { echo "cfd_status_collect: no config at $CONFIG" >&2; exit 66; }

mapfile -t records < <(python3 - "$CONFIG" <<'PY'
import sys
import yaml

with open(sys.argv[1], encoding="utf-8") as stream:
    cfg = yaml.safe_load(stream) or {}
print("cache\t" + str(cfg.get("cache", "~/.cache/digitalmodel/cfd-status.cache")))
campaign = cfg.get("campaign")
if not campaign:
    for lane in cfg.get("lanes", []):
        for case in lane.get("cases", []):
            path = case if isinstance(case, str) else case.get("path", "")
            match = __import__("re").search(r"(?:^|/)cfd/([^/]+)/cases(?:/|$)", str(path))
            if match:
                campaign = match.group(1)
                break
        if campaign:
            break
print("campaign\t" + str(campaign or ""))
for lane in cfg.get("lanes", []):
    name = str(lane["name"])
    ssh_value = lane.get("ssh")
    ssh = "local" if ssh_value is None or str(ssh_value).strip().lower() in {"", "local"} else str(ssh_value)
    for case in lane.get("cases", []):
        if isinstance(case, str):
            case = {"name": case, "path": case}
        print("probe\t{}\t{}\t{}\t{}\t{}".format(
            name, ssh, case.get("name", case["path"]), case["path"],
            case.get("force_divisor", lane.get("force_divisor", 1000))))
    for kind in ("bench_status", "queue_status"):
        path = lane.get(kind)
        if path:
            print("status\t{}\t{}\t{}\t{}".format(name, ssh, kind, path))
PY
)
[ "${#records[@]}" -gt 0 ] || { echo "cfd_status_collect: empty config" >&2; exit 65; }

configured_cache=${records[0]#*$'\t'}
Ccampaign=${records[1]#*$'\t'}
CAMPAIGN="${DM_CFD_CAMPAIGN:-$Ccampaign}"
CACHE="${DM_CFD_STATUS_CACHE:-$configured_cache}"
CACHE="${CACHE/#\~/$HOME}"
if [ "$DRY_RUN" = false ]; then
  mkdir -p "$(dirname "$CACHE")"
  TMP="${CACHE}.tmp.$$"
  trap 'rm -f "${TMP:-}"' EXIT
  printf 'stamp|%s|%s\n' "${DM_CFD_STATUS_NOW:-$(date +%s)}" "$(date -u +%FT%TZ)" > "$TMP"
fi

quote_cmd() { printf '%q ' "$@"; printf '\n'; }
for record in "${records[@]:2}"; do
  IFS=$'\t' read -r kind lane host name path extra <<< "$record"
  if [ "$kind" = probe ]; then
    if [ "$host" = local ] || [ "$host" = localhost ]; then
      if [ "$DRY_RUN" = true ]; then quote_cmd "$PROBE" "$lane" "$path" "$extra"; continue; fi
      row=$("$PROBE" "$lane" "$path" "$extra" 2>/dev/null) || row="$lane|$name|unreachable|-|-|-|-|-|-|-|-|-|-|-"
    else
      [ -n "$CAMPAIGN" ] || { echo "cfd_status_collect: campaign is not configured and cannot be inferred" >&2; exit 65; }
      remote="~/cfd/$CAMPAIGN/scripts/lane_probe.sh"
      if [ "$DRY_RUN" = true ]; then quote_cmd ssh -o BatchMode=yes -o ConnectTimeout=8 "$host" "$remote" "$lane" "$path" "$extra"; continue; fi
      row=$(timeout 25 ssh -o BatchMode=yes -o ConnectTimeout=8 "$host" "$remote" "$lane" "$path" "$extra" 2>/dev/null) \
        || row="$lane|$name|unreachable|-|-|-|-|-|-|-|-|-|-|-"
    fi
    printf 'probe|%s\n' "$row" >> "$TMP"
  else
    if [ "$DRY_RUN" = true ]; then
      if [ "$host" = local ] || [ "$host" = localhost ]; then quote_cmd cat "$path"; else quote_cmd ssh -o BatchMode=yes -o ConnectTimeout=8 "$host" cat "$path"; fi
      continue
    fi
    if [ "$host" = local ] || [ "$host" = localhost ]; then value=$(tr '\n|' ';;' < "$path" 2>/dev/null || true)
    else value=$(timeout 10 ssh -o BatchMode=yes -o ConnectTimeout=8 "$host" cat "$path" 2>/dev/null | tr '\n|' ';;' || true); fi
    printf '%s|%s|%s\n' "$name" "$lane" "${value:--}" >> "$TMP"
  fi
done

if [ "$DRY_RUN" = false ]; then mv "$TMP" "$CACHE"; trap - EXIT; fi
