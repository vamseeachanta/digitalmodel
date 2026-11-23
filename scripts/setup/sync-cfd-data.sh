#!/usr/bin/env bash
# sync-cfd-data.sh — push key CFD data (inputs, reduced outputs, reports) from a
# headless CFD box to a cloud remote via rclone, so results survive the box and
# are reachable elsewhere (digitalmodel #1495).
#
# "Key data" = the small, durable derivatives (input configs, result manifests,
# reports/HTML), NOT the multi-GB raw case trees (VTK / time dirs) — those are
# regenerable and excluded by default (opt in with SYNC_RAW=1).
#
# Uses `rclone copy` (ADDITIVE — never deletes cloud files) by default; set
# MODE=sync to mirror (deletes cloud files absent locally — use with care).
#
# Setup (one-time, HITL — provides a cloud secret):
#   rclone config           # create a remote, e.g. name it "cfdcloud"
#                           # (Google Drive, S3, Dropbox, … all supported)
# Usage:
#   RCLONE_REMOTE=cfdcloud:cfd-box scripts/setup/sync-cfd-data.sh
#   RCLONE_REMOTE=cfdcloud:cfd-box SYNC_RAW=1 scripts/setup/sync-cfd-data.sh
#   RCLONE_REMOTE=cfdcloud:cfd-box scripts/setup/sync-cfd-data.sh --install-timer 30m
#
# Env:
#   RCLONE_REMOTE  cloud target (rclone remote:path)   [required]
#   REPO_DIR       digitalmodel checkout                (default: $HOME/digitalmodel)
#   WORK_DIR       CFD scratch (raw case trees)         (default: $HOME/cfd_work)
#   MODE           copy | sync                          (default: copy)
#   SYNC_RAW       1 to also sync raw case trees        (default: 0)
set -euo pipefail

REPO_DIR="${REPO_DIR:-$HOME/digitalmodel}"
WORK_DIR="${WORK_DIR:-$HOME/cfd_work}"
MODE="${MODE:-copy}"
REMOTE="${RCLONE_REMOTE:-}"

die() { printf '\033[1;31m[sync] ERROR:\033[0m %s\n' "$*" >&2; exit 1; }
log() { printf '\033[1;34m[sync]\033[0m %s\n' "$*"; }

command -v rclone >/dev/null 2>&1 || die "rclone not installed (provision-cfd-box.sh installs it; or 'sudo apt install rclone')."
[[ -n "$REMOTE" ]] || die "set RCLONE_REMOTE=<remote:path> (configure once with 'rclone config')."
rclone lsd "${REMOTE%%:*}:" >/dev/null 2>&1 || die "rclone remote '${REMOTE%%:*}' not configured/reachable — run 'rclone config'."

# --- optional: install a periodic timer for "always sync" -------------------- #
if [[ "${1:-}" == "--install-timer" ]]; then
  every="${2:-30m}"
  cron_min="*/30"
  case "$every" in 15m) cron_min="*/15";; 30m) cron_min="*/30";; 1h) cron_min="0";; esac
  line="$cron_min * * * * RCLONE_REMOTE=$REMOTE REPO_DIR=$REPO_DIR WORK_DIR=$WORK_DIR $(readlink -f "$0") >> \$HOME/.cfd-sync.log 2>&1"
  ( crontab -l 2>/dev/null | grep -v 'sync-cfd-data.sh'; echo "$line" ) | crontab -
  log "installed cron: sync every ${every} (RCLONE_REMOTE=$REMOTE). Log: \$HOME/.cfd-sync.log"
  exit 0
fi

# --- the "key data" sync set (small, durable derivatives) -------------------- #
RC=(rclone "$MODE" --create-empty-src-dirs --transfers 8 --checksum -v)
sync_dir() {  # <local subdir> <remote subdir> [extra rclone args…]
  local src="$1" dst="$2"; shift 2
  [[ -e "$src" ]] || { log "skip (absent): $src"; return; }
  log "$MODE  $src  ->  $REMOTE/$dst"
  "${RC[@]}" "$@" "$src" "$REMOTE/$dst"
}

log "syncing key CFD data to $REMOTE (mode=$MODE, raw=${SYNC_RAW:-0})"
# committed derivatives + inputs from the repo (small, the self-contained set)
sync_dir "$REPO_DIR/docs/api/cfd"        "docs/api/cfd"          # manifests + study/report HTML
sync_dir "$REPO_DIR/docs/api/structural" "docs/api/structural"   # backbone/EGA/tld manifests
sync_dir "$REPO_DIR/config"              "config"                # input configs (yamls)
# reduced result manifests written outside the repo, if any
[[ -d "$WORK_DIR" ]] && sync_dir "$WORK_DIR" "work" --include '*_result.json' --include '*.json'

# raw case trees only on request (large; regenerable)
if [[ "${SYNC_RAW:-0}" == "1" && -d "$WORK_DIR" ]]; then
  log "SYNC_RAW=1 — also copying raw case trees (large)…"
  "${RC[@]}" "$WORK_DIR" "$REMOTE/work-raw"
fi
log "done."
