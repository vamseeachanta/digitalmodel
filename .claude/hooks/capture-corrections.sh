#!/usr/bin/env bash
# capture-corrections.sh - Lightweight hook to capture AI correction patterns
# Runs as PostToolUse hook - async, non-blocking
#
# Impact: ~20-50ms per edit, runs in background
# Storage: $WORKSPACE_STATE_DIR/corrections/ (defaults to workspace-hub)
#
# Config: Set WORKSPACE_STATE_DIR to override state location

set -uo pipefail

# Fast exit if not enabled
[[ "${CLAUDE_CAPTURE_CORRECTIONS:-true}" != "true" ]] && exit 0

# Central state: always save to workspace-hub for unified RAGS analysis
# Override with WORKSPACE_STATE_DIR if needed
WORKSPACE_HUB="${WORKSPACE_HUB:-/mnt/github/workspace-hub}"
STATE_DIR="${WORKSPACE_STATE_DIR:-${WORKSPACE_HUB}/.claude/state}/corrections"
SESSION_FILE="${STATE_DIR}/session_$(date +%Y%m%d).jsonl"
RECENT_EDITS="${STATE_DIR}/.recent_edits"
MAX_RECENT=50

# Ensure directories exist (fast, cached by OS)
mkdir -p "$STATE_DIR" 2>/dev/null

# Read input from stdin (hook receives JSON)
INPUT=$(cat)

# Extract file path (fast jq query)
FILE_PATH=$(echo "$INPUT" | jq -r '.tool_input.file_path // empty' 2>/dev/null)
[[ -z "$FILE_PATH" ]] && exit 0

TOOL_NAME=$(echo "$INPUT" | jq -r '.tool_name // "Edit"' 2>/dev/null)
TIMESTAMP=$(date -Iseconds)
FILE_BASENAME=$(basename "$FILE_PATH")

# Check if this is a correction (re-edit of recent file)
IS_CORRECTION=false
CORRECTION_GAP=0

if [[ -f "$RECENT_EDITS" ]]; then
    # Look for same file edited recently
    LAST_EDIT=$(grep -F "$FILE_PATH" "$RECENT_EDITS" 2>/dev/null | tail -1)
    if [[ -n "$LAST_EDIT" ]]; then
        LAST_TS=$(echo "$LAST_EDIT" | cut -d'|' -f1)
        LAST_EPOCH=$(date -d "$LAST_TS" +%s 2>/dev/null || echo 0)
        NOW_EPOCH=$(date +%s)
        CORRECTION_GAP=$((NOW_EPOCH - LAST_EPOCH))

        # If same file edited within 10 minutes, likely a correction
        if [[ $CORRECTION_GAP -lt 600 && $CORRECTION_GAP -gt 0 ]]; then
            IS_CORRECTION=true
        fi
    fi
fi

# Log the edit
echo "${TIMESTAMP}|${FILE_PATH}" >> "$RECENT_EDITS"

# Keep recent edits file small
if [[ $(wc -l < "$RECENT_EDITS" 2>/dev/null || echo 0) -gt $MAX_RECENT ]]; then
    tail -n $MAX_RECENT "$RECENT_EDITS" > "${RECENT_EDITS}.tmp" && mv "${RECENT_EDITS}.tmp" "$RECENT_EDITS"
fi

# If this is a correction, capture it for RAGS
if [[ "$IS_CORRECTION" == "true" ]]; then
    # Get git diff context if available
    DIFF_STAT=""
    if command -v git &>/dev/null && git rev-parse --git-dir &>/dev/null 2>&1; then
        DIFF_STAT=$(git diff --stat "$FILE_PATH" 2>/dev/null | tail -1 || echo "")
    fi

    # Append to session corrections log (JSONL format for easy processing)
    jq -cn \
        --arg ts "$TIMESTAMP" \
        --arg file "$FILE_PATH" \
        --arg basename "$FILE_BASENAME" \
        --arg tool "$TOOL_NAME" \
        --argjson gap "$CORRECTION_GAP" \
        --arg diff "$DIFF_STAT" \
        '{
            timestamp: $ts,
            file: $file,
            basename: $basename,
            tool: $tool,
            correction_gap_seconds: $gap,
            diff_stat: $diff,
            type: "correction"
        }' >> "$SESSION_FILE" 2>/dev/null
fi

exit 0
