#!/bin/bash

# ABOUTME: Pre-task hook for automatic repository readiness check
# ABOUTME: Auto-executes before new tasks to prepare work context

set -euo pipefail

# Configuration
REPO_PATH="$(pwd)"
SKILL_PATH="${HOME}/.claude/skills/workspace-hub/repo-readiness"
READINESS_SCRIPT="${SKILL_PATH}/check_readiness.sh"

# Check if readiness skill exists
if [ ! -f "$READINESS_SCRIPT" ]; then
    # Fallback to workspace-hub location
    WORKSPACE_HUB="${WORKSPACE_HUB:-/mnt/github/workspace-hub}"
    READINESS_SCRIPT="${WORKSPACE_HUB}/.claude/skills/workspace-hub/repo-readiness/check_readiness.sh"
fi

if [ ! -f "$READINESS_SCRIPT" ]; then
    echo "Warning: Repository readiness script not found, skipping check"
    exit 0
fi

# Allow bypassing readiness check
if [ "${SKIP_READINESS_CHECK:-0}" = "1" ]; then
    echo "Repository readiness check skipped (SKIP_READINESS_CHECK=1)"
    exit 0
fi

# Run readiness check
echo "========================================"
echo "Pre-Task: Repository Readiness Check"
echo "========================================"
echo ""

"$READINESS_SCRIPT" "$REPO_PATH" --update-cache

exit_code=$?

echo ""
echo "========================================"

if [ $exit_code -eq 0 ]; then
    echo "✅ Repository ready for new work"
    echo "========================================"
    exit 0
elif [ $exit_code -eq 1 ]; then
    echo "⚠️  Repository needs attention"
    echo ""
    echo "Minor issues detected. You can:"
    echo "1. Proceed with work (recommended actions in report)"
    echo "2. Fix issues first (review .claude/readiness-report.md)"
    echo "3. Skip check: SKIP_READINESS_CHECK=1 <command>"
    echo "========================================"

    # Ask user how to proceed
    read -p "Proceed with task? [Y/n] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]] || [[ -z $REPLY ]]; then
        exit 0
    else
        echo "Task cancelled by user"
        exit 1
    fi
else
    echo "❌ Repository not ready for new work"
    echo ""
    echo "Critical issues detected. Please:"
    echo "1. Review: .claude/readiness-report.md"
    echo "2. Fix critical issues"
    echo "3. Re-run readiness check"
    echo ""
    echo "To force task execution:"
    echo "SKIP_READINESS_CHECK=1 <command>"
    echo "========================================"

    # Block task execution unless forced
    exit 2
fi
