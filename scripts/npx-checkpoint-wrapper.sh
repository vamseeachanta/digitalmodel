#!/bin/bash
# ABOUTME: NPX-compatible wrapper for claude-flow checkpoint commands
# Provides: npx claude-flow@alpha checkpoint create|restore|list|clean

set -e

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECKPOINT_CLI="$SCRIPT_DIR/checkpoint_cli.py"

# Ensure we're in a git repo
if ! git rev-parse --show-toplevel > /dev/null 2>&1; then
    echo "Error: Not in a git repository" >&2
    exit 1
fi

# Check if python is available
if ! command -v python &> /dev/null; then
    if ! command -v python3 &> /dev/null; then
        echo "Error: Python not found" >&2
        exit 1
    fi
    PYTHON_CMD="python3"
else
    PYTHON_CMD="python"
fi

# Activate uv environment if available
REPO_ROOT=$(git rev-parse --show-toplevel)
if [ -d "$REPO_ROOT/.venv" ]; then
    source "$REPO_ROOT/.venv/bin/activate" 2>/dev/null || true
fi

# Execute checkpoint CLI
exec $PYTHON_CMD "$CHECKPOINT_CLI" "$@"
