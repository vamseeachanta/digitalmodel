#!/bin/bash
# ABOUTME: Wrapper script for workflow CLI using uv environment

# Ensure we're in the repository root
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# Run workflow CLI with uv
uv run python src/digitalmodel/cli/workflow_cli.py "$@"
