#!/usr/bin/env bash
# Regenerate post-clone pointers; --dry-run previews without writing artifacts.
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
exec python3 "$script_dir/plan_claude_skills.py" "$@"
