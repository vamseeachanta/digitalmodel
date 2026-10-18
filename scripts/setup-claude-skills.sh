#!/usr/bin/env bash
# Plan post-clone pointers; --apply opts into creating the four artifacts.
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
exec python3 "$script_dir/plan_claude_skills.py" "$@"
