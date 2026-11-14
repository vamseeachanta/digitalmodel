#!/usr/bin/env bash
# Bounded issue review; see docs/domains/automation/CLAUDE_SUBPROCESS_REVIEW_TEMPLATE.md.
set -euo pipefail

usage() { printf 'Usage: %s PROMPT_FILE OUTPUT_DIR\n' "${0##*/}"; }
if [[ $# == 1 && $1 == --help ]]; then usage; exit 0; fi
if [[ $# != 2 ]]; then usage >&2; exit 2; fi

prompt=$1
output=$2
if [[ ! -f "$prompt" || ! -r "$prompt" || ! -s "$prompt" ]]; then
    printf 'Error: prompt must be a readable, nonempty file.\n' >&2
    exit 2
fi
mkdir -- "$output"
claude_rc=0
claude -p --max-turns 3 --output-format text < "$prompt" \
    > "$output/review.md" 2> "$output/stderr.log" || claude_rc=$?
rc=$claude_rc
if grep -Fq -- 'Reached max turns' "$output/review.md" "$output/stderr.log"; then
    if [[ $rc == 0 ]]; then rc=3; fi
fi
if [[ $rc == 0 ]] && ! grep -q '[^[:space:]]' "$output/review.md"; then rc=4; fi
printf 'command=claude -p --max-turns 3 --output-format text\nclaude_exit_code=%s\nexit_code=%s\n' \
    "$claude_rc" "$rc" > "$output/manifest.txt"
exit "$rc"
