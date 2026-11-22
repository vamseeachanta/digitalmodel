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
review_timeout=${REVIEW_TIMEOUT:-600}
if [[ ! $review_timeout =~ ^[1-9][0-9]*$ ]]; then
    printf 'Error: REVIEW_TIMEOUT must be a positive integer in seconds.\n' >&2
    exit 2
fi
mkdir -- "$output"
started_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)
claude_rc=0
if ! command -v claude >/dev/null 2>&1; then
    printf 'claude: command not found\n' > "$output/stderr.log"
    : > "$output/review.md"
    claude_rc=127
else
    timeout --kill-after=1 "$review_timeout" claude -p --max-turns 3 --output-format text < "$prompt" \
        > "$output/review.md" 2> "$output/stderr.log" || claude_rc=$?
fi
rc=$claude_rc
status=completed
if [[ $rc != 0 ]]; then status=cli_failure; fi
if [[ $rc == 124 || $rc == 137 ]]; then rc=5; status=timeout; fi
if grep -Fq -- 'Reached max turns' "$output/review.md" "$output/stderr.log"; then
    if [[ $rc == 0 ]]; then rc=3; status=turn_exhausted; fi
fi
if [[ $rc == 0 ]] && ! grep -q '[^[:space:]]' "$output/review.md"; then
    rc=4; status=empty_review
fi
cli_version=$(timeout --kill-after=1 1 claude --version 2>/dev/null) || cli_version=unavailable
cli_version=${cli_version%%$'\n'*}
{
    printf 'command=claude -p --max-turns 3 --output-format text\n'
    printf 'prompt_file=%q\nstarted_utc=%s\ncli_version=%s\nreview_timeout_seconds=%s\n' \
        "$prompt" "$started_utc" "$cli_version" "$review_timeout"
    printf 'claude_exit_code=%s\nexit_code=%s\nstatus=%s\n' "$claude_rc" "$rc" "$status"
} > "$output/manifest.txt"
exit "$rc"
