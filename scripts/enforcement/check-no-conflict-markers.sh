#!/usr/bin/env bash
# check-no-conflict-markers.sh — block commits staging files with unresolved
# git merge-conflict markers.
#
# Issue: workspace-hub#2722. T3 adversarial review consensus MAJOR; r3+r4
# inline patches in docs/plans/2026-05-16-issue-2722-pre-commit-conflict-marker-hook.md
# closed all 29 distinct findings.
#
# Design summary:
# - Stage-aware: reads `git diff --cached --name-only --diff-filter=ACMR -z`;
#   never scans the working tree directly.
# - NUL-delimited iteration handles filenames with spaces (Gemini r2 #8).
# - Co-occurrence required: both `^<<<<<<<(\s|$)` AND `^>>>>>>>(\s|$)` must
#   appear in the same staged blob before any per-line violation is emitted.
#   This defeats markdown 7-level blockquote false-positives (Gemini r2 #4).
# - Bare `=======` is NEVER a trigger (false-positive guard for markdown
#   setext H1 underline + shell `# =====` banner art).
# - NO active-merge skip: the original Codex r2 #1 fix removed the
#   `.git/MERGE_MSG` / `REBASE_HEAD` / `CHERRY_PICK_HEAD` skip — those are
#   exactly the commits where half-resolved markers are most likely to land.
# - Forensic-allowlist:
#   * Per-line (default): line ending with `# CONFLICT_MARKER_FORENSIC_OK`
#     or `// CONFLICT_MARKER_FORENSIC_OK` or `<!-- CONFLICT_MARKER_FORENSIC_OK -->`
#     exempts only that line. Matches scripts/enforcement/check-no-abs-paths.sh
#     prior art (Claude r1 #3).
#   * Per-file (path-restricted): file containing
#     `<!-- CONFLICT_MARKER_FORENSIC_FILE_OK -->` AND located in one of
#     {docs/plans/**, docs/governance/**, docs/standards/**,
#      scripts/review/results/**, scripts/review/prompts/**} is exempted
#     entirely (Gemini r2 #1). Bounds Claude r1 #3 blanket-backdoor concern.
# - Both sentinel and marker reads come from the SAME staged blob via
#   `git show ":$file"` — eliminates working-tree-vs-staged-blob TOCTOU.
# - The byte-oriented scanner streams Git stdout directly. Shell variables
#   never hold staged bytes, including NUL or non-UTF-8 content (issue #2310).
#   Python 3 is required; Git read failures block the commit.

set -uo pipefail

exec python3 "$(dirname "${BASH_SOURCE[0]}")/check_no_conflict_markers.py" "$@"
