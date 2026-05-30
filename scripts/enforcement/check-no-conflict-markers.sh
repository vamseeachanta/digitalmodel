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
#   `git show ":$file"` — eliminates the working-tree-vs-staged-blob TOCTOU
#   bypass surfaced by Codex r2 #2 + Claude r1 #4.

set -uo pipefail
# Note: not using `set -e` because we deliberately tolerate non-zero exit
# codes from grep (no-match) and similar; explicit branching is used instead.

PATH_RESTRICTED_PREFIXES=(
  "docs/plans/"
  "docs/governance/"
  "docs/standards/"
  "scripts/review/results/"
  "scripts/review/prompts/"
)

is_path_restricted() {
  local file="$1"
  local prefix
  for prefix in "${PATH_RESTRICTED_PREFIXES[@]}"; do
    case "$file" in
      "${prefix}"*) return 0 ;;
    esac
  done
  return 1
}

has_eol_sentinel() {
  # Returns 0 if the line ends with one of the recognized per-line sentinels.
  local line="$1"
  case "$line" in
    *"# CONFLICT_MARKER_FORENSIC_OK") return 0 ;;
    *"// CONFLICT_MARKER_FORENSIC_OK") return 0 ;;
    *"<!-- CONFLICT_MARKER_FORENSIC_OK -->") return 0 ;;
  esac
  return 1
}

is_marker_line() {
  # Returns 0 if the line is a conflict-marker anchor at column 0:
  #   `<<<<<<<` followed by whitespace or end-of-line, or
  #   `>>>>>>>` followed by whitespace or end-of-line.
  # Trailing-space requirement weakened to `(\s|$)` per Claude r1 #8.
  local line="$1"
  if [[ "$line" =~ ^"<<<<<<<"([[:space:]]|$) ]]; then
    return 0
  fi
  if [[ "$line" =~ ^">>>>>>>"([[:space:]]|$) ]]; then
    return 0
  fi
  return 1
}

violations=()

# Stage-aware iteration. NUL-delimited form handles filenames with spaces.
while IFS= read -r -d '' file; do
  # Skip deletions (no staged blob to read).
  if ! git cat-file -e ":$file" 2>/dev/null; then
    continue
  fi

  # Read the staged blob ONCE. This is the single source of truth used by
  # both the per-file sentinel check AND the per-line marker scan.
  if ! staged_content="$(git show ":$file" 2>/dev/null)"; then
    continue
  fi

  # Path-restricted per-file sentinel.
  if is_path_restricted "$file"; then
    if printf '%s\n' "$staged_content" | grep -qF "<!-- CONFLICT_MARKER_FORENSIC_FILE_OK -->"; then
      continue
    fi
  fi

  # Co-occurrence pre-check: skip files that don't contain BOTH anchors.
  # Avoids markdown 7-level blockquote false-positive (`>>>>>>> ` alone).
  if ! printf '%s\n' "$staged_content" | grep -qE '^<<<<<<<([[:space:]]|$)'; then
    continue
  fi
  if ! printf '%s\n' "$staged_content" | grep -qE '^>>>>>>>([[:space:]]|$)'; then
    continue
  fi

  # Per-line scan with per-line sentinel exemption.
  lineno=0
  while IFS= read -r line; do
    lineno=$((lineno + 1))
    if is_marker_line "$line"; then
      if has_eol_sentinel "$line"; then
        continue
      fi
      violations+=("${file}:${lineno}:${line}")
    fi
  done <<< "$staged_content"

done < <(git diff --cached --name-only --diff-filter=ACMR -z 2>/dev/null)

if [[ ${#violations[@]} -eq 0 ]]; then
  exit 0
fi

cat >&2 <<'EOF'
ERROR: Unresolved merge-conflict markers detected in staged content:
EOF
printf '  %s\n' "${violations[@]}" >&2
cat >&2 <<'EOF'

To resolve: edit each file, remove the <<<<<<<, =======, >>>>>>> markers,
keep the intended content, re-stage, and retry the commit.

For legitimate forensic documentation, either:
  - append `# CONFLICT_MARKER_FORENSIC_OK` (or `//` / `<!-- -->` variant) at
    end of the marker-bearing line (per-line exemption); OR
  - if the file is under docs/plans/, docs/governance/, docs/standards/,
    scripts/review/results/, or scripts/review/prompts/, add
    `<!-- CONFLICT_MARKER_FORENSIC_FILE_OK -->` anywhere in the file
    (path-restricted whole-file exemption).
EOF
exit 1
