#!/usr/bin/env bash
# check-no-abs-paths.sh — workspace-hub #2574
# Enforce the "no hardcoded developer-machine absolute paths" rule across
# digitalmodel source, tests, and YAML fixtures. Vendored from
# workspace-hub/scripts/enforcement/check-no-abs-paths.sh and extended to
# scan .yml/.yaml files (where prior leakage was found).
#
# Usage:
#   scripts/enforcement/check-no-abs-paths.sh
#   scripts/enforcement/check-no-abs-paths.sh path/to/file ...
#
# Exit 0 if no violations; exit 1 with a list of offending lines otherwise.
#
# Allowlist (line-level): any line ending with "# abs-path-allowed" is
# ignored. One-shot bypass: ALLOW_ABS_PATHS=1.
#
# Detection: regex scan for /home/, /mnt/, /Users/, /opt/, and DOS-style
# drive letters across .sh, .py, .yml, .yaml files tracked by git.

set -euo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(git -C "$SELF_DIR" rev-parse --show-toplevel 2>/dev/null || echo "$SELF_DIR")"

declare -a TARGETS=()
if (( $# == 0 )); then
  while IFS= read -r f; do
    case "$f" in
      */tests/fixtures/*) continue ;;
    esac
    TARGETS+=("$REPO_ROOT/$f")
  done < <(cd "$REPO_ROOT" && git ls-files \
      'src/*.sh' 'src/*.py' 'src/*.yml' 'src/*.yaml' \
      'tests/*.sh' 'tests/*.py' 'tests/*.yml' 'tests/*.yaml')
else
  TARGETS=("$@")
fi

# Narrow scope: developer-machine leakage patterns that break CI.
# Excludes intentional cross-platform fallback paths (e.g. /opt/orcina,
# C:\Program Files\Orcina) which are legitimate engineering-tool discovery.
# Broader absolute-path policy lives in workspace-hub's enforcement script.
PATTERN='(/mnt/local-analysis/|/home/[a-zA-Z][a-zA-Z0-9_-]+/(workspace-hub|github|projects)|/Users/[a-zA-Z][a-zA-Z0-9_-]+/(workspace-hub|github|projects))'

declare -a VIOLATIONS=()
for file in "${TARGETS[@]}"; do
  [[ -f "$file" ]] || continue
  rel="${file#$REPO_ROOT/}"
  line_no=0
  while IFS= read -r line; do
    line_no=$((line_no + 1))
    [[ "$line" == *'# abs-path-allowed' ]] && continue
    if [[ "$line" =~ $PATTERN ]]; then
      printf '%s:%d: %s\n' "$rel" "$line_no" "$line"
      VIOLATIONS+=("$rel:$line_no")
    fi
  done < "$file"
done

if (( ${#VIOLATIONS[@]} > 0 )); then
  if [[ "${ALLOW_ABS_PATHS:-0}" == "1" ]]; then
    echo "check-no-abs-paths: ${#VIOLATIONS[@]} violation(s) found; ALLOW_ABS_PATHS=1 bypass in effect" >&2
    exit 0
  fi
  echo "" >&2
  echo "check-no-abs-paths: ${#VIOLATIONS[@]} violation(s) found." >&2
  echo "  Use Path(__file__).resolve().parents[N] or \$(git rev-parse --show-toplevel) instead." >&2
  echo "  Trailing '# abs-path-allowed' on a single line exempts that line." >&2
  echo "  One-shot bypass (logged): ALLOW_ABS_PATHS=1 ..." >&2
  exit 1
fi
exit 0
