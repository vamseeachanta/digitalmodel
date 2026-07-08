#!/usr/bin/env bash
# check-no-abs-paths.sh — workspace-hub #2574
# Enforce the "no hardcoded developer-machine / client absolute paths" rule.
# Vendored from workspace-hub and extended (dm #1433, 2026-07-08) to (a) catch
# DOS user-home + client project-share paths that a client-data audit found
# slipping through, and (b) run DIFF-SCOPED so it blocks NEW absolute paths
# going forward without failing on the (grandfathered) existing ones.
#
# Modes:
#   check-no-abs-paths.sh <file> ...        scan the given files (whole file)
#   check-no-abs-paths.sh --added <base>    scan only ADDED lines vs <base>
#                                           (the CI / going-forward gate)
#   check-no-abs-paths.sh --all             scan the whole tree (debt audit)
#   check-no-abs-paths.sh                    == --all
#
# Allowlist: any line ending "# abs-path-allowed" is ignored. One-shot bypass:
# ALLOW_ABS_PATHS=1. Fixtures under */tests/fixtures/* are always skipped.
#
# Detection (excludes legitimate cross-platform tool paths such as
# C:\Program Files\Orcina, /opt/orcina, /usr/lib/openfoam):
#   - Unix dev/client shares:   /mnt/local-analysis|dde|ace/, and
#                               /home|/Users/<user>/(workspace-hub|github|
#                               projects|Desktop|Documents)
#   - DOS developer home:       X:\Users\<name>\
#   - DOS client project share: X:\<digit>...  (e.g. K:\0198\)

set -euo pipefail

SELF_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(git -C "$SELF_DIR" rev-parse --show-toplevel 2>/dev/null || echo "$SELF_DIR")"
cd "$REPO_ROOT"

# Developer-machine / client leakage patterns (extended regex). Backslashes are
# doubled for DOS paths so the ERE sees a literal backslash.
PATTERN='(/mnt/(local-analysis|dde|ace)/|/(home|Users)/[a-zA-Z][a-zA-Z0-9_-]+/(workspace-hub|github|projects|Desktop|Documents)|[A-Za-z]:\\Users\\[a-zA-Z]|[A-Za-z]:\\[0-9])'

# Which tracked files the whole-tree / file modes consider.
GLOBS=('src/**/*.sh' 'src/**/*.py' 'src/**/*.yml' 'src/**/*.yaml'
       'tests/**/*.sh' 'tests/**/*.py' 'tests/**/*.yml' 'tests/**/*.yaml'
       'config/**/*.yml' 'config/**/*.yaml' 'docs/**/*.md')

_is_fixture() { [[ "$1" == */tests/fixtures/* ]]; }

_report_and_exit() {
  local n="$1"
  if (( n == 0 )); then exit 0; fi
  if [[ "${ALLOW_ABS_PATHS:-0}" == "1" ]]; then
    echo "check-no-abs-paths: $n violation(s); ALLOW_ABS_PATHS=1 bypass in effect" >&2
    exit 0
  fi
  {
    echo ""
    echo "check-no-abs-paths: $n developer/client absolute-path violation(s)."
    echo "  Use Path(__file__).resolve().parents[N], \$(git rev-parse --show-toplevel),"
    echo "  or a config/env var instead of a machine/client path."
    echo "  Trailing '# abs-path-allowed' exempts a line; ALLOW_ABS_PATHS=1 bypasses (logged)."
  } >&2
  exit 1
}

# ---- mode: --added <base> (scan only newly-added lines vs base) ------------- #
scan_added() {
  local base="${1:-}"
  [[ -z "$base" ]] && { echo "check-no-abs-paths: --added requires a <base> ref" >&2; exit 2; }
  if ! git rev-parse --verify "$base^{commit}" >/dev/null 2>&1; then
    base="$(git rev-parse --verify origin/main 2>/dev/null || true)"
    [[ -z "$base" ]] && { echo "check-no-abs-paths: base ref unresolved; skipping diff scan" >&2; exit 0; }
  fi
  # Walk the diff, tracking the new-file path and new-line number; emit
  # 'path<TAB>line<TAB>content' for ADDED lines only, then filter by
  # pattern/allowlist.
  local n=0
  while IFS= read -r rec; do
    local path="${rec%%$'\t'*}"; local rest="${rec#*$'\t'}"
    local line="${rest%%$'\t'*}"; local content="${rest#*$'\t'}"
    _is_fixture "$path" && continue
    [[ "$content" == *'# abs-path-allowed' ]] && continue
    if grep -qE "$PATTERN" <<<"$content"; then
      printf '%s:%s:%s\n' "$path" "$line" "$content" >&2
      n=$((n + 1))
    fi
  done < <(
    git diff --unified=0 --no-color --diff-filter=d "$base"...HEAD -- "${GLOBS[@]}" 2>/dev/null \
      | awk '
          /^\+\+\+ /   { p=$2; sub(/^b\//,"",p); next }
          /^@@ /       { m=$0; sub(/^.*\+/,"",m); sub(/[, ].*$/,"",m); ln=m+0; next }
          /^\+/        { print p "\t" ln "\t" substr($0,2); ln++; next }
        '
  )
  _report_and_exit "$n"
}

# ---- mode: files / --all (scan whole files) -------------------------------- #
scan_files() {
  local -a targets=()
  if (( $# > 0 )); then
    for f in "$@"; do _is_fixture "$f" || targets+=("$f"); done
  else
    while IFS= read -r f; do _is_fixture "$f" || targets+=("$f"); done \
      < <(git ls-files "${GLOBS[@]}")
  fi
  (( ${#targets[@]} == 0 )) && exit 0
  local n=0
  while IFS= read -r match; do
    [[ -z "$match" ]] && continue
    [[ "$match" == *'# abs-path-allowed' ]] && continue
    printf '%s\n' "$match" >&2
    n=$((n + 1))
  done < <(grep -nHE "$PATTERN" "${targets[@]}" 2>/dev/null || true)
  _report_and_exit "$n"
}

case "${1:-}" in
  --added) shift; scan_added "${1:-}" ;;
  --all)   shift; scan_files ;;
  *)       scan_files "$@" ;;
esac
