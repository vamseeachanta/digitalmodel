#!/usr/bin/env bash
# The three verifications for #1961 that do NOT route through the test suite.
#
# A scanner verified only by its own unit tests is validated against exactly the
# wrong population: it proves it detects what a test author thought to plant, on
# a surface whose defining property is that nothing internal touches it. These
# three do not depend on anyone having imagined the right thing to plant.
#
#   --oracle       D4  scan a pinned pre-fix tree and require an EXACT, TWO-SIDED
#                      match against a leak population nobody here constructed
#   --snapshot     D5  diff the public-surface snapshot base-vs-head, then prove
#                      the diff is not vacuously empty by mutating a default
#   --enumeration  D6a cross-check the scanner's census against an independent
#                      git ls-files run; require exact equality and non-zero
#   --all          all three
#
# Stage 1. Without a rule-value file the oracle reports UNAUTHENTICATED and exits
# 3; it never reports a clean pass it did not earn.
#
# Exit: 0 pass, 1 verification failed, 2 usage, 3 rule authority unavailable.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
MANIFEST="${ROOT}/scripts/legal/protected-surface-v1.json"
SCANNER="${ROOT}/scripts/legal/check_protected_identifiers.py"
SNAPSHOT="${ROOT}/scripts/legal/public_surface_snapshot.py"
PY="${PROTECTED_PYTHON:-python3}"
RULES="${PROTECTED_RULES_FILE:-}"

banner() { printf '\n=== %s ===\n' "$1"; }

verify_enumeration() {
  banner "D6a enumeration cross-check (independent git ls-files)"
  local report
  report="$(mktemp)"
  "${PY}" "${SCANNER}" --manifest "${MANIFEST}" \
      --rules "${RULES:-/nonexistent}" --root "${ROOT}" --json \
      --print-enumeration --enumerate-only > "${report}" || true
  cd "${ROOT}"
  "${PY}" - "${report}" <<'PYEOF'
import json, subprocess, sys
report = json.load(open(sys.argv[1]))
scanner = report.get("enumeration", [])
independent = sorted(
    p for p in subprocess.run(
        ["git", "ls-files", "-z"], capture_output=True, text=True, check=True
    ).stdout.split("\0") if p
)
if not scanner:
    sys.exit("FAIL: the scanner enumerated nothing -- a vacuous census")
if scanner != independent:
    only_scanner = sorted(set(scanner) - set(independent))
    only_git = sorted(set(independent) - set(scanner))
    sys.exit(f"FAIL: enumerations differ\n  only scanner: {only_scanner[:10]}\n"
             f"  only git:     {only_git[:10]}")
named = ["scripts/legal/check_protected_identifiers.py",
         "scripts/legal/protected-surface-v1.json",
         "tests/scripts/test_check_protected_identifiers.py"]
missing = [n for n in named if n not in scanner]
if missing:
    sys.exit(f"FAIL: census does not contain its own files: {missing}")
print(f"PASS: {len(scanner)} paths, exact match, self-coverage confirmed")
PYEOF
}

verify_oracle() {
  banner "D4 retrospective corpus (pinned pre-fix tree, two-sided exact match)"
  if [[ -z "${RULES}" || ! -r "${RULES}" ]]; then
    echo "UNAUTHENTICATED: no rule-value file (set PROTECTED_RULES_FILE)."
    echo "The oracle is NOT skipped-green: this is exit 3, distinct from a clean pass."
    return 3
  fi
  local ref report
  ref="$("${PY}" -c 'import json,sys;print(json.load(open(sys.argv[1]))["oracle"]["ref"])' "${MANIFEST}")"
  echo "oracle ref: ${ref}"
  report="$(mktemp)"
  "${PY}" "${SCANNER}" --manifest "${MANIFEST}" --rules "${RULES}" \
      --root "${ROOT}" --ref "${ref}" --json > "${report}" || true
  "${PY}" - "${MANIFEST}" "${report}" <<'PYEOF'
import json, sys
manifest = json.load(open(sys.argv[1]))
report = json.load(open(sys.argv[2]))
oracle = manifest["oracle"]
counts = {}
for f in report["findings"]:
    if f["class"] == "A":
        counts[f["path"]] = counts.get(f["path"], 0) + 1
expected = oracle["expected_class_a"]
for path in oracle["must_report_zero_in"]:
    expected.setdefault(path, 0)
    counts.setdefault(path, 0)
if counts != expected:
    extra = {k: v for k, v in counts.items() if expected.get(k) != v}
    missing = {k: v for k, v in expected.items() if counts.get(k) != v}
    sys.exit(f"FAIL: two-sided mismatch\n  got-but-not-expected: {extra}\n"
             f"  expected-but-not-got: {missing}")
total = sum(v for k, v in counts.items() if k not in oracle["must_report_zero_in"])
files = len([k for k, v in counts.items() if v and k not in oracle["must_report_zero_in"]])
if (total, files) != (oracle["expected_total_class_a"], oracle["expected_file_count"]):
    sys.exit(f"FAIL: totals {total}/{files} != "
             f"{oracle['expected_total_class_a']}/{oracle['expected_file_count']}")
print(f"PASS: exactly {total} class-A findings across exactly {files} files, "
      f"and exactly 0 in {oracle['must_report_zero_in']}")
PYEOF
}

verify_snapshot() {
  local base="${1:-origin/main}"
  banner "D5 public-surface snapshot diff (from Git blobs), base=${base}"
  local work
  work="$(mktemp -d)"
  trap 'rm -rf "${work}"' RETURN
  "${PY}" "${SNAPSHOT}" --root "${ROOT}" --ref "${base}" --manifest "${MANIFEST}" \
      --require-module src/digitalmodel/solvers/openfoam/__init__.py \
      --require-module src/digitalmodel/solvers/openfoam/validation/sloshing_sweep.py \
      > "${work}/base.json"
  "${PY}" "${SNAPSHOT}" --root "${ROOT}" --ref HEAD --manifest "${MANIFEST}" \
      --require-module src/digitalmodel/solvers/openfoam/__init__.py \
      --require-module src/digitalmodel/solvers/openfoam/validation/sloshing_sweep.py \
      > "${work}/head.json"
  echo "base modules: $("${PY}" -c 'import json,sys;print(len(json.load(open(sys.argv[1]))["modules"]))' "${work}/base.json")"
  echo "head modules: $("${PY}" -c 'import json,sys;print(len(json.load(open(sys.argv[1]))["modules"]))' "${work}/head.json")"
  if diff -u "${work}/base.json" "${work}/head.json" > "${work}/surface.diff"; then
    echo "PASS: public surface unchanged"
  else
    echo "DIFF: the public surface moved. Every line below belongs in the PR body."
    cat "${work}/surface.diff"
  fi

  banner "D5 mutation proof (an always-empty diff must be distinguishable)"
  mutation_proof "${work}"
}

mutation_proof() {
  local work="$1" scratch="$1/scratch"
  mkdir -p "${scratch}/src/digitalmodel"
  git -C "${scratch}" init -q -b main
  git -C "${scratch}" config user.email t@example.invalid
  git -C "${scratch}" config user.name t
  printf 'def build(count=3, label=None):\n    return count\n\n\nif __name__ == "__main__":\n    raise SystemExit(build())\n' \
      > "${scratch}/src/digitalmodel/m.py"
  git -C "${scratch}" add -A && git -C "${scratch}" commit -qm before
  "${PY}" "${SNAPSHOT}" --root "${scratch}" --ref HEAD > "${work}/m_before.json"
  printf 'def build(count=4):\n    return count\n' > "${scratch}/src/digitalmodel/m.py"
  git -C "${scratch}" add -A && git -C "${scratch}" commit -qm after
  "${PY}" "${SNAPSHOT}" --root "${scratch}" --ref HEAD > "${work}/m_after.json"
  if diff -q "${work}/m_before.json" "${work}/m_after.json" >/dev/null; then
    echo "FAIL: a changed default, a dropped optional and a removed dispatch"
    echo "      all produced an empty diff -- the comparison is vacuous"
    return 1
  fi
  echo "PASS: perturbing one default, dropping one optional and removing the"
  echo "      module dispatch each moves the snapshot, so an empty diff means something"
}

main() {
  local did=0 rc=0
  [[ $# -eq 0 ]] && { echo "usage: $0 [--oracle] [--snapshot [BASE]] [--enumeration] [--all]" >&2; exit 2; }
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --all) verify_enumeration || rc=1; verify_snapshot "${2:-origin/main}" || rc=1
             verify_oracle || rc=$?; did=1; shift ;;
      --enumeration) verify_enumeration || rc=1; did=1; shift ;;
      --oracle) verify_oracle || rc=$?; did=1; shift ;;
      --snapshot) shift; local base="origin/main"
                  [[ $# -gt 0 && "$1" != --* ]] && { base="$1"; shift; }
                  verify_snapshot "${base}" || rc=1; did=1 ;;
      *) echo "unknown argument: $1" >&2; exit 2 ;;
    esac
  done
  [[ ${did} -eq 1 ]] || exit 2
  exit "${rc}"
}

main "$@"
