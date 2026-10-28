"""Fail a pull request only for lint findings it adds to the files it changes.

The repository carries a large historical ruff backlog, so linting every changed
file outright would fail pull requests for findings they did not write. This
gate lints each changed Python file twice -- as it is on the merge base and as
it is in the tested merge -- and fails when any (rule, message) pair occurs more
often after the change than before. Line numbers are ignored, so moving code
does not create findings.

Run with ruff on PATH, for example:

    uv run --no-project --with ruff==0.6.9 python scripts/ci/lint_changed.py --base <sha>
"""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from collections import Counter

# The repository rule set (pyproject [tool.ruff.lint]) plus W605, which flags the
# invalid escape sequences that Python 3.12 turns into SyntaxWarning and that
# `-W error` test runs then fail on at import time.
EXTRA_RULES = "W605"


def git(*args: str) -> str:
    return subprocess.run(["git", *args], check=True, capture_output=True, text=True).stdout


def changed_python_files(base: str) -> list[str]:
    output = git("diff", "--name-only", "--diff-filter=ACMR", base, "HEAD", "--", "*.py")
    return [line for line in output.splitlines() if line.strip()]


def findings(path: str, source: str) -> Counter[tuple[str, str]]:
    result = subprocess.run(
        ["ruff", "check", "--force-exclude", "--no-cache", "--output-format", "json",
         "--extend-select", EXTRA_RULES, "--stdin-filename", path, "-"],
        input=source, capture_output=True, text=True,
    )
    if result.returncode not in (0, 1):
        raise SystemExit(f"ruff failed on {path}: {result.stderr.strip()}")
    rows = json.loads(result.stdout or "[]")
    return Counter((row.get("code") or "syntax-error", row["message"]) for row in rows)


def base_source(base: str, path: str) -> str:
    probe = subprocess.run(["git", "cat-file", "-e", f"{base}:{path}"], capture_output=True)
    return git("show", f"{base}:{path}") if probe.returncode == 0 else ""


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--base", required=True, help="merge-base tip the change is compared with")
    args = parser.parse_args()

    files = changed_python_files(args.base)
    print(f"Linting {len(files)} changed Python file(s) against {args.base[:12]}.")
    added_total = 0
    for path in files:
        with open(path, encoding="utf-8", errors="surrogateescape") as handle:
            after = findings(path, handle.read())
        before = findings(path, base_source(args.base, path))
        added = after - before
        for (code, message), count in sorted(added.items()):
            added_total += count
            print(f"::error file={path}::{code} {message} (+{count})")
    if added_total:
        print(f"{added_total} new lint finding(s). Fix them; existing findings in "
              "touched files do not fail this gate.")
        return 1
    print("No new lint findings.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
