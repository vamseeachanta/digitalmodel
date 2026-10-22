# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check changed Python calls with Ruff PLW1514 for explicit text encodings.

The default scope is ``git diff -U0 origin/main...HEAD``. ``--whole-tree``
checks all tracked Python files for a separate cleanup audit. Ruff 0.15.1,
pinned in uv.lock, must be available in the invoking Python environment.
Multiline calls are checked when any line in the call changes. Encoding
resolution follows Ruff, including its limits on inferred object types.
"""

import argparse
import ast
import io
import json
import os
import re
import subprocess
import sys
import tokenize
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RUFF_VERSION = "0.15.1"
HUNK = re.compile(r"^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@", re.MULTILINE)


class ChangedLines(set):
    """Added line numbers plus new-side anchors for deletion-only hunks."""

    def __init__(self):
        super().__init__()
        self.deletions = set()


def git(root, *args):
    """Return Git output without using the platform's default text codec."""
    result = subprocess.run(
        ["git", *args],
        cwd=root,
        capture_output=True,
        check=False,
        encoding="utf-8",
        errors="strict",
        env={
            key: value
            for key, value in os.environ.items()
            if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}
        },
    )
    if result.returncode:
        raise RuntimeError(
            f"Git scope discovery failed: {result.stderr[-2000:].strip()}"
        )
    return result.stdout


def added_lines(patch):
    """Return new-side line numbers from zero-context unified diff hunks."""
    lines = ChangedLines()
    hunks = list(HUNK.finditer(patch))
    for index, match in enumerate(hunks):
        start, count = int(match[1]), int(match[2] or 1)
        if count == 0:
            lines.deletions.add(start)
            continue
        end = hunks[index + 1].start() if index + 1 < len(hunks) else len(patch)
        body = patch[match.end() : end].splitlines()[1:]
        removed = Counter(line[1:] for line in body if line.startswith("-"))
        row = start
        for line in body:
            if line.startswith("+"):
                if removed[line[1:]]:
                    removed[line[1:]] -= 1
                else:
                    lines.add(row)
                row += 1
            elif line.startswith(" "):
                row += 1
    return lines


def diff_scope(root, base):
    """Preserve unchanged lines across renames and isolate diff display settings."""
    revision = f"{base}...HEAD"
    names = git(
        root,
        "diff",
        "-M",
        "--no-color",
        "--no-ext-diff",
        "--no-textconv",
        "--diff-filter=ACMR",
        "--name-status",
        "-z",
        revision,
        "--",
    )
    scope = {}
    entries = iter(names.rstrip("\0").split("\0")) if names else iter(())
    for status in entries:
        old = next(entries)
        path = next(entries) if status.startswith(("R", "C")) else old
        if path.endswith(".py"):
            patch = git(
                root,
                "diff",
                "-M",
                "--no-color",
                "--inter-hunk-context=0",
                "--no-ext-diff",
                "--no-textconv",
                "-U0",
                revision,
                "--",
                *dict.fromkeys((old, path)),
            )
            scope[path] = added_lines(patch)
    return scope


def changed_line(node, lines):
    """Anchor a finding to the changed part of a possibly multiline call."""
    if lines is None:
        return node.lineno
    touched = lines.intersection(range(node.lineno, node.end_lineno + 1))
    touched.update(
        anchor
        for anchor in getattr(lines, "deletions", ())
        if node.lineno <= anchor < node.end_lineno
    )
    return min(touched) if touched else None


def finding(path, line, code, fix):
    return {"path": path, "line": line, "code": code, "fix": fix}


def ruff_findings(root, paths):
    """Use only the pinned encoding rule, independent of local lint ignores."""
    try:
        version = subprocess.run(
            ["ruff", "--version"],
            capture_output=True,
            encoding="utf-8",
            check=False,
        )
        if version.returncode or version.stdout.strip() != f"ruff {RUFF_VERSION}":
            raise RuntimeError(
                f"Checker requires Ruff {RUFF_VERSION}; "
                f"found {version.stdout.strip()}; "
                f"{version.stderr[-2000:].strip()}"
            )
        result = subprocess.run(
            [
                "ruff",
                "check",
                "--isolated",
                "--no-cache",
                "--preview",
                "--select",
                "PLW1514",
                "--ignore-noqa",
                "--output-format",
                "json",
                "--",
                *paths,
            ],
            cwd=root,
            capture_output=True,
            encoding="utf-8",
            check=False,
        )
    except OSError as exc:
        raise RuntimeError("Ruff could not run; install the pinned version") from exc
    if result.returncode not in (0, 1):
        raise RuntimeError(f"Ruff could not run: {result.stderr[-2000:].strip()}")
    try:
        return json.loads(result.stdout)
    except (ValueError, TypeError) as exc:
        raise RuntimeError(
            f"Ruff returned invalid diagnostic JSON: {result.stderr[-2000:].strip()}"
        ) from exc


def check_files(root, scope):
    """Return findings only for scoped Python files and changed call spans."""
    root = root.resolve()
    findings, calls_by_path, valid, source_by_path = [], {}, {}, {}
    for path, lines in scope.items():
        if not path.endswith(".py") or (
            lines == set() and not getattr(lines, "deletions", ())
        ):
            continue
        try:
            # Parsing bytes honours Python encoding cookies and UTF-8 BOMs.
            raw = (root / path).read_bytes()
            tree = ast.parse(raw, filename=path)
            codec, _ = tokenize.detect_encoding(io.BytesIO(raw).readline)
            source_by_path[path] = raw.decode(codec).splitlines()
        except (OSError, SyntaxError, UnicodeError, ValueError):
            line = min(lines) if lines else 1
            findings.append(
                finding(
                    path, line, "TXT000", "make Python source readable and parseable"
                )
            )
            continue
        calls = [node for node in ast.walk(tree) if isinstance(node, ast.Call)]
        calls_by_path[path], valid[path] = calls, lines
    if valid:
        for item in ruff_findings(root, list(valid)):
            path = Path(item["filename"]).resolve().relative_to(root).as_posix()
            row, column = item["location"]["row"], item["location"]["column"]
            # AST columns count UTF-8 bytes; Ruff columns count Unicode characters.
            prefix = source_by_path[path][row - 1][: column - 1]
            byte_column = len(prefix.encode("utf-8"))
            end = item["end_location"]
            end_prefix = source_by_path[path][end["row"] - 1][: end["column"] - 1]
            end_byte_column = len(end_prefix.encode("utf-8"))
            # Ruff locates the callable; extend to its arguments for multiline edits.
            calls = [
                call
                for call in calls_by_path[path]
                if call.func.lineno == row
                and call.func.col_offset == byte_column
                and call.func.end_lineno == end["row"]
                and call.func.end_col_offset == end_byte_column
            ]
            call = max(calls, key=lambda node: node.end_lineno, default=None)
            line = (
                changed_line(call, valid[path])
                if call
                else (row if valid[path] is None or row in valid[path] else None)
            )
            if line is not None:
                findings.append(
                    finding(
                        path,
                        line,
                        "PLW1514" if item["code"] == "PLW1514" else "TXT000",
                        'add encoding="utf-8" to the text call'
                        if item["code"] == "PLW1514"
                        else item["message"],
                    )
                )
    return sorted(findings, key=lambda item: (item["path"], item["line"], item["code"]))


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("--base", default="origin/main")
    parser.add_argument("--whole-tree", action="store_true")
    parser.add_argument(
        "--json", action="store_true", help="emit structured diagnostics"
    )
    args = parser.parse_args(argv)
    try:
        if args.whole_tree:
            scope = {
                path: None
                for path in git(args.root, "ls-files", "-z").split("\0")
                if path.endswith(".py")
            }
        else:
            scope = diff_scope(args.root, args.base)
        findings = check_files(args.root, scope)
    except (RuntimeError, OSError, UnicodeError) as exc:
        print(f"text encoding check: {exc}", file=sys.stderr)
        return 2
    if args.json:
        print(json.dumps(findings, ensure_ascii=True, indent=2))
    else:
        for item in findings:
            print(f"{item['path']}:{item['line']}: {item['code']}: {item['fix']}")
        if not findings:
            print("Text encoding check passed.")
    return 1 if findings else 0


if __name__ == "__main__":
    sys.exit(main())
