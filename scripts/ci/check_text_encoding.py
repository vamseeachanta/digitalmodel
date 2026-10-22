# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check changed Python calls with Ruff PLW1514 and explicit write newlines.

The default scope is ``git diff -U0 origin/main...HEAD``. ``--whole-tree``
checks all tracked Python files for a separate cleanup audit. Ruff 0.15.1,
pinned in uv.lock, must be available in the invoking Python environment.
Multiline calls are checked when any line in the call changes. Encoding
resolution follows Ruff; the newline supplement matches open/write_text
syntax and treats dynamic modes as potentially writable text.
"""

import argparse
import ast
import io
import json
import re
import subprocess
import sys
import tokenize
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
HUNK = re.compile(r"^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@", re.MULTILINE)


def git(root, *args):
    """Return Git output without using the platform's default text codec."""
    result = subprocess.run(
        ["git", *args],
        cwd=root,
        capture_output=True,
        check=False,
        encoding="utf-8",
        errors="strict",
    )
    if result.returncode:
        raise RuntimeError("Git scope discovery failed; check the base ref and history")
    return result.stdout


def added_lines(patch):
    """Return new-side line numbers from zero-context unified diff hunks."""
    lines = set()
    for match in HUNK.finditer(patch):
        start, count = int(match[1]), int(match[2] or 1)
        lines.update(range(start, start + count))
    return lines


def diff_scope(root, base):
    """Disable rename detection so a moved Python file cannot evade checking."""
    revision = f"{base}...HEAD"
    names = git(
        root,
        "diff",
        "--no-renames",
        "--diff-filter=ACM",
        "--name-only",
        "-z",
        revision,
        "--",
    )
    scope = {}
    for path in names.split("\0"):
        if path.endswith(".py"):
            patch = git(
                root,
                "diff",
                "--no-renames",
                "--no-ext-diff",
                "--no-textconv",
                "-U0",
                revision,
                "--",
                path,
            )
            scope[path] = added_lines(patch)
    return scope


def changed_line(node, lines):
    """Anchor a finding to the changed part of a possibly multiline call."""
    if lines is None:
        return node.lineno
    touched = lines.intersection(range(node.lineno, node.end_lineno + 1))
    return min(touched) if touched else None


def finding(path, line, code, fix):
    return {"path": path, "line": line, "code": code, "fix": fix}


def argument(call, name, position, default=None):
    for keyword in call.keywords:
        if keyword.arg == name:
            return keyword.value
    return call.args[position] if len(call.args) > position else default


def open_aliases(tree):
    """Recognise the standard module and imported forms of the open builtin."""
    functions, modules = {"open"}, {"io", "builtins"}
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            modules.update(
                item.asname or item.name
                for item in node.names
                if item.name in {"io", "builtins"}
            )
        elif isinstance(node, ast.ImportFrom) and node.module in {"io", "builtins"}:
            functions.update(
                item.asname or item.name for item in node.names if item.name == "open"
            )
    return functions, modules


def needs_newline(call, aliases):
    """Require a newline policy for text writers, including update modes."""
    func = call.func
    functions, modules = aliases
    if isinstance(func, ast.Attribute) and func.attr == "write_text":
        return argument(call, "newline", 3) is None
    if not (
        isinstance(func, ast.Name)
        and func.id in functions
        or isinstance(func, ast.Attribute)
        and func.attr == "open"
    ):
        return False
    method = isinstance(func, ast.Attribute) and not (
        isinstance(func.value, ast.Name) and func.value.id in modules
    )
    mode = argument(call, "mode", 0 if method else 1, ast.Constant(value="r"))
    if isinstance(mode, ast.Constant) and isinstance(mode.value, str):
        if "b" in mode.value or not any(flag in mode.value for flag in "wax+"):
            return False
    return argument(call, "newline", 4 if method else 5) is None


def ruff_findings(root, paths):
    """Use only the pinned encoding rule, independent of local lint ignores."""
    try:
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
        raise RuntimeError("Ruff could not run; install the pinned version")
    try:
        return json.loads(result.stdout)
    except (ValueError, TypeError) as exc:
        raise RuntimeError("Ruff returned invalid diagnostic JSON") from exc


def check_files(root, scope):
    """Return findings only for scoped Python files and changed call spans."""
    root = root.resolve()
    findings, calls_by_path, valid, source_by_path = [], {}, {}, {}
    for path, lines in scope.items():
        if not path.endswith(".py") or lines == set():
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
        aliases = open_aliases(tree)
        calls = [node for node in ast.walk(tree) if isinstance(node, ast.Call)]
        calls_by_path[path], valid[path] = calls, lines
        for call in calls:
            line = changed_line(call, lines)
            if line is not None and needs_newline(call, aliases):
                findings.append(
                    finding(
                        path, line, "TXT001", 'add newline="\\n" to the text writer'
                    )
                )
    if valid:
        for item in ruff_findings(root, list(valid)):
            path = Path(item["filename"]).resolve().relative_to(root).as_posix()
            row, column = item["location"]["row"], item["location"]["column"]
            # AST columns count UTF-8 bytes; Ruff columns count Unicode characters.
            prefix = source_by_path[path][row - 1][: column - 1]
            byte_column = len(prefix.encode("utf-8"))
            # Ruff locates the callable; extend to its arguments for multiline edits.
            calls = [
                call
                for call in calls_by_path[path]
                if call.func.lineno == row and call.func.col_offset == byte_column
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
                        path, line, "PLW1514", 'add encoding="utf-8" to the text call'
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
