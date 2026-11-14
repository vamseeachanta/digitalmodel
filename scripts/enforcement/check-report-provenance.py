#!/usr/bin/env python3
"""Report provenance ratchet (#1019), using only the standard library.

Usage: python scripts/enforcement/check-report-provenance.py [--root REPO]
Exit 0: clean; exit 1: uncontracted report modules; exit 2: tool/input error.
Scans tracked src/digitalmodel Python report modules, never tests or scripts.
Grandfathered modules live in report-provenance-allowlist.txt. A trailing
``# report-lint: allowed`` exempts only the annotated HTML-producing evidence.
This heuristic checks adoption, not literal-result correctness. Runtime read
contracts are enforced by assemble_report, not by static literal detection.
"""

import argparse
import ast
import io
import subprocess
import sys
import tokenize
from pathlib import Path

APPROVED = {"assemble_report", "ReportSkeleton", "Provenance"}
ALLOWLIST = Path("scripts/enforcement/report-provenance-allowlist.txt")
ESCAPE = "# report-lint: allowed"


def exemptions(source):
    """Recognize actual trailing comments, never text inside string literals."""
    return {
        token.start[0]
        for token in tokenize.generate_tokens(io.StringIO(source).readline)
        if token.type == tokenize.COMMENT and token.string.rstrip().endswith(ESCAPE)
    }


def html_evidence(tree, source):
    lines = source.splitlines()
    for node in ast.walk(tree):
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            if node.name.endswith("_html") or (
                node.name.startswith("generate_") and "report" in node.name
            ):
                yield node.lineno
        if not isinstance(node, ast.Call):
            continue
        name = getattr(node.func, "attr", getattr(node.func, "id", ""))
        if name not in {
            "write",
            "open",
            "write_text",
            "write_html",
            "to_html",
        }:
            continue
        # Include nearby assignments such as path = out / "report.html".
        nearby = "\n".join(lines[max(0, node.lineno - 6) : node.end_lineno + 5])
        try:
            tokens = tokenize.generate_tokens(io.StringIO(nearby).readline)
            has_html = any(
                token.type == tokenize.STRING and ".html" in token.string.lower()
                for token in tokens
            )
        except (IndentationError, tokenize.TokenError):
            has_html = any(
                isinstance(item, ast.Constant)
                and isinstance(item.value, str)
                and ".html" in item.value.lower()
                for item in ast.walk(node)
            )
        if has_html:
            yield node.lineno


def compliant(tree):
    return any(
        isinstance(node, ast.ImportFrom)
        and node.module is not None
        and (
            node.module == "digitalmodel.reporting"
            or node.module.startswith("digitalmodel.reporting.")
        )
        and any(alias.name in APPROVED for alias in node.names)
        for node in ast.walk(tree)
    )


def check(root):
    allowlist = root / ALLOWLIST
    allowed = (
        {line.split("#", 1)[0].strip() for line in allowlist.read_text().splitlines()}
        if allowlist.exists()
        else set()
    )
    paths = subprocess.run(
        [
            "git",
            "ls-files",
            "-z",
            "src/digitalmodel/**/*.py",
            "src/digitalmodel/*.py",
        ],
        cwd=root,
        check=True,
        capture_output=True,
        text=True,
    ).stdout.split("\0")
    violations = []
    for name in sorted(set(filter(None, paths))):
        path = Path(name)
        if "report" not in path.stem.lower() or name in allowed:
            continue
        source = (root / path).read_text(encoding="utf-8")
        try:
            tree = ast.parse(source)
            if not compliant(tree):
                escaped = exemptions(source)
                evidence = sorted(set(html_evidence(tree, source)) - escaped)
                if evidence:
                    violations.append((name, evidence[0]))
        except (SyntaxError, tokenize.TokenError) as exc:
            raise ValueError(f"Cannot parse tracked report module {name}") from exc
    return violations


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parents[2]
    )
    args = parser.parse_args()
    try:
        violations = check(args.root)
    except (OSError, ValueError, subprocess.CalledProcessError) as exc:
        print(
            f"check-report-provenance: {type(exc).__name__}: scan failed",
            file=sys.stderr,
        )
        return 2
    for name, line in violations:
        print(f"{name}:{line}: HTML reporter must import shared provenance (#1019)")
    if violations:
        print(
            f"check-report-provenance: {len(violations)} uncontracted report module(s)",
            file=sys.stderr,
        )
    return int(bool(violations))


if __name__ == "__main__":
    sys.exit(main())
