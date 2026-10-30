# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check explicit local Python source locators in supplied Markdown files.

Only [repository/relative.py:Lstart-Lend] tokens are checked, whether bare or
inside inline code. Markdown links, external URLs and unbracketed prose are
outside this grammar. Source text is read without importing or executing it.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
from pathlib import Path

# scripts/docs is two directory levels below the repository root.
ROOT = Path(__file__).resolve().parents[2]
LOCATOR = re.compile(
    r"(?<![\w/])\["
    r"(?P<source>(?:[A-Za-z0-9_.-]+/)*[A-Za-z0-9_.-]+\.py)"
    r":L(?P<first>[0-9]+)-L(?P<last>[0-9]+)\]"
)
# A per-document operational ceiling prevents a stalled parser from blocking
# the sequential check indefinitely; this is not a measured performance target.
PARSER_TIMEOUT_SECONDS = 30
MARKDOWN_CONTEXT = r"""
const fs = require('node:fs');
const prettier = require('node:module').createRequire(process.argv[1])('prettier');
const text = fs.readFileSync(0, 'utf8');
(async () => {
    // Normalize blank-line tabs without moving the parser's character offsets.
    const input = text.replace(/^[ \t]+$/gm, blank => blank.replace(/\t/g, ' '));
    const {ast} = await prettier.__debug.parse(input, {parser: 'markdown'});
    const code = [];
    const links = [];
    const references = [];
    const definitions = new Set();
    function visit(node) {
        if (node.type === 'inlineCode' || node.type === 'code') {
            code.push(node);
        }
        if (['link', 'image', 'definition'].includes(node.type)) links.push(node);
        if (['linkReference', 'imageReference'].includes(node.type)) references.push(node);
        if (node.type === 'definition') definitions.add(node.identifier);
        for (const child of node.children || []) visit(child);
    }
    function mask(nodes) {
        let coveredEnd = -1;
        // Keep outer spans only, so nested links cannot move Unicode offsets twice.
        const spans = nodes.map(node => [node.position.start.offset, node.position.end.offset])
            .sort((a, b) => a[0] - b[0] || b[1] - a[1])
            .filter(([start, end]) => {
                if (start < coveredEnd) return false;
                coveredEnd = end;
                return true;
            });
        let masked = text;
        for (const [start, end] of spans.reverse()) {
            const blank = text.slice(start, end).replace(/[^\n]/gu, ' ');
            masked = masked.slice(0, start) + blank + masked.slice(end);
        }
        return masked;
    }
    visit(ast);
    links.push(...references.filter(node => definitions.has(node.identifier)));
    process.stdout.write(JSON.stringify({prose: mask(code), excluded: mask(links)}));
})().catch(() => process.exit(1));
"""


def reference_prose(lines: list[str]) -> tuple[str, str]:
    """Use the formatter AST to mask code and complete resolved link spans."""
    executable = shutil.which(os.environ.get("SOURCE_LOCATORS_PRETTIER", "prettier"))
    if executable is None:
        raise FileNotFoundError("Markdown formatter unavailable")
    result = subprocess.run(
        ["node", "-e", MARKDOWN_CONTEXT, str(Path(executable).resolve())],
        input="".join(lines),
        capture_output=True, text=True, encoding="utf-8", check=True,
        timeout=PARSER_TIMEOUT_SECONDS,
    )
    context = json.loads(result.stdout)
    return context["prose"], context["excluded"]


def check_markdown(markdown: Path, root: Path = ROOT) -> list[str]:
    """Return document-line diagnostics for each unresolved local locator.

    Args:
        markdown: Markdown path, absolute or relative to root.
        root: Repository root used to resolve all source paths.

    Returns:
        Diagnostics with repository-relative document names and no source text.
    """
    root = root.resolve()
    markdown = root / markdown
    resolved = markdown.resolve()
    if not resolved.is_relative_to(root):
        return ["<document>: Markdown file outside repository"]
    label = resolved.relative_to(root).as_posix()
    if resolved.suffix.lower() != ".md":
        return [f"{label}: expected Markdown file"]
    try:
        with resolved.open(encoding="utf-8") as document:
            lines = list(document)
    except (OSError, UnicodeError):
        return [f"{label}: cannot read Markdown file"]

    try:
        prose, excluded = reference_prose(lines)
    except (OSError, subprocess.SubprocessError, ValueError):
        return [f"{label}: cannot parse Markdown; pinned formatter and Node.js required"]
    prose_lines = prose.split("\n")
    excluded_lines = excluded.split("\n")
    errors = []
    # Markdown diagnostics and inclusive source bounds use one-based numbering.
    for number, line in enumerate(lines, start=1):
        for match in LOCATOR.finditer(line):
            is_prose = prose_lines[number - 1][match.start():match.end()] == match[0]
            prefix = line[:match.start()]
            backslashes = len(prefix) - len(prefix.rstrip("\\"))
            if is_prose and (
                backslashes % 2
                or excluded_lines[number - 1][match.start():match.end()] != match[0]
            ):
                continue
            source = (root / match["source"]).resolve()
            first, last = int(match["first"]), int(match["last"])
            reason = None
            if not source.is_relative_to(root):
                reason = "source outside repository"
            elif not source.is_file():
                reason = "missing Python source"
            elif first > last:
                reason = "reversed line bounds"
            else:
                try:
                    with source.open(encoding="utf-8") as source_file:
                        count = sum(1 for _ in source_file)
                    if not 1 <= first <= last <= count:
                        reason = "line bounds outside source"
                except (OSError, UnicodeError):
                    reason = "cannot read Python source"
            if reason:
                errors.append(f"{label}:{number}: {match[0]}: {reason}")
    return errors


def main() -> int:
    """Check only the named documents and return a failing status on errors."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=ROOT)
    parser.add_argument("markdown", type=Path, nargs="+")
    args = parser.parse_args()
    failed = False
    for markdown in args.markdown:
        errors = check_markdown(markdown, args.root)
        for error in errors:
            print(error)
        failed |= bool(errors)
    # CLI convention: zero for success, one for a failed documentation check.
    return int(failed)


if __name__ == "__main__":
    raise SystemExit(main())
