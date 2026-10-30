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
LINK_SUFFIX = re.compile(r"\(|:|\s*\[")
MARKDOWN_CONTEXT = r"""
const fs = require('node:fs');
const prettier = require('node:module').createRequire(process.argv[1])('prettier');
const text = fs.readFileSync(0, 'utf8');
(async () => {
    // Normalize blank-line tabs without moving the parser's character offsets.
    const input = text.replace(/^[ \t]+$/gm, blank => blank.replace(/\t/g, ' '));
    const {ast} = await prettier.__debug.parse(input, {parser: 'markdown'});
    const code = [];
    const definitions = [];
    function visit(node) {
        if (node.type === 'inlineCode' || node.type === 'code') {
            code.push([node.position.start.offset, node.position.end.offset]);
        }
        if (node.type === 'definition') definitions.push('[' + node.identifier + ']');
        for (const child of node.children || []) visit(child);
    }
    visit(ast);
    let prose = text;
    for (const [start, end] of code.sort((a, b) => b[0] - a[0])) {
        const mask = text.slice(start, end).replace(/[^\n]/gu, ' ');
        prose = prose.slice(0, start) + mask + prose.slice(end);
    }
    process.stdout.write(JSON.stringify({prose, definitions}));
})().catch(() => process.exit(1));
"""


def reference_prose(lines: list[str]) -> tuple[str, set[str]]:
    """Use the formatter AST to mask code and collect container definitions."""
    executable = shutil.which(os.environ.get("SOURCE_LOCATORS_PRETTIER", "prettier"))
    if executable is None:
        raise FileNotFoundError("Markdown formatter unavailable")
    result = subprocess.run(
        ["node", "-e", MARKDOWN_CONTEXT, str(Path(executable).resolve())],
        input="".join(lines),
        capture_output=True, text=True, encoding="utf-8", check=True, timeout=30,
    )
    context = json.loads(result.stdout)
    return context["prose"], {label.casefold() for label in context["definitions"]}


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
        prose, definitions = reference_prose(lines)
    except (OSError, subprocess.SubprocessError, ValueError):
        return [f"{label}: cannot parse Markdown; pinned formatter and Node.js required"]
    prose_lines = prose.split("\n")
    errors = []
    # Markdown diagnostics and inclusive source bounds use one-based numbering.
    for number, line in enumerate(lines, start=1):
        for match in LOCATOR.finditer(line):
            is_prose = prose_lines[number - 1][match.start():match.end()] == match[0]
            if is_prose and (
                line[max(0, match.start() - 1):match.start()] in {"!", "\\"}
                or LINK_SUFFIX.match(line, match.end())
                or match[0].casefold() in definitions
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
