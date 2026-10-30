# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check explicit local Python source locators in supplied Markdown files.

Only [repository/relative.py:Lstart-Lend] tokens are checked, whether bare or
inside inline code. Markdown links, external URLs and unbracketed prose are
outside this grammar. Source text is read without importing or executing it.
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path

# scripts/docs is two directory levels below the repository root.
ROOT = Path(__file__).resolve().parents[2]
LOCATOR = re.compile(
    r"(?<![!\\\w/])\["
    r"(?P<source>(?:[A-Za-z0-9_.-]+/)*[A-Za-z0-9_.-]+\.py)"
    r":L(?P<first>[0-9]+)-L(?P<last>[0-9]+)\]"
    r"(?!\(|:|\s*\[)"
)
REFERENCE_DEFINITION = re.compile(
    r"^ {0,3}(?P<label>\[[^\[\]\n]+\]):[ \t]*(?:\n[ \t]*)?"
    r"(?:<[^<>\n]*>|[^\s<>]+)"
    r"(?:[ \t]+(?:\"[^\"\n]*\"|'[^'\n]*'|\([^()\n]*\)))?[ \t]*$",
    re.MULTILINE,
)
CODE_SPAN = re.compile(r"(?<![\\`])(?:\\\\)*(`+)(?!`)(.*?)(?<!`)\1(?!`)", re.DOTALL)


def reference_prose(lines: list[str]) -> str:
    """Mask code without changing offsets or physical document lines."""
    masked = []
    fence = ""
    for line in lines:
        delimiter = re.match(r" {0,3}(`{3,}|~{3,})(.*)", line)
        if fence:
            masked.append(re.sub(r"[^\n]", " ", line))
            if (delimiter and delimiter[1][0] == fence[0]
                    and len(delimiter[1]) >= len(fence) and not delimiter[2].strip()):
                fence = ""
        elif delimiter and not (delimiter[1][0] == "`" and "`" in delimiter[2]):
            fence = delimiter[1]
            masked.append(re.sub(r"[^\n]", " ", line))
        else:
            masked.append(line)
    return CODE_SPAN.sub(lambda match: re.sub(r"[^\n]", " ", match[0]), "".join(masked))


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

    prose = reference_prose(lines)
    definitions = {match["label"].casefold() for match in REFERENCE_DEFINITION.finditer(prose)}
    prose_lines = prose.split("\n")
    errors = []
    # Markdown diagnostics and inclusive source bounds use one-based numbering.
    for number, line in enumerate(lines, start=1):
        for match in LOCATOR.finditer(line):
            if (match[0].casefold() in definitions
                    and prose_lines[number - 1][match.start():match.end()] == match[0]):
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
