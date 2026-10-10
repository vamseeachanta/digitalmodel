#!/usr/bin/env python3
"""Check engineering writing register compliance.

Level-2 enforcement script for the engineering writing register defined in
.claude/rules/engineering-register.md.  Scans Markdown and reStructuredText
files for common violations and reports findings with file, line, and rule.

Exclusions:
  - Verbatim third-party transcriptions (files under docs/standards/,
    any path containing /vendor/, and files whose first line is a blockquote
    attribution like "> Source: ...").
  - The unsupported-adjective rule (rule 5) accepts a criterion that appears
    in the same sentence OR the immediately following sentence.

Usage:
  python scripts/enforcement/check-engineering-register.py [paths...]
  python scripts/enforcement/check-engineering-register.py --self-test
  python scripts/enforcement/check-engineering-register.py --diff BASE

When called with --diff BASE, only files changed since BASE are scanned.
When called with no arguments, scans docs/ and src/ for .md and .rst files.
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path

ADJECTIVES_WITHOUT_CRITERION = re.compile(
    r"\b(acceptable|conservative|safe|adequate|satisfactory)\b",
    re.IGNORECASE,
)

CRITERION_PATTERN = re.compile(
    r"(per |according to |as per |clause |section |table |"
    r"DNV|API|ASME|ISO|BS |NORSOK|ABS|IEC|IEEE|NFPA|"
    r"<\s*|>\s*|≤|≥|less than|greater than|exceeds|below|above|"
    r"limit|threshold|criterion|criteria|requirement|"
    r"because|since|given that|where )",
    re.IGNORECASE,
)

FIRST_PERSON = re.compile(
    r"\b(we |we've |we're |our |I |my |us )\b",
    re.IGNORECASE,
)

SHOULD_SHALL_MISUSE = re.compile(
    r"\bshall\b.*\b(recommend|suggested|advisory|optional)\b"
    r"|\bshall\s+be\s+recommend"
    r"|\b(mandatory|required|must)\b.*\bshould\b",
    re.IGNORECASE,
)

THICKNESS_DECIMALS = re.compile(
    r"(\d+\.\d{1,2})\s*(mm|in)\b",
)

CAPTION_ABOVE_TABLE = re.compile(
    r"^(Table\s+\d+[.:].*|.*\bcaption\b.*)\s*$",
)

TABLE_START = re.compile(r"^\|.*\|.*\|")

EXCLUDED_DIRS = {
    "standards",
    "vendor",
    "third_party",
    "third-party",
    "legacy",
}


def is_excluded(path: Path) -> bool:
    parts = set(path.parts)
    if parts & EXCLUDED_DIRS:
        return True
    if "docs/standards" in str(path):
        return True
    return False


def has_criterion_nearby(lines: list[str], idx: int) -> bool:
    """Check if a criterion appears in this line or the next."""
    current = lines[idx]
    if CRITERION_PATTERN.search(current):
        return True
    if idx + 1 < len(lines) and CRITERION_PATTERN.search(lines[idx + 1]):
        return True
    return False


def prose_lines(lines: list[str]) -> list[tuple[int, str]]:
    """Return numbered prose outside Markdown code and quotations."""
    result = []
    fence = None
    for number, line in enumerate(lines, 1):
        marker = re.match(r"(`{3,}|~{3,})", line.lstrip())
        if marker and fence is None:
            fence = marker.group(1)
            continue
        if fence is not None:
            if (
                marker
                and marker.group(1)[0] == fence[0]
                and len(marker.group(1)) >= len(fence)
            ):
                fence = None
            continue
        if line.startswith(("    ", "\t")) or line.lstrip().startswith(">"):
            continue
        result.append((number, re.sub(r"`[^`]*`", "", line)))
    return result


def check_line(line: str, number: int, lines: list[str]) -> list[tuple[int, str, str]]:
    """Check register rules that apply to an individual prose line."""
    findings = []
    match = FIRST_PERSON.search(line)
    if match:
        findings.append(
            (
                number,
                "R1-subject",
                f"First-person '{match.group().strip()}' — "
                "use an analysis/component subject",
            )
        )
    if SHOULD_SHALL_MISUSE.search(line):
        findings.append(
            (number, "R4-shall-should", "Requirement and advisory language are mixed")
        )
    match = ADJECTIVES_WITHOUT_CRITERION.search(line)
    if match and not has_criterion_nearby(lines, number - 1):
        findings.append(
            (
                number,
                "R5-unsupported-adj",
                f"'{match.group()}' without governing criterion",
            )
        )
    for clause in re.split(r"[;.!?]\s+", line):
        if re.match(
            r"(?:the\s+)?(?:wall\s+)?(?:thickness|corrosion allowance)\b",
            clause.strip(),
            re.I,
        ):
            for match in THICKNESS_DECIMALS.finditer(clause):
                findings.append(
                    (
                        number,
                        "R7-thickness-decimals",
                        f"Thickness '{match.group()}' requires 3 decimals",
                    )
                )
    return findings


def check_file(path: Path) -> list[tuple[int, str, str]]:
    """Return register findings for readable prose in a file."""
    if is_excluded(path):
        return []
    return check_file_text(path.read_text(encoding="utf-8", errors="replace"))


FIXTURES: list[tuple[str, str, str | None]] = [
    # (text, expected_rule_or_None, description)
    (
        "We verified the pipeline meets criteria.",
        "R1-subject",
        "first-person detected",
    ),
    (
        "The pipeline satisfies burst criteria per DNV-ST-F101.",
        None,
        "proper subject, no finding",
    ),
    (
        "The wall thickness is acceptable.",
        "R5-unsupported-adj",
        "unsupported adjective",
    ),
    (
        "The wall thickness is acceptable per DNV-ST-F101 clause 5.4.2.",
        None,
        "adjective with criterion",
    ),
    ("The thickness is 12.5 mm.", "R7-thickness-decimals", "2-decimal thickness"),
    ("The thickness is 12.500 mm.", None, "3-decimal thickness ok"),
    (
        "The coating shall be recommended for use.",
        "R4-shall-should",
        "shall with advisory",
    ),
    ("The coating should be applied.", None, "should alone is fine"),
    ("Our analysis shows convergence.", "R1-subject", "first-person 'our'"),
    ("The analysis converged at iteration 15.", None, "no first person"),
    (
        "Results are conservative.",
        "R5-unsupported-adj",
        "conservative without criterion",
    ),
    (
        "Results are conservative because the load factor exceeds 1.5.",
        None,
        "conservative with because-clause",
    ),
    (
        "The design is safe and adequate.",
        "R5-unsupported-adj",
        "multiple unsupported adjectives",
    ),
    (
        "The thickness of 25.1 in is below the limit.",
        "R7-thickness-decimals",
        "inches with 1 decimal",
    ),
    (
        "The thickness of 25.100 in meets requirements.",
        None,
        "inches with 3 decimals",
    ),
]


def run_self_test() -> bool:
    """Run built-in test fixtures. Returns True if all pass."""
    passed = 0
    failed = 0

    for text, expected_rule, desc in FIXTURES:
        findings = check_file_text(text)
        found_rules = {f[1] for f in findings}

        if expected_rule is None:
            if findings:
                print(f"  FAIL: '{desc}' — expected no findings, got {found_rules}")
                failed += 1
            else:
                passed += 1
        else:
            if expected_rule in found_rules:
                passed += 1
            else:
                print(
                    f"  FAIL: '{desc}' — expected {expected_rule}, "
                    f"got {found_rules or 'none'}"
                )
                failed += 1

    print(f"\nSelf-test: {passed} passed, {failed} failed out of {len(FIXTURES)}")
    return failed == 0


def check_file_text(text: str) -> list[tuple[int, str, str]]:
    """Check prose directly, without platform-specific temporary-file locks."""
    lines = text.splitlines()
    if not lines or lines[0].startswith(("> Source:", "> Transcribed")):
        return []
    prose = prose_lines(lines)
    findings = [
        finding for number, line in prose for finding in check_line(line, number, lines)
    ]
    for (number, line), (next_number, next_line) in zip(prose, prose[1:]):
        if (
            next_number == number + 1
            and CAPTION_ABOVE_TABLE.match(line)
            and TABLE_START.match(next_line)
        ):
            findings.append(
                (
                    number,
                    "R6-caption-position",
                    "Caption appears above table; place it below",
                )
            )
    return findings


def find_files(paths: list[str]) -> list[Path]:
    """Find .md and .rst files to scan."""
    result = []
    for p in paths:
        path = Path(p)
        if path.is_file():
            result.append(path)
        elif path.is_dir():
            result.extend(path.rglob("*.md"))
            result.extend(path.rglob("*.rst"))
    return sorted(set(result))


def diff_files(base: str) -> list[Path]:
    """Return files changed since base commit."""
    output = subprocess.check_output(
        ["git", "diff", "--name-only", "--diff-filter=ACM", base, "--"],
        text=True,
    )
    return [Path(f) for f in output.strip().splitlines() if f.endswith((".md", ".rst"))]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*", default=["docs/", "src/"])
    parser.add_argument(
        "--self-test", action="store_true", help="Run built-in test fixtures"
    )
    parser.add_argument(
        "--diff", metavar="BASE", help="Only scan files changed since BASE"
    )
    args = parser.parse_args()

    if args.self_test:
        return 0 if run_self_test() else 1

    if args.diff:
        files = diff_files(args.diff)
    else:
        files = find_files(args.paths)

    if not files:
        print("No files to scan.")
        return 0

    total_findings = 0
    for f in files:
        findings = check_file(f)
        for line_no, rule, message in findings:
            print(f"{f}:{line_no}: [{rule}] {message}")
            total_findings += 1

    print(f"\n{len(files)} files scanned, {total_findings} findings.")
    return 1 if total_findings > 0 else 0


if __name__ == "__main__":
    sys.exit(main())
