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


def check_file(path: Path) -> list[tuple[int, str, str]]:
    """Return list of (line_number, rule, message) findings."""
    if is_excluded(path):
        return []

    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except (OSError, UnicodeDecodeError):
        return []

    lines = text.splitlines()
    if not lines:
        return []

    # Skip files that are verbatim transcriptions
    if lines[0].startswith("> Source:") or lines[0].startswith("> Transcribed"):
        return []

    findings: list[tuple[int, str, str]] = []

    for i, line in enumerate(lines, 1):
        # Skip code blocks
        if line.strip().startswith("```") or line.strip().startswith("    "):
            continue
        # Skip blockquotes (may be transcribed text)
        if line.strip().startswith(">"):
            continue

        # Rule 1: subject is analysis/component, not a person
        match = FIRST_PERSON.search(line)
        if match:
            findings.append((i, "R1-subject", f"First-person '{match.group().strip()}' — subject should be the analysis or component"))

        # Rule 4: shall/should misuse
        if SHOULD_SHALL_MISUSE.search(line):
            findings.append((i, "R4-shall-should", "'shall' used with advisory language or 'should' with mandatory language"))

        # Rule 5: unsupported adjective
        match = ADJECTIVES_WITHOUT_CRITERION.search(line)
        if match and not has_criterion_nearby(lines, i - 1):
            findings.append((i, "R5-unsupported-adj", f"'{match.group()}' without governing criterion in this or following clause"))

        # Rule 7: thickness with fewer than 3 decimals
        for m in THICKNESS_DECIMALS.finditer(line):
            value = m.group(1)
            decimals = len(value.split(".")[1])
            if decimals < 3:
                findings.append((i, "R7-thickness-decimals", f"Thickness '{m.group()}' has {decimals} decimal(s); use 3"))

    # Rule 6: caption above table (check pairs of consecutive lines)
    for i in range(len(lines) - 1):
        if CAPTION_ABOVE_TABLE.match(lines[i]) and TABLE_START.match(lines[i + 1]):
            findings.append((i + 1, "R6-caption-position", "Caption appears above table; place it below"))

    return findings


def run_self_test() -> bool:
    """Run built-in test fixtures. Returns True if all pass."""
    fixtures: list[tuple[str, str, str | None]] = [
        # (text, expected_rule_or_None, description)
        ("We verified the pipeline meets criteria.", "R1-subject", "first-person detected"),
        ("The pipeline satisfies burst criteria per DNV-ST-F101.", None, "proper subject, no finding"),
        ("The wall thickness is acceptable.", "R5-unsupported-adj", "unsupported adjective"),
        ("The wall thickness is acceptable per DNV-ST-F101 clause 5.4.2.", None, "adjective with criterion"),
        ("The thickness is 12.5 mm.", "R7-thickness-decimals", "2-decimal thickness"),
        ("The thickness is 12.500 mm.", None, "3-decimal thickness ok"),
        ("The coating shall be recommended for use.", "R4-shall-should", "shall with advisory"),
        ("The coating should be applied.", None, "should alone is fine"),
        ("Our analysis shows convergence.", "R1-subject", "first-person 'our'"),
        ("The analysis converged at iteration 15.", None, "no first person"),
        ("Results are conservative.", "R5-unsupported-adj", "conservative without criterion"),
        ("Results are conservative because the load factor exceeds 1.5.", None, "conservative with because-clause"),
        ("The design is safe and adequate.", "R5-unsupported-adj", "multiple unsupported adjectives"),
        ("The value of 25.1 in is below the limit.", "R7-thickness-decimals", "inches with 1 decimal"),
        ("The value of 25.100 in meets requirements.", None, "inches with 3 decimals"),
    ]

    passed = 0
    failed = 0

    for text, expected_rule, desc in fixtures:
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
                print(f"  FAIL: '{desc}' — expected {expected_rule}, got {found_rules or 'none'}")
                failed += 1

    print(f"\nSelf-test: {passed} passed, {failed} failed out of {len(fixtures)}")
    return failed == 0


def check_file_text(text: str) -> list[tuple[int, str, str]]:
    """Check a single text string (for self-test)."""
    import tempfile

    with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
        f.write(text)
        f.flush()
        return check_file(Path(f.name))


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
    try:
        output = subprocess.check_output(
            ["git", "diff", "--name-only", "--diff-filter=ACM", base],
            text=True,
        )
    except subprocess.CalledProcessError:
        return []
    return [
        Path(f) for f in output.strip().splitlines()
        if f.endswith((".md", ".rst"))
    ]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*", default=["docs/", "src/"])
    parser.add_argument("--self-test", action="store_true", help="Run built-in test fixtures")
    parser.add_argument("--diff", metavar="BASE", help="Only scan files changed since BASE")
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
