#!/usr/bin/env python3
"""Detect digitalmodel test domains touched by a change set."""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


ROOT_RE = re.compile(r"`(tests/[^`]+)`")
FULL_MATRIX_PREFIXES = (
    "src/digitalmodel/",
)
FULL_MATRIX_PATHS = {
    ".claude/quality-gates.yaml",
    ".github/workflows/quality-gates-by-domain.yml",
    "tests/DOMAINS.md",
    "tests/conftest.py",
    "scripts/ci/detect_touched_domains.py",
    "pytest.ini",
    "pyproject.toml",
}


@dataclass(frozen=True)
class Domain:
    name: str
    roots: tuple[str, ...]
    runner: str = "ubuntu-latest"


def normalize_root(root: str) -> str:
    root = root.strip()
    return root[2:] if root.startswith("./") else root


def parse_domains(domains_file: Path) -> list[Domain]:
    domains: list[Domain] = []
    for raw_line in domains_file.read_text().splitlines():
        line = raw_line.strip()
        if not line.startswith("|") or "`tests/" not in line:
            continue

        cells = [cell.strip() for cell in line.strip("|").split("|")]
        if len(cells) < 2:
            continue

        domain = cells[0]
        roots = tuple(normalize_root(root) for root in ROOT_RE.findall(cells[1]))
        if domain.lower() == "domain" or not roots:
            continue
        domains.append(Domain(name=domain, roots=roots))

    if not domains:
        raise ValueError(f"No domains with backtick-wrapped tests/... roots found in {domains_file}")
    return domains


def git_changed_files(base: str, head: str) -> list[str]:
    result = subprocess.run(
        ["git", "diff", "--name-only", base, head],
        check=True,
        text=True,
        capture_output=True,
    )
    return [line.strip() for line in result.stdout.splitlines() if line.strip()]


def is_full_matrix_trigger(path: str) -> bool:
    normalized = path[2:] if path.startswith("./") else path
    return normalized in FULL_MATRIX_PATHS or any(
        normalized.startswith(prefix) for prefix in FULL_MATRIX_PREFIXES
    )


def path_matches_root(path: str, root: str) -> bool:
    normalized_path = path[2:] if path.startswith("./") else path
    normalized_root = root.rstrip("/")
    if root.endswith("/"):
        return normalized_path.startswith(root)
    return normalized_path == normalized_root


def touched_domains(changed_files: list[str], domains: list[Domain]) -> list[Domain]:
    if any(is_full_matrix_trigger(path) for path in changed_files):
        return domains

    selected: list[Domain] = []
    for domain in domains:
        if any(
            path_matches_root(path, root)
            for path in changed_files
            for root in domain.roots
        ):
            selected.append(domain)
    return selected


def matrix_for(domains: list[Domain]) -> dict[str, list[dict[str, str]]]:
    return {
        "include": [
            {"domain": domain.name, "runner": domain.runner}
            for domain in domains
        ]
    }


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--domains-file", type=Path, default=Path("tests/DOMAINS.md"))
    parser.add_argument("--mode", choices=("full", "touched"), required=True)
    parser.add_argument("--base")
    parser.add_argument("--head")
    parser.add_argument("--output-format", choices=("json-matrix", "list"), default="json-matrix")
    args = parser.parse_args(argv)
    if args.mode == "touched" and (not args.base or not args.head):
        parser.error("--mode touched requires --base and --head")
    return args


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv or sys.argv[1:])
    try:
        domains = parse_domains(args.domains_file)
        selected = (
            domains
            if args.mode == "full"
            else touched_domains(git_changed_files(args.base, args.head), domains)
        )
    except Exception as exc:
        print(f"detect_touched_domains.py: {exc}", file=sys.stderr)
        return 2

    if args.output_format == "json-matrix":
        print(json.dumps(matrix_for(selected), separators=(",", ":")))
    else:
        for domain in selected:
            print(domain.name)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
