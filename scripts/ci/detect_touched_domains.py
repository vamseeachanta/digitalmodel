#!/usr/bin/env python3
"""Detect digitalmodel test domains touched by a change set."""

from __future__ import annotations

import argparse
import copy
import json
import re
import subprocess
import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path


ROOT_RE = re.compile(r"`(tests/[^`]+)`")
PYPROJECT_PATH = "pyproject.toml"
FULL_MATRIX_PREFIXES = ("src/digitalmodel/",)
FULL_MATRIX_PATHS = {
    ".claude/quality-gates.yaml",
    "tests/DOMAINS.md",
    "tests/conftest.py",
    "pytest.ini",
    PYPROJECT_PATH,
}
DOMAIN_PATHS: tuple[tuple[str, tuple[str, ...]], ...] = (
    ("src/digitalmodel/cathodic_protection/", ("cathodic-protection",)),
    ("src/digitalmodel/citations/", ("citations",)),
    (
        "src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py",
        ("cathodic-protection",),
    ),
    ("src/digitalmodel/orcaflex/", ("orcaflex",)),
    ("src/digitalmodel/visualization/agent_dashboard.py", ("workflows",)),
    ("src/digitalmodel/workflows/", ("workflows",)),
    ("tests/benchmarks/test_cp_benchmarks.py", ("cathodic-protection",)),
    (
        "tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py",
        ("cathodic-protection",),
    ),
    ("tests/orcaflex/test_mooring_design_citations.py", ("citations",)),
    ("tests/specialized/cathodic_protection/", ("cathodic-protection",)),
)
PACKAGE_DATA_DOMAIN_PATHS: tuple[tuple[str, tuple[str, ...]], ...] = (
    ("orcaflex/", ("orcaflex",)),
    ("subsea/", ("subsea",)),
    ("naval_architecture/", ("naval-architecture",)),
)
NO_DOMAIN_PATHS = (
    # These paths are covered by CI harness tests or repo-wide
    # quality gates instead of the domain matrix.
    ".github/workflows/quality-gates.yml",
    ".github/workflows/quality-gates-by-domain.yml",
    "scripts/ci/detect_touched_domains.py",
    "src/digitalmodel/workflows/automation/quality_gates.py",
    "tests/scripts/test_detect_touched_domains.py",
    "tests/workflows/automation/test_quality_gates.py",
    "tests/marine_ops/marine_engineering/visualization/test_no_regression_traces.py",
)


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
        raise ValueError(
            f"No domains with backtick-wrapped tests/... roots found in {domains_file}"
        )
    return domains


def git_changed_files(base: str, head: str) -> list[str]:
    result = subprocess.run(
        ["git", "diff", "--name-only", base, head],
        check=True,
        text=True,
        capture_output=True,
    )
    return [line.strip() for line in result.stdout.splitlines() if line.strip()]


def git_file_text(ref: str, path: str) -> str:
    result = subprocess.run(
        ["git", "show", f"{ref}:{path}"],
        check=True,
        text=True,
        capture_output=True,
    )
    return result.stdout


def is_full_matrix_trigger(path: str) -> bool:
    normalized = path[2:] if path.startswith("./") else path
    return normalized in FULL_MATRIX_PATHS or any(
        normalized.startswith(prefix) for prefix in FULL_MATRIX_PREFIXES
    )


def package_data_domains_for_item(item: str) -> set[str]:
    names: set[str] = set()
    normalized = item[2:] if item.startswith("./") else item
    for package_path, domain_names in PACKAGE_DATA_DOMAIN_PATHS:
        if normalized.startswith(package_path):
            names.update(domain_names)
    return names


def without_setuptools_package_data(payload: dict) -> dict:
    comparable = copy.deepcopy(payload)
    tool_config = comparable.get("tool")
    if not isinstance(tool_config, dict):
        return comparable

    setuptools_config = tool_config.get("setuptools")
    if isinstance(setuptools_config, dict):
        setuptools_config.pop("package-data", None)
    return comparable


def digitalmodel_package_data(payload: dict) -> set[str]:
    tool_config = payload.get("tool")
    if not isinstance(tool_config, dict):
        return set()
    setuptools_config = tool_config.get("setuptools")
    if not isinstance(setuptools_config, dict):
        return set()
    package_data = setuptools_config.get("package-data")
    if not isinstance(package_data, dict):
        return set()

    entries = package_data.get("digitalmodel", [])
    if isinstance(entries, str):
        entries = [entries]
    if not isinstance(entries, list):
        return set()
    return {str(entry) for entry in entries}


def scoped_pyproject_domains(base: str, head: str) -> set[str] | None:
    base_payload = tomllib.loads(git_file_text(base, PYPROJECT_PATH))
    head_payload = tomllib.loads(git_file_text(head, PYPROJECT_PATH))

    if without_setuptools_package_data(base_payload) != without_setuptools_package_data(
        head_payload
    ):
        return None

    changed_items = digitalmodel_package_data(base_payload) ^ digitalmodel_package_data(
        head_payload
    )
    domain_names: set[str] = set()
    for item in changed_items:
        item_domain_names = package_data_domains_for_item(item)
        if not item_domain_names:
            return None
        domain_names.update(item_domain_names)
    return domain_names


def mapped_config_domain_names(
    path: str, base: str | None, head: str | None
) -> set[str] | None:
    normalized = path[2:] if path.startswith("./") else path
    if normalized != PYPROJECT_PATH or not base or not head:
        return None
    return scoped_pyproject_domains(base, head)


def mapped_domain_names(path: str) -> set[str]:
    names: set[str] = set()
    for domain_path, domain_names in DOMAIN_PATHS:
        if path_matches_root(path, domain_path):
            names.update(domain_names)
    return names


def is_no_domain_path(path: str) -> bool:
    return any(
        path_matches_root(path, ignored_path) for ignored_path in NO_DOMAIN_PATHS
    )


def path_matches_root(path: str, root: str) -> bool:
    normalized_path = path[2:] if path.startswith("./") else path
    normalized_root = root.rstrip("/")
    if root.endswith("/"):
        return normalized_path.startswith(root)
    return normalized_path == normalized_root


def touched_domains(
    changed_files: list[str],
    domains: list[Domain],
    base: str | None = None,
    head: str | None = None,
) -> list[Domain]:
    selected_names: set[str] = set()
    for path in changed_files:
        if is_no_domain_path(path):
            continue

        config_domain_names = mapped_config_domain_names(path, base, head)
        if config_domain_names is not None:
            selected_names.update(config_domain_names)
            continue

        domain_names = mapped_domain_names(path)
        if domain_names:
            selected_names.update(domain_names)
            continue
        if is_full_matrix_trigger(path):
            return domains

        for domain in domains:
            if any(path_matches_root(path, root) for root in domain.roots):
                selected_names.add(domain.name)

    return [domain for domain in domains if domain.name in selected_names]


def matrix_for(domains: list[Domain]) -> dict[str, list[dict[str, str]]]:
    return {
        "include": [
            {"domain": domain.name, "runner": domain.runner} for domain in domains
        ]
    }


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--domains-file", type=Path, default=Path("tests/DOMAINS.md"))
    parser.add_argument("--mode", choices=("full", "touched"), required=True)
    parser.add_argument("--base")
    parser.add_argument("--head")
    parser.add_argument(
        "--output-format", choices=("json-matrix", "list"), default="json-matrix"
    )
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
            else touched_domains(
                git_changed_files(args.base, args.head),
                domains,
                args.base,
                args.head,
            )
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
