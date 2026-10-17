#!/usr/bin/env python3
# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Detect digitalmodel test domains touched by a change set."""

from __future__ import annotations

import argparse
import copy
import json
import posixpath
import re
import subprocess
import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path

ROOT_RE = re.compile(r"`(tests/[^`]+)`")
SHA256_RE = re.compile(r"[0-9a-fA-F]{64}")
MAX_MANIFEST_BYTES = 5 * 1024 * 1024
PYPROJECT_PATH = "pyproject.toml"
FULL_MATRIX_PREFIXES = ("src/digitalmodel/",)
FULL_MATRIX_PATHS = {
    ".claude/quality-gates.yaml",
    "tests/DOMAINS.md",
    "tests/conftest.py",
    # The repository-root conftest.py carries collect_ignore, which decides what
    # a recursive collect can see at all. It belongs here for the same reason
    # pytest.ini does: an edit to either can add or remove tests everywhere, so
    # it must dispatch every shard. Omitting it would make the root exclusions
    # LESS CI-sensitive after #1977 moved them out of pytest.ini -- someone
    # adding "tests" to collect_ignore would otherwise dispatch nothing at all.
    "conftest.py",
    "pytest.ini",
    PYPROJECT_PATH,
}
DOMAIN_PATHS: tuple[tuple[str, tuple[str, ...]], ...] = (
    ("data/bha_database/", ("subsea",)),
    ("data/bha_standards/", ("subsea",)),
    ("scripts/bha_database/", ("subsea",)),
    ("docs/bha_database.md", ("subsea",)),
    # Domain records bind retained inputs and outputs checked by contracts tests.
    ("docs/domains/", ("contracts",)),
    ("docs/registry/", ("contracts", "workflows", "subsea", "asset-integrity")),
    ("docs/maps/", ("contracts",)),
    ("config/repo_structure.yml", ("contracts",)),
    ("scripts/maintenance/verify_repo_structure.py", ("contracts",)),
    ("src/digitalmodel/cathodic_protection/", ("cathodic-protection",)),
    ("src/digitalmodel/citations/", ("citations",)),
    (
        "src/digitalmodel/infrastructure/base_solvers/hydrodynamics/cathodic_protection.py",
        ("cathodic-protection",),
    ),
    ("src/digitalmodel/orcaflex/", ("orcaflex",)),
    ("src/digitalmodel/visualization/agent_dashboard.py", ("workflows",)),
    ("src/digitalmodel/workflows/", ("workflows",)),
    # The capabilities census SSOT and its builder live outside src/ and tests/,
    # so without these two entries an edit to the census would trigger no shard
    # at all -- the gap that let #1637 sit red for 15 days.
    ("docs/capability-map/", ("capabilities",)),
    ("scripts/capabilities/", ("capabilities",)),
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
    """Represent one test shard and its owned paths."""

    name: str
    roots: tuple[str, ...]
    runner: str = "ubuntu-latest"


def normalize_root(root: str) -> str:
    """Normalize a repository-relative root."""
    root = root.strip()
    return root[2:] if root.startswith("./") else root


def parse_domains(domains_file: Path) -> list[Domain]:
    """Read shard roots from the ownership table."""
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
    """List changed paths between two revisions."""
    result = subprocess.run(
        ["git", "diff", "--name-only", "--no-renames", "-z", base, head],
        check=True,
        text=True,
        capture_output=True,
    )
    return [path for path in result.stdout.split("\0") if path]


def git_file_text(ref: str, path: str) -> str:
    """Read a tracked text file at a revision."""
    result = subprocess.run(
        ["git", "show", f"{ref}:{path}"],
        check=True,
        text=True,
        capture_output=True,
    )
    return result.stdout


def extract_manifest_pins(
    payload: object, manifest_path: str, tracked_files: set[str]
) -> set[str]:
    """Find tracked paths used as JSON keys or path fields beside SHA-256 digests.

    Walk objects and lists at every depth. A digest may be the key's value or
    its value's ``sha256`` member. Resolve each key against the repository root
    and the manifest folder; retain both matches when ambiguous.
    """
    pins: set[str] = set()
    pending = [payload]
    while pending:
        item = pending.pop()
        if isinstance(item, list):
            pending.extend(item)
        elif isinstance(item, dict):
            entries = list(item.items())
            if isinstance(item.get("path"), str):
                entries.append((item["path"], item.get("sha256")))
            for key, value in entries:
                pending.append(value)
                digest = value.get("sha256") if isinstance(value, dict) else value
                if not isinstance(digest, str) or not SHA256_RE.fullmatch(digest):
                    continue
                for candidate in (
                    posixpath.normpath(key),
                    posixpath.normpath(posixpath.join(posixpath.dirname(manifest_path), key)),
                ):
                    if candidate in tracked_files:
                        pins.add(candidate)
    return pins


def git_record_pins(ref: str) -> dict[str, set[str]]:
    """Read record pins with one tree listing and one read per manifest.

    Manifest.json, basis.json and *.manifest.json are inspected without regard
    to case. Invalid JSON and files over 5 MB are skipped with a notice. Git
    errors propagate so the caller can select every shard. Blob sizes are
    checked before reading their contents.
    """
    result = subprocess.run(
        ["git", "ls-tree", "-r", "--name-only", "-z", ref],
        check=True,
        text=True,
        capture_output=True,
    )
    tracked = set(result.stdout.split("\0")) - {""}
    records: dict[str, set[str]] = {}
    for path in sorted(tracked):
        if posixpath.basename(path).lower() not in {"manifest.json", "basis.json"} and not path.lower().endswith(
            ".manifest.json"
        ):
            continue
        size = subprocess.run(
            ["git", "cat-file", "-s", f"{ref}:{path}"],
            check=True, text=True, capture_output=True,
        )
        if int(size.stdout) > MAX_MANIFEST_BYTES:
            print(f"Skipping record manifest {path!r}: exceeds 5 MB", file=sys.stderr)
            continue
        try:
            text = git_file_text(ref, path)
        except UnicodeError:
            print(f"Skipping record manifest {path!r}: invalid JSON encoding", file=sys.stderr)
            continue
        try:
            payload = json.loads(text)
        except (ValueError, RecursionError):
            print(f"Skipping record manifest {path!r}: invalid JSON", file=sys.stderr)
            continue
        pins = extract_manifest_pins(payload, path, tracked)
        if pins:
            records[path] = pins
    return records


def git_pinned_paths(ref: str) -> set[str]:
    """Read the union of retained record inputs and outputs at a revision."""
    return {pin for pins in git_record_pins(ref).values() for pin in pins}


def git_pinned_domains(ref: str, domains: list[Domain]) -> dict[str, set[str]]:
    """Route each record and its pins to contracts and its committed test readers.

    Discover readers by a fixed-string reference to the manifest folder under
    tests/. A root manifest uses its filename instead. Missing readers or readers
    without a known shard select every shard. Git read failures propagate.
    """
    all_names = {domain.name for domain in domains}
    routes: dict[str, set[str]] = {}
    for manifest, pins in git_record_pins(ref).items():
        folder = posixpath.dirname(manifest) or manifest
        result = subprocess.run(
            ["git", "grep", "-l", "-z", "-F", "-e", folder, ref, "--", "tests/"],
            check=False, text=True, capture_output=True,
        )
        if result.returncode not in (0, 1):
            raise subprocess.CalledProcessError(result.returncode, result.args)
        readers = [
            item.removeprefix(f"{ref}:") for item in result.stdout.split("\0")
            if item and (
                posixpath.basename(item).startswith("test_") and item.endswith(".py")
                or item.endswith("_test.py")
            )
        ]
        names = {"contracts"}
        for reader in readers:
            owners = mapped_domain_names(reader) or {
                domain.name for domain in domains for root in domain.roots
                if path_matches_root(reader, root)
            }
            names.update(owners or all_names)
        if not readers or not names <= all_names:
            names = all_names
        for path in pins | {manifest}:
            routes.setdefault(path, set()).update(names)
    return routes


def is_full_matrix_trigger(path: str) -> bool:
    """Identify changes that require every shard."""
    normalized = path[2:] if path.startswith("./") else path
    return normalized in FULL_MATRIX_PATHS or any(
        normalized.startswith(prefix) for prefix in FULL_MATRIX_PREFIXES
    )


def package_data_domains_for_item(item: str) -> set[str]:
    """Find owners of a package-data entry."""
    names: set[str] = set()
    normalized = item[2:] if item.startswith("./") else item
    for package_path, domain_names in PACKAGE_DATA_DOMAIN_PATHS:
        if normalized.startswith(package_path):
            names.update(domain_names)
    return names


def without_setuptools_package_data(payload: dict) -> dict:
    """Copy project configuration without package-data entries."""
    comparable = copy.deepcopy(payload)
    tool_config = comparable.get("tool")
    if not isinstance(tool_config, dict):
        return comparable

    setuptools_config = tool_config.get("setuptools")
    if isinstance(setuptools_config, dict):
        setuptools_config.pop("package-data", None)
    return comparable


def digitalmodel_package_data(payload: dict) -> set[str]:
    """Extract package-data patterns from project configuration."""
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
    """Resolve package-data-only changes or request the full matrix."""
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
    """Resolve configuration changes with revision-aware routing."""
    normalized = path[2:] if path.startswith("./") else path
    if normalized != PYPROJECT_PATH or not base or not head:
        return None
    return scoped_pyproject_domains(base, head)


def mapped_domain_names(path: str) -> set[str]:
    """Union the explicit routes matching a path."""
    names: set[str] = set()
    for domain_path, domain_names in DOMAIN_PATHS:
        if path_matches_root(path, domain_path):
            names.update(domain_names)
    return names


def is_no_domain_path(path: str) -> bool:
    """Identify paths covered outside the domain matrix."""
    return any(
        path_matches_root(path, ignored_path) for ignored_path in NO_DOMAIN_PATHS
    )


def path_matches_root(path: str, root: str) -> bool:
    """Match an exact file or a directory prefix."""
    normalized_path = path[2:] if path.startswith("./") else path
    normalized_root = root.rstrip("/")
    if root.endswith("/"):
        return normalized_path.startswith(root)
    return normalized_path == normalized_root


def package_document_domains(path: str, domains: list[Domain]) -> set[str] | None:
    """Resolve package documentation owners; use every shard if unresolved.

    Args:
        path: Repository-relative changed path.
        domains: Domains parsed from the test ownership table.

    Returns:
        Selected names, or None for paths outside package documentation.
    """
    normalized = normalize_root(path)
    parts = Path(normalized).parts
    if (not normalized.startswith("src/digitalmodel/")
            or "docs" not in parts[2:-1] or Path(normalized).suffix == ".py"):
        return None
    all_names = {domain.name for domain in domains}
    try:
        import yaml
    except ImportError:
        return all_names
    registry = Path(__file__).resolve().parents[2] / "docs/registry/module-routing.yaml"
    try:
        entries = yaml.safe_load(registry.read_text(encoding="utf-8"))["modules"]
        matches = [entry for entry in entries
                   if path_matches_root(normalized, entry["entry_point"])]
        entry = max(matches, key=lambda item: len(item["entry_point"]))
        key_tests = entry["key_tests"]
        if not isinstance(key_tests, list) or not key_tests:
            return all_names
        names = {"contracts"}
        for test_root in key_tests:
            if not isinstance(test_root, str) or test_root == "tests/":
                return all_names
            relative = normalized[len(entry["entry_point"]):].split("/docs/", 1)[0]
            candidate = test_root + relative + "/" if test_root.endswith("/") else test_root
            owners = mapped_domain_names(test_root) or {
                domain.name for domain in domains for root in domain.roots
                if path_matches_root(candidate, root)
            } or {
                domain.name for domain in domains for root in domain.roots
                if path_matches_root(test_root, root) or path_matches_root(root, test_root)
            }
            if not owners or not owners <= all_names:
                return all_names
            names.update(owners)
        return names if names <= all_names else all_names
    except (OSError, ValueError, KeyError, TypeError, yaml.YAMLError):
        return all_names


def touched_domains(
    changed_files: list[str],
    domains: list[Domain],
    base: str | None = None,
    head: str | None = None,
    pinned_paths: set[str] | None = None,
    pinned_domains: dict[str, set[str]] | None = None,
) -> list[Domain]:
    """Select the union of shards affected by changed paths."""
    selected_names: set[str] = set()
    for path in changed_files:
        if pinned_domains:
            selected_names.update(pinned_domains.get(normalize_root(path), set()))
        # A pin adds record tests even for paths otherwise handled outside the matrix.
        if pinned_paths and normalize_root(path) in pinned_paths:
            selected_names.add("contracts")
        if is_no_domain_path(path):
            continue

        documentation_names = package_document_domains(path, domains)
        if documentation_names is not None:
            selected_names.update(documentation_names)
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

    if "contracts" in selected_names and not any(d.name == "contracts" for d in domains):
        return domains
    return [domain for domain in domains if domain.name in selected_names]


def matrix_for(domains: list[Domain]) -> dict[str, list[dict[str, str]]]:
    """Build the hosted matrix payload."""
    return {
        "include": [
            {"domain": domain.name, "runner": domain.runner} for domain in domains
        ]
    }


def parse_args(argv: list[str]) -> argparse.Namespace:
    """Parse detector command-line arguments."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--domains-file", type=Path, default=Path("tests/DOMAINS.md"))
    parser.add_argument("--mode", choices=("full", "touched"), required=True)
    parser.add_argument("--base")
    parser.add_argument("--head")
    parser.add_argument(
        "--output-format", choices=("json-matrix", "list"), default="json-matrix"
    )
    parser.add_argument(
        "--on-missing-base",
        choices=("error", "full"),
        default="error",
        help=(
            "Behavior when the base SHA is unreachable (zero-SHA new branch or "
            "force-push pruned history). 'error' (default) hard-fails with exit 2; "
            "'full' fails SAFE to the full matrix."
        ),
    )
    args = parser.parse_args(argv)
    if args.mode == "touched" and (not args.base or not args.head):
        parser.error("--mode touched requires --base and --head")
    return args


def main(argv: list[str] | None = None) -> int:
    """Print selected shards in matrix or list form."""
    args = parse_args(argv or sys.argv[1:])
    try:
        domains = parse_domains(args.domains_file)
        if args.mode == "full":
            selected = domains
        else:
            try:
                changed = git_changed_files(args.base, args.head)
            except subprocess.CalledProcessError:
                # Base SHA unreachable: zero-SHA new branch or force-push pruned
                # history. Fail SAFE to the full matrix when asked; otherwise let
                # the catch-all below preserve the historical exit-2 behavior.
                if args.on_missing_base == "full":
                    selected = domains
                else:
                    raise
            else:
                try:
                    pin_domains: dict[str, set[str]] = {}
                    for ref in dict.fromkeys((args.base, args.head)):
                        for path, names in git_pinned_domains(ref, domains).items():
                            pin_domains.setdefault(path, set()).update(names)
                except (subprocess.CalledProcessError, OSError):
                    print("Could not read record pins; selecting full matrix", file=sys.stderr)
                    selected = domains
                else:
                    selected = touched_domains(
                        changed,
                        domains,
                        args.base,
                        args.head,
                        pinned_domains=pin_domains,
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
