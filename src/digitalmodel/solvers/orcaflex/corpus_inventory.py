"""Deterministic, licence-free inventory of OrcaFlex corpus candidates.

Run this file directly; only PyYAML and the standard library are required.
No solver binding is imported. Counts describe local named YAML records, not
resolved model objects, schema coverage, or successful solver loading.
"""

from __future__ import annotations

import argparse
import fnmatch
import hashlib
import json
import os
import re
from pathlib import Path

import yaml
from yaml.nodes import MappingNode, Node, ScalarNode, SequenceNode

# Issue #2078, Phase 1 and Notes: inspect a 64-byte window; the binary
# signature can follow a length prefix. This is candidate detection only.
HEADER_BYTES = 64
SUFFIXES = {".yml", ".yaml", ".dat", ".sim"}
VERSION = re.compile(r"^#\s*Program:\s*OrcaFlex\s+(\d+\.\d+[a-z]?)\s*$", re.M)
DEPENDENCIES = {"BaseFile", "IncludeFile", "RestartingFrom"}
SINGLETONS = {"General", "Environment"}
SAFE_TAGS = {
    "str",
    "null",
    "bool",
    "int",
    "float",
    "timestamp",
    "binary",
    "seq",
    "map",
    "set",
    "omap",
    "pairs",
    "merge",
}


def _node_issues(node: Node, active: set[int], visited: set[int]) -> set[str]:
    """Inspect syntax without evaluating tags, expressions, or dependencies."""
    if id(node) in active:
        return {"recursive_alias"}
    if id(node) in visited:
        return set()
    visited.add(id(node))
    active = active | {id(node)}
    issues: set[str] = set()
    if node.tag not in {"tag:yaml.org,2002:" + tag for tag in SAFE_TAGS}:
        issues.add("unsupported_tag")
    if isinstance(node, MappingNode):
        keys = set()
        for key, value in node.value:
            if not isinstance(key, ScalarNode):
                issues.add("unsupported_key")
            else:
                if key.value in keys:
                    issues.add("duplicate_key")
                keys.add(key.value)
                if key.value in DEPENDENCIES:
                    issues.add("unresolved_dependency")
                if key.value == "<<":
                    issues.add("unsupported_merge")
            issues.update(_node_issues(key, active, visited))
            issues.update(_node_issues(value, active, visited))
    elif isinstance(node, SequenceNode):
        for value in node.value:
            issues.update(_node_issues(value, active, visited))
    return issues


def _named_record(node: Node) -> bool:
    """Recognise the named-list form in Orcina's Text data files help.

    Source: Text data files, Elements of a text data file, online edition
    retrieved 2026-10-19. The Lines examples define each record with Name.
    """
    return isinstance(node, MappingNode) and any(
        isinstance(key, ScalarNode)
        and key.value == "Name"
        and isinstance(value, ScalarNode)
        and bool(value.value)
        for key, value in node.value
    )


def _inspect_yaml(text: str, row: dict) -> dict:
    """Count only unambiguous local named lists; expose everything else."""
    row["format"] = "yaml"
    versions = set(VERSION.findall(text))
    row["version"] = next(iter(versions)) if len(versions) == 1 else None
    try:
        documents = list(yaml.compose_all(text, Loader=yaml.SafeLoader))
    except RecursionError:
        row.update(status="NOT_EVALUATED", issues=["excessive_nesting"])
        return row
    except yaml.YAMLError:
        row.update(status="NOT_EVALUATED", issues=["invalid_yaml"])
        return row
    # Orcina's help says multiple documents share one model. Applying update
    # semantics is outside this local census; never collapse them with dict.update.
    if len(documents) > 1:
        row.update(status="NOT_EVALUATED", issues=["multiple_documents"])
        return row
    if not documents or not isinstance(documents[0], MappingNode):
        return row
    document = documents[0]
    try:
        issues = _node_issues(document, set(), set())
    except RecursionError:
        row.update(status="NOT_EVALUATED", issues=["excessive_nesting"])
        return row
    if len(versions) > 1:
        issues.add("conflicting_versions")
    if issues - {"unresolved_dependency", "conflicting_versions"}:
        row.update(status="NOT_EVALUATED", issues=sorted(issues))
        return row
    counts: dict[str, int] = {}
    unclassified = []
    recognised = False
    for key, value in document.value:
        name = key.value
        if (
            isinstance(value, SequenceNode)
            and value.value
            and all(_named_record(item) for item in value.value)
        ):
            counts[name] = len(value.value)
            recognised = True
        elif name in SINGLETONS and isinstance(value, MappingNode):
            recognised = True
        elif name not in DEPENDENCIES:
            unclassified.append(name)
    row.update(
        object_counts=counts,
        unclassified_sections=sorted(unclassified),
        issues=sorted(issues),
        status="NOT_EVALUATED"
        if issues
        else ("local_census" if recognised else "not_recognised"),
    )
    return row


def inspect_file(path: Path, root: Path) -> dict:
    """Fingerprint a candidate and inspect syntax without opening dependencies.

    Paths in output are root-relative. Raw header comments, object names,
    parameter values, exception text and absolute paths are never copied.
    Binary versions and object counts remain unknown until conversion.
    """
    row: dict = {
        "path": path.relative_to(root).as_posix(),
        "sha256": None,
        "format": "unknown",
        "version": None,
        "status": "not_recognised",
        "object_counts": None,
        "unclassified_sections": [],
        "issues": [],
        "round_trip": "NOT_EVALUATED",
    }
    if path.is_symlink():
        row.update(status="NOT_EVALUATED", issues=["symlink_not_followed"])
        return row
    try:
        with path.open("rb") as stream:
            prefix = stream.read(HEADER_BYTES)
            stream.seek(0)
            row["sha256"] = hashlib.file_digest(stream, "sha256").hexdigest()
            # Issue #2078: the marker starts at byte offset one. Matching that
            # offset also avoids mistaking a text Program comment for binary.
            if path.suffix.lower() in {".dat", ".sim"} and prefix[1:].startswith(
                b"OrcaFlex"
            ):
                row.update(
                    format="binary",
                    status="signature_candidate",
                    issues=["binary_requires_conversion"],
                )
                return row
            stream.seek(0)
            text = stream.read().decode("utf-8-sig")
    except UnicodeDecodeError:
        row["issues"] = ["non_utf8_without_signature"]
        return row
    except OSError:
        row.update(status="NOT_EVALUATED", issues=["read_error"])
        return row
    return _inspect_yaml(text, row)


def _select_paths(directory: Path, parts: tuple[str, ...]) -> set[Path]:
    """Expand a relative glob without suppressing directory access failures."""
    if not parts:
        return set()
    head, *tail = parts
    remaining = tuple(tail)
    paths = _select_paths(directory, remaining) if head == "**" else set()
    # Path.glob suppresses permission errors, including for explicit selections.
    # scandir exposes them so the caller fails before writing a coverage manifest.
    with os.scandir(directory) as entries:
        for entry in entries:
            path = directory / entry.name
            is_directory = entry.is_dir(follow_symlinks=False)
            if head == "**":
                if is_directory:
                    paths.update(_select_paths(path, parts))
            elif fnmatch.fnmatchcase(entry.name, head):
                if remaining:
                    if is_directory:
                        paths.update(_select_paths(path, remaining))
                elif entry.is_file(follow_symlinks=False) or entry.is_symlink():
                    paths.add(path)
    return paths


def build_inventory(root: Path, patterns: list[str] | None = None) -> list[dict]:
    """Select sorted candidates; traversal errors abort before any output."""
    root = root.resolve(strict=True)
    patterns = patterns or ["**/*"]
    if any(
        Path(pattern).is_absolute() or ".." in Path(pattern).parts
        for pattern in patterns
    ):
        raise ValueError("Selection patterns must stay relative to the root")
    paths = {
        path
        for pattern in patterns
        for path in _select_paths(root, Path(pattern).parts)
        if path.suffix.lower() in SUFFIXES
    }
    return [inspect_file(path, root) for path in sorted(paths)]


def write_inventory(rows: list[dict], output: Path) -> None:
    """Write a queryable JSON Lines table and fingerprint its exact bytes."""
    output.mkdir(parents=True, exist_ok=True)
    rows = sorted(rows, key=lambda row: row["path"])
    table = "".join(json.dumps(row, sort_keys=True) + "\n" for row in rows).encode()
    (output / "inventory.jsonl").write_bytes(table)
    manifest = {
        "generator": "src/digitalmodel/solvers/orcaflex/corpus_inventory.py",
        "table": "inventory.jsonl",
        "table_sha256": hashlib.sha256(table).hexdigest(),
        "files": len(rows),
        "scope": "selected local files; dependencies unresolved; no solver loading",
    }
    (output / "manifest.json").write_bytes(
        (json.dumps(manifest, indent=2, sort_keys=True) + "\n").encode("utf-8")
    )


def main(argv: list[str] | None = None) -> int:
    """Inventory a directory; the caller chooses an explicit output location."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("root", type=Path)
    parser.add_argument(
        "--select", action="append", help="Root-relative glob; repeatable"
    )
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args(argv)
    write_inventory(build_inventory(args.root, args.select), args.output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
