# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generate a proposed family inventory without importing application code.

The default command reports drift and succeeds. --check explicitly checks the
snapshot; --write refreshes it. --ref reads paths and source blobs from a Git
revision, allowing a main snapshot to be refreshed without merging main.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import json
import subprocess
import warnings
from pathlib import Path, PurePosixPath
from typing import Any, Iterable, Mapping, cast

ROOT = Path(__file__).resolve().parents[2]
RULES = "config/ci/module-family-rules.json"
CONTRACT = "config/ci/module-families.json"


def unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    """Reject duplicate keys, including duplicate family assignments."""
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"Duplicate assignment: {key}")
        result[key] = value
    return result


def read_json(path: Path) -> dict[str, Any]:
    """Read JSON while rejecting duplicate keys."""
    data = json.loads(path.read_text(encoding="utf-8"), object_pairs_hook=unique_object)
    if not isinstance(data, dict):
        raise ValueError("Expected a JSON object")
    return data


def git(root: Path, *args: str, input_data: bytes | None = None) -> bytes:
    """Read repository metadata without modifying the index or worktree."""
    result: subprocess.CompletedProcess[bytes] = subprocess.run(
        ["git", *args], cwd=root, input=input_data, capture_output=True, check=True
    )
    return result.stdout


def tracked_paths(root: Path) -> list[str]:
    """Use only index paths; local environments and linked checkouts are absent."""
    return sorted(
        set(git(root, "ls-files", "--cached", "-z").decode().split("\0")) - {""}
    )


def module_for_path(path: str) -> str:
    """Convert a source path to its importable module or package name."""
    parts = PurePosixPath(path).relative_to("src").with_suffix("").parts
    if parts[-1] == "__init__":
        parts = parts[:-1]
    return ".".join(parts)


def is_source(path: str) -> bool:
    """Identify application Python files, including package initializers."""
    return path.startswith("src/digitalmodel/") and path.endswith(".py")


def source_modules(paths: Iterable[str]) -> set[str]:
    """Inventory modules and implicit namespace parents from tracked paths."""
    modules: set[str] = set()
    for path in paths:
        if is_source(path):
            parts = module_for_path(path).split(".")
            modules.update(".".join(parts[:i]) for i in range(1, len(parts) + 1))
    return modules


def test_files(paths: Iterable[str]) -> set[str]:
    """Inventory pytest filename patterns everywhere, including excluded tests."""
    return {
        path
        for path in paths
        if path.endswith(".py")
        and (PurePosixPath(path).name.startswith("test_") or path.endswith("_test.py"))
    }


def inventory(
    root: Path, ref: str | None = None
) -> tuple[list[str], dict[str, bytes], str]:
    """Read indexed working files or an immutable revision with batched blobs."""
    if ref is None:
        paths = [path for path in tracked_paths(root) if (root / path).is_file()]
        if any((root / path).is_symlink() for path in paths if is_source(path)):
            raise ValueError("Source symlink cannot be measured")
        sources = {
            path: (root / path).read_bytes() for path in paths if is_source(path)
        }
        return paths, sources, "working-tree"
    revision = git(root, "rev-parse", "--verify", ref + "^{commit}").decode().strip()
    entries = git(root, "ls-tree", "-rz", "--full-tree", revision).split(b"\0")
    paths, blobs = [], []
    for entry in entries:
        if not entry:
            continue
        metadata, name = entry.split(b"\t", 1)
        mode, kind, oid = metadata.split()
        if kind != b"blob":
            continue
        path = name.decode()
        paths.append(path)
        if is_source(path):
            if mode == b"120000":
                raise ValueError(f"Source symlink cannot be measured: {path}")
            blobs.append((path, oid))
    data = git(
        root,
        "cat-file",
        "--batch",
        input_data=b"".join(oid + b"\n" for _, oid in blobs),
    )
    sources, offset = {}, 0
    for path, oid in blobs:
        end = data.index(b"\n", offset)
        actual_oid, kind, size_bytes = data[offset:end].split()
        if actual_oid != oid or kind != b"blob":
            raise ValueError("Source blob mismatch")
        offset = end + 1
        size = int(size_bytes)
        sources[path] = data[offset : offset + size]
        offset += size + 1
    return sorted(paths), sources, revision


def validate_rules(rules: Mapping[str, Any]) -> None:
    """Reject unknown families and malformed rule sets before generation."""
    families = rules["families"]
    if rules["schema_version"] != 1 or len(families) != len(set(families)):
        raise ValueError("Invalid rules schema or duplicate families")
    allowed = set(families) - {"ENGINE"}
    for field in ("default_module_family", "default_test_family"):
        if rules[field] not in allowed:
            raise ValueError(f"Invalid family: {field}")
    if not set(rules["module_prefixes"].values()) <= allowed:
        raise ValueError("Unknown module family")
    if not set(rules["test_prefixes"].values()) <= set(families):
        raise ValueError("Unknown test family")
    boundaries = rules["engine_boundaries"]
    if len(boundaries) != len(set(boundaries)) or "digitalmodel" not in boundaries:
        raise ValueError("Invalid ENGINE boundaries")


def family_for_module(module: str, rules: Mapping[str, Any]) -> str:
    """Use explicit ENGINE boundaries, then the longest dotted module prefix."""
    if any(
        module == boundary
        or (boundary != "digitalmodel" and module.startswith(boundary + "."))
        for boundary in rules["engine_boundaries"]
    ):
        return "ENGINE"
    matches = [
        prefix
        for prefix in rules["module_prefixes"]
        if module == prefix or module.startswith(prefix + ".")
    ]
    return (
        cast(str, rules["module_prefixes"][max(matches, key=len)])
        if matches
        else cast(str, rules["default_module_family"])
    )


def family_for_test(path: str, rules: Mapping[str, Any]) -> str:
    """Apply the longest directory prefix or exact filename, then the default."""
    if is_source(path):
        return family_for_module(module_for_path(path), rules)
    matches = [
        prefix
        for prefix in rules["test_prefixes"]
        if path == prefix or (prefix.endswith("/") and path.startswith(prefix))
    ]
    return (
        cast(str, rules["test_prefixes"][max(matches, key=len)])
        if matches
        else cast(str, rules["default_test_family"])
    )


def measure_engine_dependencies(
    sources: Mapping[str, str | bytes], modules: Mapping[str, str]
) -> dict[str, list[str]]:
    """Measure absolute-import edges, including bare, local and guarded imports.

    Both the base of a from-import and any importable child are edges. Symbols
    are resolved to the longest known module. Relative and dynamic imports are
    outside this measurement; no application or solver module is imported.
    """
    dependencies: dict[str, set[str]] = {
        family: set() for family in set(modules.values()) - {"ENGINE"}
    }

    def target_module(name: str) -> str:
        while name and name not in modules:
            name = name.rpartition(".")[0]
        return name

    for path, source in sorted(sources.items()):
        family = modules[module_for_path(path)]
        if family == "ENGINE":
            continue
        try:
            # Parsing legacy string escapes can emit source snippets. Keep the
            # measurement static and report syntax errors by path only.
            with warnings.catch_warnings():
                warnings.simplefilter("ignore", SyntaxWarning)
                warnings.simplefilter("ignore", DeprecationWarning)
                tree = ast.parse(source, filename=path)
        except (SyntaxError, ValueError) as error:
            raise ValueError(f"Cannot parse source: {path}") from error
        for node in ast.walk(tree):
            names = []
            if isinstance(node, ast.Import):
                names = [alias.name for alias in node.names]
            elif isinstance(node, ast.ImportFrom) and node.level == 0 and node.module:
                names = [node.module] + [
                    node.module + "." + alias.name
                    for alias in node.names
                    if alias.name != "*"
                ]
            for name in names:
                target = target_module(name)
                if modules.get(target) == "ENGINE":
                    dependencies[family].add(target)
    return {family: sorted(targets) for family, targets in sorted(dependencies.items())}


def validate_engine_dependencies(
    declared: Mapping[str, list[str]],
    sources: Mapping[str, str | bytes],
    modules: Mapping[str, str],
) -> None:
    """Require exact measured dependencies, rejecting missing and extra edges."""
    measured = measure_engine_dependencies(sources, modules)
    measured.update({family: [] for family in declared if family not in measured})
    if declared != measured:
        raise ValueError("ENGINE dependencies differ from measured imports")


def digest(items: Mapping[str, bytes]) -> str:
    """Hash sorted, length-delimited input names and bytes reproducibly."""
    result = hashlib.sha256()
    for name, data in sorted(items.items()):
        for value in (name.encode(), data):
            result.update(str(len(value)).encode() + b":" + value)
    return result.hexdigest()


def build_contract(
    paths: list[str],
    sources: Mapping[str, str | bytes],
    rules: Mapping[str, Any],
    revision: str = "working-tree",
) -> dict[str, Any]:
    """Generate complete assignments and dependencies from declarative rules."""
    validate_rules(rules)
    modules = {
        module: family_for_module(module, rules)
        for module in sorted(source_modules(paths))
    }
    tests = {path: family_for_test(path, rules) for path in sorted(test_files(paths))}
    dependencies = measure_engine_dependencies(sources, modules)
    # Keep every declared family, including families absent in small fixtures.
    dependencies = {
        family: dependencies.get(family, [])
        for family in sorted(set(rules["families"]) - {"ENGINE"})
    }
    return {
        "schema_version": 1,
        "description": "Generated proposed CI inventory; coverage drift is report-only in pytest.",
        "provenance": {
            "generator": "scripts/ci/generate_module_families.py",
            "rules": RULES,
            "source_revision": revision,
            "rules_sha256": hashlib.sha256(
                json.dumps(rules, sort_keys=True).encode()
            ).hexdigest(),
            "paths_sha256": digest({path: b"" for path in paths}),
            "sources_sha256": digest(
                {
                    path: source.encode() if isinstance(source, str) else source
                    for path, source in sources.items()
                }
            ),
        },
        "families": rules["families"],
        "module_to_family": modules,
        "engine": {
            "status": "proposed; maintainer confirms",
            "boundary_rule": "Explicit module/package boundaries; the root facade is exact only.",
            "boundaries": sorted(rules["engine_boundaries"]),
            "modules": [
                module for module, family in modules.items() if family == "ENGINE"
            ],
        },
        "family_to_engine_deps": dependencies,
        "test_home_rule": (
            "Only Git-tracked test_*.py or *_test.py files are inventoried, including excluded tests. "
            "Node IDs inherit the file home. Local environments and linked checkouts are not walked. "
            "New files use the longest matching prefix or the declared default; coverage drift is "
            "report-only until family gates are activated. No CI selection is implied."
        ),
        "test_file_to_family": tests,
    }


def coverage_report(
    declared: Mapping[str, Any], generated: Mapping[str, Any]
) -> dict[str, Any]:
    """Describe snapshot drift without turning inventory additions into a gate."""
    report: dict[str, Any] = {}
    for field in ("module_to_family", "test_file_to_family"):
        old, new = declared[field], generated[field]
        report[field] = {
            "unassigned": sorted(set(new) - set(old)),
            "stale": sorted(set(old) - set(new)),
            "changed": sorted(
                key for key in old.keys() & new.keys() if old[key] != new[key]
            ),
        }
    report["engine_dependencies_changed"] = (
        declared["family_to_engine_deps"] != generated["family_to_engine_deps"]
    )
    return report


def main(argv: list[str] | None = None) -> int:
    """Report by default; write or strictly check a snapshot only on request."""
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--write", action="store_true")
    mode.add_argument("--check", action="store_true")
    parser.add_argument("--repo", type=Path, default=ROOT)
    parser.add_argument(
        "--ref", help="Read an immutable Git revision instead of working files"
    )
    args = parser.parse_args(argv)
    try:
        rules = read_json(args.repo / RULES)
        paths, sources, revision = inventory(args.repo, args.ref)
        generated = build_contract(paths, sources, rules, revision)
        encoded = json.dumps(generated, indent=2) + "\n"
        output = args.repo / CONTRACT
        if args.write:
            output.write_text(encoded, encoding="utf-8")
        elif args.check:
            if not output.exists() or output.read_text(encoding="utf-8") != encoded:
                print(
                    "Family snapshot differs; regenerate with --write and the same --ref."
                )
                return 1
        else:
            print(json.dumps(coverage_report(read_json(output), generated), indent=2))
        print(
            f"modules={len(generated['module_to_family'])} "
            f"tests={len(generated['test_file_to_family'])} "
            f"ENGINE={len(generated['engine']['modules'])}"
        )
        return 0
    except (OSError, ValueError, KeyError, subprocess.CalledProcessError):
        print(
            "Family generation failed: unreadable input, invalid rules or unparseable source."
        )
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
