# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generate the advisory CI family inventory from tracked Python files."""

from __future__ import annotations

import argparse
import ast
import hashlib
import io
import json
import os
from pathlib import Path, PurePosixPath
import subprocess
import sys
import tokenize

RULES_PATH = "config/ci/module-family-rules.json"
OUTPUT_PATH = "config/ci/module-families.json"


def unique_object(pairs):
    """Reject duplicate JSON keys rather than silently replacing assignments."""
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"Duplicate assignment: {key}")
        result[key] = value
    return result


def validate_rules(rules):
    """Reject unknown families and malformed component prefixes."""
    families = set(rules["families"])
    if rules["schema_version"] != 1 or len(families) != len(rules["families"]):
        raise ValueError("Invalid rules schema or duplicate family")
    values = [rules["default_family"], *rules["module_prefix_to_family"].values(),
              *rules["test_path_to_family"].values()]
    if not set(values) <= families or "ENGINE" not in families:
        raise ValueError("Unknown family")
    for module in [*rules["module_prefix_to_family"], *rules["engine_exact_modules"]]:
        if module != "digitalmodel" and not module.startswith("digitalmodel."):
            raise ValueError("Invalid module prefix")
    for path in rules["test_path_to_family"]:
        if path.startswith("/") or ".." in path.split("/") or "\\" in path:
            raise ValueError("Invalid test path prefix")


def load_rules(root):
    """Read the tracked classification rules, independent of the snapshot."""
    rules = json.loads((root / RULES_PATH).read_text(encoding="utf-8"),
                       object_pairs_hook=unique_object)
    validate_rules(rules)
    return rules


def git_read(root, *args, input=None):
    """Read Git metadata without changing the index or traversing directories."""
    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    return subprocess.run(["git", "-C", root.as_posix(), *args], check=True,
                          capture_output=True, input=input, env=env).stdout


def read_inventory(root, ref=None):
    """Read regular tracked files from the worktree or a named Git tree.

    Symlinks and submodules are excluded. Untracked environments and nested
    checkouts never enter the inventory. A deleted worktree file is omitted.
    Tree blobs are read in a single batch, without exporting another checkout.
    """
    root = root.resolve()
    actual = Path(git_read(root, "rev-parse", "--show-toplevel").decode("utf-8").strip())
    if actual.resolve() != root:
        raise ValueError("Requested root must match the repository root")
    args = ("ls-tree", "-r", "-z", ref) if ref else ("ls-files", "--stage", "-z")
    entries = {}
    for record in git_read(root, *args).split(b"\0"):
        if not record:
            continue
        metadata, path_bytes = record.split(b"\t", 1)
        mode, kind_or_oid, oid_or_stage = metadata.decode().split()
        if not ref and oid_or_stage != "0":
            raise ValueError("Resolve unmerged index entries before generating")
        path = path_bytes.decode("utf-8")
        if mode not in {"100644", "100755"} or not path.endswith(".py"):
            continue
        if not ref and not (root / path).is_file():
            continue
        entries[path] = oid_or_stage if ref else None
    sources = sorted(path for path in entries if path.startswith("src/digitalmodel/"))
    contents = {}
    if ref and sources:
        objects = "\n".join(entries[path] for path in sources) + "\n"
        stream = io.BytesIO(git_read(root, "cat-file", "--batch",
                                    input=objects.encode("utf-8")))
        for path in sources:
            _, kind, size = stream.readline().split()
            if kind != b"blob":
                raise ValueError("Expected a regular source blob")
            contents[path] = stream.read(int(size))
            if stream.read(1) != b"\n":
                raise ValueError("Invalid Git batch response")
    else:
        contents = {path: (root / path).read_bytes() for path in sources}
    decoded = {}
    for path, content in contents.items():
        encoding, _ = tokenize.detect_encoding(io.BytesIO(content).readline)
        decoded[path] = content.decode(encoding)
    tests = sorted(path for path in entries if (
        PurePosixPath(path).name.startswith("test_") or path.endswith("_test.py")
    ))
    return decoded, tests


def module_for_path(path):
    """Translate a source path or package directory to its import name."""
    parts = PurePosixPath(path).relative_to("src").with_suffix("").parts
    if parts[-1] == "__init__":
        parts = parts[:-1]
    return ".".join(parts)


def source_modules(sources):
    """Include every source module and its implicit namespace parents."""
    result = set()
    for path in sources:
        parts = module_for_path(path).split(".")
        result.update(".".join(parts[:i]) for i in range(1, len(parts) + 1))
    return result


def module_family(module, rules):
    """Apply exact root decisions before the longest dotted prefix."""
    if module in rules["engine_exact_modules"]:
        return "ENGINE"
    matches = [prefix for prefix in rules["module_prefix_to_family"]
               if module == prefix or module.startswith(prefix + ".")]
    return (rules["module_prefix_to_family"][max(matches, key=len)]
            if matches else rules["default_family"])


def test_family(path, rules):
    """Apply exact file exceptions or the longest directory prefix."""
    matches = [prefix for prefix in rules["test_path_to_family"]
               if path == prefix or prefix.endswith("/") and path.startswith(prefix)]
    return (rules["test_path_to_family"][max(matches, key=len)]
            if matches else rules["default_family"])


def engine_dependencies(sources, homes, families):
    """Measure absolute AST imports, including bare root and from-imports.

    An imported symbol resolves to the longest inventoried module. Relative,
    dynamic and string-addressed imports are outside this measurement.
    """
    result = {family: set() for family in families if family != "ENGINE"}
    for path, text in sorted(sources.items()):
        family = homes[module_for_path(path)]
        if family == "ENGINE":
            continue
        for node in ast.walk(ast.parse(text, filename=path)):
            if isinstance(node, ast.Import):
                imports = [alias.name for alias in node.names]
            elif isinstance(node, ast.ImportFrom) and not node.level and node.module:
                imports = [node.module + "." + alias.name for alias in node.names]
            else:
                continue
            for name in imports:
                if not (name == "digitalmodel" or name.startswith("digitalmodel.")):
                    continue
                while name and name not in homes:
                    name = name.rpartition(".")[0]
                if homes.get(name) == "ENGINE":
                    result[family].add(name)
    return {family: sorted(edges) for family, edges in sorted(result.items())}


def build_contract(sources, tests, rules):
    """Build a reproducible snapshot from rules, source text and test paths."""
    validate_rules(rules)
    modules = {module: module_family(module, rules)
               for module in sorted(source_modules(sources))}
    digest_input = {"rules": rules, "sources": sources, "tests": sorted(tests)}
    digest = hashlib.sha256(json.dumps(digest_input, sort_keys=True).encode()).hexdigest()
    return {
        "schema_version": 2,
        "description": "Generated advisory CI family inventory; no CI selection change.",
        "coverage_mode": "report-only",
        "provenance": {
            "generator": "scripts/ci/generate_module_families.py",
            "rules": RULES_PATH,
            "input_sha256": digest,
            "dependency_measurement": "absolute AST imports; longest inventoried module",
        },
        "families": sorted(rules["families"]),
        "engine": {
            "status": "proposed; maintainer confirmation required",
            "boundary_rule": "Exact root facade; longest dotted prefix for other modules.",
            "boundaries": sorted(set(rules["engine_exact_modules"]) | {
                module for module, family in rules["module_prefix_to_family"].items()
                if family == "ENGINE"
            }),
            "modules": [module for module, family in modules.items() if family == "ENGINE"],
        },
        "module_to_family": modules,
        "family_to_engine_deps": engine_dependencies(sources, modules, rules["families"]),
        "test_home_rule": (
            "Each tracked regular test_*.py or *_test.py file has one home. "
            "Node IDs inherit the file home, including excluded files. Git symlinks, "
            "submodules and untracked environments or checkouts are not traversed. "
            "New paths inherit the longest prefix or the platform default; "
            "snapshot freshness is report-only and does not select CI jobs."
        ),
        "test_file_to_family": {path: test_family(path, rules) for path in sorted(tests)},
    }


def render(contract):
    """Serialize deterministically without machine-specific metadata."""
    return json.dumps(contract, indent=2) + "\n"


def main(argv=None):
    """Write, check or report snapshot drift without running application code."""
    parser = argparse.ArgumentParser(description=__doc__)
    modes = parser.add_mutually_exclusive_group(required=True)
    modes.add_argument("--write", action="store_true")
    modes.add_argument("--check", action="store_true")
    modes.add_argument("--report", action="store_true")
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--ref", help="Read a Git tree instead of worktree source files")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args(argv)
    rules = load_rules(args.root)
    contract = build_contract(*read_inventory(args.root, args.ref), rules)
    output = args.output or args.root / OUTPUT_PATH
    rendered = render(contract)
    fresh = output.is_file() and output.read_text(encoding="utf-8") == rendered
    if args.write:
        output.write_text(rendered, encoding="utf-8", newline="\n")
    print(f"Family inventory: {len(contract['module_to_family'])} modules, "
          f"{len(contract['test_file_to_family'])} test files, "
          f"{len(contract['engine']['modules'])} ENGINE modules; "
          + ("written" if args.write else "current" if fresh else "out of date"))
    return int(args.check and not fresh)


if __name__ == "__main__":
    sys.exit(main())
