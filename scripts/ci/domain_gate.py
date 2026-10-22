# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Render shared CI commands with reviewed, file-specific PR deferrals."""

from __future__ import annotations

import argparse
import ast
import configparser
import json
import re
import subprocess
from fnmatch import fnmatch
from pathlib import Path

import yaml

POLICY = "config/ci-domain-exceptions.json"
# These are the new roots in the four failing shards, never whole shards.
INITIAL_HELD_ROOTS = {
    "tests-marine-ops-other": (
        "tests/production_chemistry/", "tests/marine_ops/vessel_db/",
        "tests/pipelay/", "tests/tug/", "tests/well_access/",
    ),
    "tests-solver-smoke": (
        "tests/solvers/ghs/", "tests/parametric/", "tests/parametrics/",
        "tests/solvers/benchmark/", "tests/solvers/mystran/", "tests/test_usecase_registry.py",
    ),
    "tests-workflows": (
        "tests/governance/", "tests/legal/", "tests/maintenance/", "tests/workflow_api/",
    ),
    "tests-misc": (
        "tests/test_engine_embed_root.py", "tests/test_main_exit_status.py",
        "tests/test_main_module_routing.py",
    ),
}
ADDITIONAL_ROOTS = {
    "tests-infrastructure-core": ("tests/data/",),
    "tests-structural": (
        "tests/test_steel_shapes.py", "tests/test_steel_shapes_import.py",
        "tests/test_fetch_steel_shapes.py",
    ),
    "tests-hydrodynamics-other": (
        "tests/hydrodynamics/test_quad_mesh_quality.py",
        "tests/hydrodynamics/test_shear_flow_values.py",
    ),
    "tests-marine-ops-other": ("tests/marine_ops/test_response_benchmark.py",),
}
OWNERSHIP_IGNORES = {
    "tests-infrastructure-core": ("tests/benchmarks/test_cp_benchmarks.py",),
    "tests-marine-engineering": (
        "tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py",
    ),
}


def test_files(root: Path) -> set[str]:
    """Return candidate pytest files, including collection-excluded files."""
    return {
        path.relative_to(root).as_posix() for path in (root / "tests").rglob("*.py")
        if fnmatch(path.name, "test_*.py") or fnmatch(path.name, "*_test.py")
    }


def initial_hold_files(root: Path) -> set[str]:
    """Bound the initial deferral to explicitly reviewed additions."""
    return {
        path for path in test_files(root)
        if any(path == item or path.startswith(item) for roots in INITIAL_HELD_ROOTS.values() for item in roots)
    }


def validate_shrink_only(current: dict, trusted: dict) -> None:
    """Reject new held files or changed ownership relative to a trusted revision."""
    if not set(current["held"]) <= set(trusted["held"]):
        raise ValueError("The reviewed deferral registry must not grow")
    for path, entry in current["held"].items():
        if entry.get("gate") != trusted["held"][path].get("gate"):
            raise ValueError(f"A deferral cannot change ownership: {path}")


def read_policy(root: Path) -> dict:
    """Read a narrow exception registry; never accept directories or wildcards."""
    policy = json.loads((root / POLICY).read_text())
    if set(policy) != {"held", "collection"}:
        raise ValueError("Expected held and collection exception maps")
    for entries in policy.values():
        for path, entry in entries.items():
            if (not path.startswith("tests/") or ".." in Path(path).parts
                    or any(char in path for char in "*?[\\") or not path.endswith(".py")
                    or not (root / path).is_file()):
                raise ValueError(f"Exception must name an existing test file: {path}")
            if not re.fullmatch(r"#[0-9]+", entry["issue"]) or not entry["reason"].strip():
                raise ValueError(f"Exception needs an issue and a reason: {path}")
    if not set(policy["held"]) <= initial_hold_files(root):
        raise ValueError("Deferrals extend beyond the reviewed new roots")
    return policy


def check_trusted_policy(root: Path, ref: str) -> None:
    """Compare with the PR base registry; its first introduction needs review."""
    # A missing file is permitted only for the registry's first introduction.
    listing = subprocess.run(
        ["git", "ls-tree", "--name-only", ref, "--", POLICY], cwd=root,
        check=True, capture_output=True, text=True,
    )
    if listing.stdout.strip():
        previous = subprocess.run(
            ["git", "show", f"{ref}:{POLICY}"], cwd=root,
            check=True, capture_output=True, text=True,
        )
        validate_shrink_only(read_policy(root), json.loads(previous.stdout))


def collection_exclusions(root: Path) -> set[str]:
    """Read declarative collection controls without importing any conftest."""
    files = test_files(root)
    excluded = set()
    config = configparser.ConfigParser(interpolation=None)
    config.read(root / "pytest.ini")
    patterns = config["pytest"]["norecursedirs"].split()
    for path in files:
        for directory in Path(path).parents:
            if any(fnmatch(directory.name, pattern) if "/" not in pattern else
                   fnmatch("/" + directory.as_posix(), "*/" + pattern)
                   for pattern in patterns):
                excluded.add(path)
    for conftest in [root / "conftest.py", *(root / "tests").rglob("conftest.py")]:
        tree = ast.parse(conftest.read_text())
        for node in ast.walk(tree):
            if ((isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
                 and node.name == "pytest_ignore_collect")
                or (isinstance(node, ast.Name) and node.id in {"collect_ignore", "collect_ignore_glob"}
                    and isinstance(node.ctx, ast.Load))
                or (isinstance(node, (ast.AugAssign, ast.AnnAssign))
                    and isinstance(node.target, ast.Name)
                    and node.target.id in {"collect_ignore", "collect_ignore_glob"})):
                raise ValueError(f"Unsupported dynamic collection exclusion: {conftest.relative_to(root)}")
            if not isinstance(node, ast.Assign):
                continue
            targets = {target.id for target in node.targets if isinstance(target, ast.Name)}
            if not targets & {"collect_ignore", "collect_ignore_glob"}:
                continue
            if not isinstance(node.value, (ast.List, ast.Tuple)):
                raise ValueError(f"Non-declarative collection exclusions: {conftest.relative_to(root)}")
            for item in node.value.elts:
                if isinstance(item, ast.Constant) and isinstance(item.value, str):
                    value = item.value
                elif (isinstance(item, ast.Call) and isinstance(item.func, ast.Name)
                      and item.func.id == "str" and len(item.args) == 1
                      and isinstance(item.args[0], ast.BinOp)
                      and isinstance(item.args[0].op, ast.Div)
                      and isinstance(item.args[0].left, ast.Name)
                      and item.args[0].left.id == "_tests_dir"
                      and isinstance(item.args[0].right, ast.Constant)
                      and isinstance(item.args[0].right.value, str)):
                    value = item.args[0].right.value
                else:
                    raise ValueError(f"Unsupported collection exclusion: {conftest.relative_to(root)}")
                relative = (conftest.parent / value).relative_to(root).as_posix()
                excluded.update(path for path in files if
                                (fnmatch(path, relative) if "collect_ignore_glob" in targets else
                                 path == relative or path.startswith(relative + "/")))
    return excluded


def command_for(name: str, root: Path, sweep: bool = False) -> str:
    """Build the actual PR or sweep command, preserving all other gate options."""
    gates = yaml.safe_load((root / ".claude/quality-gates.yaml").read_text())["gates"]
    command = gates[name]["command"]
    additions = [path for path in ADDITIONAL_ROOTS.get(name, ()) if (root / path).exists()]
    ignored = list(OWNERSHIP_IGNORES.get(name, ()))
    if not sweep:
        policy = read_policy(root)
        ignored.extend(path for path, entry in policy["held"].items() if entry["gate"] == name)
    return " ".join([command, *additions, *(f"--ignore={path}" for path in sorted(ignored))])


def main() -> None:
    """Print the command consumed by the hosted workflow."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--gate", required=True)
    parser.add_argument("--sweep", action="store_true")
    args = parser.parse_args()
    print(command_for(args.gate, Path(__file__).resolve().parents[2], sweep=args.sweep))


if __name__ == "__main__":
    main()
