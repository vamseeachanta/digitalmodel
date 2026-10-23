# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""AST-only fallback for numeric packs until the shared checker is available.

Run this file as a script with --audit-existing for a read-only audit of packs
tracked at the supplied Git ref. No numerical package or solver is imported.
The strict contract uses a section-2 API table with package-relative names.
Legacy prose is reported as inventory drift, not silently treated as a pass.
"""

import ast
import json
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
MODULES = ("seismic", "uncertainty", "residual")
LOCATOR = re.compile(r"\[([^\]\n]+):L(\d+)-L(\d+)\]")


def public_definitions(package: Path, root: Path) -> dict[str, tuple[str, int]]:
    """Index local public definitions/methods; deduplicate __all__ re-exports.

    Imported library objects and data/type aliases are not local definitions.
    The three packages expose no renamed public re-exports. AST traversal
    intentionally excludes function-local definitions and private classes.
    """
    found = {}
    for path in sorted(package.rglob("*.py")):
        if "docs" in path.relative_to(package).parts:
            continue
        tree = ast.parse(path.read_text(encoding="utf-8"))
        parts = list(path.relative_to(package).with_suffix("").parts)
        if parts[-1] == "__init__":
            parts.pop()
        prefix = ".".join(parts)

        def visit(nodes, parent):
            for node in nodes:
                if not isinstance(node, (ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)):
                    continue
                if node.name.startswith("_"):
                    continue
                name = ".".join(filter(None, [parent, node.name]))
                found[name] = (path.relative_to(root).as_posix(), node.lineno)
                if isinstance(node, ast.ClassDef):
                    visit(node.body, name)

        visit(tree.body, prefix)
    return found


def section_two(text: str) -> str:
    match = re.search(r"(?ms)^## 2\..*?(?=^## 3\.|\Z)", text)
    return match[0] if match else ""


def check_pack(text: str, package: Path, root: Path) -> list[str]:
    """Compare the exact API table to AST and validate all evidence bounds."""
    expected = public_definitions(package, root)
    rows = dict(re.findall(r"(?m)^\| `([\w.]+)` \|(.*)$", section_two(text)))
    problems = [f"{name}: code-only" for name in sorted(expected.keys() - rows.keys())]
    problems += [f"{name}: pack-only" for name in sorted(rows.keys() - expected.keys())]
    for name in sorted(expected.keys() & rows.keys()):
        path, line = expected[name]
        if not any(p == path and int(start) <= line <= int(end)
                   for p, start, end in LOCATOR.findall(rows[name])):
            problems.append(f"{name}: declaration locator missing")
    for path, start, end in LOCATOR.findall(text):
        target = (root / path).resolve()
        if not target.is_relative_to(root.resolve()) or not target.is_file():
            problems.append(f"{path}: locator file missing or outside repository")
        elif not 1 <= int(start) <= int(end) <= len(target.read_text().splitlines()):
            problems.append(f"{path}: locator out of bounds L{start}-L{end}")
    if not LOCATOR.search(text):
        problems.append("evidence locators missing")
    return problems


@pytest.mark.parametrize("module", MODULES)
def test_new_pack_contracts(module):
    package = ROOT / "src/digitalmodel" / module
    text = (package / "docs/agent-pack.md").read_text()
    assert check_pack(text, package, ROOT) == []


@pytest.mark.parametrize("module", MODULES)
def test_missing_function_is_rejected(module, tmp_path):
    package = ROOT / "src/digitalmodel" / module
    text = (package / "docs/agent-pack.md").read_text()
    name = next(iter(public_definitions(package, ROOT)))
    # Remove only the section-2 name; prose or examples must not mask the drift.
    changed = text.replace(f"| `{name}` |", "| removed |", 1)
    copy = tmp_path / "agent-pack.md"
    copy.write_text(changed)
    assert f"{name}: code-only" in check_pack(copy.read_text(), package, ROOT)
    result = subprocess.run(
        [sys.executable, __file__, "--pack", str(copy), "--package", str(package)],
        capture_output=True, text=True, check=False,
    )
    assert result.returncode == 1
    assert f"{name}: code-only" in result.stdout


@pytest.mark.parametrize("module", MODULES)
def test_checker_cli_accepts_new_pack(module):
    package = ROOT / "src/digitalmodel" / module
    result = subprocess.run(
        [sys.executable, __file__, "--pack", str(package / "docs/agent-pack.md"),
         "--package", str(package)], capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr


def test_checker_rejects_extra_name_and_invalid_locator(tmp_path):
    package = tmp_path / "pkg"
    package.mkdir()
    (package / "example.py").write_text("def real():\n    return 1\n")
    text = ("## 2. Inputs\n| `example.real` | [pkg/example.py:L1-L2] |\n"
            "| `example.absent` | [pkg/example.py:L1-L2] |\n"
            "## 3. Method\n[pkg/example.py:L0-L9]\n")
    errors = check_pack(text, package, tmp_path)
    assert "example.absent: pack-only" in errors
    assert "pkg/example.py: locator out of bounds L0-L9" in errors


def test_checker_requires_the_declaration_locator(tmp_path):
    package = tmp_path / "pkg"
    package.mkdir()
    (package / "example.py").write_text("# heading\ndef real():\n    return 1\n")
    text = "## 2. Inputs\n| `example.real` | [pkg/example.py:L1-L1] |\n## 3. Method\n"
    assert check_pack(text, package, tmp_path) == [
        "example.real: declaration locator missing",
    ]


@pytest.mark.parametrize("module", MODULES)
def test_new_pack_node_ids(module, tmp_path):
    """Collect every cited node, including section-5 gates, without executing it."""
    pack = ROOT / "src/digitalmodel" / module / "docs/agent-pack.md"
    section = re.search(r"(?ms)^## 4\..*?(?=^## 6\.|\Z)", pack.read_text())
    ids = sorted(set(re.findall(r"`(tests/[^`\n]+::[^`\n]+)`", section[0])))
    assert len(ids) >= 3
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    result = subprocess.run(
        [sys.executable, "-m", "pytest", "--collect-only", "-q",
         "-p", "no:cacheprovider", f"--basetemp={tmp_path / 'collection'}", *ids],
        cwd=ROOT, env=env, capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr


def test_registered_pack_paths_exist_and_new_headings_are_ordered():
    registry = yaml.safe_load((ROOT / "docs/registry/module-routing.yaml").read_text())
    registered = {}
    for entry in registry["modules"]:
        for submodule in entry.get("submodules", {}).values():
            if "agent_pack" in submodule:
                path = submodule["agent_pack"]
                assert (ROOT / path).is_file(), path
                registered[path] = submodule["qa_family"]
    mapping = yaml.safe_load((ROOT / "docs/registry/implementation-map.yaml").read_text())
    for module in MODULES:
        path = f"src/digitalmodel/{module}/docs/agent-pack.md"
        assert registered[path] == "EC"
        assert mapping["agent_packs"][module] == {"agent_pack": path, "qa_family": "EC"}
        assert re.findall(r"(?m)^## (\d+)\.", (ROOT / path).read_text()) == [
            str(number) for number in range(1, 11)
        ]


def test_existing_audit_identifies_every_pinned_pack():
    audit = json.loads((ROOT / "docs/registry/numeric-pack-drift.json").read_text())
    result = subprocess.run(
        ["git", "ls-tree", "-r", "--name-only", audit["code_pin"], "src/digitalmodel"],
        cwd=ROOT, capture_output=True, text=True, check=True,
    )
    expected = {path for path in result.stdout.splitlines()
                if path.endswith("/docs/agent-pack.md")}
    assert {row["pack"] for row in audit["packs"]} == expected
    for row in audit["packs"]:
        for key in ("code_only", "pack_only", "evidence"):
            assert row[key] == sorted(set(row[key]))


def audit_existing(ref: str) -> list[dict]:
    """Audit tracked packs without changing them; retain every diagnostic.

    Public definitions are compared with the strict API-table convention. For
    older prose forms this reports table migration needs, not deleted APIs.
    Source files are read from the unchanged working tree at the launch ref;
    the caller must verify no numeric source change before using this mode.
    """
    result = subprocess.run(
        ["git", "ls-tree", "-r", "--name-only", ref, "src/digitalmodel"],
        cwd=ROOT, capture_output=True, text=True, check=True,
    )
    rows = []
    for name in result.stdout.splitlines():
        if not name.endswith("/docs/agent-pack.md"):
            continue
        pack = ROOT / name
        try:
            errors = check_pack(pack.read_text(), pack.parent.parent, ROOT)
        except (SyntaxError, UnicodeError, OSError) as exc:
            errors = [f"AST audit unavailable: {type(exc).__name__}"]
        rows.append({"pack": name, "diagnostics": errors})
    return rows


def main():
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    modes = parser.add_mutually_exclusive_group(required=True)
    modes.add_argument("--audit-existing", metavar="REF")
    modes.add_argument("--pack", type=Path)
    parser.add_argument("--package", type=Path)
    args = parser.parse_args()
    if args.audit_existing:
        print(json.dumps(audit_existing(args.audit_existing), indent=2))
        return 0
    if args.package is None:
        parser.error("--package is required with --pack")
    problems = check_pack(args.pack.read_text(), args.package, ROOT)
    for problem in problems:
        print(problem)
    return int(bool(problems))


if __name__ == "__main__":
    raise SystemExit(main())
