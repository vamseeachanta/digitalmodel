# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""AST inventory and citation gates for pinned subsea packs; no solver imports.

The shared checker is absent at the code pin. This helper remains local to
these tests until its interface is available. Public means locally defined
functions/classes and their public methods/properties, plus explicit __all__
exports. Imported third-party implementation details are excluded.
"""

import argparse
import ast
import json
import re
import subprocess
import sys
from collections import Counter
from pathlib import Path

import pytest
import yaml

from tests.docs.test_agent_pack_node_ids import collect_pack

ROOT = Path(__file__).resolve().parents[2]
PACKAGES = ("trees", "connectors", "rov")
PIN = "3554b14fde67f44b1c52b81b6c72106282eaff29"


def public_inventory(package: Path) -> dict[str, tuple[Path, int, int]]:
    """Read public definitions, methods and explicit exports without importing."""
    found = {}
    exports = set()
    assignments = {}
    for path in sorted(package.rglob("*.py")):
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for node in tree.body:
            if isinstance(node, (ast.Assign, ast.AnnAssign)):
                targets = node.targets if isinstance(node, ast.Assign) else [node.target]
                for target in targets:
                    if isinstance(target, ast.Name):
                        assignments[target.id] = (path, node.lineno, node.end_lineno)
            if isinstance(node, (ast.Assign, ast.AugAssign)):
                targets = node.targets if isinstance(node, ast.Assign) else [node.target]
                if any(isinstance(t, ast.Name) and t.id == "__all__" for t in targets):
                    exports.update(ast.literal_eval(node.value))
            if not isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                continue
            if node.name.startswith("_"):
                continue
            found[node.name] = (path, node.lineno, node.end_lineno)
            if isinstance(node, ast.ClassDef):
                for child in node.body:
                    if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef)):
                        if not child.name.startswith("_"):
                            found[f"{node.name}.{child.name}"] = (
                                path, child.lineno, child.end_lineno,
                            )
    for name in exports - found.keys():
        if name in assignments:
            found[name] = assignments[name]
        else:
            raise AssertionError(f"unresolved public export: {name}")
    return found


def check_pack(pack: Path, package: Path, root: Path = ROOT) -> list[str]:
    """Return function-list and source-locator drift, with actionable names."""
    text = pack.read_text(encoding="utf-8")
    section = re.search(r"(?ms)^## 2\..*?(?=^## 3\.|\Z)", text)
    rows = re.findall(r"(?m)^\| `([\w.]+)` \|(.+)$", section[0] if section else "")
    documented = {name: row for name, row in rows}
    inventory = public_inventory(package)
    errors = [f"missing: {name}" for name in sorted(inventory.keys() - documented.keys())]
    errors += [f"extra: {name}" for name in sorted(documented.keys() - inventory.keys())]
    for name in sorted(inventory.keys() & documented.keys()):
        path, start, end = inventory[name]
        expected = path.relative_to(root).as_posix()
        locators = re.findall(r"\[([^\]\n]+):L(\d+)-L(\d+)\]", documented[name])
        if not any(p == expected and int(lo) <= start <= end <= int(hi)
                   for p, lo, hi in locators):
            errors.append(f"definition locator: {name}")
    for path, lo, hi in re.findall(r"\[([^\]\n]+):L(\d+)-L(\d+)\]", text):
        source = root / path
        if (not source.is_file() or not 1 <= int(lo) <= int(hi)
                <= len(source.read_text(encoding="utf-8").splitlines())):
            errors.append(f"invalid locator: {path}:L{lo}-L{hi}")
    if len(rows) != len(documented):
        errors.append("duplicate contract row")
    return errors


def pack_path(name: str) -> Path:
    return ROOT / "src/digitalmodel/subsea" / name / "docs/agent-pack.md"


@pytest.mark.parametrize("name", PACKAGES)
def test_pack_contracts(name):
    pack = pack_path(name)
    assert check_pack(pack, pack.parent.parent) == []
    text = pack.read_text()
    assert re.findall(r"(?m)^## (\d+)\.", text) == [str(i) for i in range(1, 11)]
    assert f"`{PIN}`" in text
    # The calculation files themselves must still match the declared code pin.
    for source in pack.parent.parent.glob("*.py"):
        pinned = subprocess.run(
            ["git", "show", f"{PIN}:{source.relative_to(ROOT).as_posix()}"],
            cwd=ROOT, check=True, capture_output=True,
        )
        assert source.read_bytes() == pinned.stdout


@pytest.mark.parametrize("name", PACKAGES)
def test_missing_public_name_fails(name, tmp_path):
    pack = pack_path(name)
    text = pack.read_text()
    first = next(name for name in public_inventory(pack.parent.parent)
                 if name[0].islower() and "." not in name)
    copy = tmp_path / "agent-pack.md"
    copy.write_text(re.sub(rf"(?m)^\| `{re.escape(first)}` \|.*\n", "", text))
    assert f"missing: {first}" in check_pack(copy, pack.parent.parent)


@pytest.mark.parametrize("name", PACKAGES)
def test_all_example_and_gate_nodes_collect(name, tmp_path):
    collect_pack(pack_path(name), ROOT, tmp_path / "collection", include_gates=True)


def test_invalid_locator_fails(tmp_path):
    pack = pack_path("trees")
    copy = tmp_path / "agent-pack.md"
    copy.write_text(pack.read_text() + "\n[src/digitalmodel/subsea/trees/__init__.py:L1-L99999]\n")
    assert any(error.startswith("invalid locator:")
               for error in check_pack(copy, pack.parent.parent))


def test_extra_name_fails(tmp_path):
    pack = pack_path("trees")
    copy = tmp_path / "agent-pack.md"
    copy.write_text(pack.read_text().replace(
        "## 3.", "| `nonexistent_function` | none |\n\n## 3.", 1,
    ))
    assert "extra: nonexistent_function" in check_pack(copy, pack.parent.parent)


def test_registry_pack_paths_and_new_entries():
    registry = yaml.safe_load((ROOT / "docs/registry/module-routing.yaml").read_text())

    def paths(value):
        if isinstance(value, dict):
            for key, item in value.items():
                if key == "agent_pack":
                    yield item
                else:
                    yield from paths(item)
        elif isinstance(value, list):
            for item in value:
                yield from paths(item)

    for path in paths(registry):
        assert (ROOT / path).is_file(), path
    subsea = next(row for row in registry["modules"] if row["module"] == "subsea")
    implementation = yaml.safe_load((ROOT / "docs/registry/implementation-map.yaml").read_text())
    for name in PACKAGES:
        expected = pack_path(name).relative_to(ROOT).as_posix()
        assert subsea["submodules"][name] == {"agent_pack": expected, "qa_family": "EC"}
        assert implementation["knowledge_packs"][f"subsea/{name}"] == {
            "agent_pack": expected, "qa_family": "EC",
        }


def test_registry_additions_preserve_existing_metadata():
    for name in ("module-routing.yaml", "implementation-map.yaml"):
        path = f"docs/registry/{name}"
        pinned = subprocess.run(["git", "show", f"{PIN}:{path}"], cwd=ROOT,
                                check=True, capture_output=True, text=True)
        before = yaml.safe_load(pinned.stdout)
        after = yaml.safe_load((ROOT / path).read_text())
        if name == "implementation-map.yaml":
            assert set(after.pop("knowledge_packs")) == {
                f"subsea/{name}" for name in PACKAGES
            }
        else:
            subsea = next(row for row in after["modules"] if row["module"] == "subsea")
            for package in PACKAGES:
                del subsea["submodules"][package]
        assert after == before


def test_baseline_audit_report_is_complete():
    report = json.loads((ROOT / "docs/registry/agent-pack-drift.json").read_text())
    assert report["code_pin"] == PIN
    pinned = subprocess.run(["git", "ls-tree", "-r", "--name-only", PIN], cwd=ROOT,
                            check=True, capture_output=True, text=True)
    expected = {path for path in pinned.stdout.splitlines()
                if path.startswith("src/digitalmodel/") and path.endswith("/docs/agent-pack.md")}
    assert {row["pack"] for row in report["packs"]} == expected
    summary = report["summary"]
    assert summary["existing_packs"] == len(expected)
    counts = Counter(error.split(":")[0] for row in report["packs"]
                     for error in row["contract_diagnostics"])
    assert summary["diagnostic_counts"] == counts
    assert summary["strict_contract_diagnostics"] == sum(counts.values())
    for status in ("passed", "failed"):
        assert summary[f"existing_pack_collection_{status}"] == sum(
            row["node_collection"]["status"] == status for row in report["packs"]
        )


def main(argv=None):
    """Run the local contract checker, returning a conventional exit status."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("packs", nargs="*", type=Path)
    parser.add_argument("--package", type=Path)
    parser.add_argument("--audit-base", action="store_true")
    args = parser.parse_args(argv)
    failures = []
    selected = args.packs
    if args.audit_base:
        pinned = subprocess.run(["git", "ls-tree", "-r", "--name-only", PIN],
                                cwd=ROOT, check=True, capture_output=True, text=True)
        selected = [ROOT / path for path in pinned.stdout.splitlines()
                    if path.startswith("src/digitalmodel/")
                    and path.endswith("/docs/agent-pack.md")]
    for pack in selected or [pack_path(name) for name in PACKAGES]:
        try:
            errors = check_pack(pack, args.package or pack.parent.parent)
        except (ValueError, SyntaxError, AssertionError, TypeError) as exc:
            if not args.audit_base:
                raise
            errors = [f"inventory unavailable: {type(exc).__name__}"]
        label = pack.relative_to(ROOT) if pack.is_relative_to(ROOT) else pack.name
        failures.extend(f"{label}: {error}" for error in errors)
    print("\n".join(failures) if failures else "pack contracts: pass")
    return int(bool(failures))


@pytest.mark.parametrize("name", PACKAGES)
def test_checker_cli_exit_codes(name, tmp_path):
    pack = pack_path(name)
    command = [sys.executable, "-m", "tests.docs.test_subsea_agent_packs"]
    result = subprocess.run(command + [str(pack)], cwd=ROOT, capture_output=True, text=True)
    assert result.returncode == 0, result.stdout + result.stderr
    first = next(name for name in public_inventory(pack.parent.parent)
                 if name[0].islower() and "." not in name)
    copy = tmp_path / "agent-pack.md"
    copy.write_text(re.sub(rf"(?m)^\| `{re.escape(first)}` \|.*\n", "", pack.read_text()))
    result = subprocess.run(command + [str(copy), "--package", str(pack.parent.parent)],
                            cwd=ROOT, capture_output=True, text=True)
    assert result.returncode == 1, result.stdout + result.stderr
    assert f"missing: {first}" in result.stdout


def test_ast_inventory_ignores_imports_and_includes_methods_exports(tmp_path):
    (tmp_path / "__init__.py").write_text(
        "from unavailable import imported\n"
        "__all__ = ['Scale']\n__all__ += ['VERSION']\nVERSION = 1\n"
        "def public(): pass\ndef _private(): pass\n"
        "class Scale:\n"
        "    def apply(self): pass\n"
        "    @property\n    def factor(self): return 1\n"
        "    def _hidden(self): pass\n"
    )
    assert set(public_inventory(tmp_path)) == {
        "Scale", "Scale.apply", "Scale.factor", "VERSION", "public",
    }


if __name__ == "__main__":
    raise SystemExit(main())
