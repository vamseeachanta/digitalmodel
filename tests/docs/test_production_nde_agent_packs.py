# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline AST contract for the production and NDE knowledge packs.

The shared function checker was absent at the recorded code pin. Inventory
includes locally defined public functions/classes and public class methods
(including properties), recursively across the package. Constructors appear
in class contracts; private helpers and imported third-party names are excluded.
Literal __all__ re-exports must also appear in section 2.
"""

import ast
import importlib.util
import json
import re
import subprocess
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
PACKAGES = ("production_engineering", "nde")
LOCATOR = re.compile(r"\[([^\]\n]+):L(\d+)-L(\d+)\]")


def public_inventory(package: Path) -> dict[str, tuple[Path, int, int]]:
    """Read source without importing a solver or optional dataset backend."""
    inventory = {}
    for source in sorted(package.rglob("*.py")):
        tree = ast.parse(source.read_text(encoding="utf-8"))
        module = source.relative_to(package).with_suffix("").as_posix().replace("/", ".")
        for node in tree.body:
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                if node.name.startswith("_"):
                    continue
                key = f"{module}.{node.name}"
                inventory[key] = (source, node.lineno, node.end_lineno)
                if isinstance(node, ast.ClassDef):
                    for method in node.body:
                        if isinstance(method, (ast.FunctionDef, ast.AsyncFunctionDef)):
                            if not method.name.startswith("_"):
                                inventory[f"{key}.{method.name}"] = (
                                    source, method.lineno, method.end_lineno,
                                )
            if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id == "__all__"
                for target in node.targets
            ):
                for name in ast.literal_eval(node.value):
                    if name not in {getattr(item, "name", None) for item in tree.body}:
                        inventory[f"{module}.{name}"] = (
                            source, node.lineno, node.end_lineno,
                        )
    return inventory


def check_pack(pack: Path, package: Path, root: Path = ROOT) -> list[str]:
    """Return name drift and missing, out-of-bounds or wrong-symbol locators."""
    text = pack.read_text(encoding="utf-8")
    section = re.search(r"(?ms)^## 2\..*?(?=^## 3\.|\Z)", text)
    if section is None:
        return ["missing section 2"]
    inventory = public_inventory(package)
    # A distinct row key prevents a reference elsewhere from hiding a missing row.
    rows = {}
    for line in section[0].splitlines():
        match = re.match(r"\| `([^`]+)` \|", line)
        if match:
            rows.setdefault(match[1], []).append(line)
    problems = [f"missing function {name}" for name in sorted(inventory.keys() - rows.keys())]
    problems += [f"extra function {name}" for name in sorted(rows.keys() - inventory.keys())]
    for name in inventory.keys() & rows.keys():
        source, start, end = inventory[name]
        expected = (source.relative_to(root).as_posix(), str(start), str(end))
        if len(rows[name]) != 1 or expected not in LOCATOR.findall(rows[name][0]):
            problems.append(f"wrong symbol locator {name}")
    for path, start, end in LOCATOR.findall(text):
        target = root / path
        if not target.is_file():
            problems.append(f"missing locator file {path}")
        elif not 1 <= int(start) <= int(end) <= len(target.read_text().splitlines()):
            problems.append(f"locator bounds {path}:{start}-{end}")
    return problems


def audit_existing_packs(base: str) -> dict:
    """Report strict-schema diagnostics for unchanged packs at a git ref.

    Legacy packs do not share these qualified inventory-row keys. A missing
    row is schema/inventory evidence, not proof a function changed since the
    legacy pack's own pin. Recursive package scopes can overlap. This audit
    deliberately leaves existing packs untouched and does not import them.
    """
    def git(*args):
        return subprocess.run(
            ["git", *args], cwd=ROOT, check=True, capture_output=True, text=True,
        ).stdout

    paths = git("ls-tree", "-r", "--name-only", base, "src/digitalmodel").splitlines()
    findings = []
    for relative in paths:
        if not relative.endswith("/docs/agent-pack.md"):
            continue
        pack = ROOT / relative
        # The audit describes base packs, never a silently edited working copy.
        assert pack.read_text() == git("show", f"{base}:{relative}")
        try:
            diagnostics = check_pack(pack, pack.parent.parent)
        except (SyntaxError, ValueError, TypeError) as error:
            diagnostics = [f"unsupported inventory syntax: {type(error).__name__}"]
        groups = {}
        for diagnostic in diagnostics:
            direction = next((prefix for prefix in (
                "missing function", "extra function", "wrong symbol locator",
                "missing locator file", "locator bounds",
            ) if diagnostic.startswith(prefix + " ")), "schema")
            value = diagnostic[len(direction) + 1:] if direction != "schema" else diagnostic
            groups.setdefault(direction, []).append(value)
        findings.append({"pack": relative, "diagnostics": groups})
    return {
        "summary": "Strict qualified-row and locator audit; legacy formats are not normalized.",
        "revision_history": [{
            "revision": 1, "date": "2026-10-22",
            "change": "Record existing-pack follow-ups without editing their contents.",
        }],
        "code_pin": git("rev-parse", base).strip(),
        "checker": "tests/docs/test_production_nde_agent_packs.py::check_pack",
        "limits": [
            "Missing qualified rows do not by themselves establish changed implementation.",
            "Legacy formats and narrower declared scopes require maintainer triage.",
            "Recursive package scopes overlap; counts are per pack, not unique functions.",
            "Dynamic export expressions are reported as unsupported syntax, not evaluated.",
        ],
        "packs": findings,
    }


@pytest.mark.parametrize("name", PACKAGES)
def test_pack_public_functions_and_locators(name):
    package = ROOT / "src/digitalmodel" / name
    assert check_pack(package / "docs/agent-pack.md", package) == []


@pytest.mark.parametrize("name", PACKAGES)
def test_removed_public_function_is_detected(name, tmp_path):
    package = ROOT / "src/digitalmodel" / name
    text = (package / "docs/agent-pack.md").read_text()
    symbol = next(iter(public_inventory(package)))
    copy = tmp_path / "agent-pack.md"
    copy.write_text(text.replace(f"| `{symbol}` |", "| `removed` |", 1))
    assert f"missing function {symbol}" in check_pack(copy, package)


def test_out_of_bounds_and_wrong_symbol_locators_are_detected(tmp_path):
    package = ROOT / "src/digitalmodel/nde"
    text = (package / "docs/agent-pack.md").read_text()
    copy = tmp_path / "agent-pack.md"
    copy.write_text(text.replace("pulse_timing.py:L9-L51", "pulse_timing.py:L1-L99999"))
    problems = check_pack(copy, package)
    assert any("locator bounds" in item for item in problems)
    assert "wrong symbol locator pulse_timing.pulse_peak_time" in problems


def test_existing_pack_audit_retains_direction_and_rejects_edited_pack(tmp_path, monkeypatch):
    """A tiny git fixture exercises audit provenance and missing-row reporting."""
    import sys

    module = sys.modules[__name__]
    monkeypatch.setattr(module, "ROOT", tmp_path)
    relative = "src/digitalmodel/example/docs/agent-pack.md"
    pack = tmp_path / relative
    pack.parent.mkdir(parents=True)
    (pack.parent.parent / "sample.py").write_text("def calculate():\n    return None\n")
    original = "## 2. Contract\nNo inventory rows.\n## 3. Method\n"
    pack.write_text(original)

    def fake_git(command, **kwargs):
        if command[1] == "ls-tree":
            output = relative + "\n"
        elif command[1] == "show":
            output = original
        else:
            assert command[1:] == ["rev-parse", "reference"]
            output = "a" * 40 + "\n"
        return subprocess.CompletedProcess(command, 0, stdout=output)

    monkeypatch.setattr(subprocess, "run", fake_git)
    report = audit_existing_packs("reference")
    assert report["code_pin"] == "a" * 40
    assert report["packs"] == [{
        "pack": relative,
        "diagnostics": {"missing function": ["sample.calculate"]},
    }]
    pack.write_text(original + "Edited.\n")
    with pytest.raises(AssertionError):
        audit_existing_packs("reference")


@pytest.mark.parametrize("name", PACKAGES)
def test_pack_cited_nodes_collect(name, tmp_path):
    spec = importlib.util.spec_from_file_location(
        "pack_node_check", ROOT / "tests/docs/test_agent_pack_node_ids.py",
    )
    checker = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(checker)
    checker.collect_pack(
        ROOT / f"src/digitalmodel/{name}/docs/agent-pack.md", ROOT,
        tmp_path / "collection", include_gates=True,
    )


def test_registry_pack_paths_and_ten_sections():
    registry = yaml.safe_load((ROOT / "docs/registry/module-routing.yaml").read_text())
    found = set()

    def visit(value):
        if isinstance(value, dict):
            if "agent_pack" in value:
                assert (ROOT / value["agent_pack"]).is_file(), value["agent_pack"]
                found.add(value["agent_pack"])
            for child in value.values():
                visit(child)
        elif isinstance(value, list):
            for child in value:
                visit(child)

    visit(registry)
    implementation_map = yaml.safe_load(
        (ROOT / "docs/registry/implementation-map.yaml").read_text(),
    )
    for name in PACKAGES:
        path = f"src/digitalmodel/{name}/docs/agent-pack.md"
        assert path in found
        assert implementation_map["agent_packs"][name]["agent_pack"] == path
        text = (ROOT / path).read_text()
        assert re.findall(r"^## (\d+)\.", text, re.M) == [str(i) for i in range(1, 11)]


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--audit-existing", required=True, metavar="GIT_REF")
    args = parser.parse_args()
    print(json.dumps(audit_existing_packs(args.audit_existing), indent=2))
