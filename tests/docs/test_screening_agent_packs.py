# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""AST contract fallback until the shared pack checker reaches the base.

Inventory covers package-owned top-level functions/classes and public class
methods/properties in direct Python files. Imported third-party utilities,
private definitions and local implementation closures are not public APIs.
No engineering module or solver is imported by this checker.
"""

import ast
import re
from pathlib import Path

import pytest
import yaml

from tests.docs.test_agent_pack_node_ids import collect_pack

ROOT = Path(__file__).resolve().parents[2]
MODULES = ("foam_system", "lifting_lug", "corrosion")


def public_inventory(package):
    """Map qualified names to defining file and inclusive AST body range."""
    inventory = {}
    for path in sorted(package.glob("*.py")):
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for node in tree.body:
            if not isinstance(
                node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)
            ):
                continue
            if node.name.startswith("_"):
                continue
            inventory[f"{path.stem}.{node.name}"] = (path, node.lineno, node.end_lineno)
            if isinstance(node, ast.ClassDef):
                for method in node.body:
                    if isinstance(method, (ast.FunctionDef, ast.AsyncFunctionDef)):
                        if not method.name.startswith("_"):
                            name = f"{path.stem}.{node.name}.{method.name}"
                            inventory[name] = (path, method.lineno, method.end_lineno)
    return inventory


def check_pack(pack, package, root=ROOT):
    """Return 0 for an exact function table with valid defining locators, else 1."""
    text = pack.read_text(encoding="utf-8")
    section = re.search(r"(?ms)^## 2\..*?(?=^## 3\.|\Z)", text)
    if section is None:
        return 1
    rows = re.findall(r"(?m)^\| `([^`]+)` \| (.*)$", section[0])
    expected = public_inventory(package)
    if len(rows) != len(expected) or {name for name, _ in rows} != set(expected):
        return 1
    for name, row in rows:
        path, start, end = expected[name]
        locator = f"[{path.relative_to(root).as_posix()}:L{start}-L{end}]"
        if locator not in row:
            return 1
    locators = re.findall(r"\[([^\[\]\s]+):L(\d+)-L(\d+)\]", text)
    if not locators:
        return 1
    for filename, first, last in locators:
        path = (root / filename).resolve()
        if not path.is_relative_to(root.resolve()) or not path.is_file():
            return 1
        if not 1 <= int(first) <= int(last) <= len(path.read_text().splitlines()):
            return 1
    return 0


@pytest.mark.parametrize("module", MODULES)
def test_public_contract_and_locators(module):
    package = ROOT / "src/digitalmodel" / module
    assert check_pack(package / "docs/agent-pack.md", package) == 0


@pytest.mark.parametrize("module", MODULES)
def test_missing_public_name_fails(module, tmp_path):
    package = ROOT / "src/digitalmodel" / module
    original = package / "docs/agent-pack.md"
    name = next(iter(public_inventory(package)))
    copy = tmp_path / "agent-pack.md"
    copy.write_text(original.read_text().replace(f"| `{name}` |", "| `missing` |", 1))
    assert check_pack(copy, package) == 1


def test_invalid_locator_fails(tmp_path):
    package = ROOT / "src/digitalmodel/foam_system"
    text = (package / "docs/agent-pack.md").read_text()
    copy = tmp_path / "agent-pack.md"
    copy.write_text(text + "\n[src/digitalmodel/foam_system/sizing.py:L1-L999999]\n")
    assert check_pack(copy, package) == 1


@pytest.mark.parametrize("module", MODULES)
def test_all_cited_nodes_collect(module, tmp_path):
    pack = ROOT / "src/digitalmodel" / module / "docs/agent-pack.md"
    collect_pack(pack, ROOT, tmp_path / "collection", include_gates=True)


def test_registry_paths_and_new_pack_sections():
    data = yaml.safe_load((ROOT / "docs/registry/module-routing.yaml").read_text())

    def paths(node):
        if isinstance(node, dict):
            for key, value in node.items():
                if key == "agent_pack":
                    yield value
                else:
                    yield from paths(value)
        elif isinstance(node, list):
            for value in node:
                yield from paths(value)

    registered = set(paths(data))
    for path in registered:
        assert (ROOT / path).is_file(), path
    for module in MODULES:
        name = f"src/digitalmodel/{module}/docs/agent-pack.md"
        assert name in registered
        text = (ROOT / name).read_text()
        assert re.findall(r"(?m)^## (\d+)\.", text) == [str(i) for i in range(1, 11)]


def test_implementation_map_registration():
    data = yaml.safe_load((ROOT / "docs/registry/implementation-map.yaml").read_text())
    for module in MODULES:
        assert data["agent_packs"][module] == {
            "agent_pack": f"src/digitalmodel/{module}/docs/agent-pack.md",
            "qa_family": "EC",
        }
