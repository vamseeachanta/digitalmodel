"""Static pack/API contracts without importing calculation or solver packages.

The shared function checker was absent at the code pin. This local AST helper
checks definitions, explicit re-exports and public methods. It can be replaced
by the shared checker once that tool reaches the base branch.
"""

import ast
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
MODULES = ("tug", "passing_ship_forces", "vessel_seakeeping")
PACKS = tuple(ROOT / f"src/digitalmodel/{name}/docs/agent-pack.md" for name in MODULES)
LOCATOR = re.compile(r"\[([A-Za-z0-9_./-]+):L(\d+)-L(\d+)\]")


def _definition(root, module, symbol, seen=None):
    """Resolve a named class/function through static local imports only."""
    seen = set() if seen is None else seen
    key = (module, symbol)
    if key in seen:
        return None
    seen.add(key)
    stem = root / "src" / Path(*module.split("."))
    path = stem.with_suffix(".py")
    if not path.is_file():
        path = stem / "__init__.py"
    if not path.is_file():
        return None
    tree = ast.parse(path.read_text(encoding="utf-8"))
    for node in tree.body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            if node.name == symbol:
                return path, node
        if isinstance(node, ast.ImportFrom):
            for alias in node.names:
                if (alias.asname or alias.name) != symbol:
                    continue
                target = node.module or ""
                if node.level:
                    parts = module.split(".")
                    base = parts if path.name == "__init__.py" else parts[:-1]
                    base = base[:len(base) - node.level + 1]
                    target = ".".join(base + ([target] if target else []))
                return _definition(root, target, alias.name, seen)
    return None


def public_inventory(package, root=ROOT):
    """Return qualified local/public export names and defining AST locators.

    Per-file literal __all__ controls exports when present; otherwise public
    local definitions are used. Imported names require an explicit export.
    Properties/public methods are included; private/dunder methods are not.
    No module is imported and no calculator is executed.
    """
    inventory = {}
    for path in sorted(package.rglob("*.py")):
        if any(part.startswith(".") for part in path.relative_to(package).parts):
            continue
        tree = ast.parse(path.read_text(encoding="utf-8"))
        definitions = {node.name: node for node in tree.body if isinstance(
            node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)
        )}
        exports = None
        for node in tree.body:
            if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id == "__all__"
                for target in node.targets
            ):
                try:
                    exports = ast.literal_eval(node.value)
                except (ValueError, TypeError):
                    pass
        names = exports if exports is not None else definitions
        prefix = ".".join(path.relative_to(package).with_suffix("").parts)
        module = ".".join(path.relative_to(root / "src").with_suffix("").parts)
        if path.name == "__init__.py":
            module = module.removesuffix(".__init__")
        for name in names:
            if name.startswith("_"):
                continue
            resolved = ((path, definitions[name]) if name in definitions
                        else _definition(root, module, name))
            if resolved is None:  # Constants and modules are not callable definitions.
                continue
            source, node = resolved
            key = f"{prefix}.{name}"
            inventory[key] = (source, node.lineno, node.end_lineno)
            if isinstance(node, ast.ClassDef):
                for method in node.body:
                    if isinstance(method, (ast.FunctionDef, ast.AsyncFunctionDef)):
                        if not method.name.startswith("_"):
                            inventory[f"{key}.{method.name}"] = (
                                source, method.lineno, method.end_lineno
                            )
    return inventory


def check_pack(pack, package, root=ROOT):
    """Return deterministic findings for section-2 inventory and all locators."""
    text = pack.read_text(encoding="utf-8")
    section = re.search(r"(?ms)^## 2\..*?(?=^## 3\.|\Z)", text)
    rows = re.findall(r"(?m)^\| `([^`]+)` \|([^\n]+)", section[0] if section else "")
    documented = {name for name, _ in rows}
    inventory = public_inventory(package, root)
    errors = [f"missing public symbol: {name}" for name in sorted(inventory.keys() - documented)]
    errors += [f"extra public symbol: {name}" for name in sorted(documented - inventory.keys())]
    for name, row in rows:
        if name not in inventory:
            continue
        source, start, end = inventory[name]
        relative = source.relative_to(root).as_posix()
        if not any(path == relative and int(lo) <= start and int(hi) >= end
                   for path, lo, hi in LOCATOR.findall(row)):
            errors.append(f"definition not covered by row locator: {name}")
    locators = LOCATOR.findall(text)
    if not locators:
        errors.append("no evidence locators")
    for path, first, last in locators:
        target = root / path
        if Path(path).is_absolute() or ".." in Path(path).parts or not target.is_file():
            errors.append(f"missing or non-relative locator: {path}")
            continue
        count = len(target.read_text(encoding="utf-8").splitlines())
        if not 1 <= int(first) <= int(last) <= count:
            errors.append(f"out-of-bounds locator: {path}:L{first}-L{last}")
    return errors


def checker_status(pack, package, root=ROOT):
    """Equivalent checker exit contract: zero is clean, one is drift."""
    return int(bool(check_pack(pack, package, root)))


@pytest.mark.parametrize("pack", PACKS, ids=lambda path: path.parent.parent.name)
def test_new_pack_contracts(pack):
    assert check_pack(pack, pack.parent.parent) == []
    assert checker_status(pack, pack.parent.parent) == 0


@pytest.mark.parametrize("pack", PACKS, ids=lambda path: path.parent.parent.name)
def test_deleted_function_is_detected(pack, tmp_path):
    text = pack.read_text(encoding="utf-8")
    # Remove the function name from a copy; references elsewhere cannot mask it.
    symbol = "bollard_pull.bollard_pull" if pack.parent.parent.name == "tug" else "workflow.router"
    altered = tmp_path / "agent-pack.md"
    altered.write_text(text.replace(f"| `{symbol}` |", "| omitted |"), encoding="utf-8")
    errors = check_pack(altered, pack.parent.parent)
    assert f"missing public symbol: {symbol}" in errors
    assert checker_status(altered, pack.parent.parent) == 1


def test_checker_detects_extra_symbol_and_invalid_locator(tmp_path):
    pack = PACKS[0]
    altered = tmp_path / "agent-pack.md"
    altered.write_text(pack.read_text().replace(
        "## 3.", "| `not_a_public_function` | unsupported |\n\n## 3.", 1
    ) + "\n[src/digitalmodel/tug/constants.py:L1-L999999]\n")
    errors = check_pack(altered, pack.parent.parent)
    assert "extra public symbol: not_a_public_function" in errors
    assert any("out-of-bounds locator" in error for error in errors)


def test_locator_parser_ignores_interval_brackets():
    text = "range [0,1) and [src/digitalmodel/tug/constants.py:L1-L5]"
    assert LOCATOR.findall(text) == [("src/digitalmodel/tug/constants.py", "1", "5")]


def test_checker_inventory_includes_public_methods_and_explicit_exports(tmp_path):
    package = tmp_path / "src" / "synthetic"
    package.mkdir(parents=True)
    (package / "__init__.py").write_text(
        "from .api import Public\n__all__ = ['Public']\n"
    )
    (package / "api.py").write_text(
        "class Public:\n    def method(self): pass\n    def _private(self): pass\n"
        "def _helper(): pass\n"
    )
    assert set(public_inventory(package, tmp_path)) == {
        "__init__.Public", "__init__.Public.method", "api.Public", "api.Public.method"
    }


def test_all_new_pack_node_ids_collect(tmp_path):
    """Collect every citation, including gates, without running cited tests."""
    ids = sorted({node for pack in PACKS for node in re.findall(
        r"`(tests/[^`\n]+::[^`\n]+)`", pack.read_text(encoding="utf-8")
    )})
    assert ids
    env = {key: value for key, value in os.environ.items()
           if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}}
    result = subprocess.run(
        [sys.executable, "-m", "pytest", "--collect-only", "-q", "-p", "no:cacheprovider",
         f"--basetemp={tmp_path / 'collection'}", *ids],
        cwd=ROOT, env=env, text=True, capture_output=True, check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr


def _registered_packs(value):
    if isinstance(value, dict):
        if "agent_pack" in value:
            yield value["agent_pack"]
        for child in value.values():
            yield from _registered_packs(child)
    elif isinstance(value, list):
        for child in value:
            yield from _registered_packs(child)


def test_registered_agent_pack_paths_exist():
    data = yaml.safe_load((ROOT / "docs/registry/module-routing.yaml").read_text())
    paths = list(_registered_packs(data))
    assert paths
    assert all((ROOT / path).is_file() for path in paths)
    for module in MODULES:
        entry = next(row for row in data["modules"] if row["module"] == module)
        registration = entry["submodules"][module]
        assert registration == {
            "agent_pack": f"src/digitalmodel/{module}/docs/agent-pack.md", "qa_family": "EC"
        }


@pytest.mark.parametrize("pack", PACKS, ids=lambda path: path.parent.parent.name)
def test_new_pack_has_ten_ordered_sections(pack):
    headings = re.findall(r"(?m)^## (\d+)\.", pack.read_text(encoding="utf-8"))
    assert headings == [str(number) for number in range(1, 11)]


def test_implementation_map_pack_entries_match_routing():
    data = yaml.safe_load((ROOT / "docs/registry/implementation-map.yaml").read_text())
    entries = data["agent_packs"]
    assert list(entries) == sorted(entries)
    for module in MODULES:
        assert entries[module] == {
            "agent_pack": f"src/digitalmodel/{module}/docs/agent-pack.md",
            "code_pin": "3554b14fde67f44b1c52b81b6c72106282eaff29",
            "qa_family": "EC",
        }
