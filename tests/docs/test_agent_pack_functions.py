"""AST-only documentation inventory checks; fixture arithmetic is not a design basis."""

from __future__ import annotations

import runpy
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/docs/check_agent_pack_functions.py"


@pytest.fixture
def inventory(tmp_path):
    package = tmp_path / "src/example"
    (package / "docs").mkdir(parents=True)
    source = package / "model.py"
    source.write_text("def flow():\n    return 1\n", encoding="utf-8", newline="\n")
    pack = package / "docs/agent-pack.md"
    pack.write_text(
        "## 2. Inputs and outputs\n"
        "| Public symbol | Inputs | Output | Evidence |\n"
        "|---|---|---|---|\n"
        "| `flow` | none | integer | [src/example/model.py:L1-L2] |\n"
        "## 3. Method\n", encoding="utf-8", newline="\n",
    )
    return tmp_path, source, pack


def check(inventory):
    root, _, pack = inventory
    return runpy.run_path(str(SCRIPT))["check_pack"](pack, root)


def test_matching_pack_passes(inventory):
    assert check(inventory) == []


def test_added_function_fails(inventory):
    inventory[1].write_text("def flow():\n    return 1\ndef added():\n    pass\n", encoding="utf-8", newline="\n")
    assert any("code-to-pack: added" in error for error in check(inventory))


def test_removed_function_fails(inventory):
    inventory[1].write_text("# no definitions\n# retained line\n", encoding="utf-8", newline="\n")
    assert any("pack-to-code: flow" in error for error in check(inventory))


def test_locator_past_end_fails(inventory):
    pack = inventory[2]
    pack.write_text(pack.read_text(encoding="utf-8").replace("L1-L2", "L1-L99"), encoding="utf-8", newline="\n")
    assert any("locator: flow" in error and "outside" in error for error in check(inventory))


def test_renamed_function_fails_both_directions(inventory):
    inventory[1].write_text("def renamed():\n    return 1\n", encoding="utf-8", newline="\n")
    errors = check(inventory)
    assert any("pack-to-code: flow" in error for error in errors)
    assert any("code-to-pack: renamed" in error for error in errors)


def test_locator_must_include_definition_name(inventory):
    pack = inventory[2]
    pack.write_text(pack.read_text(encoding="utf-8").replace("L1-L2", "L2-L2"), encoding="utf-8", newline="\n")
    assert any("locator: flow" in error and "definition" in error for error in check(inventory))


def test_private_helpers_excluded_public_methods_included(inventory):
    inventory[1].write_text(
        "def flow():\n    return 1\n"
        "def _helper():\n    pass\n"
        "class Result:\n    def area(self):\n        pass\n"
        "    def _internal(self):\n        pass\n",
        encoding="utf-8", newline="\n",
    )
    errors = check(inventory)
    assert any("code-to-pack: Result " in error for error in errors)
    assert any("code-to-pack: Result.area " in error for error in errors)
    assert all("_helper" not in error and "_internal" not in error for error in errors)


def test_no_package_import_and_cli_status(inventory):
    root, source, pack = inventory
    source.write_text("raise RuntimeError('must not import')\ndef flow():\n    return 1\n", encoding="utf-8", newline="\n")
    pack.write_text(pack.read_text(encoding="utf-8").replace("L1-L2", "L2-L3"), encoding="utf-8", newline="\n")
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), "--pack", str(pack)],
        capture_output=True, text=True, encoding="utf-8", check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    pack.write_text(pack.read_text(encoding="utf-8").replace("`flow`", "`absent`"), encoding="utf-8", newline="\n")
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "--root", str(root), "--pack", str(pack)],
        capture_output=True, text=True, encoding="utf-8", check=False,
    )
    assert result.returncode == 1
    assert "agent-pack.md" in result.stdout and "pack-to-code: absent" in result.stdout


def test_missing_table_cannot_silently_pass(inventory):
    inventory[2].write_text("## 2. Inputs\nNo list.\n## 3. Method\n", encoding="utf-8", newline="\n")
    assert any("pack-to-code: <section-2>" in error for error in check(inventory))


def test_new_packs_match_code():
    checker = runpy.run_path(str(SCRIPT))["check_pack"]
    for module in ("capping_stack", "hipps"):
        pack = ROOT / f"src/digitalmodel/subsea/{module}/docs/agent-pack.md"
        assert checker(pack, ROOT) == []


def test_private_definition_exported_by_initializer_is_public(inventory):
    root, source, _ = inventory
    (source.parent / "__init__.py").write_text(
        "from .model import _exported\n__all__ = ['_exported']\n",
        encoding="utf-8", newline="\n",
    )
    source.write_text(source.read_text(encoding="utf-8") + "def _exported():\n    pass\n", encoding="utf-8", newline="\n")
    assert any("code-to-pack: _exported" in error for error in check(inventory))


def test_duplicate_function_names_need_separate_source_rows(inventory):
    _, source, _ = inventory
    (source.parent / "second.py").write_text("def flow():\n    pass\n", encoding="utf-8", newline="\n")
    errors = check(inventory)
    assert any("code-to-pack: flow" in error and "second.py" in error for error in errors)


def test_same_name_text_without_definition_does_not_satisfy_locator(inventory):
    _, source, pack = inventory
    source.write_text("def flow():\n    return 1\n# flow\n", encoding="utf-8", newline="\n")
    pack.write_text(pack.read_text(encoding="utf-8").replace("L1-L2", "L3-L3"), encoding="utf-8", newline="\n")
    assert any("locator: flow" in error for error in check(inventory))


def test_locator_outside_package_fails(inventory):
    root, _, pack = inventory
    (root / "elsewhere.py").write_text("def flow():\n    pass\n", encoding="utf-8", newline="\n")
    pack.write_text(pack.read_text(encoding="utf-8").replace("src/example/model.py", "elsewhere.py"), encoding="utf-8", newline="\n")
    assert any("locator: flow" in error and "outside package" in error
               for error in check(inventory))


def test_module_exports_do_not_expose_unrelated_private_definitions(inventory):
    root, source, _ = inventory
    source.write_text(
        "def _helper():\n    pass\n__all__ = ['_helper']\n",
        encoding="utf-8", newline="\n",
    )
    (source.parent / "second.py").write_text(
        "def _helper():\n    pass\n", encoding="utf-8", newline="\n",
    )
    definitions = runpy.run_path(str(SCRIPT))["public_definitions"](source.parent, root)
    assert definitions == {("src/example/model.py", "_helper"): 1}


@pytest.mark.parametrize("import_source", [".model", "example.model"])
def test_aliased_local_reexport_resolves_original_definition(inventory, import_source):
    root, source, _ = inventory
    source.write_text("def _hidden():\n    pass\n", encoding="utf-8", newline="\n")
    (source.parent / "__init__.py").write_text(
        f"from {import_source} import _hidden as exposed\n__all__ = ['exposed']\n",
        encoding="utf-8", newline="\n",
    )
    (source.parent / "second.py").write_text(
        "def _hidden():\n    pass\n", encoding="utf-8", newline="\n",
    )
    definitions = runpy.run_path(str(SCRIPT))["public_definitions"](source.parent, root)
    assert definitions == {("src/example/model.py", "_hidden"): 1}


def test_chained_relative_reexport_resolves_nested_source(inventory):
    root, source, _ = inventory
    nested = source.parent / "nested"
    nested.mkdir()
    source.write_text("# no definitions\n", encoding="utf-8", newline="\n")
    (nested / "__init__.py").write_text("", encoding="utf-8", newline="\n")
    (nested / "model.py").write_text(
        "def _hidden():\n    pass\n", encoding="utf-8", newline="\n",
    )
    (source.parent / "bridge.py").write_text(
        "from .nested.model import _hidden as exposed\n", encoding="utf-8", newline="\n",
    )
    (source.parent / "__init__.py").write_text(
        "from .bridge import exposed\n__all__ = ['exposed']\n",
        encoding="utf-8", newline="\n",
    )
    definitions = runpy.run_path(str(SCRIPT))["public_definitions"](source.parent, root)
    assert definitions == {("src/example/nested/model.py", "_hidden"): 1}


def test_module_export_does_not_expose_private_class_member(inventory):
    root, source, _ = inventory
    source.write_text(
        "def _helper():\n    pass\n__all__ = ['_helper']\n"
        "class Result:\n    def _helper(self):\n        pass\n",
        encoding="utf-8", newline="\n",
    )
    definitions = runpy.run_path(str(SCRIPT))["public_definitions"](source.parent, root)
    assert definitions == {
        ("src/example/model.py", "_helper"): 1,
        ("src/example/model.py", "Result"): 4,
    }
