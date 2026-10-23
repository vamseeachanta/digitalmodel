"""Inventory expectations are derived from the literal fixture source below."""

import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[2]


def auditor():
    spec = importlib.util.spec_from_file_location(
        "audit_runtime_asserts", ROOT / "scripts/audit_runtime_asserts.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_inventory_uses_syntax_without_importing_or_counting_comments(tmp_path):
    """Two real assertions exist; the comment and docstring contain no AST Assert."""
    source = tmp_path / "src" / "sample.py"
    source.parent.mkdir()
    source.write_text(
        '"""assert digest == expected"""\n'
        '# assert identity == expected\n'
        'raise RuntimeError("must never import this source")\n'
        'def load():\n'
        '    assert (\n'
        '        digest == expected\n'
        '    ), "changed evidence"\n'
        '    assert source_id in resolver\n',
        encoding="utf-8",
    )
    result = auditor().inventory(tmp_path, ["src"])
    assert result["files_scanned"] == ["src/sample.py"]
    assert result["assertions"] == [
        {"path": "src/sample.py", "line": 5, "scope": "load",
         "test": "digest == expected", "message": "'changed evidence'"},
        {"path": "src/sample.py", "line": 8, "scope": "load",
         "test": "source_id in resolver", "message": None},
    ]


def test_inventory_preserves_nested_scope_and_deduplicates_paths(tmp_path):
    """A method, a nested function and a module each supply one assertion."""
    (tmp_path / "example.py").write_text(
        'class Reader:\n'
        '    def load(self):\n'
        '        assert self.source\n'
        '        def verify():\n'
        '            assert self.digest\n'
        'assert ready\n',
        encoding="utf-8",
    )
    result = auditor().inventory(tmp_path, [".", "example.py"])
    assert result["files_scanned"] == ["example.py"]
    assert [row["scope"] for row in result["assertions"]] == [
        "Reader.load", "Reader.load.verify", "<module>"
    ]


def test_inventory_normalizes_aliases_and_supports_async_definitions(tmp_path):
    (tmp_path / "src").mkdir()
    (tmp_path / "reader.py").write_text(
        "async def load():\n    assert identity\n", encoding="utf-8"
    )
    result = auditor().inventory(tmp_path, ["reader.py", "src/../reader.py"])
    assert result["files_scanned"] == ["reader.py"]
    assert len(result["assertions"]) == 1
    assert result["assertions"][0]["scope"] == "load"


def test_inventory_does_not_report_success_for_missing_or_invalid_source(tmp_path):
    with pytest.raises(FileNotFoundError):
        auditor().inventory(tmp_path, ["missing"])
    (tmp_path / "broken.py").write_text("def broken(\n", encoding="utf-8")
    with pytest.raises(SyntaxError):
        auditor().inventory(tmp_path, ["."])


def test_inventory_refuses_source_outside_requested_root(tmp_path):
    outside = tmp_path / "outside.py"
    outside.write_text("assert digest\n", encoding="utf-8")
    root = tmp_path / "root"
    root.mkdir()
    with pytest.raises(ValueError, match="outside inventory root"):
        auditor().inventory(root, ["../outside.py"])


def test_inventory_reads_declared_python_source_encoding(tmp_path):
    (tmp_path / "encoded.py").write_bytes(
        b'# coding: latin-1\nassert digest, "chang\xe9"\n'
    )
    result = auditor().inventory(tmp_path, ["."])
    assert result["assertions"][0]["message"] == "'chang\u00e9'"


@pytest.mark.parametrize("mode", [(), ("-O",), ("-OO",)])
def test_cli_inventories_asserts_even_when_interpreter_optimization_is_enabled(mode, tmp_path):
    """AST source inspection must retain the single literal assertion in every mode."""
    (tmp_path / "example.py").write_text("assert digest\n", encoding="utf-8")
    env = os.environ.copy()
    env.pop("PYTHONOPTIMIZE", None)
    result = subprocess.run(
        [sys.executable, *mode, str(ROOT / "scripts/audit_runtime_asserts.py"),
         "--root", str(tmp_path), "."],
        env=env, capture_output=True, text=True, check=True,
    )
    assert json.loads(result.stdout) == auditor().inventory(tmp_path, ["."])
