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
    assert json.loads(result.stdout) == {
        "files_scanned": ["example.py"],
        "assertions": [
            {"path": "example.py", "line": 1, "scope": "<module>",
             "test": "digest", "message": None},
        ],
    }


@pytest.mark.parametrize("error_type", [PermissionError, OSError])
def test_inventory_propagates_directory_enumeration_errors(tmp_path, monkeypatch, error_type):
    """Failure to enumerate a nested directory cannot establish complete coverage."""
    blocked = tmp_path / "blocked"
    blocked.mkdir()
    (blocked / "hidden.py").write_text("assert digest\n", encoding="utf-8")
    original_scandir = os.scandir

    def scandir(path):
        if Path(path) == blocked:
            raise error_type("directory enumeration failed")
        return original_scandir(path)

    monkeypatch.setattr(os, "scandir", scandir)
    with pytest.raises(error_type, match="directory enumeration failed"):
        auditor().inventory(tmp_path, ["."])


@pytest.mark.parametrize("mode", [(), ("-O",), ("-OO",)])
def test_cli_refuses_inventory_when_directory_enumeration_fails(mode, tmp_path):
    """Inject an enumeration refusal independently of filesystem privilege levels."""
    (tmp_path / "visible.py").write_text("assert ready\n", encoding="utf-8")
    blocked = tmp_path / "blocked"
    blocked.mkdir()
    (blocked / "hidden.py").write_text("assert digest\n", encoding="utf-8")
    program = '''
import os
from pathlib import Path
import runpy
import sys

script, root = sys.argv[1:]
blocked = Path(root) / "blocked"
original_scandir = os.scandir

def scandir(path):
    if Path(path) == blocked:
        raise PermissionError("directory enumeration failed")
    return original_scandir(path)

os.scandir = scandir
sys.argv = [script, "--root", root, "."]
runpy.run_path(script, run_name="__main__")
'''
    env = os.environ.copy()
    env.pop("PYTHONOPTIMIZE", None)
    result = subprocess.run(
        [sys.executable, *mode, "-c", program,
         str(ROOT / "scripts/audit_runtime_asserts.py"), str(tmp_path)],
        env=env, capture_output=True, text=True, check=False,
    )
    assert result.returncode != 0
    assert result.stdout == ""
    assert "PermissionError: directory enumeration failed" in result.stderr


@pytest.mark.parametrize("outside", [False, True])
def test_inventory_does_not_traverse_directory_symlinks(tmp_path, outside):
    """Nested directory links remain outside the recursive inventory boundary."""
    root = tmp_path / "root"
    root.mkdir()
    target = (tmp_path if outside else root) / "target"
    target.mkdir()
    (target / "example.py").write_text("assert digest\n", encoding="utf-8")
    (root / "linked").symlink_to(target, target_is_directory=True)
    result = auditor().inventory(root, ["."])
    assert result == {
        "files_scanned": [] if outside else ["target/example.py"],
        "assertions": [] if outside else [
            {"path": "target/example.py", "line": 1, "scope": "<module>",
             "test": "digest", "message": None},
        ],
    }
