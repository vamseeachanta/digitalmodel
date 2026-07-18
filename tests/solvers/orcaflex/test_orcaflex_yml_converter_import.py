"""Regression tests for orcaflex_yml_converter import behavior.

Regression: the module used to call sys.exit(1) at import time when
OrcFxAPI was absent, killing any host process (pytest collection, tool
enumeration) that merely imported it. It must instead follow the sibling
convention (e.g. orcaflex_utilities.py): catch ImportError and defer the
availability check to call time.
"""

import importlib
import subprocess
import sys

import pytest


MODULE = "digitalmodel.solvers.orcaflex.orcaflex_yml_converter"


def test_import_does_not_kill_interpreter():
    """Importing in a fresh interpreter must not exit non-zero."""
    proc = subprocess.run(
        [
            sys.executable,
            "-c",
            f"import {MODULE} as m; print('IMPORT_OK', m.OrcFxAPI is None)",
        ],
        capture_output=True,
        text=True,
    )
    assert proc.returncode == 0, (
        f"import exited rc={proc.returncode}: {proc.stdout} {proc.stderr}"
    )
    assert "IMPORT_OK" in proc.stdout


def test_in_process_import_no_systemexit():
    """In-process import (as pytest collection would do) must succeed."""
    module = importlib.import_module(MODULE)
    assert hasattr(module, "convert_to_yml")


def test_convert_returns_false_without_orcfxapi(tmp_path):
    """Without OrcFxAPI, convert_to_yml fails loudly but gracefully."""
    module = importlib.import_module(MODULE)
    if module.OrcFxAPI is not None:
        pytest.skip("OrcFxAPI installed on this machine")

    dat = tmp_path / "model.dat"
    dat.write_text("dummy")
    assert module.convert_to_yml(str(dat)) is False


def test_main_returns_error_without_orcfxapi(monkeypatch):
    """CLI entry returns exit code 1 (not process kill) without OrcFxAPI."""
    module = importlib.import_module(MODULE)
    if module.OrcFxAPI is not None:
        pytest.skip("OrcFxAPI installed on this machine")

    monkeypatch.setattr(sys, "argv", ["orcaflex_yml_converter.py", "x.dat"])
    assert module.main() == 1
