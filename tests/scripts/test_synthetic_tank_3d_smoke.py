"""CLI contract tests for the synthetic tank 3D smoke entry point."""

from __future__ import annotations

import importlib.util
import sys
from types import SimpleNamespace
from pathlib import Path

import pytest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/cfd/run_synthetic_tank_3d_smoke.py"


def load_script():
    spec = importlib.util.spec_from_file_location("synthetic_tank_3d_smoke", SCRIPT)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_cli_requires_dispatcher_rank_binding(monkeypatch: pytest.MonkeyPatch) -> None:
    script = load_script()
    monkeypatch.delenv("CFD_DISPATCH_RANKS", raising=False)

    assert script.main(["--ranks", "8", "--work-dir", "/tmp/synthetic-smoke"]) == 2


def test_cli_rejects_rank_different_from_dispatcher(monkeypatch: pytest.MonkeyPatch) -> None:
    script = load_script()
    monkeypatch.setenv("CFD_DISPATCH_RANKS", "8")

    assert script.main(["--ranks", "4", "--work-dir", "/tmp/synthetic-smoke"]) == 2


def test_cli_uses_dispatcher_rank_when_explicit_rank_is_omitted(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    script = load_script()
    monkeypatch.setenv("CFD_DISPATCH_RANKS", "8")
    monkeypatch.setattr(script, "run_pipeline", lambda **kwargs: kwargs["ranks"])

    assert script.main(["--work-dir", "/tmp/synthetic-smoke"]) == 0


def test_toolchain_uses_installed_gmsh_version(monkeypatch: pytest.MonkeyPatch) -> None:
    script = load_script()
    monkeypatch.setitem(sys.modules, "gmsh", SimpleNamespace(__version__="4.15.1"))

    assert script._toolchain().gmsh_version == "4.15.1"
