#!/usr/bin/env python3
"""
ABOUTME: Unit tests for the OpenFOAM CFD doctor. OpenFOAM is not required — the
toolchain probes (shutil.which, importlib, /proc/meminfo) are mocked so both the
solver-capable and dry-run-only verdicts are exercised deterministically.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam import doctor as doc


def _statuses(checks):
    return {c.name: c.status for c in checks}


class TestDryRunOnlyHost:
    """No OpenFOAM on PATH -> dry-run-only, but not a hard failure."""

    def test_no_openfoam_is_dry_run_only(self, tmp_path: Path, monkeypatch):
        monkeypatch.setattr(doc.shutil, "which", lambda _name: None)
        monkeypatch.setattr(doc, "_module_available", lambda _m: True)
        checks, capability = doc.run_doctor(output_dir=tmp_path)
        assert capability == "dry-run-only"
        # missing solver is a WARN, never a hard FAIL (case-gen still works)
        assert not doc.has_failure(checks)
        st = _statuses(checks)
        assert st["OpenFOAM utilities"] == "WARN"
        assert st["Host capability"] == "WARN"


class TestSolverCapableHost:
    """All utilities + solvers present -> solver-capable."""

    def test_full_toolchain_is_solver_capable(self, tmp_path: Path, monkeypatch):
        monkeypatch.setattr(doc.shutil, "which", lambda name: f"/usr/bin/{name}")
        monkeypatch.setattr(doc, "_module_available", lambda _m: True)
        monkeypatch.setenv("WM_PROJECT_VERSION", "v2406")
        checks, capability = doc.run_doctor(output_dir=tmp_path)
        assert capability == "solver-capable"
        assert not doc.has_failure(checks)
        st = _statuses(checks)
        assert st["OpenFOAM utilities"] == "PASS"
        assert st["OpenFOAM solvers"] == "PASS"
        assert st["OpenFOAM version"] == "PASS"
        assert st["Host capability"] == "PASS"


class TestHardFailures:
    """Required Python deps or an unwritable output root are hard FAILs."""

    def test_missing_pyvista_is_hard_failure(self, tmp_path: Path, monkeypatch):
        monkeypatch.setattr(doc.shutil, "which", lambda name: f"/usr/bin/{name}")
        monkeypatch.setattr(
            doc, "_module_available", lambda m: m != "pyvista"
        )
        checks, _ = doc.run_doctor(output_dir=tmp_path)
        assert doc.has_failure(checks)
        assert _statuses(checks)["Python: pyvista"] == "FAIL"

    def test_unwritable_output_root_is_hard_failure(self, monkeypatch):
        monkeypatch.setattr(doc.shutil, "which", lambda name: f"/usr/bin/{name}")
        monkeypatch.setattr(doc, "_module_available", lambda _m: True)
        # Point at an existing path, force the write probe to fail.
        def boom(*_a, **_k):
            raise OSError("read-only filesystem")

        monkeypatch.setattr(doc.tempfile, "NamedTemporaryFile", boom)
        checks = [doc._check_writable(Path("/"))]
        assert doc.has_failure(checks)
        assert checks[0].status == "FAIL"

    def test_absent_output_root_is_warn_not_fail(self, tmp_path: Path):
        check = doc._check_writable(tmp_path / "does_not_exist")
        assert check.status == "WARN"


class TestMissingGmshIsWarn:
    """gmsh is optional (meshing) -> WARN, not FAIL."""

    def test_missing_gmsh_warns(self, tmp_path: Path, monkeypatch):
        monkeypatch.setattr(doc.shutil, "which", lambda name: f"/usr/bin/{name}")
        monkeypatch.setattr(doc, "_module_available", lambda m: m != "gmsh")
        checks, _ = doc.run_doctor(output_dir=tmp_path)
        assert _statuses(checks)["Python: gmsh"] == "WARN"
        assert not doc.has_failure(checks)
