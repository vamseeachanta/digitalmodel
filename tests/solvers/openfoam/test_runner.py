#!/usr/bin/env python3
"""
ABOUTME: Unit tests for the fail-closed OpenFOAM solver runner. OpenFOAM is not
required: these cover case validation, solver detection from controlDict, the
fail-closed DRY_RUN path when no install is present, and the staged execution
path with subprocess mocked.
"""

from __future__ import annotations

import subprocess
from pathlib import Path

import pytest

from digitalmodel.solvers.openfoam.runner import (
    OpenFOAMRunConfig,
    OpenFOAMRunner,
    OpenFOAMRunStatus,
)


# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------
def _make_case(root: Path, *, application: str = "interFoam") -> Path:
    """Create a minimal but structurally-valid OpenFOAM case directory."""
    case = root / "case"
    for sub in ("system", "constant", "0"):
        (case / sub).mkdir(parents=True)
    (case / "system" / "controlDict").write_text(
        "FoamFile { object controlDict; }\n"
        f"application     {application};\n"
        "startTime       0;\n"
    )
    return case


# ---------------------------------------------------------------------------
# case validation (no OpenFOAM needed)
# ---------------------------------------------------------------------------
class TestCaseValidation:
    def test_missing_case_dir_fails_closed(self, tmp_path: Path):
        result = OpenFOAMRunner().run(tmp_path / "does_not_exist")
        assert result.status is OpenFOAMRunStatus.FAILED
        assert "not found" in result.error_message

    def test_missing_required_subdir_fails(self, tmp_path: Path):
        case = tmp_path / "case"
        (case / "system").mkdir(parents=True)
        (case / "system" / "controlDict").write_text("application interFoam;")
        # constant/ and 0/ are absent
        result = OpenFOAMRunner().run(case)
        assert result.status is OpenFOAMRunStatus.FAILED
        assert "constant" in result.error_message

    def test_missing_control_dict_fails(self, tmp_path: Path):
        case = tmp_path / "case"
        for sub in ("system", "constant", "0"):
            (case / sub).mkdir(parents=True)
        result = OpenFOAMRunner().run(case)
        assert result.status is OpenFOAMRunStatus.FAILED
        assert "controlDict" in result.error_message


# ---------------------------------------------------------------------------
# solver detection
# ---------------------------------------------------------------------------
class TestSolverDetection:
    def test_reads_application_from_control_dict(self, tmp_path: Path):
        case = _make_case(tmp_path, application="simpleFoam")
        # dry-run avoids needing an install; solver is still resolved.
        result = OpenFOAMRunner(OpenFOAMRunConfig(dry_run=True)).run(case)
        assert result.solver == "simpleFoam"
        assert result.status is OpenFOAMRunStatus.DRY_RUN

    def test_explicit_solver_overrides_control_dict(self, tmp_path: Path):
        case = _make_case(tmp_path, application="interFoam")
        cfg = OpenFOAMRunConfig(solver="pimpleFoam", dry_run=True)
        result = OpenFOAMRunner(cfg).run(case)
        assert result.solver == "pimpleFoam"

    def test_unresolvable_solver_fails(self, tmp_path: Path):
        case = tmp_path / "case"
        for sub in ("system", "constant", "0"):
            (case / sub).mkdir(parents=True)
        (case / "system" / "controlDict").write_text("startTime 0;\n")  # no application
        result = OpenFOAMRunner().run(case)
        assert result.status is OpenFOAMRunStatus.FAILED
        assert "solver" in result.error_message.lower()


# ---------------------------------------------------------------------------
# fail-closed when OpenFOAM is absent
# ---------------------------------------------------------------------------
class TestFailClosed:
    def test_no_install_falls_back_to_dry_run(self, tmp_path: Path, monkeypatch):
        case = _make_case(tmp_path)
        # Force "no OpenFOAM on PATH".
        monkeypatch.setattr(
            "digitalmodel.solvers.openfoam.runner.shutil.which", lambda _: None
        )
        result = OpenFOAMRunner().run(case)
        assert result.status is OpenFOAMRunStatus.DRY_RUN
        assert "No OpenFOAM" in result.error_message
        assert result.stages == []  # nothing executed


# ---------------------------------------------------------------------------
# staged execution (subprocess mocked — no OpenFOAM needed)
# ---------------------------------------------------------------------------
class TestStagedExecution:
    def _patch(self, monkeypatch, *, returncode=0, stdout="end\n"):
        monkeypatch.setattr(
            "digitalmodel.solvers.openfoam.runner.shutil.which",
            lambda name: f"/usr/bin/{name}",
        )

        def fake_run(argv, **kwargs):
            return subprocess.CompletedProcess(argv, returncode, stdout=stdout, stderr="")

        monkeypatch.setattr(
            "digitalmodel.solvers.openfoam.runner.subprocess.run", fake_run
        )

    def test_happy_path_runs_mesh_solve_vtk(self, tmp_path: Path, monkeypatch):
        case = _make_case(tmp_path, application="interFoam")
        self._patch(monkeypatch)
        result = OpenFOAMRunner().run(case)
        assert result.status is OpenFOAMRunStatus.COMPLETED
        names = [s.name for s in result.stages]
        assert names == ["blockMesh", "interFoam", "foamToVTK"]
        assert all(s.ok for s in result.stages)
        # logs were written
        assert (case / "log.blockMesh").is_file()
        assert (case / "log.interFoam").is_file()

    def test_snappy_inserted_after_blockmesh(self, tmp_path: Path, monkeypatch):
        case = _make_case(tmp_path, application="simpleFoam")
        self._patch(monkeypatch)
        cfg = OpenFOAMRunConfig(run_snappy=True, to_vtk=False)
        result = OpenFOAMRunner(cfg).run(case)
        names = [s.name for s in result.stages]
        assert names == ["blockMesh", "snappyHexMesh", "simpleFoam"]

    def test_nonzero_return_code_fails_and_stops(self, tmp_path: Path, monkeypatch):
        case = _make_case(tmp_path)
        self._patch(monkeypatch, returncode=1)
        result = OpenFOAMRunner().run(case)
        assert result.status is OpenFOAMRunStatus.FAILED
        # stops at the first failing stage (blockMesh) — no solver stage
        assert len(result.stages) == 1
        assert result.stages[0].name == "blockMesh"

    def test_fatal_error_in_log_detected_despite_rc_zero(
        self, tmp_path: Path, monkeypatch
    ):
        case = _make_case(tmp_path)
        self._patch(monkeypatch, returncode=0, stdout="--> FOAM FATAL ERROR\n")
        result = OpenFOAMRunner().run(case)
        assert result.status is OpenFOAMRunStatus.FAILED
        assert "FOAM FATAL ERROR" in result.stages[0].error_message
