"""Legacy pool and MPI execution-order characterization."""

import json
from pathlib import Path
from unittest.mock import Mock, patch

import pytest
import yaml

from digitalmodel.workflows import openfoam_run_batch as ofb

EXAMPLE_DIR = (
    Path(__file__).resolve().parents[2]
    / "examples"
    / "workflows"
    / "openfoam-run-batch"
)


def _example_cfg(tmp_path: Path) -> dict:
    cfg = yaml.safe_load((EXAMPLE_DIR / "input.yml").read_text())
    cfg["_config_dir_path"] = str(tmp_path)
    return cfg


def _mpi_item(tmp_path: Path) -> dict:
    base = {"case_type": "current_loading", "solver": "interFoam"}
    return ofb._render_cases(base, [{}], {}, tmp_path / "work")[0]


def test_no_solver_and_mock_false_fails_fast(tmp_path):
    cfg = _example_cfg(tmp_path)
    cfg["openfoam_run_batch"]["run_batch"]["mock"] = False
    with patch.object(ofb.shutil, "which", return_value=None):
        with pytest.raises(RuntimeError, match="requires-solver"):
            ofb.router(cfg)


def test_mpi_command_plan_shape():
    plan = ofb.mpi_command_plan(solver="interFoam", workers=16, reconstruct=True)
    names = [argv[0] for argv in plan]
    assert names.index("decomposePar") < names.index("mpirun")
    mpirun = next(argv for argv in plan if argv[0] == "mpirun")
    assert mpirun[:3] == ["mpirun", "-np", "16"]
    assert "-parallel" in mpirun and "interFoam" in mpirun
    assert names[-1] == "reconstructPar"
    assert ofb.mpi_command_plan("interFoam", 4, reconstruct=False)[-1][0] == "mpirun"


def test_mpi_run_executes_plan_and_prunes(tmp_path):
    item = _mpi_item(tmp_path)
    recorded = []

    def fake_runner(argv, cwd, _log, _timeout):
        recorded.append(argv)
        if argv[0] == "decomposePar":
            (Path(cwd) / "processor0").mkdir(parents=True, exist_ok=True)
            (Path(cwd) / "processor1").mkdir(parents=True, exist_ok=True)
        return 0

    row = ofb._run_case_mpi(
        item,
        {"reconstruct": True},
        workers=8,
        mock=False,
        command_runner=fake_runner,
    )
    names = [argv[0] for argv in recorded]
    assert row["status"] == "completed"
    assert names.index("decomposePar") < names.index("mpirun")
    assert recorded[names.index("mpirun")][:3] == ["mpirun", "-np", "8"]
    assert not list(item["work_dir"].glob("processor*"))
    assert "numberOfSubdomains 8" in (
        item["work_dir"] / "system" / "decomposeParDict"
    ).read_text()


def test_mpi_reconstruct_false_preserves_processor_dirs(tmp_path):
    item = _mpi_item(tmp_path)
    recorded = []

    def fake_runner(argv, cwd, _log, _timeout):
        recorded.append(argv)
        if argv[0] == "decomposePar":
            (Path(cwd) / "processor0").mkdir(parents=True, exist_ok=True)
        return 0

    row = ofb._run_case_mpi(
        item,
        {"reconstruct": False},
        workers=4,
        mock=False,
        command_runner=fake_runner,
    )
    assert row["status"] == "completed"
    assert "reconstructPar" not in [argv[0] for argv in recorded]
    assert (item["work_dir"] / "processor0").is_dir()


def test_mpi_stage_failure_fails_case_and_is_retried(tmp_path):
    item = _mpi_item(tmp_path)

    def failing_runner(argv, cwd, _log, _timeout):
        if argv[0] == "decomposePar":
            (Path(cwd) / "processor0").mkdir(parents=True, exist_ok=True)
        return 1 if argv[0] == "mpirun" else 0

    row = ofb._run_case_mpi(
        item, {}, workers=2, mock=False, command_runner=failing_runner
    )
    assert row["status"] == "failed"
    assert "mpirun" in row["error"]
    checkpoint = json.loads((item["work_dir"] / "_result.json").read_text())
    assert checkpoint["status"] == "failed"
    assert (item["work_dir"] / "processor0").is_dir()
    row = ofb._run_case_mpi(
        item,
        {},
        workers=2,
        mock=False,
        command_runner=lambda *_args: 0,
    )
    assert row["status"] == "completed"


def test_mpi_resume_restarts_from_latest_time(tmp_path):
    item = _mpi_item(tmp_path)
    case_dir = item["work_dir"]
    (case_dir / "system").mkdir(parents=True)
    (case_dir / "system" / "controlDict").write_text(
        "application     interFoam;\nstartFrom       startTime;\n"
    )
    (case_dir / "processor0").mkdir()
    recorded = []

    def fake_runner(argv, _cwd, _log, _timeout):
        recorded.append(argv)
        return 0

    with patch.object(
        ofb, "_build_case", side_effect=AssertionError("resume must not rebuild")
    ):
        row = ofb._run_case_mpi(
            item,
            {"resume": True, "reconstruct": True},
            workers=4,
            mock=False,
            command_runner=fake_runner,
        )
    assert row["status"] == "completed"
    assert [argv[0] for argv in recorded] == ["mpirun", "reconstructPar"]
    assert "latestTime" in (case_dir / "system" / "controlDict").read_text()
    assert ofb.mpi_command_plan("interFoam", 4, resume=True)[0][0] == "mpirun"


def test_solver_ready_requires_reconstructpar_for_mpi(monkeypatch):
    present = {"blockMesh", "interFoam", "decomposePar", "mpirun"}
    monkeypatch.setattr(
        ofb.shutil,
        "which",
        lambda executable: "/usr/bin/stub" if executable in present else None,
    )
    assert not ofb._solver_ready("mpi", "blockMesh", "interFoam", True)
    assert ofb._solver_ready("mpi", "blockMesh", "interFoam", False)
    assert ofb._solver_ready("pool", "blockMesh", "interFoam", True)


def test_mpi_runner_honors_legacy_execute_plan_monkeypatch(monkeypatch, tmp_path):
    item = _mpi_item(tmp_path)
    expected = ofb._row(
        item,
        status="completed",
        case_dir=item["work_dir"],
        solver="sentinel",
    )
    execute = Mock(return_value=expected)
    monkeypatch.setattr(ofb, "_execute_mpi_plan", execute)
    row = ofb._run_case_mpi(
        item,
        {"reconstruct": False},
        workers=2,
        mock=False,
        command_runner=lambda *_args: 0,
    )
    assert row["solver"] == "sentinel"
    execute.assert_called_once()
