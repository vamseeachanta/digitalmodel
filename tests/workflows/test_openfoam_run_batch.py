"""Solver-free (mock-mode) tests for the openfoam_run_batch workflow (#1560).

No OpenFOAM on PATH is needed: mock mode authors each case tree with the
license-free builder but runs no solver, and the MPI command plan is asserted
via an injected command runner. Mirrors the #1558 orcaflex_run_batch tests.
"""

import json
import os
from pathlib import Path
from unittest.mock import patch

import pandas as pd
import pytest
import yaml

from digitalmodel.workflows import openfoam_run_batch as ofb

REPO_ROOT = Path(__file__).resolve().parents[2]
EXAMPLE_DIR = REPO_ROOT / "examples" / "workflows" / "openfoam-run-batch"


def _example_cfg(tmp_path: Path) -> dict:
    cfg = yaml.safe_load((EXAMPLE_DIR / "input.yml").read_text())
    cfg["_config_dir_path"] = str(tmp_path)
    return cfg


# --------------------------------------------------------------------------- #
#  example input: mock batch end-to-end                                       #
# --------------------------------------------------------------------------- #
def test_example_input_runs_mock_batch(tmp_path):
    cfg = _example_cfg(tmp_path)

    result = ofb.router(cfg)

    manifest = tmp_path / "results" / "cases.csv"
    summary_path = tmp_path / "results" / "batch_summary.json"
    assert manifest.exists()
    assert summary_path.exists()

    rows = pd.read_csv(manifest)
    assert len(rows) == 2
    assert rows["status"].tolist() == ["completed", "completed"]
    assert rows["name"].tolist() == ["current_simpleFoam", "current_pimpleFoam"]
    # The swept knob column AND the reserved actual-solver column both land.
    assert sorted(rows["solver_app"].tolist()) == ["pimpleFoam", "simpleFoam"]
    assert sorted(rows["solver"].tolist()) == ["pimpleFoam", "simpleFoam"]
    assert bool(rows["mock"].all())

    summary = json.loads(summary_path.read_text())
    assert summary["total_cases"] == 2
    assert summary["completed"] == 2
    assert summary["failed"] == 0
    assert summary["mode"] == "pool"
    assert summary["workers"] == 2
    assert summary["mock"] is True
    assert summary["host_cpu_count"] == os.cpu_count()
    assert summary["timeout_seconds"] == 43200

    # The license-free builder authored a real case tree per case, under the
    # work dir (never under results/).
    for name in ("current_simpleFoam", "current_pimpleFoam"):
        assert (tmp_path / "batch_runs" / name / "system" / "controlDict").is_file()
    # Atomic checkpoint writes leave no temp files behind.
    assert not list((tmp_path / "batch_runs").rglob("*.tmp"))

    outputs = result["openfoam_run_batch"]["outputs"]
    assert outputs["manifest"] == str(manifest)
    assert outputs["summary"] == str(summary_path)


# --------------------------------------------------------------------------- #
#  matrix determinism                                                         #
# --------------------------------------------------------------------------- #
def test_case_matrix_is_deterministic(tmp_path):
    # A yaml_matrix variants block reuses parametric_run's _load_cases and
    # accepts string knobs (factorial's ParameterSweep is numeric-only).
    base = {"case_type": "current_loading", "solver": "simpleFoam"}
    variants = {
        "source": "yaml_matrix",
        "list": [{"solver_app": "simpleFoam"}, {"solver_app": "pimpleFoam"}],
        "mapping": {"solver_app": "solver"},
    }

    first = ofb._render_cases(
        base, ofb._resolve_case_matrix(None, variants, tmp_path),
        variants["mapping"], tmp_path / "w",
    )
    second = ofb._render_cases(
        base, ofb._resolve_case_matrix(None, variants, tmp_path),
        variants["mapping"], tmp_path / "w",
    )
    assert [i["name"] for i in first] == [i["name"] for i in second]
    assert [i["case"] for i in first] == [i["case"] for i in second]
    assert [i["settings"]["solver"] for i in first] == ["simpleFoam", "pimpleFoam"]
    # Deterministic, index-ordered case names.
    assert [i["name"] for i in first] == [
        "current_loading_case_000", "current_loading_case_001",
    ]


def test_variants_yaml_matrix_runs_through_router(tmp_path):
    cfg = _example_cfg(tmp_path)
    settings = cfg["openfoam_run_batch"]
    settings.pop("cases")
    settings.pop("mapping")
    settings["variants"] = {
        "source": "yaml_matrix",
        "list": [{"solver_app": "simpleFoam"}, {"solver_app": "interFoam"}],
        "mapping": {"solver_app": "solver"},
    }

    ofb.router(cfg)

    rows = pd.read_csv(tmp_path / "results" / "cases.csv")
    assert len(rows) == 2
    assert sorted(rows["solver"].tolist()) == ["interFoam", "simpleFoam"]
    assert rows["status"].tolist() == ["completed", "completed"]


def test_explicit_cases_and_variants_are_mutually_exclusive(tmp_path):
    with pytest.raises(ValueError, match="either cases"):
        ofb._resolve_case_matrix([{"name": "a"}], {"source": "factorial"}, tmp_path)


# --------------------------------------------------------------------------- #
#  worker defaults (0.9 x cores, floor, min 1)                                #
# --------------------------------------------------------------------------- #
def test_workers_default_is_ninety_percent_of_cores(monkeypatch):
    monkeypatch.setattr(ofb.os, "cpu_count", lambda: 64)
    assert ofb.resolve_workers({}) == 57
    assert ofb.resolve_workers({"workers": 4}) == 4


def test_workers_default_never_below_one(monkeypatch):
    monkeypatch.setattr(ofb.os, "cpu_count", lambda: 1)
    assert ofb.resolve_workers({}) == 1


def test_workers_default_applied_in_summary(tmp_path, monkeypatch):
    monkeypatch.setattr(ofb.os, "cpu_count", lambda: 4)
    cfg = _example_cfg(tmp_path)
    del cfg["openfoam_run_batch"]["run_batch"]["workers"]

    ofb.router(cfg)

    summary = json.loads((tmp_path / "results" / "batch_summary.json").read_text())
    assert summary["workers"] == 3


# --------------------------------------------------------------------------- #
#  idempotent / resumable checkpoints                                         #
# --------------------------------------------------------------------------- #
def test_completed_checkpoint_is_skipped(tmp_path):
    cfg = _example_cfg(tmp_path)
    # Pre-seed a completed checkpoint for the first case with a sentinel marker.
    case0 = tmp_path / "batch_runs" / "current_simpleFoam"
    case0.mkdir(parents=True)
    (case0 / "_result.json").write_text(
        json.dumps({
            "index": 0, "name": "current_simpleFoam",
            "status": "completed", "solver": "sentinel", "mock": True,
            "error": None, "case_dir": str(case0), "wall_seconds": 0.0,
        })
    )

    build_calls = []
    real_build = ofb._build_case
    with patch.object(ofb, "_build_case",
                      side_effect=lambda item: build_calls.append(item["name"])
                      or real_build(item)):
        ofb.router(cfg)

    # The pre-seeded case came from its checkpoint (never rebuilt); only the
    # other case was built.
    assert build_calls == ["current_pimpleFoam"]
    rows = pd.read_csv(tmp_path / "results" / "cases.csv")
    row0 = rows[rows["name"] == "current_simpleFoam"].iloc[0]
    assert row0["solver"] == "sentinel"


# --------------------------------------------------------------------------- #
#  results contract: only small .csv/.json rollups under results/             #
# --------------------------------------------------------------------------- #
def test_results_dir_holds_only_csv_json_rollups(tmp_path):
    cfg = _example_cfg(tmp_path)
    ofb.router(cfg)

    results = tmp_path / "results"
    files = [p for p in results.rglob("*") if p.is_file()]
    assert files, "expected rollups under results/"
    for path in files:
        assert path.suffix in {".csv", ".json"}, path
        assert path.stat().st_size < 2 * 1024 * 1024
    assert len(files) <= 100
    # No heavy case artifacts ever land under results/.
    assert not list(results.rglob("processor*"))
    assert not list(results.rglob("*.foam"))
    assert not list(results.rglob("VTK"))


# --------------------------------------------------------------------------- #
#  fail-closed: real run with no solver on PATH                               #
# --------------------------------------------------------------------------- #
def test_no_solver_and_mock_false_fails_fast(tmp_path):
    cfg = _example_cfg(tmp_path)
    cfg["openfoam_run_batch"]["run_batch"]["mock"] = False
    with patch.object(ofb.shutil, "which", return_value=None):
        with pytest.raises(RuntimeError, match="requires-solver"):
            ofb.router(cfg)


# --------------------------------------------------------------------------- #
#  engine / registry routing reaches the workflow                             #
# --------------------------------------------------------------------------- #
def test_engine_resolves_basename_to_workflow():
    from digitalmodel.engine import engine

    cfg = {"basename": "openfoam_run_batch", "openfoam_run_batch": {}}
    with patch("digitalmodel.engine.app_manager") as mock_app_manager:
        mock_app_manager.save_cfg.return_value = None
        with patch(
            "digitalmodel.workflows.openfoam_run_batch.router",
            return_value=cfg,
        ) as mock_router:
            result = engine(cfg=cfg, config_flag=False)
    mock_router.assert_called_once()
    assert result is not None


# --------------------------------------------------------------------------- #
#  mpi mode: command construction + processor* prune                          #
# --------------------------------------------------------------------------- #
def test_mpi_command_plan_shape():
    plan = ofb.mpi_command_plan(solver="interFoam", workers=16, reconstruct=True)
    names = [argv[0] for argv in plan]
    assert names.index("decomposePar") < names.index("mpirun")
    mpirun = next(a for a in plan if a[0] == "mpirun")
    assert mpirun[:3] == ["mpirun", "-np", "16"]
    assert "-parallel" in mpirun and "interFoam" in mpirun
    assert names[-1] == "reconstructPar"
    assert ofb.mpi_command_plan("interFoam", 4, reconstruct=False)[-1][0] == "mpirun"


def test_mpi_run_executes_plan_and_prunes(tmp_path):
    base = {"case_type": "current_loading", "solver": "interFoam"}
    item = ofb._render_cases(base, [{}], {}, tmp_path / "work")[0]

    recorded = []

    def fake_runner(argv, cwd, log, timeout):
        recorded.append(argv)
        if argv[0] == "decomposePar":
            # Simulate the decomposed sub-meshes the prune step must remove.
            (Path(cwd) / "processor0").mkdir(parents=True, exist_ok=True)
            (Path(cwd) / "processor1").mkdir(parents=True, exist_ok=True)
        return 0

    row = ofb._run_case_mpi(
        item, {"reconstruct": True}, workers=8, mock=False,
        command_runner=fake_runner,
    )

    assert row["status"] == "completed"
    names = [argv[0] for argv in recorded]
    assert names.index("decomposePar") < names.index("mpirun")
    assert ["mpirun", "-np", "8"] == recorded[names.index("mpirun")][:3]
    # processor* dirs were pruned after the solve.
    assert not list((tmp_path / "work" / item["name"]).glob("processor*"))
    # decomposeParDict was pinned to the rank count.
    dpd = (tmp_path / "work" / item["name"] / "system" / "decomposeParDict").read_text()
    assert "numberOfSubdomains 8" in dpd


def test_mpi_mode_rejects_multi_case_matrix(tmp_path):
    cfg = _example_cfg(tmp_path)
    cfg["openfoam_run_batch"]["run_batch"]["mode"] = "mpi"  # 2-case matrix
    with pytest.raises(ValueError, match="exactly ONE case"):
        ofb.router(cfg)


def test_mpi_reconstruct_false_preserves_processor_dirs(tmp_path):
    # With reconstruct: false the decomposed fields under processor* are the
    # ONLY copy of the solved output — pruning them would destroy the result.
    base = {"case_type": "current_loading", "solver": "interFoam"}
    item = ofb._render_cases(base, [{}], {}, tmp_path / "work")[0]

    recorded = []

    def fake_runner(argv, cwd, log, timeout):
        recorded.append(argv)
        if argv[0] == "decomposePar":
            (Path(cwd) / "processor0").mkdir(parents=True, exist_ok=True)
        return 0

    row = ofb._run_case_mpi(
        item, {"reconstruct": False}, workers=4, mock=False,
        command_runner=fake_runner,
    )

    assert row["status"] == "completed"
    names = [argv[0] for argv in recorded]
    assert "reconstructPar" not in names
    assert (tmp_path / "work" / item["name"] / "processor0").is_dir()


def test_mpi_stage_failure_fails_case_and_is_retried(tmp_path):
    # A non-zero stage exit must mark the case failed (no false "completed"
    # checkpoint), preserve processor* for diagnosis, and a re-run must RETRY
    # (only a completed checkpoint is skipped).
    base = {"case_type": "current_loading", "solver": "interFoam"}
    item = ofb._render_cases(base, [{}], {}, tmp_path / "work")[0]
    case_dir = tmp_path / "work" / item["name"]

    def failing_runner(argv, cwd, log, timeout):
        if argv[0] == "decomposePar":
            (Path(cwd) / "processor0").mkdir(parents=True, exist_ok=True)
        return 1 if argv[0] == "mpirun" else 0

    row = ofb._run_case_mpi(
        item, {}, workers=2, mock=False, command_runner=failing_runner
    )
    assert row["status"] == "failed"
    assert "mpirun" in row["error"]
    checkpoint = json.loads((case_dir / "_result.json").read_text())
    assert checkpoint["status"] == "failed"
    # Decomposed state is kept on failure (diagnosis / potential resume).
    assert (case_dir / "processor0").is_dir()

    # Retry with a healthy runner: the failed checkpoint does NOT short-circuit.
    row2 = ofb._run_case_mpi(
        item, {}, workers=2, mock=False,
        command_runner=lambda argv, cwd, log, timeout: 0,
    )
    assert row2["status"] == "completed"


def test_mpi_resume_restarts_from_latest_time(tmp_path):
    # resume: true + an existing processor* decomposition -> skip mesh and
    # decompose stages, patch controlDict to latestTime, never rebuild.
    base = {"case_type": "current_loading", "solver": "interFoam"}
    item = ofb._render_cases(base, [{}], {}, tmp_path / "work")[0]
    case_dir = tmp_path / "work" / item["name"]
    (case_dir / "system").mkdir(parents=True)
    (case_dir / "system" / "controlDict").write_text(
        "application     interFoam;\nstartFrom       startTime;\n"
    )
    (case_dir / "processor0").mkdir()

    recorded = []

    def fake_runner(argv, cwd, log, timeout):
        recorded.append(argv)
        return 0

    with patch.object(
        ofb, "_build_case",
        side_effect=AssertionError("resume must not rebuild the case"),
    ):
        row = ofb._run_case_mpi(
            item, {"resume": True, "reconstruct": True}, workers=4, mock=False,
            command_runner=fake_runner,
        )

    assert row["status"] == "completed"
    assert [argv[0] for argv in recorded] == ["mpirun", "reconstructPar"]
    assert "latestTime" in (case_dir / "system" / "controlDict").read_text()
    assert ofb.mpi_command_plan("interFoam", 4, resume=True)[0][0] == "mpirun"


def test_failed_pool_checkpoint_is_retried(tmp_path):
    cfg = _example_cfg(tmp_path)
    case0 = tmp_path / "batch_runs" / "current_simpleFoam"
    case0.mkdir(parents=True)
    (case0 / "_result.json").write_text(
        json.dumps({"index": 0, "name": "current_simpleFoam",
                    "status": "failed", "error": "killed"})
    )

    ofb.router(cfg)

    rows = pd.read_csv(tmp_path / "results" / "cases.csv")
    row0 = rows[rows["name"] == "current_simpleFoam"].iloc[0]
    assert row0["status"] == "completed"  # retried, not skipped as failed


def test_corrupt_checkpoint_is_treated_as_absent(tmp_path):
    work = tmp_path / "case"
    work.mkdir()
    (work / "_result.json").write_text('{"status": "compl')  # truncated
    assert ofb._load_checkpoint(work) is None
    (work / "_result.json").write_text("[]")  # valid JSON, wrong shape
    assert ofb._load_checkpoint(work) is None


def test_solver_ready_requires_reconstructpar_for_mpi(monkeypatch):
    present = {"blockMesh", "interFoam", "decomposePar", "mpirun"}
    monkeypatch.setattr(
        ofb.shutil, "which",
        lambda exe: "/usr/bin/stub" if exe in present else None,
    )
    assert not ofb._solver_ready("mpi", "blockMesh", "interFoam", True)
    assert ofb._solver_ready("mpi", "blockMesh", "interFoam", False)
    assert ofb._solver_ready("pool", "blockMesh", "interFoam", True)


def test_reserved_case_knob_name_raises(tmp_path):
    base = {"case_type": "current_loading"}
    with pytest.raises(ValueError, match="reserved manifest"):
        ofb._render_cases(base, [{"status": "x"}], {}, tmp_path / "w")
    with pytest.raises(ValueError, match="reserved manifest"):
        ofb._render_cases(base, [{"solver": "interFoam"}], {}, tmp_path / "w")


# --------------------------------------------------------------------------- #
#  REAL engine configure path (base-config module must be packaged)           #
# --------------------------------------------------------------------------- #
def test_engine_configure_path_runs_example_end_to_end(tmp_path, monkeypatch):
    """The canonical ``python -m digitalmodel <input.yml>`` path: engine()
    with the REAL ApplicationManager.configure, which loads the packaged
    base_configs/modules/openfoam_run_batch/openfoam_run_batch.yml and
    deep-merges it under the user input.

    Guards the integration gap the initial #1560 PR shipped: workflow +
    engine route existed but no base-config module yml, so configure raised
    FileNotFoundError on the real host before ever reaching the router —
    invisible to the router-level tests above, which bypass configure."""
    from digitalmodel.engine import engine

    (tmp_path / "input.yml").write_text((EXAMPLE_DIR / "input.yml").read_text())
    monkeypatch.chdir(tmp_path)

    result = engine(inputfile=str(tmp_path / "input.yml"))

    settings = result["openfoam_run_batch"]
    rows = pd.read_csv(settings["outputs"]["manifest"])
    assert rows["status"].tolist() == ["completed", "completed"]
    assert sorted(rows["solver"].tolist()) == ["pimpleFoam", "simpleFoam"]
    summary = json.loads(Path(settings["outputs"]["summary"]).read_text())
    assert summary["mock"] is True
    assert summary["total_cases"] == 2
    # Base-config defaults merged in without clobbering the user's input.
    assert summary["mode"] == "pool"
    assert summary["workers"] == 2
