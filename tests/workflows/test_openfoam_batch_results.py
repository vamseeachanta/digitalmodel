"""Legacy result rows, summaries, and checkpoint characterization."""

import json
import os
from pathlib import Path
from unittest.mock import patch

import pandas as pd
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


def test_example_input_runs_mock_batch(tmp_path):
    result = ofb.router(_example_cfg(tmp_path))
    manifest = tmp_path / "results" / "cases.csv"
    summary_path = tmp_path / "results" / "batch_summary.json"
    rows = pd.read_csv(manifest)
    assert rows["status"].tolist() == ["completed", "completed"]
    assert rows["name"].tolist() == ["current_simpleFoam", "current_pimpleFoam"]
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
    assert not list((tmp_path / "batch_runs").rglob("*.tmp"))
    outputs = result["openfoam_run_batch"]["outputs"]
    assert outputs["manifest"] == str(manifest)
    assert outputs["summary"] == str(summary_path)


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


def test_completed_checkpoint_is_skipped(tmp_path):
    cfg = _example_cfg(tmp_path)
    case0 = tmp_path / "batch_runs" / "current_simpleFoam"
    case0.mkdir(parents=True)
    (case0 / "_result.json").write_text(
        json.dumps(
            {
                "index": 0,
                "name": "current_simpleFoam",
                "status": "completed",
                "solver": "sentinel",
                "mock": True,
                "error": None,
                "case_dir": str(case0),
                "wall_seconds": 0.0,
            }
        )
    )
    build_calls = []
    real_build = ofb._build_case
    with patch.object(
        ofb,
        "_build_case",
        side_effect=lambda item: build_calls.append(item["name"]) or real_build(item),
    ):
        ofb.router(cfg)
    assert build_calls == ["current_pimpleFoam"]
    rows = pd.read_csv(tmp_path / "results" / "cases.csv")
    assert rows[rows["name"] == "current_simpleFoam"].iloc[0]["solver"] == "sentinel"


def test_failed_pool_checkpoint_is_retried(tmp_path):
    cfg = _example_cfg(tmp_path)
    case0 = tmp_path / "batch_runs" / "current_simpleFoam"
    case0.mkdir(parents=True)
    (case0 / "_result.json").write_text(
        json.dumps(
            {
                "index": 0,
                "name": "current_simpleFoam",
                "status": "failed",
                "error": "killed",
            }
        )
    )
    ofb.router(cfg)
    rows = pd.read_csv(tmp_path / "results" / "cases.csv")
    assert rows[rows["name"] == "current_simpleFoam"].iloc[0]["status"] == "completed"


def test_corrupt_checkpoint_is_treated_as_absent(tmp_path):
    work = tmp_path / "case"
    work.mkdir()
    (work / "_result.json").write_text('{"status": "compl')
    assert ofb._load_checkpoint(work) is None
    (work / "_result.json").write_text("[]")
    assert ofb._load_checkpoint(work) is None
