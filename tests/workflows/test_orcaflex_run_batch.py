"""License-free (mock-mode) tests for the orcaflex_run_batch workflow (#1554)."""

import json
import os
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.workflows import orcaflex_run_batch as orb

REPO_ROOT = Path(__file__).resolve().parents[2]
EXAMPLE_DIR = REPO_ROOT / "examples" / "workflows" / "orcaflex-run-batch"


def _copy_example(tmp_path: Path) -> dict:
    for name in ("input.yml", "base_model.yml"):
        (tmp_path / name).write_text((EXAMPLE_DIR / name).read_text())
    cfg = yaml.safe_load((tmp_path / "input.yml").read_text())
    cfg["_config_dir_path"] = str(tmp_path)
    return cfg


def test_example_input_runs_mock_batch(tmp_path):
    cfg = _copy_example(tmp_path)

    result = orb.router(cfg)

    manifest = tmp_path / "results" / "cases.csv"
    summary_path = tmp_path / "results" / "batch_summary.json"
    assert manifest.exists()
    assert summary_path.exists()

    rows = pd.read_csv(manifest)
    assert len(rows) == 3
    assert rows["status"].tolist() == ["completed"] * 3
    assert sorted(rows["wave_heading_deg"].tolist()) == [0.0, 45.0, 90.0]
    assert all(rows["wall_seconds"] >= 0)

    summary = json.loads(summary_path.read_text())
    assert summary["total_cases"] == 3
    assert summary["completed"] == 3
    assert summary["failed"] == 0
    assert summary["workers"] == 2
    assert summary["mock"] is True
    assert summary["host_cpu_count"] == os.cpu_count()

    # Case-gen reuse: rendered case models carry the swept heading AND the
    # simulation-duration override injected via parametric_run's _set_dotted.
    rendered_headings = set()
    for case_file in sorted((tmp_path / "batch_runs" / "cases").glob("*.yml")):
        model = yaml.safe_load(case_file.read_text())
        rendered_headings.add(model["Environment"]["WaveDirection"])
        assert model["General"]["StageDuration"] == [8.0, 20.0]
    assert rendered_headings == {0.0, 45.0, 90.0}

    # Mock sims stay under batch_runs/sims, never under results/.
    assert len(list((tmp_path / "batch_runs" / "sims").glob("*.sim"))) == 3
    assert not list((tmp_path / "results").rglob("*.sim"))

    outputs = result["orcaflex_run_batch"]["outputs"]
    assert outputs["manifest"] == str(manifest)
    assert outputs["summary"] == str(summary_path)


def test_yaml_matrix_variants_reuse_parametric_case_gen(tmp_path):
    cfg = _copy_example(tmp_path)
    cfg["orcaflex_run_batch"]["variants"] = {
        "source": "yaml_matrix",
        "list": [
            {"heading": 0.0, "height": 1.5},
            {"heading": 90.0, "height": 3.0},
        ],
        "mapping": {
            "heading": "Environment.WaveDirection",
            "height": "Environment.WaveHeight",
        },
    }

    orb.router(cfg)

    rows = pd.read_csv(tmp_path / "results" / "cases.csv")
    assert len(rows) == 2
    assert rows["status"].tolist() == ["completed"] * 2
    case_files = sorted((tmp_path / "batch_runs" / "cases").glob("*.yml"))
    models = [yaml.safe_load(f.read_text()) for f in case_files]
    assert {m["Environment"]["WaveHeight"] for m in models} == {1.5, 3.0}


def test_workers_default_is_ninety_percent_of_cores(monkeypatch):
    monkeypatch.setattr(orb.os, "cpu_count", lambda: 64)
    assert orb.resolve_workers({}) == 57
    # Explicit workers always wins over the host-aware default.
    assert orb.resolve_workers({"workers": 4}) == 4


def test_workers_default_never_below_one(monkeypatch):
    monkeypatch.setattr(orb.os, "cpu_count", lambda: 1)
    assert orb.resolve_workers({}) == 1


def test_workers_default_applied_in_summary(tmp_path, monkeypatch):
    monkeypatch.setattr(orb.os, "cpu_count", lambda: 4)
    cfg = _copy_example(tmp_path)
    del cfg["orcaflex_run_batch"]["run_batch"]["workers"]

    orb.router(cfg)

    summary = json.loads((tmp_path / "results" / "batch_summary.json").read_text())
    assert summary["workers"] == 3


def test_license_absent_and_mock_false_fails_fast(tmp_path, monkeypatch):
    cfg = _copy_example(tmp_path)
    cfg["orcaflex_run_batch"]["run_batch"]["mock"] = False
    monkeypatch.setattr(orb, "_license_available", lambda: False)

    with pytest.raises(RuntimeError, match="OrcaFlex license / OrcFxAPI"):
        orb.router(cfg)


def test_failing_case_does_not_kill_batch(tmp_path):
    cfg = _copy_example(tmp_path)
    settings = cfg["orcaflex_run_batch"]
    settings["models"]["files"] = ["base_model.yml", "missing_model.yml"]
    settings.pop("variants")
    settings.pop("analysis")

    orb.router(cfg)

    rows = pd.read_csv(tmp_path / "results" / "cases.csv")
    assert len(rows) == 2
    by_model = dict(zip(rows["model"], rows["status"]))
    assert by_model["base_model.yml"] == "completed"
    assert by_model["missing_model.yml"] == "failed"
    failed_row = rows[rows["model"] == "missing_model.yml"].iloc[0]
    assert "not found" in failed_row["error"]

    summary = json.loads((tmp_path / "results" / "batch_summary.json").read_text())
    assert summary["completed"] == 1
    assert summary["failed"] == 1
