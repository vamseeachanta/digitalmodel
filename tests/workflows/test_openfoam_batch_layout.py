"""Legacy case and bounded-result placement characterization."""

from pathlib import Path

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


def test_legacy_case_and_result_placement(tmp_path):
    result = ofb.router(_example_cfg(tmp_path))
    settings = result["openfoam_run_batch"]
    assert settings["outputs"]["manifest"] == str(tmp_path / "results" / "cases.csv")
    assert settings["outputs"]["summary"] == str(
        tmp_path / "results" / "batch_summary.json"
    )
    for name in ("current_simpleFoam", "current_pimpleFoam"):
        assert (tmp_path / "batch_runs" / name / "system" / "controlDict").is_file()


def test_results_dir_holds_only_csv_json_rollups(tmp_path):
    ofb.router(_example_cfg(tmp_path))
    results = tmp_path / "results"
    files = [path for path in results.rglob("*") if path.is_file()]
    assert files
    assert all(path.suffix in {".csv", ".json"} for path in files)
    assert all(path.stat().st_size < 2 * 1024 * 1024 for path in files)
    assert len(files) <= 100
    assert not list(results.rglob("processor*"))
    assert not list(results.rglob("*.foam"))
    assert not list(results.rglob("VTK"))


def test_mpi_mode_rejects_multi_case_matrix(tmp_path):
    cfg = _example_cfg(tmp_path)
    cfg["openfoam_run_batch"]["run_batch"]["mode"] = "mpi"
    with pytest.raises(ValueError, match="exactly ONE case"):
        ofb.router(cfg)
