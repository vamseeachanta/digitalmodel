"""Batch engine, native-YAML and process-verdict regressions (1564/2051).

All solver execution is explicitly mocked or stubbed; no licence is acquired.
"""

import csv
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest
import yaml

from digitalmodel import run_contract
from digitalmodel.hydrodynamics.diffraction.validation_runner import ALL_VERDICTS
from digitalmodel.solvers.orcaflex.yaml_utils import orcaflex_dump
from digitalmodel.workflows import orcaflex_run_batch as orb


REPO_ROOT = Path(__file__).resolve().parents[2]
EXAMPLE_DIR = REPO_ROOT / "examples/workflows/orcaflex-run-batch"


def _example(tmp_path):
    for name in ("input.yml", "base_model.yml"):
        (tmp_path / name).write_bytes((EXAMPLE_DIR / name).read_bytes())
    cfg = yaml.safe_load((tmp_path / "input.yml").read_text())
    cfg["_config_dir_path"] = str(tmp_path)
    return cfg


def _single_model(cfg, missing=False):
    settings = cfg["orcaflex_run_batch"]
    settings.pop("variants", None)
    settings.pop("analysis", None)
    settings["models"]["files"] = ["base_model.yml"]
    if missing:
        settings["models"]["files"].append("missing_model.yml")
    settings["run_batch"]["workers"] = 1
    return settings


def _read_summary(cfg):
    path = cfg["orcaflex_run_batch"]["outputs"]["summary"]
    return json.loads(Path(path).read_text())


def _assert_verdict(cfg, expected, licensed, exit_status):
    verdict = run_contract.from_cfg(cfg)
    assert verdict is not None, "batch must expose the central verdict contract"
    assert verdict.verdict == expected
    assert verdict.verdict in ALL_VERDICTS
    assert verdict.solver_available is licensed
    assert run_contract.exit_status_for(verdict) == exit_status
    assert verdict.output_dir == cfg["orcaflex_run_batch"]["output_directory"]
    return verdict


def test_genuine_engine_configure_preserves_example_overrides(tmp_path, monkeypatch):
    from digitalmodel.engine import engine

    _example(tmp_path)
    monkeypatch.chdir(tmp_path)
    result = engine(inputfile=str(tmp_path / "input.yml"))

    summary = _read_summary(result)
    assert summary["completed"] == 3
    assert summary["failed"] == 0
    assert summary["workers"] == 2
    assert summary["mock"] is True
    _assert_verdict(result, "SKIPPED", False, 0)


def test_packaged_defaults_agree_with_router():
    import importlib.resources

    resource = importlib.resources.files("digitalmodel").joinpath(
        "base_configs/modules/orcaflex_run_batch/orcaflex_run_batch.yml"
    )
    defaults = yaml.safe_load(resource.read_text())
    assert defaults["basename"] == "orcaflex_run_batch"
    settings = defaults["orcaflex_run_batch"]
    assert settings["models"] == {}
    assert settings["variants"] == {}
    assert settings["analysis"]["type"] == orb.DEFAULT_ANALYSIS_TYPE
    run = settings["run_batch"]
    assert run["workers"] is None
    assert run["mock"] is False
    assert run["save_sim"] is True
    assert run["output_dir"] == orb.DEFAULT_OUTPUT_DIR
    assert run["work_dir"] == orb.DEFAULT_WORK_DIR


@pytest.mark.parametrize("default_token", ["~", ""])
def test_duration_render_preserves_native_default_spelling_and_source(tmp_path, default_token):
    cfg = _example(tmp_path)
    source = tmp_path / "base_model.yml"
    source.write_text(
        f"General:\n  StageDuration: [1.0, 5.0]\n  LogStartTime: {default_token}\n"
        f"  ImplicitVariableMaxTimeStep: {default_token}\n  StaticsMinDamping: 1.5\n"
        "Environment:\n  WaveDirection: 0\n  IncludeCurrentLoad: Yes\n"
        "  IncludeWindLoad: No\n",
        encoding="utf-8",
    )
    original = source.read_bytes()
    orb.router(cfg)
    rendered = sorted((tmp_path / "batch_runs/cases").glob("*.yml"))
    assert len(rendered) == 3
    for path in rendered:
        text = path.read_text()
        expected_token = "~" if default_token else "''"
        assert f"LogStartTime: {expected_token}" in text
        assert f"ImplicitVariableMaxTimeStep: {expected_token}" in text
        assert "IncludeCurrentLoad: Yes" in text
        assert "IncludeWindLoad: No" in text
        model = yaml.safe_load(text)
        assert model["General"]["StageDuration"] == [1.0, 20.0]
        assert model["General"]["StaticsMinDamping"] == 1.5
    assert source.read_bytes() == original


def test_native_blank_script_is_not_rewritten_as_default_null(tmp_path):
    cfg = _example(tmp_path)
    source = tmp_path / "base_model.yml"
    source.write_text(
        "General:\n  StageDuration: [1, 5]\n  RestartStateRecordingTest: \n"
        "  LogStartTime: ~\n  RestartStateRecordingPeriodicCount: 0\n"
        "Environment:\n  WaveDirection: 0\n  IncludeCurrentLoad: No\n"
    )
    original = source.read_bytes()
    orb.router(cfg)
    rendered = next((tmp_path / "batch_runs/cases").glob("*.yml"))
    model = yaml.safe_load(rendered.read_text())
    assert model["General"]["RestartStateRecordingTest"] == ""
    assert model["General"]["LogStartTime"] is None
    assert model["General"]["RestartStateRecordingPeriodicCount"] == 0
    assert model["Environment"]["IncludeCurrentLoad"] is False
    assert source.read_bytes() == original


def test_canonical_reader_distinguishes_blanks_nulls_and_quoted_text(tmp_path):
    from digitalmodel.solvers.orcaflex.yaml_utils import orcaflex_load

    source = tmp_path / "scalars.yml"
    source.write_text(
        "blank: \ntilde: ~\nnull_word: null\nnull_title: Null\nnull_upper: NULL\n"
        "quoted_blank: ''\nquoted_tilde: '~'\nquoted_null: 'null'\n"
        "zero: 0\nfalse_value: No\n"
    )
    model, _ = orcaflex_load(source)
    assert model["blank"] == ""
    assert model["quoted_blank"] == ""
    assert model["quoted_tilde"] == "~"
    assert model["quoted_null"] == "null"
    for key in ("tilde", "null_word", "null_title", "null_upper"):
        assert model[key] is None
    assert model["zero"] == 0
    assert model["false_value"] is False
    assert yaml.safe_load("blank: ")["blank"] is None


def test_partial_batch_keeps_diagnostics_and_refuses(tmp_path):
    cfg = _example(tmp_path)
    _single_model(cfg, missing=True)
    result = orb.router(cfg)
    summary = _read_summary(result)
    assert summary["completed"] == 1
    assert summary["failed"] == 1
    with Path(result["orcaflex_run_batch"]["outputs"]["manifest"]).open() as f:
        rows = list(csv.DictReader(f))
    assert [r["status"] for r in rows] == ["completed", "failed"]
    assert "not found" in rows[1]["error"]
    verdict = _assert_verdict(result, "FAIL", False, 1)
    assert verdict.issues
    assert Path(rows[0]["sim_path"]).is_file()


def test_native_yaml_utf8_bom_names_survive_rendering(tmp_path):
    cfg = _example(tmp_path)
    source = tmp_path / "base_model.yml"
    model = "General:\n  StageDuration: [1, 5]\nName: \u5206\u6790\n"
    source.write_text(model, encoding="utf-8-sig")
    original = source.read_bytes()
    orb.router(cfg)
    for path in (tmp_path / "batch_runs/cases").glob("*.yml"):
        assert yaml.safe_load(path.read_text(encoding="utf-8"))["Name"] == "\u5206\u6790"
    assert source.read_bytes() == original


def test_canonical_writer_emits_utf8(tmp_path):
    path = tmp_path / "unicode.yml"
    orcaflex_dump({"Name": "\u5206\u6790"}, path)
    assert yaml.safe_load(path.read_text(encoding="utf-8")) == {"Name": "\u5206\u6790"}


def test_successful_explicit_mock_is_not_licensed_success(tmp_path):
    result = orb.router(_example(tmp_path))
    _assert_verdict(result, "SKIPPED", False, 0)
    for sim in (tmp_path / "batch_runs/sims").glob("*.sim"):
        assert "mock" in sim.read_text().lower()


def test_real_mode_success_with_stubbed_solver_uses_pass(tmp_path, monkeypatch):
    cfg = _example(tmp_path)
    settings = _single_model(cfg)
    settings["run_batch"]["mock"] = False
    monkeypatch.setattr(orb, "_license_available", lambda: True)

    def stub_pool(self, files, config):
        return {"results": [{"file_path": files[0], "status": "success",
                             "duration": 0.01, "output_files": []}]}

    monkeypatch.setattr(orb.OrcaFlexParallelAnalysis, "process_files_parallel", stub_pool)
    result = orb.router(cfg)
    assert _read_summary(result)["completed"] == 1
    _assert_verdict(result, "PASS", True, 0)


@pytest.mark.parametrize("empty_cases", [False, True])
def test_incomplete_or_empty_executor_result_refuses(tmp_path, monkeypatch, empty_cases):
    cfg = _example(tmp_path)
    _single_model(cfg)
    if empty_cases:
        monkeypatch.setattr(orb, "_render_cases", lambda **kwargs: [])
    monkeypatch.setattr(
        orb._MockOrcaFlexBatch, "process_files_parallel",
        lambda self, files, config: {"results": [], "total_duration": 0.0},
    )
    result = orb.router(cfg)
    summary = _read_summary(result)
    assert summary["completed"] == 0
    assert summary["total_cases"] == (0 if empty_cases else 1)
    _assert_verdict(result, "FAIL", False, 1)


@pytest.mark.parametrize("missing", [False, True])
def test_engine_cli_exit_and_fresh_sidecar(tmp_path, missing):
    import assetutilities

    cfg = _example(tmp_path)
    _single_model(cfg, missing=missing)
    input_path = tmp_path / "input.yml"
    input_path.write_text(yaml.safe_dump(cfg), encoding="utf-8")
    env = os.environ.copy()
    # Match pytest's resolved dependency, including a sibling source checkout.
    dependency_root = Path(assetutilities.__file__).resolve().parents[1]
    env["PYTHONPATH"] = os.pathsep.join((str(REPO_ROOT / "src"), str(dependency_root)))
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    result = subprocess.run(
        [sys.executable, "-B", "-m", "digitalmodel", input_path.name],
        cwd=tmp_path, env=env, capture_output=True, text=True, timeout=60,
    )
    assert result.returncode == int(missing), result.stdout + result.stderr
    summary_path = tmp_path / "results/batch_summary.json"
    assert summary_path.is_file(), result.stdout + result.stderr
    summary = json.loads(summary_path.read_text())
    assert summary["completed"] == 1
    assert summary["failed"] == int(missing)
    sidecar = json.loads((tmp_path / "results/run_verdict.json").read_text())
    assert sidecar["verdict"] == ("FAIL" if missing else "SKIPPED")
    assert sidecar["solver_available"] is False
    assert sidecar["refused"] is missing
