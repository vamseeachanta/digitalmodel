"""Legacy configuration, routing, and public-surface characterization."""

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


def test_case_matrix_is_deterministic(tmp_path):
    base = {"case_type": "current_loading", "solver": "simpleFoam"}
    variants = {
        "source": "yaml_matrix",
        "list": [{"solver_app": "simpleFoam"}, {"solver_app": "pimpleFoam"}],
        "mapping": {"solver_app": "solver"},
    }
    first = ofb._render_cases(
        base,
        ofb._resolve_case_matrix(None, variants, tmp_path),
        variants["mapping"],
        tmp_path / "w",
    )
    second = ofb._render_cases(
        base,
        ofb._resolve_case_matrix(None, variants, tmp_path),
        variants["mapping"],
        tmp_path / "w",
    )
    assert [item["name"] for item in first] == [item["name"] for item in second]
    assert [item["case"] for item in first] == [item["case"] for item in second]
    assert [item["settings"]["solver"] for item in first] == [
        "simpleFoam",
        "pimpleFoam",
    ]
    assert [item["name"] for item in first] == [
        "current_loading_case_000",
        "current_loading_case_001",
    ]


def test_explicit_cases_and_variants_are_mutually_exclusive(tmp_path):
    with pytest.raises(ValueError, match="either cases"):
        ofb._resolve_case_matrix([{"name": "a"}], {"source": "factorial"}, tmp_path)


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


def test_invalid_timeout_rejects_before_creating_directories(tmp_path):
    cfg = {
        "_config_dir_path": str(tmp_path),
        "openfoam_run_batch": {
            "base": {"case_type": "current_loading"},
            "run_batch": {"mock": True, "timeout_seconds": "invalid"},
        },
    }
    with pytest.raises(ValueError, match="invalid literal"):
        ofb.router(cfg)
    assert not (tmp_path / "results").exists()
    assert not (tmp_path / "batch_runs").exists()


def test_resolve_workers_honors_legacy_default_workers_monkeypatch(monkeypatch):
    monkeypatch.setattr(ofb, "default_workers", lambda: 73)
    assert ofb.resolve_workers({}) == 73


def test_resolve_dir_honors_legacy_resolve_path_monkeypatch(monkeypatch, tmp_path):
    sentinel = tmp_path / "sentinel"
    resolve_path = Mock(return_value=sentinel)
    monkeypatch.setattr(ofb, "_resolve_path", resolve_path)
    assert ofb._resolve_dir("ignored", tmp_path) == sentinel
    resolve_path.assert_called_once_with("ignored", tmp_path)


def test_reserved_case_knob_name_raises(tmp_path):
    base = {"case_type": "current_loading"}
    with pytest.raises(ValueError, match="reserved manifest"):
        ofb._render_cases(base, [{"status": "x"}], {}, tmp_path / "w")
    with pytest.raises(ValueError, match="reserved manifest"):
        ofb._render_cases(base, [{"solver": "interFoam"}], {}, tmp_path / "w")


def test_engine_resolves_basename_to_workflow():
    from digitalmodel.engine import engine

    cfg = {"basename": "openfoam_run_batch", "openfoam_run_batch": {}}
    with patch("digitalmodel.engine.app_manager") as app_manager:
        app_manager.save_cfg.return_value = None
        with patch(
            "digitalmodel.workflows.openfoam_run_batch.router", return_value=cfg
        ) as router:
            result = engine(cfg=cfg, config_flag=False)
    router.assert_called_once()
    assert result is not None


def test_engine_configure_path_runs_example_end_to_end(tmp_path, monkeypatch):
    from digitalmodel.engine import engine

    input_path = tmp_path / "input.yml"
    input_path.write_text((EXAMPLE_DIR / "input.yml").read_text())
    monkeypatch.chdir(tmp_path)
    result = engine(inputfile=str(input_path))
    settings = result["openfoam_run_batch"]
    summary = json.loads(Path(settings["outputs"]["summary"]).read_text())
    assert summary["mock"] is True
    assert summary["total_cases"] == 2
    assert summary["mode"] == "pool"
    assert summary["workers"] == 2


def test_legacy_facade_keeps_implicit_public_export_surface():
    assert not hasattr(ofb, "__all__")
    assert {name for name in vars(ofb) if not name.startswith("_")} == {
        "Any",
        "CHECKPOINT_FILENAME",
        "Callable",
        "DEFAULT_MESH_UTILITY",
        "DEFAULT_MODE",
        "DEFAULT_OUTPUT_DIR",
        "DEFAULT_TIMEOUT_SECONDS",
        "DEFAULT_WORK_DIR",
        "Path",
        "SOLVER_ERROR_MESSAGE",
        "ThreadPoolExecutor",
        "VALID_MODES",
        "WORKER_CORE_FRACTION",
        "annotations",
        "datetime",
        "deepcopy",
        "default_workers",
        "json",
        "logger",
        "math",
        "mpi_command_plan",
        "os",
        "pd",
        "re",
        "resolve_workers",
        "router",
        "shutil",
        "subprocess",
        "time",
        "timezone",
    }
