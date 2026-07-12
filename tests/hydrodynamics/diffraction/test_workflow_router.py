from pathlib import Path
from types import SimpleNamespace
from unittest.mock import patch

import pytest
import yaml

FIXTURES_DIR = Path(__file__).parent / "fixtures"
REPO_ROOT = Path(__file__).resolve().parents[3]
UNIT_BOX = REPO_ROOT / "examples" / "hydrodynamics" / "diffraction" / "unit_box_rao"


def _workflow_cfg(tmp_path: Path, solver: str) -> dict:
    return {
        "basename": "diffraction",
        "_config_dir_path": str(FIXTURES_DIR),
        "Analysis": {"result_folder": str(tmp_path / "results")},
        "diffraction": {
            "operation": "convert_spec",
            "spec": "spec_ship_raos.yml",
            "solver": solver,
            "format": "single",
            "output_directory": "prepared",
        },
    }


def test_diffraction_workflow_generates_orcawave_input(tmp_path: Path) -> None:
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    cfg = DiffractionWorkflow().router(_workflow_cfg(tmp_path, "orcawave"))

    generated = Path(cfg["diffraction"]["outputs"]["orcawave"])
    data = yaml.safe_load(generated.read_text())
    body = data["Bodies"][0]

    assert generated == tmp_path / "results" / "prepared" / "Ship_001.yml"
    assert data["WaterDepth"] == pytest.approx(500.0)
    assert data["WaterDensity"] == pytest.approx(1.025)
    assert len(data["PeriodOrFrequency"]) == 15
    assert data["PeriodOrFrequency"][0] == pytest.approx(4.1888)
    assert data["WaveHeading"] == [0.0, 45.0, 90.0, 135.0, 180.0]
    assert body["BodyName"] == "Ship_001"
    assert body["BodyMeshFileName"] == "sample_box.gdf"


def test_diffraction_workflow_generates_aqwa_deck(tmp_path: Path) -> None:
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    cfg = DiffractionWorkflow().router(_workflow_cfg(tmp_path, "aqwa"))

    generated = Path(cfg["diffraction"]["outputs"]["aqwa"])
    content = generated.read_text()

    assert generated == tmp_path / "results" / "prepared" / "test_ship_raos.dat"
    assert "JOB AQWA  LINE" in content
    assert "      DPTH       500" in content
    assert content.count("HRTZ") == 15
    assert content.count("QPPL") == 5
    assert "could not be loaded" not in content


# --- operation: run_orcawave (issue #900) -----------------------------------


def _solve_cfg(
    tmp_path: Path, dry_run: bool = True, operation: str = "run_orcawave"
) -> dict:
    return {
        "basename": "diffraction",
        "_config_dir_path": str(UNIT_BOX),
        "Analysis": {"result_folder": str(tmp_path / "results")},
        "diffraction": {
            "operation": operation,
            "spec": "spec.yml",
            "output_directory": "out",
            "dry_run": dry_run,
        },
    }


def _fake_result(status_value: str):
    from digitalmodel.hydrodynamics.diffraction.orcawave_runner import RunStatus

    return SimpleNamespace(
        status=RunStatus(status_value),
        output_dir="out",
        input_file="UnitBoxRAO.yml",
        modular_files=[],
        mesh_files=[],
        error_message="boom",
    )


def test_run_orcawave_dry_run_offline(tmp_path: Path) -> None:
    # Real runner, no license: converts + skips the solver -> DRY_RUN, records outputs.
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    cfg = DiffractionWorkflow().router(_solve_cfg(tmp_path, dry_run=True))
    settings = cfg["diffraction"]
    assert settings["run_status"] == "dry_run"
    assert "input_file" in settings["outputs"]


def test_unsupported_operation_lists_supported(tmp_path: Path) -> None:
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    with pytest.raises(ValueError, match="run_orcawave"):
        DiffractionWorkflow().router(_solve_cfg(tmp_path, operation="bogus"))


def test_run_orcawave_failed_status_raises(tmp_path: Path) -> None:
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    with patch(
        "digitalmodel.hydrodynamics.diffraction.orcawave_runner.run_orcawave",
        return_value=_fake_result("failed"),
    ):
        with pytest.raises(RuntimeError, match="OrcaWave solve failed"):
            DiffractionWorkflow().router(_solve_cfg(tmp_path, dry_run=False))


def test_run_orcawave_silent_dry_run_fallback_raises(tmp_path: Path) -> None:
    # Solver unavailable: runner returns DRY_RUN although a real solve was requested.
    # The lane must NOT read that as success.
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    with patch(
        "digitalmodel.hydrodynamics.diffraction.orcawave_runner.run_orcawave",
        return_value=_fake_result("dry_run"),
    ):
        with pytest.raises(RuntimeError, match="fell back to dry-run"):
            DiffractionWorkflow().router(_solve_cfg(tmp_path, dry_run=False))


def test_run_orcawave_completed_exports_results_json(tmp_path: Path) -> None:
    # A successful solve must serialize DiffractionResults.to_dict() into the
    # output dir so the license-free side (postprocess, pamphlet rao_artifact,
    # queue result-return) can consume it (#1537).
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    out_dir = tmp_path / "results" / "out"
    fake = _fake_result("completed")
    fake.output_dir = str(out_dir)
    fake.diffraction_results = SimpleNamespace(
        to_dict=lambda: {"vessel_name": "UnitBoxRAO", "raos": {"frequencies": [0.5]}}
    )

    with patch(
        "digitalmodel.hydrodynamics.diffraction.orcawave_runner.run_orcawave",
        return_value=fake,
    ):
        cfg = DiffractionWorkflow().router(_solve_cfg(tmp_path, dry_run=False))

    settings = cfg["diffraction"]
    exported = Path(settings["outputs"]["diffraction_results_json"])
    assert exported == out_dir / "diffraction_results.json"
    import json

    data = json.loads(exported.read_text())
    assert data["vessel_name"] == "UnitBoxRAO"
    assert data["raos"]["frequencies"] == [0.5]


def test_run_orcawave_completed_without_results_object(tmp_path: Path) -> None:
    # Runners that carry no diffraction_results (e.g. AQWA path today) must not
    # break: no file, no settings key.
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    fake = _fake_result("completed")
    fake.output_dir = str(tmp_path / "results" / "out")

    with patch(
        "digitalmodel.hydrodynamics.diffraction.orcawave_runner.run_orcawave",
        return_value=fake,
    ):
        cfg = DiffractionWorkflow().router(_solve_cfg(tmp_path, dry_run=False))

    assert "diffraction_results_json" not in cfg["diffraction"]["outputs"]


# --- operation: run_aqwa (issue #939) ---------------------------------------


def _fake_aqwa_result(status):
    from digitalmodel.hydrodynamics.diffraction.aqwa_runner import AQWARunStatus

    return SimpleNamespace(
        status=getattr(AQWARunStatus, status),
        output_dir="out",
        input_file="UnitBox.dat",
        modular_files=[],
        mesh_files=[],
        error_message="boom",
    )


def test_run_aqwa_dry_run_offline(tmp_path: Path) -> None:
    # Real runner, no license: prepares + skips the solver -> DRY_RUN, records outputs.
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    cfg = DiffractionWorkflow().router(
        _solve_cfg(tmp_path, dry_run=True, operation="run_aqwa")
    )
    settings = cfg["diffraction"]
    assert settings["run_status"] == "dry_run"
    assert "input_file" in settings["outputs"]


def test_run_aqwa_failed_status_raises(tmp_path: Path) -> None:
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    with patch(
        "digitalmodel.hydrodynamics.diffraction.aqwa_runner.run_aqwa",
        return_value=_fake_aqwa_result("FAILED"),
    ):
        with pytest.raises(RuntimeError, match="AQWA solve failed"):
            DiffractionWorkflow().router(
                _solve_cfg(tmp_path, dry_run=False, operation="run_aqwa")
            )


def test_run_aqwa_silent_dry_run_fallback_raises(tmp_path: Path) -> None:
    # AQWA solver unavailable: runner returns DRY_RUN despite a real solve request.
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    with patch(
        "digitalmodel.hydrodynamics.diffraction.aqwa_runner.run_aqwa",
        return_value=_fake_aqwa_result("DRY_RUN"),
    ):
        with pytest.raises(RuntimeError, match="fell back to dry-run"):
            DiffractionWorkflow().router(
                _solve_cfg(tmp_path, dry_run=False, operation="run_aqwa")
            )


# --- diffraction.thread_count forwarding (dm#1555) ---------------------------


def _solve_cfg_with_threads(tmp_path: Path, thread_count: int | None) -> dict:
    cfg = _solve_cfg(tmp_path, dry_run=False)
    if thread_count is not None:
        cfg["diffraction"]["thread_count"] = thread_count
    return cfg


def test_run_orcawave_forwards_explicit_thread_count(tmp_path: Path) -> None:
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    with patch(
        "digitalmodel.hydrodynamics.diffraction.orcawave_runner.run_orcawave",
        return_value=_fake_result("completed"),
    ) as run_solve:
        DiffractionWorkflow().router(_solve_cfg_with_threads(tmp_path, 8))
    assert run_solve.call_args.kwargs["thread_count"] == 8


def test_run_orcawave_unset_thread_count_forwards_none(tmp_path: Path) -> None:
    # None lets run_orcawave resolve the host-aware ~90%-of-cores default.
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    with patch(
        "digitalmodel.hydrodynamics.diffraction.orcawave_runner.run_orcawave",
        return_value=_fake_result("completed"),
    ) as run_solve:
        DiffractionWorkflow().router(_solve_cfg_with_threads(tmp_path, None))
    assert run_solve.call_args.kwargs["thread_count"] is None


def test_run_orcawave_records_effective_thread_count(tmp_path: Path) -> None:
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    result = _fake_result("completed")
    result.thread_count = 57
    with patch(
        "digitalmodel.hydrodynamics.diffraction.orcawave_runner.run_orcawave",
        return_value=result,
    ):
        cfg = DiffractionWorkflow().router(_solve_cfg_with_threads(tmp_path, None))
    assert cfg["diffraction"]["thread_count_effective"] == 57


def test_run_aqwa_does_not_receive_thread_count(tmp_path: Path) -> None:
    # AQWA keeps its own core handling (#546); the kwarg is OrcaWave-only.
    from digitalmodel.hydrodynamics.diffraction.workflow import DiffractionWorkflow

    with patch(
        "digitalmodel.hydrodynamics.diffraction.aqwa_runner.run_aqwa",
        return_value=_fake_aqwa_result("COMPLETED"),
    ) as run_solve:
        DiffractionWorkflow().router(
            _solve_cfg(tmp_path, dry_run=False, operation="run_aqwa")
        )
    assert "thread_count" not in run_solve.call_args.kwargs
