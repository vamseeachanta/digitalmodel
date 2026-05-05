# ABOUTME: TDD tests for preliminary turning-circle and tactical-diameter estimation.
# ABOUTME: Locks Nomoto input workflow, packaged YAML, artifacts, charts, and caveats for issue #2568.

import csv
import json
import math
import subprocess
import zipfile
from importlib import resources
from pathlib import Path

import pytest
import yaml


# Serialize wheel-build tests across this module under pytest-xdist:
# default `--dist loadscope` keeps a module on one worker, but adding
# `xdist_group` is defense-in-depth in case dispatch policy changes.
# The load-bearing fix is removing `--no-isolation` below — isolated
# builds prevent shared-state races on `build/` and `digitalmodel.egg-info/`
# (issue #2617).
pytestmark = pytest.mark.xdist_group(name="wheel_build")


BASE_CASE = {
    "speed_m_s": 7.5,
    "rudder_angle_deg": 20.0,
    "K_per_s": 0.12,
    "T_s": 20.0,
    "duration_s": 240.0,
    "dt_s": 1.0,
}


def test_nomoto_response_zero_rudder_stays_straight():
    from digitalmodel.naval_architecture.turning_circle import simulate_nomoto_turning_circle

    result = simulate_nomoto_turning_circle(
        **{**BASE_CASE, "rudder_angle_deg": 0.0},
    )

    assert result["metrics"]["advance_m"] is None
    assert result["metrics"]["transfer_m"] is None
    assert max(abs(row["heading_deg"]) for row in result["time_history"]) < 1e-9
    assert max(abs(row["y_m"]) for row in result["time_history"]) < 1e-9


def test_nomoto_response_sign_symmetry():
    from digitalmodel.naval_architecture.turning_circle import simulate_nomoto_turning_circle

    port = simulate_nomoto_turning_circle(**BASE_CASE)
    starboard = simulate_nomoto_turning_circle(
        **{**BASE_CASE, "rudder_angle_deg": -BASE_CASE["rudder_angle_deg"]}
    )

    assert port["metrics"]["advance_m"] == pytest.approx(
        starboard["metrics"]["advance_m"], rel=1e-3
    )
    assert port["metrics"]["transfer_m"] == pytest.approx(
        -starboard["metrics"]["transfer_m"], rel=1e-3
    )
    assert port["metrics"]["tactical_diameter_m"] == pytest.approx(
        -starboard["metrics"]["tactical_diameter_m"], rel=1e-3
    )
    assert port["time_history"][-1]["heading_deg"] == pytest.approx(
        -starboard["time_history"][-1]["heading_deg"], rel=1e-3
    )


def test_nomoto_time_step_reduction_stability():
    from digitalmodel.naval_architecture.turning_circle import simulate_nomoto_turning_circle

    coarse = simulate_nomoto_turning_circle(**BASE_CASE)
    fine = simulate_nomoto_turning_circle(**{**BASE_CASE, "dt_s": 0.5})

    assert coarse["metrics"]["tactical_diameter_m"] == pytest.approx(
        fine["metrics"]["tactical_diameter_m"], rel=0.03
    )
    assert coarse["metrics"]["advance_m"] == pytest.approx(
        fine["metrics"]["advance_m"], rel=0.03
    )


def test_metrics_warn_when_heading_target_not_reached():
    from digitalmodel.naval_architecture.turning_circle import simulate_nomoto_turning_circle

    result = simulate_nomoto_turning_circle(
        **{**BASE_CASE, "duration_s": 20.0, "dt_s": 0.5}
    )

    assert result["metrics"]["advance_m"] is None
    assert result["metrics"]["transfer_m"] is None
    assert result["metrics"]["tactical_diameter_m"] is None
    assert any("heading target" in warning.lower() for warning in result["warnings"])
    assert result["metrics"]["metric_status"] == "warning"


def test_packaged_turning_circle_yaml():
    from digitalmodel.naval_architecture.turning_circle import load_packaged_turning_circle_yaml

    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "turning_circle_typical_ship.yml"
    )
    with resource.open("r", encoding="utf-8") as stream:
        payload = yaml.safe_load(stream)

    assert payload["case"]["id"] == "typical_single_screw_turning_circle"
    assert payload["outputs"]["charts"]["required"] == [
        "trajectory_by_case",
        "yaw_rate_vs_time",
        "heading_vs_time",
        "turning_metrics_vs_rudder_angle",
    ]

    config = load_packaged_turning_circle_yaml()
    assert config.case_id == payload["case"]["id"]
    assert len(config.speeds_m_s) == 2
    assert len(config.rudder_angles_deg) == 3


def test_packaged_yaml_in_built_distribution(tmp_path):
    repo_root = Path(__file__).parents[2]
    result = subprocess.run(
        [
            "uv",
            "run",
            "--no-sync",
            "--with",
            "build",
            "--with",
            "setuptools",
            "--with",
            "wheel",
            "python",
            "-m",
            "build",
            "--wheel",
            "--outdir",
            str(tmp_path),
        ],
        cwd=repo_root,
        text=True,
        capture_output=True,
        check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    wheel = next(tmp_path.glob("*.whl"))
    with zipfile.ZipFile(wheel) as archive:
        assert (
            "digitalmodel/naval_architecture/data/turning_circle_typical_ship.yml"
            in archive.namelist()
        )
        assert (
            "digitalmodel/naval_architecture/data/yaw_moment_typical_ship.yml"
            in archive.namelist()
        )


def test_output_artifacts_and_charts(tmp_path):
    from digitalmodel.naval_architecture.turning_circle import (
        METRICS_HEADERS,
        TIME_HISTORY_HEADERS,
        load_packaged_turning_circle_yaml,
        run_turning_circle_sweep,
        write_turning_circle_results,
    )

    result = run_turning_circle_sweep(load_packaged_turning_circle_yaml())
    manifest = write_turning_circle_results(
        result,
        tmp_path,
        table_formats=("csv", "json"),
        chart_formats=("png", "html"),
    )

    with open(manifest["tables"]["time_history_csv"], newline="", encoding="utf-8") as stream:
        assert next(csv.reader(stream)) == TIME_HISTORY_HEADERS
    with open(manifest["tables"]["metrics_csv"], newline="", encoding="utf-8") as stream:
        assert next(csv.reader(stream)) == METRICS_HEADERS
    with open(manifest["tables"]["json"], encoding="utf-8") as stream:
        payload = json.load(stream)

    assert set(payload) == {"metadata", "provenance", "cases", "metrics", "time_history", "artifacts"}
    assert set(manifest["charts"]) == {
        "trajectory_by_case",
        "yaw_rate_vs_time",
        "heading_vs_time",
        "turning_metrics_vs_rudder_angle",
    }
    assert Path(manifest["sidecars"]["provenance"]).exists()
    assert Path(manifest["sidecars"]["artifact_manifest"]).exists()

    for chart_name, files in manifest["charts"].items():
        assert files["png"].read_bytes().startswith(b"\x89PNG")
        html_text = files["html"].read_text(encoding="utf-8")
        assert chart_name.replace("_", " ").title() in html_text
        assert "Plotly.newPlot" in html_text


def test_no_compliance_overclaim(tmp_path):
    from digitalmodel.naval_architecture.turning_circle import (
        load_packaged_turning_circle_yaml,
        run_turning_circle_sweep,
        write_turning_circle_results,
    )

    result = run_turning_circle_sweep(load_packaged_turning_circle_yaml())
    manifest = write_turning_circle_results(result, tmp_path, chart_formats=())
    provenance = json.loads(
        Path(manifest["sidecars"]["provenance"]).read_text(encoding="utf-8")
    )
    docs_path = Path(__file__).parents[2] / "docs/domains/marine-engineering/turning-circle-estimator.md"
    docs_text = docs_path.read_text(encoding="utf-8")

    limitation_text = json.dumps(provenance).lower()
    assert "preliminary" in limitation_text
    assert "not mmg" in limitation_text or "not a full mmg" in limitation_text
    assert "not" in limitation_text and "compliance" in limitation_text
    assert "preliminary" in docs_text.lower()
    assert "not mmg" in docs_text.lower() or "not a full mmg" in docs_text.lower()
    assert "imo" in docs_text.lower()
    assert "abs" in docs_text.lower()
    assert "not" in docs_text.lower() and "compliance" in docs_text.lower()


def test_public_import_surface():
    from digitalmodel.naval_architecture import (
        load_packaged_turning_circle_yaml,
        run_turning_circle_sweep,
        simulate_nomoto_turning_circle,
        write_turning_circle_results,
    )

    assert load_packaged_turning_circle_yaml
    assert run_turning_circle_sweep
    assert simulate_nomoto_turning_circle
    assert write_turning_circle_results


@pytest.mark.parametrize(
    "field,value",
    [
        ("speed_m_s", 0.0),
        ("T_s", 0.0),
        ("dt_s", 0.0),
        ("duration_s", 1.0),
    ],
)
def test_invalid_direct_nomoto_inputs_raise(field, value):
    from digitalmodel.naval_architecture.turning_circle import simulate_nomoto_turning_circle

    kwargs = dict(BASE_CASE)
    kwargs[field] = value
    if field == "duration_s":
        kwargs["dt_s"] = 2.0
    with pytest.raises(ValueError):
        simulate_nomoto_turning_circle(**kwargs)


def test_heading_crossing_metrics_use_interpolation():
    from digitalmodel.naval_architecture.turning_circle import simulate_nomoto_turning_circle

    result = simulate_nomoto_turning_circle(
        speed_m_s=6.0,
        rudder_angle_deg=25.0,
        K_per_s=0.2,
        T_s=15.0,
        duration_s=180.0,
        dt_s=2.0,
    )

    metric_time = result["metrics"]["advance_transfer_time_s"]
    sampled_times = {row["time_s"] for row in result["time_history"]}
    assert metric_time not in sampled_times
    assert 0.0 < metric_time < 180.0
    assert math.isfinite(result["metrics"]["advance_m"])
    assert math.isfinite(result["metrics"]["transfer_m"])
