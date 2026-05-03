# ABOUTME: TDD tests for preliminary rudder-stock torque sweeps.
# ABOUTME: Locks sign semantics, packaged YAML, outputs, charts, and provenance for issue #2565.

import csv
import json
import os
import subprocess
import zipfile
from importlib import resources
from pathlib import Path

import pytest
import yaml

from digitalmodel.naval_architecture.maneuverability import rudder_normal_force

# Serialize wheel-build tests across this module under pytest-xdist:
# default `--dist loadscope` keeps a module on one worker, but adding
# `xdist_group` is defense-in-depth in case dispatch policy changes.
# The load-bearing fix is removing `--no-isolation` below — isolated
# builds prevent shared-state races on `build/` and `digitalmodel.egg-info/`
# (issue #2617).
pytestmark = pytest.mark.xdist_group(name="wheel_build")

BASE_RUDDER_KWARGS = {
    "velocity_m_s": 5.0,
    "rho_kg_m3": 1025.0,
    "rudder_area_m2": 20.0,
    "rudder_span_m": 5.0,
    "rudder_angle_deg": 10.0,
    "behind_hull": False,
}


def test_rudder_stock_torque_zero_arm_is_zero_torque():
    from digitalmodel.naval_architecture.rudder_stock_torque import rudder_stock_torque

    result = rudder_stock_torque(
        **BASE_RUDDER_KWARGS,
        stock_to_center_of_pressure_arm_m=0.0,
    )

    assert result.scalar_normal_force_N > 0
    assert result.hydrodynamic_rudder_stock_torque_Nm == pytest.approx(0.0)
    assert result.required_steering_gear_holding_torque_Nm == pytest.approx(0.0)


def test_rudder_stock_torque_scales_linearly_with_arm():
    from digitalmodel.naval_architecture.rudder_stock_torque import rudder_stock_torque

    short = rudder_stock_torque(
        **BASE_RUDDER_KWARGS,
        stock_to_center_of_pressure_arm_m=0.5,
    )
    long = rudder_stock_torque(
        **BASE_RUDDER_KWARGS,
        stock_to_center_of_pressure_arm_m=1.0,
    )

    assert long.hydrodynamic_rudder_stock_torque_Nm == pytest.approx(
        2.0 * short.hydrodynamic_rudder_stock_torque_Nm
    )
    assert long.required_steering_gear_holding_torque_Nm == pytest.approx(
        2.0 * short.required_steering_gear_holding_torque_Nm
    )


def test_rudder_stock_torque_scales_with_speed_squared():
    from digitalmodel.naval_architecture.rudder_stock_torque import rudder_stock_torque

    low = rudder_stock_torque(
        **BASE_RUDDER_KWARGS,
        stock_to_center_of_pressure_arm_m=0.75,
    )
    high = rudder_stock_torque(
        **{**BASE_RUDDER_KWARGS, "velocity_m_s": 10.0},
        stock_to_center_of_pressure_arm_m=0.75,
    )

    assert high.hydrodynamic_rudder_stock_torque_Nm == pytest.approx(
        4.0 * low.hydrodynamic_rudder_stock_torque_Nm
    )


def test_rudder_stock_torque_preserves_scalar_force_source(monkeypatch):
    import importlib

    module = importlib.import_module("digitalmodel.naval_architecture.rudder_stock_torque")

    captured = {}

    def fake_rudder_normal_force(**kwargs):
        captured.update(kwargs)
        return 12.0

    monkeypatch.setattr(module, "rudder_normal_force", fake_rudder_normal_force)
    result = module.rudder_stock_torque(
        velocity_m_s=5.0,
        rho_kg_m3=1025.0,
        rudder_area_m2=20.0,
        rudder_span_m=5.0,
        rudder_angle_deg=10.0,
        stock_to_center_of_pressure_arm_m=0.75,
        behind_hull=False,
    )

    assert captured == BASE_RUDDER_KWARGS
    assert result.hydrodynamic_rudder_stock_torque_Nm == pytest.approx(9.0)
    assert result.required_steering_gear_holding_torque_Nm == pytest.approx(-9.0)


def test_base_case_positive_angle_absolute_torque_sign():
    from digitalmodel.naval_architecture.rudder_stock_torque import rudder_stock_torque

    scalar_normal_force_N = rudder_normal_force(**BASE_RUDDER_KWARGS)
    assert scalar_normal_force_N > 0

    result = rudder_stock_torque(
        **BASE_RUDDER_KWARGS,
        stock_to_center_of_pressure_arm_m=0.75,
    )

    assert result.hydrodynamic_rudder_stock_torque_Nm > 0
    assert result.required_steering_gear_holding_torque_Nm < 0
    assert result.required_steering_gear_holding_torque_Nm == pytest.approx(
        -result.hydrodynamic_rudder_stock_torque_Nm
    )
    assert result.metadata["sign_convention"]["positive_hydrodynamic_torque"]
    assert "counterclockwise viewed from above" in result.metadata["sign_convention"][
        "positive_hydrodynamic_torque"
    ]


def test_negative_angle_reverses_hydrodynamic_and_holding_torque_signs():
    from digitalmodel.naval_architecture.rudder_stock_torque import rudder_stock_torque

    positive = rudder_stock_torque(
        **BASE_RUDDER_KWARGS,
        stock_to_center_of_pressure_arm_m=0.75,
    )
    negative = rudder_stock_torque(
        **{**BASE_RUDDER_KWARGS, "rudder_angle_deg": -10.0},
        stock_to_center_of_pressure_arm_m=0.75,
    )

    assert negative.hydrodynamic_rudder_stock_torque_Nm == pytest.approx(
        -positive.hydrodynamic_rudder_stock_torque_Nm
    )
    assert negative.required_steering_gear_holding_torque_Nm == pytest.approx(
        -positive.required_steering_gear_holding_torque_Nm
    )


def test_single_row_torque_identity():
    from digitalmodel.naval_architecture.rudder_stock_torque import rudder_stock_torque

    arm = 0.75
    result = rudder_stock_torque(
        **BASE_RUDDER_KWARGS,
        stock_to_center_of_pressure_arm_m=arm,
    )

    assert result.hydrodynamic_rudder_stock_torque_Nm == pytest.approx(
        result.scalar_normal_force_N * arm
    )
    assert result.rudder_stock_torque_abs_Nm == pytest.approx(
        abs(result.hydrodynamic_rudder_stock_torque_Nm)
    )


def test_load_packaged_rudder_stock_yaml_with_importlib_resources():
    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "rudder_stock_torque_typical_ship.yml"
    )
    with resource.open("r", encoding="utf-8") as stream:
        payload = yaml.safe_load(stream)

    assert payload["case"]["id"] == "typical_single_screw_ship_rudder_stock_torque"
    assert payload["stock"]["stock_to_center_of_pressure_arm_m"] == pytest.approx(0.75)
    assert payload["outputs"]["charts"]["required"] == [
        "rudder_stock_torque_vs_rudder_angle_by_speed",
        "rudder_stock_torque_vs_speed_by_rudder_angle",
        "scalar_normal_force_vs_rudder_angle_by_speed",
        "rudder_stock_torque_speed_angle_heatmap",
    ]


def test_packaged_yaml_in_built_distribution_preserves_existing_package_data(tmp_path):
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
            "digitalmodel/naval_architecture/data/rudder_stock_torque_typical_ship.yml"
            in archive.namelist()
        )
        assert (
            "digitalmodel/naval_architecture/data/yaw_moment_typical_ship.yml"
            in archive.namelist()
        )
        assert (
            "digitalmodel/subsea/cross_sections/fixtures/66kv_inter_array_cable.yml"
            in archive.namelist()
        )


def test_load_rudder_stock_yaml_converts_knots():
    from digitalmodel.naval_architecture.rudder_stock_torque import (
        KNOT_TO_M_PER_S,
        load_packaged_rudder_stock_torque_yaml,
    )

    config = load_packaged_rudder_stock_torque_yaml()
    assert config.speeds_m_s[2] == pytest.approx(5.0 * KNOT_TO_M_PER_S)
    assert config.speeds_kn[2] == pytest.approx(5.0)


def test_sweep_cardinality_matches_grid():
    from digitalmodel.naval_architecture.rudder_stock_torque import (
        load_packaged_rudder_stock_torque_yaml,
        run_rudder_stock_torque_sweep,
    )

    result = run_rudder_stock_torque_sweep(load_packaged_rudder_stock_torque_yaml())
    assert len(result["rows"]) == 35


def test_zero_speed_all_angles_are_zero_force_and_torque():
    from digitalmodel.naval_architecture.rudder_stock_torque import (
        load_packaged_rudder_stock_torque_yaml,
        run_rudder_stock_torque_sweep,
    )

    result = run_rudder_stock_torque_sweep(load_packaged_rudder_stock_torque_yaml())
    zero_rows = [row for row in result["rows"] if row["speed_m_s"] == 0.0]
    assert zero_rows
    assert all(row["scalar_normal_force_N"] == pytest.approx(0.0) for row in zero_rows)
    assert all(
        row["hydrodynamic_rudder_stock_torque_Nm"] == pytest.approx(0.0)
        for row in zero_rows
    )
    assert all(
        row["required_steering_gear_holding_torque_Nm"] == pytest.approx(0.0)
        for row in zero_rows
    )


def test_invalid_negative_stock_arm_rejected():
    from digitalmodel.naval_architecture.rudder_stock_torque import rudder_stock_torque

    with pytest.raises(ValueError, match="stock_to_center_of_pressure_arm_m"):
        rudder_stock_torque(
            **BASE_RUDDER_KWARGS,
            stock_to_center_of_pressure_arm_m=-0.1,
        )


def test_empty_speeds_or_angles_are_rejected_before_plotting():
    from digitalmodel.naval_architecture.rudder_stock_torque import (
        load_packaged_rudder_stock_torque_yaml,
        validate_rudder_stock_torque_input,
    )

    base = load_packaged_rudder_stock_torque_yaml().raw
    no_speeds = yaml.safe_load(yaml.safe_dump(base))
    no_speeds["sweep"]["speeds"]["values"] = []
    with pytest.raises(ValueError, match="sweep.speeds.values"):
        validate_rudder_stock_torque_input(no_speeds)

    no_angles = yaml.safe_load(yaml.safe_dump(base))
    no_angles["sweep"]["rudder_angles_deg"] = []
    with pytest.raises(ValueError, match="sweep.rudder_angles_deg"):
        validate_rudder_stock_torque_input(no_angles)


def test_required_chart_subset_is_honored(tmp_path):
    from digitalmodel.naval_architecture.rudder_stock_torque import (
        load_packaged_rudder_stock_torque_yaml,
        run_rudder_stock_torque_sweep,
        validate_rudder_stock_torque_input,
        write_rudder_stock_torque_results,
    )

    payload = yaml.safe_load(yaml.safe_dump(load_packaged_rudder_stock_torque_yaml().raw))
    payload["outputs"]["charts"]["required"] = [
        "rudder_stock_torque_speed_angle_heatmap"
    ]
    config = validate_rudder_stock_torque_input(payload)
    result = run_rudder_stock_torque_sweep(config)
    manifest = write_rudder_stock_torque_results(result, tmp_path, chart_formats=("png",))

    assert set(manifest["charts"]) == {"rudder_stock_torque_speed_angle_heatmap"}
    assert (tmp_path / "rudder_stock_torque_speed_angle_heatmap.png").exists()
    assert not (tmp_path / "rudder_stock_torque_vs_rudder_angle_by_speed.png").exists()


def test_write_results_csv_json_sidecar_manifest_and_required_charts(tmp_path):
    from digitalmodel.naval_architecture.rudder_stock_torque import (
        CSV_HEADERS,
        REQUIRED_CHARTS,
        load_packaged_rudder_stock_torque_yaml,
        run_rudder_stock_torque_sweep,
        write_rudder_stock_torque_results,
    )

    config = load_packaged_rudder_stock_torque_yaml()
    result = run_rudder_stock_torque_sweep(config)
    manifest = write_rudder_stock_torque_results(
        result,
        tmp_path,
        table_formats=("csv", "json"),
        chart_formats=("png", "html"),
    )

    assert (tmp_path / "rudder_stock_torque_sweep.csv").exists()
    assert (tmp_path / "rudder_stock_torque_sweep.json").exists()
    assert (tmp_path / "rudder_stock_torque_provenance.json").exists()
    assert (tmp_path / "artifact_manifest.json").exists()
    assert set(manifest["charts"]) == set(REQUIRED_CHARTS)
    for chart_name in REQUIRED_CHARTS:
        assert (tmp_path / f"{chart_name}.png").exists()
        assert (tmp_path / f"{chart_name}.html").exists()

    with (tmp_path / "rudder_stock_torque_sweep.csv").open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == CSV_HEADERS
        first_row = next(reader)
    assert "hydrodynamic_rudder_stock_torque_Nm" in first_row
    assert "required_steering_gear_holding_torque_Nm" in first_row

    payload = json.loads((tmp_path / "rudder_stock_torque_sweep.json").read_text())
    assert payload["artifacts"]["sidecars"]["provenance"].endswith(
        "rudder_stock_torque_provenance.json"
    )


def test_output_rows_include_units_and_abs_torque():
    from digitalmodel.naval_architecture.rudder_stock_torque import (
        load_packaged_rudder_stock_torque_yaml,
        run_rudder_stock_torque_sweep,
    )

    result = run_rudder_stock_torque_sweep(load_packaged_rudder_stock_torque_yaml())
    row = next(row for row in result["rows"] if row["speed_m_s"] > 0 and row["rudder_angle_deg"] > 0)

    assert "hydrodynamic_rudder_stock_torque_Nm" in row
    assert "required_steering_gear_holding_torque_Nm" in row
    assert "rudder_stock_torque_abs_Nm" in row
    assert "rudder_stock_torque_abs_kNm" in row
    assert row["rudder_stock_torque_abs_Nm"] == pytest.approx(
        abs(row["hydrodynamic_rudder_stock_torque_Nm"])
    )
    assert result["metadata"]["units"]["rudder_stock_torque_abs_kNm"] == "kN*m"


def test_provenance_declares_user_supplied_arm_and_constant_arm_assumption(tmp_path):
    from digitalmodel.naval_architecture.rudder_stock_torque import (
        load_packaged_rudder_stock_torque_yaml,
        run_rudder_stock_torque_sweep,
        write_rudder_stock_torque_results,
    )

    result = run_rudder_stock_torque_sweep(load_packaged_rudder_stock_torque_yaml())
    write_rudder_stock_torque_results(result, tmp_path)
    provenance = json.loads((tmp_path / "rudder_stock_torque_provenance.json").read_text())

    assert provenance["torque_relation"] == (
        "hydrodynamic_rudder_stock_torque_Nm = "
        "scalar_normal_force_N * stock_to_center_of_pressure_arm_m"
    )
    assert provenance["holding_torque_relation"] == (
        "required_steering_gear_holding_torque_Nm = "
        "-hydrodynamic_rudder_stock_torque_Nm"
    )
    assert provenance["stock_arm_source"] == "user_supplied_constant_perpendicular_arm"
    assert "not a standards-derived coefficient" in provenance["stock_arm_note"]
    assert "class/SOLAS compliance" in provenance["scope_limitations"]


def test_public_import_surface_outside_pytest_path_injection():
    repo_root = Path(__file__).parents[2]
    result = subprocess.run(
        [
            "uv",
            "run",
            "--no-sync",
            "--with",
            "pyyaml",
            "python",
            "-c",
            (
                "from digitalmodel.naval_architecture import "
                "load_packaged_rudder_stock_torque_yaml, "
                "run_rudder_stock_torque_sweep; "
                "cfg=load_packaged_rudder_stock_torque_yaml(); "
                "assert len(run_rudder_stock_torque_sweep(cfg)['rows']) == 35"
            ),
        ],
        cwd=repo_root,
        env={**os.environ, "PYTHONPATH": "src"},
        text=True,
        capture_output=True,
        check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
