# ABOUTME: TDD tests for typical-ship rudder-induced yaw moment sweeps.
# ABOUTME: Locks YAML, sign convention, outputs, charts, and provenance for issue #2564.

import csv
import json
import math
import subprocess
import zipfile
from importlib import resources
from pathlib import Path

import pytest
import yaml

from digitalmodel.naval_architecture.maneuverability import rudder_normal_force


BASE_RUDDER_KWARGS = {
    "velocity_m_s": 5.0,
    "rho_kg_m3": 1025.0,
    "rudder_area_m2": 20.0,
    "rudder_span_m": 5.0,
    "rudder_angle_deg": 10.0,
    "behind_hull": False,
}


def test_existing_rudder_normal_force_positive_angle_precondition():
    scalar_normal_force_N = rudder_normal_force(**BASE_RUDDER_KWARGS)
    assert scalar_normal_force_N > 0


def test_yaw_moment_positive_angle_stern_rudder_declared_port_mapping():
    from digitalmodel.naval_architecture.yaw_moment import rudder_yaw_moment

    result = rudder_yaw_moment(
        **BASE_RUDDER_KWARGS,
        x_rudder_from_cg_m=-45.0,
        positive_force_direction="port",
    )

    assert result.scalar_normal_force_N > 0
    assert result.transverse_force_N > 0
    assert result.yaw_moment_Nm < 0
    assert result.yaw_moment_Nm == pytest.approx(result.transverse_force_N * -45.0)
    assert result.metadata["sign_convention"]["positive_yaw_moment"] == "bow_to_port"


def test_yaw_moment_starboard_mapping_flips_transverse_force_and_moment():
    from digitalmodel.naval_architecture.yaw_moment import rudder_yaw_moment

    port = rudder_yaw_moment(
        **BASE_RUDDER_KWARGS,
        x_rudder_from_cg_m=-45.0,
        positive_force_direction="port",
    )
    starboard = rudder_yaw_moment(
        **BASE_RUDDER_KWARGS,
        x_rudder_from_cg_m=-45.0,
        positive_force_direction="starboard",
    )

    assert starboard.scalar_normal_force_N == pytest.approx(port.scalar_normal_force_N)
    assert starboard.transverse_force_N == pytest.approx(-port.transverse_force_N)
    assert starboard.yaw_moment_Nm == pytest.approx(-port.yaw_moment_Nm)


def test_yaw_moment_zero_rudder_angle_is_zero():
    from digitalmodel.naval_architecture.yaw_moment import rudder_yaw_moment

    result = rudder_yaw_moment(
        velocity_m_s=5.0,
        rho_kg_m3=1025.0,
        rudder_area_m2=20.0,
        rudder_span_m=5.0,
        rudder_angle_deg=0.0,
        x_rudder_from_cg_m=-45.0,
        behind_hull=False,
    )
    assert result.scalar_normal_force_N == pytest.approx(0.0)
    assert result.yaw_moment_Nm == pytest.approx(0.0)


def test_yaw_moment_scales_with_speed_squared():
    from digitalmodel.naval_architecture.yaw_moment import rudder_yaw_moment

    low = rudder_yaw_moment(**BASE_RUDDER_KWARGS, x_rudder_from_cg_m=-45.0)
    high = rudder_yaw_moment(
        **{**BASE_RUDDER_KWARGS, "velocity_m_s": 10.0},
        x_rudder_from_cg_m=-45.0,
    )
    assert high.yaw_moment_Nm == pytest.approx(4.0 * low.yaw_moment_Nm)


def test_yaw_moment_uses_keyword_call_to_rudder_normal_force(monkeypatch):
    import digitalmodel.naval_architecture.yaw_moment as module

    captured = {}

    def fake_rudder_normal_force(**kwargs):
        captured.update(kwargs)
        return 3.0

    monkeypatch.setattr(module, "rudder_normal_force", fake_rudder_normal_force)
    result = module.rudder_yaw_moment(
        velocity_m_s=5.0,
        rho_kg_m3=1025.0,
        rudder_area_m2=20.0,
        rudder_span_m=5.0,
        rudder_angle_deg=10.0,
        x_rudder_from_cg_m=-45.0,
        behind_hull=False,
    )

    assert captured == BASE_RUDDER_KWARGS
    assert result.yaw_moment_Nm == pytest.approx(-135.0)


def test_load_packaged_typical_ship_yaml_with_importlib_resources():
    resource = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "yaw_moment_typical_ship.yml"
    )
    with resource.open("r", encoding="utf-8") as stream:
        payload = yaml.safe_load(stream)

    assert payload["case"]["id"] == "typical_single_screw_ship"
    assert payload["outputs"]["charts"]["required"] == [
        "yaw_moment_vs_rudder_angle_by_speed",
        "yaw_moment_vs_speed_by_rudder_angle",
        "transverse_force_vs_rudder_angle_by_speed",
        "yaw_moment_speed_angle_heatmap",
    ]


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
            "--no-isolation",
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
            "digitalmodel/naval_architecture/data/yaw_moment_typical_ship.yml"
            in archive.namelist()
        )
        assert (
            "digitalmodel/subsea/cross_sections/fixtures/66kv_inter_array_cable.yml"
            in archive.namelist()
        )


def test_load_user_yaml_from_explicit_path(tmp_path):
    from digitalmodel.naval_architecture.yaw_moment import load_yaw_moment_input

    source = resources.files("digitalmodel.naval_architecture.data").joinpath(
        "yaw_moment_typical_ship.yml"
    )
    user_path = tmp_path / "case.yml"
    user_path.write_text(source.read_text(encoding="utf-8"), encoding="utf-8")

    config = load_yaw_moment_input(user_path)
    assert config.case_id == "typical_single_screw_ship"


def test_load_typical_ship_yaml_converts_knots():
    from digitalmodel.naval_architecture.yaw_moment import KNOT_TO_M_PER_S, load_packaged_typical_ship_yaml

    config = load_packaged_typical_ship_yaml()
    assert config.speeds_m_s[2] == pytest.approx(5.0 * KNOT_TO_M_PER_S)
    assert config.speeds_kn[2] == pytest.approx(5.0)


def test_sweep_cardinality_matches_grid():
    from digitalmodel.naval_architecture.yaw_moment import load_packaged_typical_ship_yaml, run_yaw_moment_sweep

    config = load_packaged_typical_ship_yaml()
    result = run_yaw_moment_sweep(config)
    assert len(result["rows"]) == 35


def test_zero_speed_all_angles_are_zero_force_and_moment():
    from digitalmodel.naval_architecture.yaw_moment import load_packaged_typical_ship_yaml, run_yaw_moment_sweep

    result = run_yaw_moment_sweep(load_packaged_typical_ship_yaml())
    zero_rows = [row for row in result["rows"] if row["speed_m_s"] == 0.0]
    assert zero_rows
    assert all(row["scalar_normal_force_N"] == pytest.approx(0.0) for row in zero_rows)
    assert all(row["yaw_moment_Nm"] == pytest.approx(0.0) for row in zero_rows)


def test_write_results_csv_json_and_required_chart_manifest(tmp_path):
    from digitalmodel.naval_architecture.yaw_moment import (
        CSV_HEADERS,
        REQUIRED_CHARTS,
        load_packaged_typical_ship_yaml,
        run_yaw_moment_sweep,
        write_yaw_moment_results,
    )

    config = load_packaged_typical_ship_yaml()
    result = run_yaw_moment_sweep(config)
    manifest = write_yaw_moment_results(result, tmp_path, table_formats=("csv", "json"), chart_formats=("png", "html"))

    with open(manifest["tables"]["csv"], newline="", encoding="utf-8") as stream:
        reader = csv.reader(stream)
        assert next(reader) == CSV_HEADERS
    with open(manifest["tables"]["json"], encoding="utf-8") as stream:
        payload = json.load(stream)
    assert set(payload) == {"metadata", "provenance", "rows", "artifacts"}
    assert payload["metadata"]["units"]["yaw_moment_Nm"] == "N*m"
    assert payload["metadata"]["sign_convention"]["positive_force_direction"] == "port"
    assert "code_id" not in payload["provenance"]
    assert set(manifest["charts"]) == set(REQUIRED_CHARTS)

    for chart_name in REQUIRED_CHARTS:
        png = manifest["charts"][chart_name]["png"]
        html = manifest["charts"][chart_name]["html"]
        assert png.read_bytes().startswith(b"\x89PNG")
        html_text = html.read_text(encoding="utf-8")
        assert chart_name.replace("_", " ").title() in html_text
        assert "Plotly.newPlot" in html_text


def test_yaw_moment_heatmap_grid_shape():
    from digitalmodel.naval_architecture.yaw_moment import build_yaw_moment_heatmap_grid, load_packaged_typical_ship_yaml, run_yaw_moment_sweep

    config = load_packaged_typical_ship_yaml()
    result = run_yaw_moment_sweep(config)
    grid = build_yaw_moment_heatmap_grid(result["rows"])
    assert len(grid["z"]) == len(config.speeds_m_s)
    assert all(len(row) == len(config.rudder_angles_deg) for row in grid["z"])


@pytest.mark.parametrize(
    "field,value",
    [
        ("velocity_m_s", -0.1),
        ("rho_kg_m3", 0.0),
        ("rho_kg_m3", math.nan),
        ("rudder_angle_deg", math.inf),
        ("rudder_area_m2", 0.0),
        ("rudder_span_m", 0.0),
        ("x_rudder_from_cg_m", math.nan),
    ],
)
def test_invalid_inputs_rejected(field, value):
    from digitalmodel.naval_architecture.yaw_moment import rudder_yaw_moment

    kwargs = {
        **BASE_RUDDER_KWARGS,
        "x_rudder_from_cg_m": -45.0,
    }
    kwargs[field] = value
    with pytest.raises(ValueError):
        rudder_yaw_moment(**kwargs)
