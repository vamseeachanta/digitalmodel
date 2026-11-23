# ABOUTME: Tests for onshore_layout — field-development tracer (#1508, epic #1507).
# ABOUTME: Loader, terrain, 3D routing, Darcy-Weisbach screening, plot, API end-to-end.
"""Tests for digitalmodel.field_development.onshore_layout."""

from __future__ import annotations

import json
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.field_development.onshore_layout import (
    LAMINAR_REYNOLDS_LIMIT,
    build_layout,
    build_terrain,
    darcy_weisbach_pressure_drop,
    load_field_config,
    screen_layout,
    swamee_jain_friction_factor,
)

DEMO_CONFIG = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "field_development"
    / "data"
    / "onshore_demo_field.yml"
)


def _minimal_layout_config(terrain: dict, host_xy=(0.0, 0.0), well_xy=(300.0, 0.0)):
    """Smallest config build_layout accepts — one well, one flowline."""
    return {
        "field": {"name": "T"},
        "terrain": terrain,
        "host": {"id": "H", "x_m": host_xy[0], "y_m": host_xy[1]},
        "wells": [
            {"id": "W", "x_m": well_xy[0], "y_m": well_xy[1], "rate_m3_per_day": 100.0}
        ],
        "flowlines": [
            {
                "id": "FL",
                "from_well": "W",
                "to": "H",
                "inner_diameter_m": 0.1524,
                "roughness_m": 4.5e-5,
            }
        ],
        "routing": {"sample_spacing_m": 10.0},
    }


# --- config loader -----------------------------------------------------------
class TestLoadFieldConfig:
    def test_bundled_demo_config_loads(self):
        config = load_field_config(DEMO_CONFIG)
        assert config["field"]["name"] == "Demo Onshore Field"
        assert len(config["wells"]) == 3
        assert len(config["flowlines"]) == 3
        assert config["terrain"]["kind"] == "synthetic"

    def test_missing_section_raises(self, tmp_path):
        config = load_field_config(DEMO_CONFIG)
        del config["fluid"]
        bad = tmp_path / "bad.yml"
        bad.write_text(yaml.safe_dump(config), encoding="utf-8")
        with pytest.raises(ValueError, match="missing required sections.*fluid"):
            load_field_config(bad)

    def test_unknown_flowline_endpoint_raises(self):
        config = _minimal_layout_config(
            {
                "kind": "synthetic",
                "x_min_m": 0,
                "x_max_m": 1000,
                "y_min_m": 0,
                "y_max_m": 1000,
                "nx": 11,
                "ny": 11,
                "base_elevation_m": 100.0,
                "amplitude_m": 0.0,
                "x_wavelength_m": 500.0,
                "y_wavelength_m": 500.0,
            }
        )
        config["flowlines"][0]["from_well"] = "NOPE"
        with pytest.raises(ValueError, match="unknown asset 'NOPE'"):
            build_layout(config)


# --- terrain -----------------------------------------------------------------
class TestTerrain:
    def test_synthetic_grid_matches_analytic_at_nodes(self):
        spec = {
            "kind": "synthetic",
            "x_min_m": 0,
            "x_max_m": 3000,
            "y_min_m": 0,
            "y_max_m": 3000,
            "nx": 61,
            "ny": 61,
            "base_elevation_m": 350.0,
            "amplitude_m": 40.0,
            "x_wavelength_m": 2400.0,
            "y_wavelength_m": 1800.0,
        }
        terrain = build_terrain(spec, Path("."))
        # node (0, 0): sin(0) = 0 -> base elevation
        assert terrain.elevation_at(0.0, 0.0) == pytest.approx(350.0)
        # node (600, 0): x = wavelength/4 -> sin = 1, cos(0) = 1 -> base + amplitude
        assert terrain.elevation_at(600.0, 0.0) == pytest.approx(390.0)

    def test_csv_grid_bilinear_is_exact_on_a_plane(self, tmp_path):
        # plane z = x/10 + y/10 sampled on a 2x2 grid
        (tmp_path / "dem.csv").write_text("0,30\n40,70\n", encoding="utf-8")
        spec = {
            "kind": "csv_grid",
            "csv_path": "dem.csv",
            "x_min_m": 0,
            "x_max_m": 300,
            "y_min_m": 0,
            "y_max_m": 400,
        }
        terrain = build_terrain(spec, tmp_path)
        assert terrain.elevation_at(150.0, 200.0) == pytest.approx(35.0)
        assert terrain.elevation_at(300.0, 400.0) == pytest.approx(70.0)

    def test_out_of_domain_query_raises(self):
        spec = {
            "kind": "synthetic",
            "x_min_m": 0,
            "x_max_m": 100,
            "y_min_m": 0,
            "y_max_m": 100,
            "nx": 3,
            "ny": 3,
            "base_elevation_m": 0.0,
            "amplitude_m": 0.0,
            "x_wavelength_m": 100.0,
            "y_wavelength_m": 100.0,
        }
        terrain = build_terrain(spec, Path("."))
        with pytest.raises(ValueError, match="outside the terrain domain"):
            terrain.elevation_at(101.0, 50.0)

    def test_unknown_kind_raises(self):
        with pytest.raises(ValueError, match="terrain.kind"):
            build_terrain({"kind": "usgs_3dep"}, Path("."))


# --- routing over terrain ----------------------------------------------------
class TestRouting:
    def test_flat_terrain_3d_length_equals_plan_length(self):
        config = _minimal_layout_config(
            {
                "kind": "synthetic",
                "x_min_m": 0,
                "x_max_m": 1000,
                "y_min_m": 0,
                "y_max_m": 1000,
                "nx": 11,
                "ny": 11,
                "base_elevation_m": 200.0,
                "amplitude_m": 0.0,
                "x_wavelength_m": 500.0,
                "y_wavelength_m": 500.0,
            },
            host_xy=(0.0, 0.0),
            well_xy=(300.0, 400.0),
        )
        layout = build_layout(config)
        fl = layout.flowlines[0]
        assert fl.plan_length_m == pytest.approx(500.0)
        assert fl.terrain_length_m == pytest.approx(500.0)
        assert fl.elevation_change_m == pytest.approx(0.0)

    def test_planar_slope_3d_length_is_hypotenuse(self, tmp_path):
        # uniform slope in x only: z rises 0 -> 30 m over 300 m
        (tmp_path / "slope.csv").write_text("0,30\n0,30\n", encoding="utf-8")
        config = _minimal_layout_config(
            {
                "kind": "csv_grid",
                "csv_path": "slope.csv",
                "x_min_m": 0,
                "x_max_m": 300,
                "y_min_m": 0,
                "y_max_m": 100,
            },
            host_xy=(0.0, 50.0),
            well_xy=(300.0, 50.0),
        )
        layout = build_layout(config, base_dir=tmp_path)
        fl = layout.flowlines[0]
        assert fl.plan_length_m == pytest.approx(300.0)
        # straight line on a plane: L3d = sqrt(plan^2 + dz^2)
        assert fl.terrain_length_m == pytest.approx(math.hypot(300.0, 30.0))
        # flowline runs well -> host, i.e. downhill: dz = z_host - z_well = -30
        assert fl.elevation_change_m == pytest.approx(-30.0)

    def test_assets_sit_on_terrain_surface(self):
        config = load_field_config(DEMO_CONFIG)
        layout = build_layout(config, base_dir=DEMO_CONFIG.parent)
        for asset in [layout.host, *layout.wells]:
            assert asset.z_m == pytest.approx(
                layout.terrain.elevation_at(asset.x_m, asset.y_m)
            )


# --- screening calc ----------------------------------------------------------
class TestScreeningCalc:
    def test_turbulent_darcy_weisbach_hand_calc(self):
        """Hand-computed: rho=850, mu=5 cP, D=0.1524 m, eps=45 um, v=1 m/s, L=1 km.

        A  = pi 0.1524^2/4          = 0.0182415 m^2
        Re = 850*1*0.1524/0.005     = 25,908
        f  (Swamee-Jain)            = 0.02504
        dp = f (L/D) rho v^2/2      = 0.02504 * 6561.7 * 425 = 69.8 kPa
        """
        area = math.pi * 0.1524**2 / 4.0
        result = darcy_weisbach_pressure_drop(
            rate_m3_per_s=area * 1.0,  # v = 1 m/s exactly
            length_m=1_000.0,
            inner_diameter_m=0.1524,
            roughness_m=4.5e-5,
            density_kg_per_m3=850.0,
            viscosity_pa_s=0.005,
        )
        assert result["velocity_m_per_s"] == pytest.approx(1.0)
        assert result["reynolds"] == pytest.approx(25_908.0)
        assert result["flow_regime"] == "turbulent"
        assert result["friction_factor"] == pytest.approx(0.02504, rel=1e-3)
        assert result["dp_friction_kpa"] == pytest.approx(69.8, rel=5e-3)
        assert result["dp_elevation_kpa"] == pytest.approx(0.0)

    def test_laminar_matches_hagen_poiseuille(self):
        # Re = 850*0.5*0.1524/0.05 = 1295 < 2300 -> laminar
        d, v, rho, mu, length = 0.1524, 0.5, 850.0, 0.05, 500.0
        area = math.pi * d**2 / 4.0
        result = darcy_weisbach_pressure_drop(
            rate_m3_per_s=area * v,
            length_m=length,
            inner_diameter_m=d,
            roughness_m=4.5e-5,
            density_kg_per_m3=rho,
            viscosity_pa_s=mu,
        )
        assert result["flow_regime"] == "laminar"
        assert result["reynolds"] < LAMINAR_REYNOLDS_LIMIT
        # laminar Darcy-Weisbach reduces to Hagen-Poiseuille: dp = 32 mu L v / D^2
        expected_kpa = 32.0 * mu * length * v / d**2 / 1_000.0
        assert result["dp_friction_kpa"] == pytest.approx(expected_kpa, rel=1e-9)

    def test_elevation_term_is_rho_g_dz(self):
        area = math.pi * 0.1524**2 / 4.0
        result = darcy_weisbach_pressure_drop(
            rate_m3_per_s=area * 1.0,
            length_m=100.0,
            inner_diameter_m=0.1524,
            roughness_m=4.5e-5,
            density_kg_per_m3=850.0,
            viscosity_pa_s=0.005,
            elevation_change_m=10.0,
        )
        assert result["dp_elevation_kpa"] == pytest.approx(
            850.0 * 9.80665 * 10.0 / 1_000.0
        )
        assert result["dp_total_kpa"] == pytest.approx(
            result["dp_friction_kpa"] + result["dp_elevation_kpa"]
        )

    def test_laminar_friction_factor_is_64_over_re(self):
        assert swamee_jain_friction_factor(1_000.0, 3e-4) == pytest.approx(0.064)

    def test_invalid_inputs_raise(self):
        with pytest.raises(ValueError):
            swamee_jain_friction_factor(0.0, 3e-4)
        with pytest.raises(ValueError):
            darcy_weisbach_pressure_drop(
                rate_m3_per_s=0.0,
                length_m=100.0,
                inner_diameter_m=0.1524,
                roughness_m=4.5e-5,
                density_kg_per_m3=850.0,
                viscosity_pa_s=0.005,
            )


# --- API end-to-end ----------------------------------------------------------
class TestScreenLayoutApi:
    def test_end_to_end_on_bundled_demo_field(self, tmp_path):
        result = screen_layout(DEMO_CONFIG, output_dir=tmp_path)

        assert result["field"] == "Demo Onshore Field"
        assert len(result["wells"]) == 3
        assert len(result["flowlines"]) == 3

        for row in result["flowlines"]:
            assert row["plan_length_m"] > 0
            # 3D length over terrain can never be shorter than plan length
            assert row["terrain_length_m"] >= row["plan_length_m"]
            screening = row["screening"]
            assert screening["velocity_m_per_s"] > 0
            assert screening["reynolds"] > 0
            assert 0 < screening["friction_factor"] < 0.1
            assert math.isfinite(screening["dp_total_kpa"])
            assert screening["velocity_ok"] is True  # demo rates are well below 4 m/s

        plot_path = Path(result["plot_path"])
        assert plot_path.parent == tmp_path
        assert plot_path.exists() and plot_path.stat().st_size > 0

        scene_path = Path(result["scene_path"])
        assert scene_path.exists()
        scene = json.loads(scene_path.read_text(encoding="utf-8"))
        assert scene["field_name"] == "Demo Onshore Field"
        assert len(scene["assets"]) == 4  # host + 3 wells
        assert len(scene["flowlines"]) == 3
        assert len(scene["terrain"]["z_m"]) == 61
        assert len(scene["terrain"]["z_m"][0]) == 61

    def test_results_are_deterministic(self, tmp_path):
        first = screen_layout(DEMO_CONFIG, output_dir=tmp_path / "a")
        second = screen_layout(DEMO_CONFIG, output_dir=tmp_path / "b")
        assert first["flowlines"] == second["flowlines"]
        assert first["terrain"] == second["terrain"]

    def test_velocity_flag_trips_when_threshold_tightened(self, tmp_path):
        config = load_field_config(DEMO_CONFIG)
        config["screening"]["max_velocity_m_per_s"] = 0.01
        # keep the synthetic terrain; only outputs + threshold change
        config["plot"]["output_path"] = "layout.png"
        config["scene_export"]["output_path"] = "scene.json"
        tight = tmp_path / "tight.yml"
        tight.write_text(yaml.safe_dump(config), encoding="utf-8")
        result = screen_layout(tight, output_dir=tmp_path)
        assert all(
            row["screening"]["velocity_ok"] is False for row in result["flowlines"]
        )
