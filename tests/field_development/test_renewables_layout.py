# ABOUTME: Tests for the renewables layout extension (#1513, epic #1507 workstream B3).
# ABOUTME: Wind asset kinds, electrical cable validation, wind demo e2e, determinism.
"""Renewables extension of the field-layout schema (#1513).

Covers the new asset kinds (``wind_turbine``, ``offshore_substation``), the
electrical connection kinds (``array_cable``, ``export_cable`` with
``voltage_kv`` / ``conductor_size_mm2``), the electrical-connectivity
validation rules, the floating-wind demo config end-to-end (layout + plot +
scene JSON), and determinism. Offline: no Blender needed — the fly-through
script consumes the scene JSON asserted here.
"""

from __future__ import annotations

import json
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.field_development.layout_model import (
    ASSET_KINDS,
    CONNECTION_KINDS,
    ELECTRICAL_ASSET_KINDS,
    ELECTRICAL_CONNECTION_KINDS,
    build_layout_model,
    build_layout_model_from_file,
    layout_model_to_config,
    layout_summary,
    load_layout_config,
)
from digitalmodel.field_development.visualization import (
    ASSET_STYLES,
    CONNECTION_STYLES,
    SCENE_SCHEMA,
    export_layout_scene_json,
    layout_scene_dict,
    render_layout_visualization,
)

DATA_DIR = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "field_development"
    / "data"
)
WIND_CONFIG = DATA_DIR / "offshore_wind_demo_field.yml"
OFFSHORE_CONFIG = DATA_DIR / "offshore_demo_field.yml"


def _flat_surface(base_elevation_m: float, extent_m: float = 1000.0) -> dict:
    """Flat synthetic surface spec (amplitude 0) at a fixed elevation."""
    return {
        "kind": "synthetic",
        "x_min_m": 0.0,
        "x_max_m": extent_m,
        "y_min_m": 0.0,
        "y_max_m": extent_m,
        "nx": 11,
        "ny": 11,
        "base_elevation_m": base_elevation_m,
        "amplitude_m": 0.0,
        "x_wavelength_m": extent_m / 2.0,
        "y_wavelength_m": extent_m / 2.0,
    }


def _minimal_config(assets: list, connections: list) -> dict:
    return {
        "field": {"name": "T"},
        "surface": _flat_surface(-100.0),
        "assets": assets,
        "connections": connections,
        "routing": {"sample_spacing_m": 100.0},
    }


def _wind_assets() -> list:
    """One of each electrical-capable kind plus a well/manifold for negatives."""
    return [
        {"id": "T1", "kind": "wind_turbine", "x_m": 0.0, "y_m": 0.0, "z_m": 0.0},
        {"id": "T2", "kind": "wind_turbine", "x_m": 500.0, "y_m": 0.0, "z_m": 0.0},
        {
            "id": "OSS",
            "kind": "offshore_substation",
            "x_m": 500.0,
            "y_m": 500.0,
            "z_m": 0.0,
        },
        {"id": "H", "kind": "host", "x_m": 900.0, "y_m": 500.0},
        {"id": "W", "kind": "well", "x_m": 0.0, "y_m": 500.0},
        {"id": "M", "kind": "manifold", "x_m": 100.0, "y_m": 500.0},
    ]


# --- schema vocabulary ---------------------------------------------------------
class TestSchemaVocabulary:
    def test_wind_kinds_registered(self):
        assert {"wind_turbine", "offshore_substation"} <= ASSET_KINDS
        assert {"array_cable", "export_cable"} <= CONNECTION_KINDS
        assert ELECTRICAL_CONNECTION_KINDS == {"array_cable", "export_cable"}
        assert ELECTRICAL_ASSET_KINDS == {
            "wind_turbine",
            "offshore_substation",
            "host",
        }

    def test_wind_demo_config_loads(self):
        config = load_layout_config(WIND_CONFIG)
        assert config["field"]["name"] == "Demo Floating Wind Farm"
        kinds = {a["kind"] for a in config["assets"]}
        assert kinds == {"wind_turbine", "offshore_substation", "host"}
        assert {c["kind"] for c in config["connections"]} == {
            "array_cable",
            "export_cable",
        }

    def test_rated_power_and_subtype_parsed(self):
        model = build_layout_model_from_file(WIND_CONFIG)
        turbines = model.assets_of_kind("wind_turbine")
        assert len(turbines) == 6
        for turbine in turbines:
            assert turbine.subtype == "floating"
            assert turbine.rated_power_mw == pytest.approx(15.0)
            assert turbine.on_surface is False  # floats at the sea surface
            assert turbine.z_m == pytest.approx(0.0)

    def test_electrical_properties_parsed(self):
        model = build_layout_model_from_file(WIND_CONFIG)
        for cable in model.connections_of_kind("array_cable"):
            assert cable.voltage_kv == pytest.approx(66.0)
            assert cable.conductor_size_mm2 in (300.0, 400.0)
            assert cable.inner_diameter_m is None  # no bore hydraulics
        export = model.connections_of_kind("export_cable")
        assert len(export) == 1
        assert export[0].voltage_kv == pytest.approx(220.0)
        assert export[0].conductor_size_mm2 == pytest.approx(800.0)
        assert export[0].voltage_kv > 66.0  # export above collection voltage


# --- electrical-connectivity validation ----------------------------------------
class TestElectricalValidation:
    def test_array_cable_to_well_rejected(self):
        config = _minimal_config(
            _wind_assets(),
            [{"id": "AC", "kind": "array_cable", "from": "T1", "to": "W"}],
        )
        with pytest.raises(ValueError, match="electrical-capable.*'W' is a 'well'"):
            build_layout_model(config)

    def test_export_cable_from_manifold_rejected(self):
        config = _minimal_config(
            _wind_assets(),
            [{"id": "EX", "kind": "export_cable", "from": "M", "to": "H"}],
        )
        with pytest.raises(ValueError, match="'M' is a 'manifold'"):
            build_layout_model(config)

    def test_flowline_to_turbine_rejected(self):
        config = _minimal_config(
            _wind_assets(),
            [{"id": "FL", "kind": "flowline", "from": "M", "to": "T1"}],
        )
        with pytest.raises(ValueError, match="power-only asset 'T1'"):
            build_layout_model(config)

    def test_umbilical_to_substation_rejected(self):
        config = _minimal_config(
            _wind_assets(),
            [{"id": "UMB", "kind": "umbilical", "from": "H", "to": "OSS"}],
        )
        with pytest.raises(ValueError, match="power-only asset 'OSS'"):
            build_layout_model(config)

    def test_valid_electrical_topology_accepted(self):
        config = _minimal_config(
            _wind_assets(),
            [
                {"id": "AC1", "kind": "array_cable", "from": "T1", "to": "T2"},
                {"id": "AC2", "kind": "array_cable", "from": "T2", "to": "OSS"},
                {"id": "EX1", "kind": "export_cable", "from": "OSS", "to": "H"},
            ],
        )
        model = build_layout_model(config)
        assert len(model.connections) == 3

    def test_voltage_on_non_electrical_connection_rejected(self):
        config = _minimal_config(
            _wind_assets(),
            [
                {
                    "id": "FL",
                    "kind": "flowline",
                    "from": "W",
                    "to": "M",
                    "voltage_kv": 66.0,
                }
            ],
        )
        with pytest.raises(ValueError, match="not an electrical kind"):
            build_layout_model(config)

    def test_non_positive_voltage_rejected(self):
        config = _minimal_config(
            _wind_assets(),
            [
                {
                    "id": "AC",
                    "kind": "array_cable",
                    "from": "T1",
                    "to": "T2",
                    "voltage_kv": 0.0,
                }
            ],
        )
        with pytest.raises(ValueError, match="voltage_kv must be > 0"):
            build_layout_model(config)

    def test_non_positive_rated_power_rejected(self):
        assets = _wind_assets()
        assets[0]["rated_power_mw"] = -5.0
        with pytest.raises(ValueError, match="rated_power_mw must be > 0"):
            build_layout_model(_minimal_config(assets, []))


# --- schema round-trip ----------------------------------------------------------
class TestRoundTrip:
    def test_wind_demo_round_trips_through_yaml(self, tmp_path):
        model = build_layout_model_from_file(WIND_CONFIG)
        config = layout_model_to_config(model)
        dumped = tmp_path / "roundtrip.yml"
        dumped.write_text(yaml.safe_dump(config), encoding="utf-8")

        rebuilt = build_layout_model_from_file(dumped)
        assert layout_summary(rebuilt) == layout_summary(model)
        for c0, c1 in zip(model.connections, rebuilt.connections):
            assert c1.kind == c0.kind
            assert c1.voltage_kv == c0.voltage_kv
            assert c1.conductor_size_mm2 == c0.conductor_size_mm2
            assert c1.route_length_m == pytest.approx(c0.route_length_m)
        for a0, a1 in zip(model.assets, rebuilt.assets):
            assert a1.kind == a0.kind
            assert a1.subtype == a0.subtype
            assert a1.rated_power_mw == a0.rated_power_mw

    def test_electrical_properties_reemitted(self):
        model = build_layout_model_from_file(WIND_CONFIG)
        config = layout_model_to_config(model)
        ac1 = next(c for c in config["connections"] if c["id"] == "AC-1")
        assert ac1["voltage_kv"] == pytest.approx(66.0)
        assert ac1["conductor_size_mm2"] == pytest.approx(300.0)
        wtg1 = next(a for a in config["assets"] if a["id"] == "WTG-1")
        assert wtg1["rated_power_mw"] == pytest.approx(15.0)
        assert wtg1["z_m"] == pytest.approx(0.0)  # floating override re-emitted


# --- wind demo end-to-end -------------------------------------------------------
class TestWindDemoEndToEnd:
    def test_asset_taxonomy_counts(self):
        model = build_layout_model_from_file(WIND_CONFIG)
        assert len(model.assets_of_kind("wind_turbine")) == 6
        assert len(model.assets_of_kind("offshore_substation")) == 1
        assert len(model.assets_of_kind("host")) == 1
        assert len(model.connections_of_kind("array_cable")) == 6
        assert len(model.connections_of_kind("export_cable")) == 1

    def test_bathymetry_is_floating_wind_depth_range(self):
        model = build_layout_model_from_file(WIND_CONFIG)
        assert float(model.surface.z_m.max()) < 0.0  # fully subsea
        assert float(model.surface.z_m.min()) > -300.0  # not deepwater O&G depths

    def test_array_cable_plan_length_hand_computed(self):
        model = build_layout_model_from_file(WIND_CONFIG)
        ac1 = next(c for c in model.connections if c.connection_id == "AC-1")
        # WTG-1 (1500, 2500) -> WTG-2 (3500, 2500): straight 2000 m in plan
        assert ac1.plan_length_m == pytest.approx(2000.0)
        # both ends float at z = 0 over ~150 m water depth: the sampled route
        # dips to the seabed, so 3D length exceeds plan length
        assert ac1.route_length_m > ac1.plan_length_m
        assert ac1.elevation_change_m == pytest.approx(0.0)

    def test_export_cable_routes_via_waypoint(self):
        model = build_layout_model_from_file(WIND_CONFIG)
        export = model.connections_of_kind("export_cable")[0]
        assert export.waypoints_plan_m == [(8000.0, 4000.0)]
        # OSS-1 (7000,4000) -> WP (8000,4000) -> SHORE-1 (8800,4000)
        assert export.plan_length_m == pytest.approx(
            math.hypot(1000.0, 0.0) + math.hypot(800.0, 0.0)
        )
        assert export.route_length_m >= export.plan_length_m

    def test_summary_is_json_able(self):
        summary = layout_summary(build_layout_model_from_file(WIND_CONFIG))
        assert summary["surface"]["is_subsea"] is True
        assert summary["assets_by_kind"] == {
            "wind_turbine": 6,
            "offshore_substation": 1,
            "host": 1,
        }
        json.dumps(summary)  # must not raise


# --- visualization: 2D plot + scene JSON ----------------------------------------
class TestWindVisualization:
    def test_every_wind_kind_has_styles(self):
        # style maps stay complete after the vocabulary extension
        assert set(ASSET_STYLES) == set(ASSET_KINDS)
        assert set(CONNECTION_STYLES) == set(CONNECTION_KINDS)

    def test_demo_configs_jointly_cover_all_kinds(self):
        wind = layout_scene_dict(build_layout_model_from_file(WIND_CONFIG))
        oil = layout_scene_dict(build_layout_model_from_file(OFFSHORE_CONFIG))
        asset_kinds = {a["kind"] for a in wind["assets"] + oil["assets"]}
        connection_kinds = {c["kind"] for c in wind["connections"] + oil["connections"]}
        assert asset_kinds == set(ASSET_KINDS)
        assert connection_kinds == set(CONNECTION_KINDS)

    def test_wind_scene_schema_unchanged_and_carries_kinds(self, tmp_path):
        # New kinds ride inside the existing scene shape: schema stays at /2.
        model = build_layout_model_from_file(WIND_CONFIG)
        out = tmp_path / "scene.json"
        export_layout_scene_json(model, out)
        scene = json.loads(out.read_text(encoding="utf-8"))
        assert scene["schema"] == SCENE_SCHEMA == "digitalmodel.field_layout_scene/2"
        assert {a["kind"] for a in scene["assets"]} == {
            "wind_turbine",
            "offshore_substation",
            "host",
        }
        assert scene["is_subsea"] is True
        turbine = next(a for a in scene["assets"] if a["id"] == "WTG-1")
        assert turbine["subtype"] == "floating"
        assert turbine["on_surface"] is False

    def test_end_to_end_artifacts(self, tmp_path):
        result = render_layout_visualization(WIND_CONFIG, tmp_path)
        plot = Path(result["plot_path"])
        scene_path = Path(result["scene_path"])
        assert plot.exists() and plot.stat().st_size > 10_000
        assert scene_path.exists()
        assert result["field"] == "Demo Floating Wind Farm"
        for conn in result["connections"]:
            assert conn["route_length_m"] >= conn["plan_length_m"] > 0.0

    def test_scene_export_is_deterministic(self, tmp_path):
        model = build_layout_model_from_file(WIND_CONFIG)
        a, b = tmp_path / "a.json", tmp_path / "b.json"
        export_layout_scene_json(model, a)
        export_layout_scene_json(model, b)
        assert a.read_bytes() == b.read_bytes()

    def test_model_build_is_deterministic(self):
        first = layout_summary(build_layout_model_from_file(WIND_CONFIG))
        second = layout_summary(build_layout_model_from_file(WIND_CONFIG))
        assert first == second
