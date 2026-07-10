# ABOUTME: Tests for layout_model — full field-layout model (#1509, epic #1507).
# ABOUTME: Schema round-trip, connectivity validation, waypoint routing, offshore e2e, tracer back-compat.
"""Tests for digitalmodel.field_development.layout_model."""

from __future__ import annotations

import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.field_development.layout_model import (
    ASSET_KINDS,
    CONNECTION_KINDS,
    build_layout_model,
    build_layout_model_from_file,
    layout_model_to_config,
    layout_summary,
    load_layout_config,
)
from digitalmodel.field_development.onshore_layout import build_layout

DATA_DIR = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "field_development"
    / "data"
)
OFFSHORE_CONFIG = DATA_DIR / "offshore_demo_field.yml"
ONSHORE_CONFIG = DATA_DIR / "onshore_demo_field.yml"


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


def _minimal_config(surface: dict, assets: list, connections: list) -> dict:
    return {
        "field": {"name": "T"},
        "surface": surface,
        "assets": assets,
        "connections": connections,
        "routing": {"sample_spacing_m": 10.0},
    }


# --- schema + validation -----------------------------------------------------
class TestSchema:
    def test_offshore_demo_config_loads(self):
        config = load_layout_config(OFFSHORE_CONFIG)
        assert config["field"]["name"] == "Demo Offshore Field"
        kinds = {a["kind"] for a in config["assets"]}
        assert kinds == {"vessel", "manifold", "well", "tree", "host"}
        assert {c["kind"] for c in config["connections"]} == {
            "jumper",
            "flowline",
            "umbilical",
            "pipeline",
        }
        assert kinds <= ASSET_KINDS

    def test_unknown_asset_kind_rejected(self):
        config = _minimal_config(
            _flat_surface(100.0),
            [{"id": "A", "kind": "spaceship", "x_m": 0.0, "y_m": 0.0}],
            [],
        )
        with pytest.raises(ValueError, match="unknown kind 'spaceship'"):
            build_layout_model(config)

    def test_unknown_connection_kind_rejected(self):
        config = _minimal_config(
            _flat_surface(100.0),
            [
                {"id": "A", "kind": "well", "x_m": 0.0, "y_m": 0.0},
                {"id": "B", "kind": "host", "x_m": 100.0, "y_m": 0.0},
            ],
            [{"id": "C-1", "kind": "teleporter", "from": "A", "to": "B"}],
        )
        with pytest.raises(ValueError, match="unknown kind 'teleporter'"):
            build_layout_model(config)

    def test_dangling_connection_reference_rejected(self):
        config = _minimal_config(
            _flat_surface(100.0),
            [{"id": "A", "kind": "well", "x_m": 0.0, "y_m": 0.0}],
            [{"id": "C-1", "kind": "flowline", "from": "A", "to": "GHOST"}],
        )
        with pytest.raises(ValueError, match="unknown asset 'GHOST'.*dangling"):
            build_layout_model(config)

    def test_duplicate_asset_id_rejected(self):
        config = _minimal_config(
            _flat_surface(100.0),
            [
                {"id": "A", "kind": "well", "x_m": 0.0, "y_m": 0.0},
                {"id": "A", "kind": "manifold", "x_m": 50.0, "y_m": 0.0},
            ],
            [],
        )
        with pytest.raises(ValueError, match="duplicate asset id 'A'"):
            build_layout_model(config)

    def test_duplicate_connection_id_rejected(self):
        assets = [
            {"id": "A", "kind": "well", "x_m": 0.0, "y_m": 0.0},
            {"id": "B", "kind": "host", "x_m": 100.0, "y_m": 0.0},
        ]
        conns = [
            {"id": "C-1", "kind": "flowline", "from": "A", "to": "B"},
            {"id": "C-1", "kind": "umbilical", "from": "B", "to": "A"},
        ]
        config = _minimal_config(_flat_surface(100.0), assets, conns)
        with pytest.raises(ValueError, match="duplicate connection id 'C-1'"):
            build_layout_model(config)

    def test_missing_section_rejected(self):
        with pytest.raises(ValueError, match="missing required sections"):
            build_layout_model(
                {"field": {"name": "T"}, "assets": [], "connections": []}
            )


# --- schema round-trip -------------------------------------------------------
class TestRoundTrip:
    def test_offshore_demo_round_trips_through_yaml(self, tmp_path):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        config = layout_model_to_config(model)
        dumped = tmp_path / "roundtrip.yml"
        dumped.write_text(yaml.safe_dump(config), encoding="utf-8")

        rebuilt = build_layout_model_from_file(dumped)
        assert layout_summary(rebuilt) == layout_summary(model)
        # typed equivalence, not just summary equality
        assert [a.asset_id for a in rebuilt.assets] == [
            a.asset_id for a in model.assets
        ]
        for c0, c1 in zip(model.connections, rebuilt.connections):
            assert c1.kind == c0.kind
            assert c1.route_length_m == pytest.approx(c0.route_length_m)
            assert c1.waypoints_plan_m == c0.waypoints_plan_m

    def test_off_surface_flag_survives_round_trip(self):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        config = layout_model_to_config(model)
        fpso = next(a for a in config["assets"] if a["id"] == "FPSO-1")
        assert fpso["z_m"] == pytest.approx(0.0)  # explicit override re-emitted
        manifold = next(a for a in config["assets"] if a["id"] == "MAN-1")
        assert "z_m" not in manifold  # draped assets stay schema-draped


# --- waypoint routing over the surface ---------------------------------------
class TestWaypointRouting:
    def test_flat_surface_waypoint_lengths(self):
        # L-shaped route: (0,0) -> waypoint (300,0) -> (300,400); flat surface
        config = _minimal_config(
            _flat_surface(-100.0),
            [
                {"id": "M", "kind": "manifold", "x_m": 0.0, "y_m": 0.0},
                {"id": "H", "kind": "host", "x_m": 300.0, "y_m": 400.0},
            ],
            [
                {
                    "id": "FL",
                    "kind": "flowline",
                    "from": "M",
                    "to": "H",
                    "waypoints": [{"x_m": 300.0, "y_m": 0.0}],
                }
            ],
        )
        model = build_layout_model(config)
        conn = model.connections[0]
        assert conn.plan_length_m == pytest.approx(700.0)  # 300 + 400
        assert conn.route_length_m == pytest.approx(700.0)  # flat: 3D == plan
        assert conn.elevation_change_m == pytest.approx(0.0)
        # path passes exactly through the waypoint
        waypoint_hits = [p for p in conn.path_xyz_m if p[0] == 300.0 and p[1] == 0.0]
        assert len(waypoint_hits) == 1

    def test_sloped_surface_route_length_is_sum_of_hypotenuses(self, tmp_path):
        # uniform slope in x only: z rises 0 -> 30 m over 300 m (plane)
        (tmp_path / "slope.csv").write_text("0,30\n0,30\n", encoding="utf-8")
        surface = {
            "kind": "csv_grid",
            "csv_path": "slope.csv",
            "x_min_m": 0,
            "x_max_m": 300,
            "y_min_m": 0,
            "y_max_m": 100,
        }
        config = _minimal_config(
            surface,
            [
                {"id": "W", "kind": "well", "x_m": 0.0, "y_m": 50.0},
                {"id": "H", "kind": "host", "x_m": 300.0, "y_m": 50.0},
            ],
            [
                {
                    "id": "FL",
                    "kind": "flowline",
                    "from": "W",
                    "to": "H",
                    "waypoints": [{"x_m": 150.0, "y_m": 50.0}],
                }
            ],
        )
        model = build_layout_model(config, base_dir=tmp_path)
        conn = model.connections[0]
        assert conn.plan_length_m == pytest.approx(300.0)
        # two collinear legs on a plane: each hypot(150, 15); sum = hypot(300, 30)
        assert conn.route_length_m == pytest.approx(math.hypot(300.0, 30.0))
        assert conn.elevation_change_m == pytest.approx(30.0)

    def test_off_surface_endpoint_rises_from_seabed(self):
        # flat bathymetry at -100 m; vessel floats at z = 0 (explicit override)
        config = _minimal_config(
            _flat_surface(-100.0),
            [
                {"id": "M", "kind": "manifold", "x_m": 0.0, "y_m": 0.0},
                {"id": "V", "kind": "vessel", "x_m": 300.0, "y_m": 0.0, "z_m": 0.0},
            ],
            [{"id": "FL", "kind": "flowline", "from": "M", "to": "V"}],
        )
        config["routing"]["sample_spacing_m"] = 300.0  # single straight segment
        model = build_layout_model(config)
        conn = model.connections[0]
        assert model.asset("M").z_m == pytest.approx(-100.0)
        assert model.asset("V").z_m == pytest.approx(0.0)
        assert model.asset("V").on_surface is False
        assert conn.plan_length_m == pytest.approx(300.0)
        # one segment from (0,0,-100) to (300,0,0): true 3D hypotenuse
        assert conn.route_length_m == pytest.approx(math.hypot(300.0, 100.0))
        assert conn.elevation_change_m == pytest.approx(100.0)

    def test_bathymetry_negative_elevations_supported(self):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        assert float(model.surface.z_m.max()) < 0.0  # everything under water
        for asset in model.assets:
            if asset.on_surface:
                assert asset.z_m < 0.0
                assert asset.z_m == pytest.approx(
                    model.surface.elevation_at(asset.x_m, asset.y_m)
                )


# --- offshore demo end-to-end ------------------------------------------------
class TestOffshoreDemoEndToEnd:
    def test_asset_taxonomy_counts(self):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        assert len(model.assets_of_kind("well")) == 3
        assert len(model.assets_of_kind("tree")) == 3
        assert len(model.assets_of_kind("manifold")) == 1
        assert len(model.assets_of_kind("vessel")) == 1
        assert len(model.assets_of_kind("host")) == 1
        assert len(model.connections_of_kind("jumper")) == 3
        assert len(model.connections_of_kind("flowline")) == 1
        assert len(model.connections_of_kind("umbilical")) == 1
        assert len(model.connections_of_kind("pipeline")) == 1
        assert {c.kind for c in model.connections} <= CONNECTION_KINDS

    def test_flowline_waypoint_plan_length_hand_computed(self):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        fl = next(c for c in model.connections if c.connection_id == "FL-1")
        # MAN-1 (2500,2500) -> WP (3200,3600) -> FPSO-1 (4000,4000)
        expected = math.hypot(700.0, 1100.0) + math.hypot(800.0, 400.0)
        assert fl.plan_length_m == pytest.approx(expected)
        # route drapes ~1400 m-deep seabed then rises to the FPSO at z = 0
        assert fl.route_length_m > fl.plan_length_m
        assert fl.elevation_change_m > 1000.0  # seabed up to waterline

    def test_route_lengths_never_shorter_than_plan(self):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        for conn in model.connections:
            assert conn.route_length_m >= conn.plan_length_m > 0.0

    def test_umbilical_has_no_bore_hydraulics(self):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        umb = next(c for c in model.connections if c.kind == "umbilical")
        assert umb.inner_diameter_m is None
        assert umb.roughness_m is None

    def test_summary_is_json_able_and_flags_subsea(self):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        summary = layout_summary(model)
        assert summary["field"] == "Demo Offshore Field"
        assert summary["surface"]["is_subsea"] is True
        assert summary["assets_by_kind"] == {
            "vessel": 1,
            "manifold": 1,
            "well": 3,
            "tree": 3,
            "host": 1,
        }
        assert len(summary["connections"]) == 6
        import json

        json.dumps(summary)  # must not raise


# --- tracer schema back-compat (#1508) ----------------------------------------
class TestTracerBackCompat:
    def test_onshore_demo_loads_through_generalized_loader(self):
        config = load_layout_config(ONSHORE_CONFIG)
        assert "assets" in config and "connections" in config
        assert "surface" in config  # terrain alias normalized
        kinds = [a["kind"] for a in config["assets"]]
        assert kinds.count("host") == 1
        assert kinds.count("well") == 3
        assert all(c["kind"] == "flowline" for c in config["connections"])
        assert all("from" in c for c in config["connections"])

    def test_tracer_route_lengths_match_onshore_layout(self):
        """The generalized engine reproduces the tracer's 3D flowline lengths."""
        model = build_layout_model_from_file(ONSHORE_CONFIG)

        tracer_config = yaml.safe_load(ONSHORE_CONFIG.read_text(encoding="utf-8"))
        tracer_layout = build_layout(tracer_config, base_dir=ONSHORE_CONFIG.parent)

        tracer_by_id = {fl.flowline_id: fl for fl in tracer_layout.flowlines}
        assert len(model.connections) == len(tracer_by_id) == 3
        for conn in model.connections:
            tracer_fl = tracer_by_id[conn.connection_id]
            assert conn.plan_length_m == pytest.approx(tracer_fl.plan_length_m)
            assert conn.route_length_m == pytest.approx(tracer_fl.terrain_length_m)
            assert conn.elevation_change_m == pytest.approx(
                tracer_fl.elevation_change_m
            )

    def test_tracer_asset_placement_matches(self):
        model = build_layout_model_from_file(ONSHORE_CONFIG)
        tracer_config = yaml.safe_load(ONSHORE_CONFIG.read_text(encoding="utf-8"))
        tracer_layout = build_layout(tracer_config, base_dir=ONSHORE_CONFIG.parent)
        assert model.asset("CPF-1").z_m == pytest.approx(tracer_layout.host.z_m)
        for well in tracer_layout.wells:
            assert model.asset(well.asset_id).z_m == pytest.approx(well.z_m)
            assert model.asset(well.asset_id).rate_m3_per_day == pytest.approx(
                well.rate_m3_per_day
            )
