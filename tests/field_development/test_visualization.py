# ABOUTME: Tests for visualization — general 2D plot + typed 3D scene export (#1510).
# ABOUTME: File-creation + layout-number + scene-schema assertions for both demos.
"""Tests for digitalmodel.field_development.visualization.

Offline and deterministic: no Blender needed — the Blender fly-through script
consumes the scene JSON asserted here (schema digitalmodel.field_layout_scene/2).
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from digitalmodel.field_development.layout_model import (
    ASSET_KINDS,
    CONNECTION_KINDS,
    build_layout_model_from_file,
)
from digitalmodel.field_development.visualization import (
    ASSET_STYLES,
    CONNECTION_STYLES,
    SCENE_SCHEMA,
    export_layout_scene_json,
    layout_scene_dict,
    plot_field_layout,
    render_layout_visualization,
)

DATA_DIR = (
    Path(__file__).resolve().parents[2]
    / "src"
    / "digitalmodel"
    / "field_development"
    / "data"
)
OFFSHORE_CONFIG = DATA_DIR / "offshore_demo_field.yml"
ONSHORE_CONFIG = DATA_DIR / "onshore_demo_field.yml"

DEMO_CONFIGS = [
    pytest.param(OFFSHORE_CONFIG, id="offshore"),
    pytest.param(ONSHORE_CONFIG, id="onshore"),
]


# --- style completeness (fixed per-kind assignment, never cycled) --------------
class TestStyles:
    def test_every_asset_kind_has_a_style(self):
        assert set(ASSET_STYLES) == set(ASSET_KINDS)

    def test_every_connection_kind_has_a_style(self):
        assert set(CONNECTION_STYLES) == set(CONNECTION_KINDS)

    def test_styles_are_distinct_per_kind(self):
        assert len({(m, c) for m, c, _ in ASSET_STYLES.values()}) == len(ASSET_STYLES)
        assert len({(c, ls) for c, ls, _ in CONNECTION_STYLES.values()}) == len(
            CONNECTION_STYLES
        )


# --- 2D plot -------------------------------------------------------------------
class TestPlot:
    @pytest.mark.parametrize("config", DEMO_CONFIGS)
    def test_plot_creates_png(self, config, tmp_path):
        model = build_layout_model_from_file(config)
        out = tmp_path / "layout.png"
        saved = plot_field_layout(model, out)
        assert saved == str(out)
        assert out.exists()
        assert out.stat().st_size > 10_000  # a real figure, not an empty stub

    def test_plot_svg_format(self, tmp_path):
        model = build_layout_model_from_file(ONSHORE_CONFIG)
        out = tmp_path / "layout.svg"
        plot_field_layout(model, out, output_format="svg")
        assert out.exists()
        text = out.read_text(encoding="utf-8")
        assert "<svg" in text


# --- 3D scene export -----------------------------------------------------------
class TestSceneExport:
    @pytest.mark.parametrize("config", DEMO_CONFIGS)
    def test_scene_json_written_and_loadable(self, config, tmp_path):
        model = build_layout_model_from_file(config)
        out = tmp_path / "scene.json"
        saved = export_layout_scene_json(model, out)
        assert saved == str(out)
        scene = json.loads(out.read_text(encoding="utf-8"))
        assert scene["schema"] == SCENE_SCHEMA
        assert scene == layout_scene_dict(model)

    def test_offshore_scene_carries_every_asset_kind(self):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        scene = layout_scene_dict(model)
        assert {a["kind"] for a in scene["assets"]} == set(ASSET_KINDS)
        assert {c["kind"] for c in scene["connections"]} == set(CONNECTION_KINDS)

    def test_offshore_scene_flags_subsea_and_onshore_does_not(self):
        offshore = layout_scene_dict(build_layout_model_from_file(OFFSHORE_CONFIG))
        onshore = layout_scene_dict(build_layout_model_from_file(ONSHORE_CONFIG))
        assert offshore["is_subsea"] is True
        assert onshore["is_subsea"] is False

    def test_waypoints_preserved_in_scene(self):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        scene = layout_scene_dict(model)
        fl = next(c for c in scene["connections"] if c["id"] == "FL-1")
        assert fl["waypoints_plan_m"] == [[3200.0, 3600.0]]
        # the sampled 3D path passes exactly through the plan waypoint
        assert any(x == 3200.0 and y == 3600.0 for x, y, _ in fl["path_xyz_m"])

    def test_scene_geometry_matches_model(self):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        scene = layout_scene_dict(model)
        fpso = next(a for a in scene["assets"] if a["id"] == "FPSO-1")
        assert fpso["z_m"] == pytest.approx(0.0)
        assert fpso["on_surface"] is False
        for conn_dict, conn in zip(scene["connections"], model.connections):
            assert conn_dict["route_length_m"] == pytest.approx(conn.route_length_m)
            assert len(conn_dict["path_xyz_m"]) == len(conn.path_xyz_m)
        surface = scene["surface"]
        assert len(surface["z_m"]) == model.surface.z_m.shape[0]
        assert len(surface["z_m"][0]) == model.surface.z_m.shape[1]
        assert max(max(row) for row in surface["z_m"]) < 0.0  # bathymetry

    def test_export_is_deterministic(self, tmp_path):
        model = build_layout_model_from_file(OFFSHORE_CONFIG)
        a, b = tmp_path / "a.json", tmp_path / "b.json"
        export_layout_scene_json(model, a)
        export_layout_scene_json(model, b)
        assert a.read_bytes() == b.read_bytes()


# --- pipeline convenience --------------------------------------------------------
class TestRenderLayoutVisualization:
    @pytest.mark.parametrize("config", DEMO_CONFIGS)
    def test_end_to_end_artifacts_and_summary(self, config, tmp_path):
        result = render_layout_visualization(config, tmp_path)
        plot = Path(result["plot_path"])
        scene_path = Path(result["scene_path"])
        assert plot.exists() and plot.suffix == ".png"
        assert scene_path.exists() and scene_path.suffix == ".json"
        assert plot.name == f"{config.stem}_layout.png"
        assert scene_path.name == f"{config.stem}_scene.json"
        # summary numbers ride along for CI assertions
        assert result["connections"]
        for conn in result["connections"]:
            assert conn["route_length_m"] >= conn["plan_length_m"] > 0.0

    def test_offshore_summary_numbers(self, tmp_path):
        result = render_layout_visualization(OFFSHORE_CONFIG, tmp_path)
        assert result["field"] == "Demo Offshore Field"
        assert result["surface"]["is_subsea"] is True
        assert result["assets_by_kind"] == {
            "vessel": 1,
            "manifold": 1,
            "well": 3,
            "tree": 3,
            "host": 1,
        }
        scene = json.loads(Path(result["scene_path"]).read_text(encoding="utf-8"))
        assert {c["kind"] for c in scene["connections"]} == set(CONNECTION_KINDS)
