# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Screening catenary routing and riser-shape validation tests."""

from __future__ import annotations

import math

import pytest

from digitalmodel.field_development.layout_model import build_layout_model


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


class TestCatenaryRiser:
    """Screening catenary geometry on a synthetic, flat seabed."""

    @staticmethod
    def config(shape="catenary", spacing=25.0, span=300.0, angle=30.0, z=0.0):
        config = _minimal_config(
            _flat_surface(-100.0),
            [
                {"id": "M", "kind": "manifold", "x_m": 0.0, "y_m": 0.0},
                {"id": "V", "kind": "vessel", "x_m": span, "y_m": 0.0, "z_m": z},
            ],
            [{"id": "FL", "kind": "flowline", "from": "M", "to": "V",
              "riser_shape": shape, "riser_hangoff_deg": angle}],
        )
        config["routing"]["sample_spacing_m"] = spacing
        return config

    def test_geometry_hand_check(self):
        from digitalmodel.marine_ops.marine_analysis.catenary.simplified import (
            SimplifiedCatenarySolver,
        )

        conn = build_layout_model(self.config(spacing=1.0)).connections[0]
        result = SimplifiedCatenarySolver().solve_from_angle(30.0, 100.0)
        # Independent hand geometry: a=100, S=100*sqrt(3), X=100*asinh(sqrt(3)).
        span = 100.0 * math.asinh(math.sqrt(3.0))
        arc = 100.0 * math.sqrt(3.0)
        assert result.horizontal_distance == pytest.approx(span)
        assert result.arc_length == pytest.approx(arc)
        touchdown = next(p for p in conn.path_xyz_m if p[0] == pytest.approx(300 - span))
        assert touchdown.tolist() == pytest.approx([300 - span, 0, -100])
        assert conn.path_xyz_m[-1].tolist() == [300.0, 0.0, 0.0]
        curve = conn.path_xyz_m[conn.path_xyz_m[:, 0] >= touchdown[0]]
        sampled_arc = sum(math.dist(p, q) for p, q in zip(curve[:-1], curve[1:]))
        # One-metre chords underestimate the analytic arc by less than 0.001 m.
        assert sampled_arc == pytest.approx(arc, abs=0.001)
        for x, _, z in curve:
            assert z == pytest.approx(-100 + 100 * (math.cosh((x-touchdown[0])/100)-1))

    def test_length_invariant_and_known_keys(self):
        conn = build_layout_model(self.config()).connections[0]
        assert conn.route_length_m >= conn.plan_length_m == 300.0
        assert "riser_shape" not in conn.properties
        assert "riser_hangoff_deg" not in conn.properties

    @pytest.mark.parametrize("changes", [
        {"span": 100.0}, {"z": -100.0}, {"z": -110.0},
        {"angle": 0.0}, {"angle": 90.0}, {"angle": float("nan")},
    ])
    def test_degenerate_geometry_keeps_straight_path(self, changes):
        curved = build_layout_model(self.config(**changes)).connections[0]
        straight = build_layout_model(self.config(shape="straight", **changes)).connections[0]
        assert curved.path_xyz_m.tobytes() == straight.path_xyz_m.tobytes()
        assert curved.route_length_m == straight.route_length_m

    def test_default_shape_and_default_angle(self):
        config = self.config()
        del config["connections"][0]["riser_shape"]
        default = build_layout_model(config).connections[0]
        straight = build_layout_model(self.config(shape="straight")).connections[0]
        assert default.path_xyz_m.tobytes() == straight.path_xyz_m.tobytes()
        config["connections"][0]["riser_shape"] = "catenary"
        del config["connections"][0]["riser_hangoff_deg"]
        implicit = build_layout_model(config).connections[0]
        explicit = build_layout_model(self.config()).connections[0]
        assert implicit.path_xyz_m.tobytes() == explicit.path_xyz_m.tobytes()

    def test_scene_export_contains_curve(self):
        from digitalmodel.field_development.visualization import layout_scene_dict

        model = build_layout_model(self.config())
        conn = model.connections[0]
        scene = layout_scene_dict(model)["connections"][0]
        assert len(scene["path_xyz_m"]) > 2
        assert scene["path_xyz_m"] == conn.path_xyz_m.tolist()
        assert scene["route_length_m"] == conn.route_length_m
        assert any(-100.0 < p[2] < 0.0 for p in scene["path_xyz_m"])

    def test_fine_sampling_direction_golden(self):
        # Sign-flip note: at 300 m spacing the straight stand-in is the direct
        # hypotenuse (316.228 m), shorter than the catenary. This golden uses 25 m.
        straight = build_layout_model(self.config(shape="straight")).connections[0]
        curved = build_layout_model(self.config()).connections[0]
        # Printed before pinning: 341.354719987 < 378.077640640 m.
        # Seven catenary chords under-read the 341.509291064 m analytic route.
        assert curved.route_length_m < straight.route_length_m
        assert curved.route_length_m == pytest.approx(341.3547199865193, abs=1e-9)
        assert straight.route_length_m == pytest.approx(378.07764064044153, abs=1e-9)

    def test_waypoint_sets_final_approach_plane(self):
        config = self.config()
        config["assets"][1].update(x_m=200.0, y_m=400.0)
        config["connections"][0]["waypoints"] = [{"x_m": 200.0, "y_m": 100.0}]
        conn = build_layout_model(config).connections[0]
        span = 100 * math.asinh(math.sqrt(3))
        touchdown = next(p for p in conn.path_xyz_m if p[1] == pytest.approx(400-span))
        assert touchdown.tolist() == pytest.approx([200.0, 400-span, -100.0])
        curve = conn.path_xyz_m[conn.path_xyz_m[:, 1] >= touchdown[1]]
        assert all(p[0] == 200.0 for p in curve)
        assert conn.path_xyz_m[-1].tolist() == [200.0, 400.0, 0.0]
        assert conn.route_length_m >= conn.plan_length_m


class TestRiserShapeValidation:
    """Shape vocabulary applies regardless of endpoint surface placement."""

    @staticmethod
    def config(shape, on_surface):
        config = TestCatenaryRiser.config(shape=shape)
        if on_surface:
            del config["assets"][1]["z_m"]
        if shape is None:
            del config["connections"][0]["riser_shape"]
        return config

    @pytest.mark.parametrize("on_surface", [False, True], ids=["floating", "surface"])
    def test_unknown_shape_raises(self, on_surface):
        config = self.config("catneary", on_surface)
        with pytest.raises(ValueError, match="catneary.*straight.*catenary"):
            build_layout_model(config)

    @pytest.mark.parametrize("on_surface", [False, True], ids=["floating", "surface"])
    @pytest.mark.parametrize(
        "shape", [None, "straight", "catenary"],
        ids=["absent", "straight", "catenary"],
    )
    def test_documented_shapes_route(self, shape, on_surface):
        model = build_layout_model(self.config(shape, on_surface))
        assert len(model.connections) == 1
        assert model.connections[0].route_length_m >= model.connections[0].plan_length_m
