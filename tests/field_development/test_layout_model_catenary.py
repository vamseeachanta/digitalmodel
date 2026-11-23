# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Screening catenary routing and riser-shape validation tests."""

from __future__ import annotations

import math

import numpy as np
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


class TestCatenaryTransforms:
    """Independent hand calculation for rigid transforms of a synthetic route.

    Use the existing synthetic case: 300 m plan length, 100 m rise and
    30 degrees from vertical. For z(q)=a*(cosh(q/a)-1), the top slope is
    cot(30 degrees)=sqrt(3), so cosh(X/a)=2, a=100 m,
    X=100*asinh(sqrt(3)) and S=100*sqrt(3). Touchdown station is 300-X.
    A 3-4-5 triangle gives the unit direction (3/5, 4/5), with both plan
    coordinates varying. The chosen translation (200, 150, 40) m moves the
    surface and assets together and keeps them within the synthetic grid.

    Sampling uses 0.5 m spacing, chosen to bound chord error below the
    existing hand check's 0.001 m tolerance; coordinate comparisons allow
    only 1e-10 m for floating-point arithmetic on this metre-scale case.
    """

    @staticmethod
    def config(shift=(0.0, 0.0, 0.0), span=300.0, shape="catenary", spacing=0.5):
        config = TestCatenaryRiser.config(shape=shape, spacing=spacing, span=span)
        dx, dy, dz = shift
        config["assets"][0].update(x_m=dx, y_m=dy)
        config["assets"][1].update(
            x_m=dx + span * 3.0 / 5.0,
            y_m=dy + span * 4.0 / 5.0,
            z_m=dz,
        )
        surface = config["surface"]
        for axis, offset in (("x", dx), ("y", dy)):
            surface[f"{axis}_min_m"] += offset
            surface[f"{axis}_max_m"] += offset
        surface["base_elevation_m"] += dz
        return config

    @staticmethod
    def assert_hand_coordinates(conn, shift=(0.0, 0.0, 0.0)):
        # Hand calculation above; no solver result supplies expected values.
        a = 100.0
        span = a * math.asinh(math.sqrt(3.0))
        arc = a * math.sqrt(3.0)
        touchdown = 300.0 - span
        spacing = 0.5
        bed_segments = math.ceil(touchdown / spacing)
        curve_segments = math.ceil(arc / spacing)

        def point(station, rise):
            dx, dy, dz = shift
            return (dx + station * 3.0 / 5.0,
                    dy + station * 4.0 / 5.0, dz - a + rise)

        # Uniform stations on each piece, including touchdown exactly once.
        expected = [point(touchdown * i / bed_segments, 0.0)
                    for i in range(bed_segments + 1)]
        for i in range(1, curve_segments + 1):
            q = span * i / curve_segments
            expected.append(point(touchdown + q, a * (math.cosh(q / a) - 1.0)))
        np.testing.assert_allclose(conn.path_xyz_m, expected, rtol=0.0, atol=1e-10)
        np.testing.assert_allclose(
            conn.path_xyz_m[[0, bed_segments, -1]],
            [point(0.0, 0.0), point(touchdown, 0.0), point(300.0, a)],
            rtol=0.0, atol=1e-10,
        )

        # For f(q)=a*(cosh(q/a)-1), |f''| <= M=cosh(X/a)/a.
        # g(v)=sqrt(1+v*v) has g''<=1. On a chord of plan width h,
        # the Jensen gap is <= Var(f')/2 <= M*M*h*h/24 (the variance
        # bound follows by integrating |f'(u)-f'(v)|<=M*|u-v| twice).
        # Multiplying by h and summing gives arc-chord <= X*M*M*h*h/24.
        h = span / curve_segments
        bound = span * (math.cosh(span / a) / a) ** 2 * h ** 2 / 24.0
        assert bound < 0.001
        expected_length = math.fsum(math.dist(p, q)
                                    for p, q in zip(expected[:-1], expected[1:]))
        assert conn.route_length_m == pytest.approx(expected_length, rel=0.0, abs=1e-10)
        assert 0.0 < touchdown + arc - conn.route_length_m <= bound
        assert conn.plan_length_m == pytest.approx(300.0, rel=0.0, abs=1e-10)
        assert conn.elevation_change_m == pytest.approx(a, rel=0.0, abs=1e-10)
        assert conn.route_length_m > math.hypot(300.0, a)

    def test_catenary_routing_rotated_hand_coordinates(self):
        rotated = build_layout_model(self.config()).connections[0]
        self.assert_hand_coordinates(rotated)
        axis_aligned = build_layout_model(
            TestCatenaryRiser.config(spacing=0.5)
        ).connections[0]
        assert rotated.route_length_m == pytest.approx(
            axis_aligned.route_length_m, rel=0.0, abs=1e-10
        )
        # A single 300 m plan chord makes straight mode the direct endpoint
        # line. The existing fine-spacing golden retains its opposite sign.
        straight = build_layout_model(
            self.config(shape="straight", spacing=300.0)
        ).connections[0]
        assert len(straight.path_xyz_m) == 2
        assert straight.route_length_m == pytest.approx(math.hypot(300.0, 100.0))
        assert rotated.route_length_m > straight.route_length_m

    def test_catenary_routing_translated_datum(self):
        shift = (200.0, 150.0, 40.0)
        original = build_layout_model(self.config()).connections[0]
        translated = build_layout_model(self.config(shift=shift)).connections[0]
        self.assert_hand_coordinates(translated, shift)
        np.testing.assert_allclose(
            translated.path_xyz_m, original.path_xyz_m + shift,
            rtol=0.0, atol=1e-10,
        )
        assert translated.route_length_m == pytest.approx(
            original.route_length_m, rel=0.0, abs=1e-10
        )

    def test_rotated_short_leg_retains_straight_fallback(self):
        # A 100 m approach cannot contain X=100*asinh(sqrt(3)) m of rise.
        span = 100.0
        assert span < 100.0 * math.asinh(math.sqrt(3.0))
        curved = build_layout_model(self.config(span=span)).connections[0]
        straight = build_layout_model(
            self.config(span=span, shape="straight")
        ).connections[0]
        assert curved.path_xyz_m.tobytes() == straight.path_xyz_m.tobytes()
        assert curved.route_length_m == straight.route_length_m
        # Independent drape: 0.5 m plan chords at z=-100 until the last
        # sample, which rises to z=0. Its length is 99.5+hypot(0.5,100).
        spacing = 0.5
        segments = math.ceil(span / spacing)
        expected = [(span * i / segments * 3.0 / 5.0,
                     span * i / segments * 4.0 / 5.0,
                     0.0 if i == segments else -100.0)
                    for i in range(segments + 1)]
        np.testing.assert_allclose(curved.path_xyz_m, expected, rtol=0.0, atol=1e-10)
        assert curved.route_length_m == pytest.approx(
            span - spacing + math.hypot(spacing, 100.0), rel=0.0, abs=1e-10
        )


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
