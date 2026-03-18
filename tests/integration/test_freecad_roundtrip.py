#!/usr/bin/env python3
"""
ABOUTME: Round-trip integration test — hull profile → NURBS surface → ManifoldChecker.
Chains FreeCADHullGenerator and ManifoldChecker on a box-barge profile.
"""

import pytest

from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)
from digitalmodel.visualization.design_tools.freecad_hull import (
    FreeCADHullGenerator,
)
from digitalmodel.visualization.design_tools.manifold_check import (
    ManifoldChecker,
)


def _make_box_barge() -> HullProfile:
    """Simple box barge for round-trip testing."""
    half_beam = 10.0
    depth = 10.0
    stations = []
    for i in range(11):
        x = 100.0 * i / 10
        stations.append(
            HullStation(
                x_position=x,
                waterline_offsets=[
                    (0.0, half_beam),
                    (depth / 2, half_beam),
                    (depth, half_beam),
                ],
            )
        )
    return HullProfile(
        name="roundtrip_barge",
        hull_type=HullType.BARGE,
        stations=stations,
        length_bp=100.0,
        beam=20.0,
        draft=8.0,
        depth=depth,
        source="integration_test",
    )


def _make_ship_hull() -> HullProfile:
    """Ship-shaped hull with fined bow/stern for manifold testing."""
    half_beam = 10.0
    depth = 10.0
    # Shape factors along length: tapered at ends
    shape = [0.0, 0.3, 0.7, 0.9, 1.0, 1.0, 1.0, 0.9, 0.7, 0.3, 0.0]
    stations = []
    for i, s in enumerate(shape):
        x = 100.0 * i / 10
        y = half_beam * s
        stations.append(
            HullStation(
                x_position=x,
                waterline_offsets=[
                    (0.0, y * 0.5),
                    (depth / 2, y * 0.8),
                    (depth, y),
                ],
            )
        )
    return HullProfile(
        name="roundtrip_ship",
        hull_type=HullType.SHIP,
        stations=stations,
        length_bp=100.0,
        beam=20.0,
        draft=8.0,
        depth=depth,
        source="integration_test",
    )


class TestHullNURBSRoundtrip:
    """Chain: HullProfile → FreeCADHullGenerator → ManifoldChecker."""

    def test_box_barge_manifold_pass(self):
        """AC: Box barge surface passes all manifold checks."""
        profile = _make_box_barge()
        gen = FreeCADHullGenerator(profile)
        surface_data = gen.generate_nurbs_surface()
        pts = surface_data["surface_points"]
        checker = ManifoldChecker(pts)
        result = checker.run_all_checks()
        assert result["pass"] is True, (
            f"Manifold check failed: {result}"
        )

    def test_ship_hull_manifold_pass(self):
        """Ship-shaped hull surface passes manifold checks."""
        profile = _make_ship_hull()
        gen = FreeCADHullGenerator(profile)
        surface_data = gen.generate_nurbs_surface()
        pts = surface_data["surface_points"]
        checker = ManifoldChecker(pts)
        result = checker.run_all_checks()
        assert result["pass"] is True, (
            f"Manifold check failed: {result}"
        )

    def test_surface_points_shape(self):
        """Surface points have correct (n_stations, n_wl, 3) shape."""
        profile = _make_box_barge()
        gen = FreeCADHullGenerator(profile)
        surface_data = gen.generate_nurbs_surface()
        pts = surface_data["surface_points"]
        assert pts.ndim == 3
        assert pts.shape[0] == 11  # n_stations
        assert pts.shape[2] == 3   # x, y, z

    def test_surface_stations_count(self):
        """Generator reports correct station count."""
        profile = _make_box_barge()
        gen = FreeCADHullGenerator(profile)
        surface_data = gen.generate_nurbs_surface()
        assert surface_data["stations_used"] == 11

    def test_watertight_check(self):
        """Box barge surface is watertight (no gaps)."""
        profile = _make_box_barge()
        gen = FreeCADHullGenerator(profile)
        surface_data = gen.generate_nurbs_surface()
        pts = surface_data["surface_points"]
        checker = ManifoldChecker(pts)
        assert checker.check_watertight() is True

    def test_no_self_intersection(self):
        """Box barge surface has no self-intersections."""
        profile = _make_box_barge()
        gen = FreeCADHullGenerator(profile)
        surface_data = gen.generate_nurbs_surface()
        pts = surface_data["surface_points"]
        checker = ManifoldChecker(pts)
        assert checker.check_self_intersection() is True

    def test_normals_consistent(self):
        """Box barge surface normals are consistent."""
        profile = _make_box_barge()
        gen = FreeCADHullGenerator(profile)
        surface_data = gen.generate_nurbs_surface()
        pts = surface_data["surface_points"]
        checker = ManifoldChecker(pts)
        assert checker.check_normals_consistent() is True
