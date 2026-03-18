#!/usr/bin/env python3
"""
ABOUTME: Analytical validation of HullHydrostatics against known closed-form
solutions — box barge (V, Awp, KB, BM) and Cb=0.7 shaped hull displacement.
"""

import pytest

from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)
from digitalmodel.visualization.design_tools.hull_hydrostatics import (
    HullHydrostatics,
)

TOLERANCE = 0.02  # 2% relative tolerance


def _make_box_barge(
    length: float = 100.0,
    beam: float = 20.0,
    draft: float = 8.0,
    depth: float = 10.0,
    n_stations: int = 21,
) -> HullProfile:
    """Create a rectangular box barge with uniform cross-sections.

    Every station has identical waterline offsets: constant half-breadth
    from keel to depth. This gives exact analytical hydrostatics.
    """
    half_beam = beam / 2.0
    stations = []
    for i in range(n_stations):
        x = length * i / (n_stations - 1)
        stations.append(
            HullStation(
                x_position=x,
                waterline_offsets=[
                    (0.0, half_beam),
                    (depth, half_beam),
                ],
            )
        )
    return HullProfile(
        name="box_barge_validation",
        hull_type=HullType.BARGE,
        stations=stations,
        length_bp=length,
        beam=beam,
        draft=draft,
        depth=depth,
        source="analytical_validation",
    )


def _make_cb07_hull(
    length: float = 100.0,
    beam: float = 20.0,
    draft: float = 8.0,
    depth: float = 10.0,
) -> HullProfile:
    """Create a wall-sided hull with Cb = 0.7.

    Uses 11 stations at x = 0, 10, ..., 100 with half-breadths:
    [0, 0, B/2, B/2, B/2, B/2, B/2, B/2, B/2, 0, 0]
    Gives trapezoidal integral(y, 0, L) = 700, so:
    V = 2*T*700 = 11200 = L*B*T*0.7 ✓
    """
    half_beam = beam / 2.0
    # Shape that gives Cb=0.7 via trapezoidal integration
    y_values = [0.0, 0.0, half_beam, half_beam, half_beam,
                half_beam, half_beam, half_beam, half_beam, 0.0, 0.0]
    n_stations = len(y_values)
    stations = []
    for i, y in enumerate(y_values):
        x = length * i / (n_stations - 1)
        if y == 0.0:
            # Minimal offset for zero-breadth station
            offsets = [(0.0, 0.0), (depth, 0.0)]
        else:
            offsets = [(0.0, y), (depth, y)]
        stations.append(
            HullStation(x_position=x, waterline_offsets=offsets)
        )
    return HullProfile(
        name="cb07_validation",
        hull_type=HullType.SHIP,
        stations=stations,
        length_bp=length,
        beam=beam,
        draft=draft,
        depth=depth,
        source="analytical_validation",
        block_coefficient=0.7,
    )


def _rel_err(computed: float, expected: float) -> float:
    """Relative error as a fraction."""
    if expected == 0.0:
        return abs(computed)
    return abs(computed - expected) / abs(expected)


# ===================================================================
# Box barge tests
# ===================================================================


class TestBoxBargeHydrostatics:
    """Validate hydrostatics against box barge analytical solutions."""

    @pytest.fixture
    def barge(self):
        return _make_box_barge()

    @pytest.fixture
    def hydro(self, barge):
        return HullHydrostatics(barge)

    def test_displaced_volume(self, hydro):
        """AC: V = L*B*T within 2%."""
        expected = 100.0 * 20.0 * 8.0  # 16000 m³
        computed = hydro.compute_displaced_volume()
        err = _rel_err(computed, expected)
        assert err < TOLERANCE, (
            f"V = {computed:.1f}, expected {expected:.1f}, "
            f"error = {err:.4f} ({err*100:.2f}%)"
        )

    def test_waterplane_area(self, hydro):
        """AC: Awp = L*B within 2%."""
        expected = 100.0 * 20.0  # 2000 m²
        computed = hydro.compute_waterplane_area()
        err = _rel_err(computed, expected)
        assert err < TOLERANCE, (
            f"Awp = {computed:.1f}, expected {expected:.1f}, "
            f"error = {err:.4f} ({err*100:.2f}%)"
        )

    def test_kb(self, hydro):
        """AC: KB = T/2 within 2%."""
        expected = 8.0 / 2.0  # 4.0 m
        computed = hydro.compute_kb()
        err = _rel_err(computed, expected)
        assert err < TOLERANCE, (
            f"KB = {computed:.4f}, expected {expected:.4f}, "
            f"error = {err:.4f} ({err*100:.2f}%)"
        )

    def test_bm_transverse(self, hydro):
        """AC: BM = B²/(12T) within 2% for wall-sided vessel."""
        expected = 20.0 ** 2 / (12.0 * 8.0)  # 4.1667 m
        computed = hydro.compute_bm_transverse()
        err = _rel_err(computed, expected)
        assert err < TOLERANCE, (
            f"BM = {computed:.4f}, expected {expected:.4f}, "
            f"error = {err:.4f} ({err*100:.2f}%)"
        )


# ===================================================================
# Cb=0.7 hull tests
# ===================================================================


class TestCb07HullDisplacement:
    """Validate shaped hull displacement against Cb formula."""

    @pytest.fixture
    def hull(self):
        return _make_cb07_hull()

    @pytest.fixture
    def hydro(self, hull):
        return HullHydrostatics(hull)

    def test_displacement_cb07(self, hydro):
        """AC: displacement = rho*L*B*T*Cb within 2%."""
        rho = 1.025  # seawater density t/m³
        cb = 0.7
        expected = rho * 100.0 * 20.0 * 8.0 * cb  # 11480 tonnes
        computed = hydro.compute_displacement(seawater_density=rho)
        err = _rel_err(computed, expected)
        assert err < TOLERANCE, (
            f"Displacement = {computed:.1f} t, expected {expected:.1f} t, "
            f"error = {err:.4f} ({err*100:.2f}%)"
        )

    def test_volume_cb07(self, hydro):
        """Volume = L*B*T*Cb within 2%."""
        expected = 100.0 * 20.0 * 8.0 * 0.7  # 11200 m³
        computed = hydro.compute_displaced_volume()
        err = _rel_err(computed, expected)
        assert err < TOLERANCE, (
            f"V = {computed:.1f}, expected {expected:.1f}, "
            f"error = {err:.4f} ({err*100:.2f}%)"
        )
