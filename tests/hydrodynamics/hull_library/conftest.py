"""Shared fixtures for hull library tests."""

import pytest

from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)


@pytest.fixture
def box_profile():
    """A 100m x 20m x 10m box hull (half-breadth = 10m, depth = 12m).

    Three stations (AP, midship, FP) with constant rectangular
    cross-section. Suitable for verifying mesh generation, coarsening,
    and schematic output on the simplest possible geometry.
    """
    stations = [
        HullStation(
            x_position=0.0,
            waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
        ),
        HullStation(
            x_position=50.0,
            waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
        ),
        HullStation(
            x_position=100.0,
            waterline_offsets=[(0.0, 10.0), (10.0, 10.0)],
        ),
    ]
    return HullProfile(
        name="unit_box",
        hull_type=HullType.BARGE,
        stations=stations,
        length_bp=100.0,
        beam=20.0,
        draft=10.0,
        depth=12.0,
        source="test",
    )


@pytest.fixture
def ship_profile():
    """A tapered 100m x 20m x 8m ship hull (depth = 12m).

    Five stations from stern to bow with varying cross-sections:
    narrow at stern (x=0), widening to midship (x=50), then narrowing
    to the bow (x=100). Suitable for testing adaptive density, curvature
    handling, and varying-breadth geometry.
    """
    stations = [
        HullStation(
            x_position=0.0,
            waterline_offsets=[
                (0.0, 0.0),
                (2.0, 3.0),
                (5.0, 4.0),
                (8.0, 5.0),
            ],
        ),
        HullStation(
            x_position=25.0,
            waterline_offsets=[
                (0.0, 0.0),
                (2.0, 7.0),
                (5.0, 9.0),
                (8.0, 10.0),
            ],
        ),
        HullStation(
            x_position=50.0,
            waterline_offsets=[
                (0.0, 0.0),
                (2.0, 7.0),
                (5.0, 10.0),
                (8.0, 10.0),
            ],
        ),
        HullStation(
            x_position=75.0,
            waterline_offsets=[
                (0.0, 0.0),
                (2.0, 7.0),
                (5.0, 9.0),
                (8.0, 10.0),
            ],
        ),
        HullStation(
            x_position=100.0,
            waterline_offsets=[
                (0.0, 0.0),
                (2.0, 2.0),
                (5.0, 3.0),
                (8.0, 4.0),
            ],
        ),
    ]
    return HullProfile(
        name="test_ship",
        hull_type=HullType.SHIP,
        stations=stations,
        length_bp=100.0,
        beam=20.0,
        draft=8.0,
        depth=12.0,
        source="test",
    )
