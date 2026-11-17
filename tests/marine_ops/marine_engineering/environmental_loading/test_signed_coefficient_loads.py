"""Regression tests for signed wind/current coefficient loads in ocimf.py, using illustrative hand-calculated examples. Method and limits: docs/coefficient-load-method.md in the environmental_loading package."""

import csv
from io import StringIO

import pytest

from digitalmodel.marine_ops.marine_engineering.environmental_loading.ocimf import (
    EnvironmentalConditions,
    EnvironmentalForces,
    OCIMFDatabase,
    VesselGeometry,
)


COEFFICIENTS = ("CXw", "CYw", "CMw", "CXc", "CYc", "CMc")


def coefficient_database(rows):
    """Create an in-memory illustrative example, without a data fixture."""
    stream = StringIO()
    writer = csv.writer(stream)
    writer.writerow(("heading", "displacement", *COEFFICIENTS))
    writer.writerows(rows)
    stream.seek(0)
    return OCIMFDatabase(stream)


@pytest.fixture
def signed_database():
    return coefficient_database([
        (0, 20000, -1, 0, 0, -0.5, 0, 0),
        (90, 20000, 0, 1.5, -0.25, 0, 2, 0.5),
        (180, 20000, 1, 0, 0, 0.5, 0, 0),
    ])


@pytest.fixture
def explicit_geometry():
    # Illustrative example: each projected area is supplied independently.
    return VesselGeometry(
        loa=120, beam=24, draft=6,
        frontal_area_wind=300, lateral_area_wind=1800,
        frontal_area_current=140, lateral_area_current=700,
    )


@pytest.mark.parametrize("heading, expected", [
    (0, (-1, 0, 0, -0.5, 0, 0)),
    (45, (-0.5, 0.75, -0.125, -0.25, 1, 0.25)),
    (90, (0, 1.5, -0.25, 0, 2, 0.5)),
    (135, (0.5, 0.75, -0.125, 0.25, 1, 0.25)),
    (180, (1, 0, 0, 0.5, 0, 0)),
])
def test_signed_knots_and_halfway_interpolation(signed_database, heading, expected):
    values = signed_database.get_coefficients(heading, 20000)
    assert tuple(getattr(values, name) for name in COEFFICIENTS) == pytest.approx(expected)


def test_regular_surface_uses_bilinear_corner_weights():
    database = coefficient_database([
        (0, 10000, *([0] * 6)),
        (0, 30000, *([2] * 6)),
        (180, 10000, *([4] * 6)),
        (180, 30000, *([10] * 6)),
    ])
    # One quarter across heading, halfway across displacement: four corners.
    expected = 0.75 * 0.5 * 0 + 0.75 * 0.5 * 2 + 0.25 * 0.5 * 4 + 0.25 * 0.5 * 10
    actual = database.get_coefficients(45, 20000)
    assert [getattr(actual, name) for name in COEFFICIENTS] == pytest.approx([expected] * 6)


def test_separate_wind_and_current_headings(signed_database, explicit_geometry):
    conditions = EnvironmentalConditions(
        wind_speed=12, wind_direction=45, current_speed=2,
        current_direction=90, air_density=1.2, water_density=1000,
    )
    loads = EnvironmentalForces(signed_database).calculate_total_forces(
        conditions, explicit_geometry, 20000,
    )
    # Hand calculation: pressure is rho*speed^2/2; yaw uses lateral area*length.
    wind_pressure = 1.2 * 12**2 / 2
    current_pressure = 1000 * 2**2 / 2
    expected_wind = (
        -0.5 * wind_pressure * 300,
        0.75 * wind_pressure * 1800,
        -0.125 * wind_pressure * 1800 * 120,
    )
    expected_current = (
        0,
        2 * current_pressure * 700,
        0.5 * current_pressure * 700 * 120,
    )
    assert (loads.wind_fx, loads.wind_fy, loads.wind_mz) == pytest.approx(expected_wind)
    assert (loads.current_fx, loads.current_fy, loads.current_mz) == pytest.approx(expected_current)
    assert (loads.total_fx, loads.total_fy, loads.total_mz) == pytest.approx(
        tuple(w + c for w, c in zip(expected_wind, expected_current))
    )


@pytest.mark.parametrize("speed", [0, 5, 15])
def test_quadratic_speed_and_signed_moment(signed_database, explicit_geometry, speed):
    conditions = EnvironmentalConditions(
        wind_speed=speed, wind_direction=90,
        current_speed=speed, current_direction=90,
        air_density=1.2, water_density=1000,
    )
    forces = EnvironmentalForces(signed_database)
    wind = forces.calculate_wind_forces(conditions, explicit_geometry, 20000)
    current = forces.calculate_current_forces(conditions, explicit_geometry, 20000)
    assert wind == pytest.approx((0, 1.2 * speed**2 / 2 * 1800 * 1.5,
                                 -1.2 * speed**2 / 2 * 1800 * 120 * 0.25))
    assert current == pytest.approx((0, 1000 * speed**2 / 2 * 700 * 2,
                                    1000 * speed**2 / 2 * 700 * 120 * 0.5))


@pytest.mark.parametrize("heading", [-45, 315, 405])
def test_characterizes_current_even_heading_folding(signed_database, heading):
    # Characterization of current behaviour, not a physical requirement; update if a signed port/starboard convention is adopted.
    actual = signed_database.get_coefficients(heading, 20000)
    assert (actual.CXw, actual.CYw, actual.CMw) == pytest.approx((-0.5, 0.75, -0.125))


def test_existing_out_of_range_policy_warns_and_clamps():
    database = coefficient_database([
        (0, 10000, *([-1] * 6)),
        (90, 10000, *([1] * 6)),
    ])
    with pytest.warns(UserWarning, match="outside database range"):
        values = database.get_coefficients(120, 10000)
    assert [getattr(values, name) for name in COEFFICIENTS] == pytest.approx([1] * 6)


@pytest.mark.parametrize("missing", ["heading", "displacement", *COEFFICIENTS])
def test_existing_database_rejects_missing_columns(missing):
    """Extend test_missing_columns to every required column."""
    names = ["heading", "displacement", *COEFFICIENTS]
    names.remove(missing)
    stream = StringIO(",".join(names) + "\n" + ",".join(["1"] * len(names)) + "\n")
    with pytest.raises(ValueError, match="Missing required columns"):
        OCIMFDatabase(stream)
