# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Check an analytic surrogate lookup, never solver evidence.

Basis: independent uniform-bar equilibrium, F = integral(sigma dA) = sigma*A
for constant axial stress. Choose A = 2500 mm^2 as a synthetic test geometry;
no material properties or measured data are assumed. SI prefix definitions give
1000 N/kN, 1e-6 m^2/mm^2 and 1e6 Pa/MPa. Hence
sigma_MPa = (T_kN * 1000) / (2500 * 1e-6) / 1e6 = 0.4 * T_kN.

Hand-calculated table rows (T_kN, sigma_MPa) are (25, 10), (100, 40) and
(250, 100). These positive loads deliberately omit the origin so lower-bound
extrapolation can be checked at nonnegative loads. The chosen area also gives
a slope different from the converter's default lookup. Expectations below use
only force/area, never the converter or its interpolator. A relative tolerance
of 1e-12 and absolute tolerance of 1e-12 MPa allow floating-point roundoff while
remaining far below the spacing between the synthetic stress values.
"""

import csv

import numpy as np
import pytest

from digitalmodel.structural.fatigue_apps.tension_stress_conversion import (
    TensionStressConverter,
)


# Basis and hand calculations for these constants are given in the module docstring.
AREA_MM2 = 2500.0
NEWTONS_PER_KILONEWTON = 1000.0
SQUARE_METRES_PER_SQUARE_MILLIMETRE = 1e-6
PASCALS_PER_MEGAPASCAL = 1e6
LOOKUP_ROWS = ((25.0, 10.0), (100.0, 40.0), (250.0, 100.0))
RELATIVE_TOLERANCE = 1e-12
ABSOLUTE_TOLERANCE_MPA = 1e-12


def _uniform_bar_stress_mpa(tension_kn):
    """Independent force/area oracle using the stated geometry and SI factors."""
    force_n = np.asarray(tension_kn) * NEWTONS_PER_KILONEWTON
    area_m2 = AREA_MM2 * SQUARE_METRES_PER_SQUARE_MILLIMETRE
    return force_n / area_m2 / PASCALS_PER_MEGAPASCAL


def _make_converter(tmp_path, rows):
    """Write the analytic surrogate and all converter outputs in temporary storage."""
    lookup_path = tmp_path / "analytic_uniform_bar_lookup.csv"
    with lookup_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.writer(stream)
        writer.writerow(("tension_kn", "stress_mpa"))
        writer.writerows(rows)
    return TensionStressConverter(
        lookup_table_path=str(lookup_path),
        output_dir=str(tmp_path / "stress_ranges"),
    )


@pytest.fixture
def uniform_bar_converter(tmp_path):
    """Use the explicitly supplied analytic table; no default lookup is needed."""
    return _make_converter(tmp_path, LOOKUP_ROWS)


def test_uniform_bar_lookup_nodes(uniform_bar_converter):
    """The hand table gives 10, 40 and 100 MPa at 25, 100 and 250 kN by F/A."""
    tensions_kn, tabulated_stresses_mpa = np.asarray(LOOKUP_ROWS).T
    expected_mpa = _uniform_bar_stress_mpa(tensions_kn)
    np.testing.assert_allclose(
        tabulated_stresses_mpa,
        expected_mpa,
        rtol=RELATIVE_TOLERANCE,
        atol=ABSOLUTE_TOLERANCE_MPA,
    )
    np.testing.assert_allclose(
        uniform_bar_converter.convert_tension_to_stress(tensions_kn),
        expected_mpa,
        rtol=RELATIVE_TOLERANCE,
        atol=ABSOLUTE_TOLERANCE_MPA,
    )
    for tension_kn, stress_mpa in LOOKUP_ROWS:
        assert uniform_bar_converter.convert_tension_to_stress(tension_kn) == (
            pytest.approx(
                stress_mpa,
                rel=RELATIVE_TOLERANCE,
                abs=ABSOLUTE_TOLERANCE_MPA,
            )
        )


def test_uniform_bar_interpolation_and_extrapolation(uniform_bar_converter):
    """Check both intervals and both outside regions with the same F/A oracle.

    Choose interval midpoints 62.5 = (25+100)/2 and 175 = (100+250)/2 kN;
    below-table loads are zero and 12.5 = 25/2 kN, and above-table loads are
    300 = 250+50 and 500 = 2*250 kN. Force/area gives respective stresses
    0, 5, 25, 70, 120 and 200 MPa. Zero checks extrapolation through the origin.
    """
    tensions_kn = np.array([0.0, 12.5, 62.5, 175.0, 300.0, 500.0])
    expected_mpa = _uniform_bar_stress_mpa(tensions_kn)
    np.testing.assert_allclose(
        uniform_bar_converter.convert_tension_to_stress(tensions_kn),
        expected_mpa,
        rtol=RELATIVE_TOLERANCE,
        atol=ABSOLUTE_TOLERANCE_MPA,
    )
    for tension_kn, stress_mpa in zip(tensions_kn, expected_mpa):
        assert uniform_bar_converter.convert_tension_to_stress(float(tension_kn)) == (
            pytest.approx(
                stress_mpa,
                rel=RELATIVE_TOLERANCE,
                abs=ABSOLUTE_TOLERANCE_MPA,
            )
        )


def test_unsorted_uniform_bar_lookup(tmp_path):
    """Permute the hand table and queries without changing the force/area answer.

    The source order is 250, 25, 100 kN. Queries reuse the documented nodes,
    midpoints, zero and upper extrapolation load in a different order, giving
    200, 10, 25, 0, 100, 70 and 40 MPa respectively by the independent oracle.
    """
    converter = _make_converter(
        tmp_path, ((250.0, 100.0), (25.0, 10.0), (100.0, 40.0))
    )
    np.testing.assert_array_equal(
        converter.lookup_table["tension_kn"].to_numpy(),
        np.array([25.0, 100.0, 250.0]),
    )
    tensions_kn = np.array([500.0, 25.0, 62.5, 0.0, 250.0, 175.0, 100.0])
    np.testing.assert_allclose(
        converter.convert_tension_to_stress(tensions_kn),
        _uniform_bar_stress_mpa(tensions_kn),
        rtol=RELATIVE_TOLERANCE,
        atol=ABSOLUTE_TOLERANCE_MPA,
    )
