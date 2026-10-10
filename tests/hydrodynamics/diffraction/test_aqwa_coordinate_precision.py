"""AQWA node coordinates must keep the precision a 10-character fixed-format field allows.

Found on the ship-hull benchmark (digitalmodel-data issue 40): five significant figures wrote
x = 182.3698 m as 182.37 (5 mm error) and y = -0.00032717 m as an 11-character field that AQWA
rejected ("embedded space in COOR card"). A mesh written for AQWA then differs from the same mesh
given to another program.
"""
import math

import pytest

from digitalmodel.hydrodynamics.diffraction.aqwa_backend import _fmt_coord


@pytest.mark.parametrize("value", [
    182.36987654, -18.016252, 149.10987, 0.0, -0.00032717, 1.2e-9, -7.9999999, 203.62084873,
    -1234.56789, 98765.4321, 5.0e-6, -3.3e-12, 1.0e5 - 1e-3,
])
def test_field_is_exactly_ten_characters(value):
    s = _fmt_coord(value)
    assert len(s) == 10, f"field overflow or underflow: {s!r}"
    assert "." in s, f"AQWA needs a decimal point: {s!r}"


@pytest.mark.parametrize("value", [182.36987654, -18.016252, 203.62084873, -7.9999999, 6.37104278, -0.26243685])
def test_round_trip_error_below_a_micrometre_for_hull_coordinates(value):
    assert abs(float(_fmt_coord(value)) - value) <= 1e-6 * max(1.0, abs(value)) / 1.0 + 1e-6


def test_tiny_values_round_trip_to_within_a_micrometre():
    for v in (-0.00032717, 1.2e-9, 5.0e-6, -3.3e-12):
        assert abs(float(_fmt_coord(v)) - v) <= 1e-6


def test_workbench_style_values_unchanged():
    # values Workbench writes already fit; they must read back identically
    for v in (149.10987, -18.016252, 0.0):
        assert float(_fmt_coord(v)) == pytest.approx(v, abs=0.0)


def test_rejects_non_finite():
    with pytest.raises(ValueError):
        _fmt_coord(math.nan)
