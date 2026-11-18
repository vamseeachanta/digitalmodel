# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exact unit factors and fail-closed resolution, independent of file readers."""

import pytest

from digitalmodel.cad import UnitsError
from digitalmodel.cad.units import resolve_units


@pytest.mark.parametrize(
    ("unit", "factor"),
    [
        ("m", 1),
        ("mm", 1 / 1000),
        ("cm", 1 / 100),
        ("in", 254 / 10000),
        ("ft", 3048 / 10000),
    ],
)
def test_exact_conversion(unit, factor):
    """Hand calculation: metre identity; milli=1/1000, centi=1/100.

    NIST SI Prefixes, prefix table (accessed 2026-10-19); inch=254/10000 m,
    foot=3048/10000 m, NIST SP 811 (2008), Appendix B.8, inch and foot rows.
    """
    decision = resolve_units(unit, None)
    assert decision.source_units == unit
    assert decision.metres_per_unit == factor
    assert decision.basis == "file"


def test_missing_units_require_input():
    with pytest.raises(UnitsError, match="State units"):
        resolve_units(None, None)


def test_caller_units_are_recorded():
    decision = resolve_units(None, "mm")
    assert decision.source_units == "mm"
    assert decision.basis == "caller"


def test_matching_declaration_is_recorded():
    assert resolve_units("m", "m").basis == "file_and_caller"


def test_conflicting_declaration_is_refused():
    with pytest.raises(UnitsError, match="conflict"):
        resolve_units("mm", "m")


@pytest.mark.parametrize("value", ["feet", "", "MM", "survey_ft", 1])
def test_unsupported_units_are_refused(value):
    with pytest.raises(UnitsError, match="Unsupported"):
        resolve_units(None, value)
