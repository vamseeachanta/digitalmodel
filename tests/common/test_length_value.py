# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Value checks for explicit length conversion after specification parsing."""

import pytest

from digitalmodel.common.spec_authoring import ProjectBundle
from digitalmodel.common.units import length_in_metres


def test_parsed_dimension_converts_to_metres(tmp_path):
    """NIST SP 811 (2008), Appendix B.8, foot: 10*0.3048 = 3.048 m.

    Ten feet is a synthetic dimension, not a default or a measured asset.
    The expected decimal is hand calculated independently of the converter.
    """
    path = tmp_path / "spec.yaml"
    path.write_text("units: ft\nvessel_particulars:\n  length: 10.0\n")
    bundle = ProjectBundle.from_yaml(path)
    assert length_in_metres(bundle.vessel_particulars["length"], bundle.units) == 3.048
    assert bundle.vessel_particulars["length"] == 10.0
    assert bundle.units == "ft"


def test_si_length_is_preserved():
    """The identity conversion preserves the synthetic 3.048 m hand result."""
    assert length_in_metres(3.048, "m") == 3.048


def test_zero_length_is_preserved():
    """Zero length is the arithmetic identity, not a design acceptance limit."""
    assert length_in_metres(0.0, "ft") == 0.0


@pytest.mark.parametrize("unit", [None, "", "survey_ft", "FT", "mm"])
def test_unknown_units_require_explicit_resolution(unit):
    with pytest.raises(ValueError, match="unit"):
        length_in_metres(10.0, unit)


@pytest.mark.parametrize(
    "value",
    [
        True,
        "10",
        -1.0,
        float("inf"),
        float("nan"),
        10**400,
    ],
)
def test_invalid_dimensions_are_rejected(value):
    with pytest.raises(ValueError, match="length"):
        length_in_metres(value, "ft")
