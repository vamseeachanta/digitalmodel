# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand-derived pipe area checks, including the JSON representation."""

import math

import pytest

from digitalmodel.data_models.pipe_section import PipeSection


def test_pipe_area_after_serialization():
    """OpenStax Precalculus 2e (2021), §5.1, Area of a Sector: A=θ*r²/2.

    With θ = 2π radians (a full circle), A=pi*r².
    Synthetic diameters Do=4 m, Di=2 m give radii 2 m and 1 m. Subtracting
    circle areas gives (4-1)*pi = 3*pi m². JSON retains both input diameters;
    the reconstructed model must retain the independently derived area.
    """
    section = PipeSection(outer_diameter_m=4, inner_diameter_m=2)
    restored = PipeSection.from_json(section.to_json())
    assert restored.outer_diameter_m == 4
    assert restored.inner_diameter_m == 2
    assert section.area_m2 == 3 * math.pi
    assert restored.area_m2 == 3 * math.pi


def test_solid_circle_is_supported():
    """The same circle formula gives pi m² for chosen radius 1 m, no bore."""
    assert PipeSection(outer_diameter_m=2, inner_diameter_m=0).area_m2 == math.pi


@pytest.mark.parametrize(
    "outer,inner",
    [
        (0, 0),
        (-1, 0),
        (2, -1),
        (2, 2),
        (2, 3),
        (float("inf"), 1),
        (4, float("nan")),
        (True, 0),
        ("4", 2),
    ],
)
def test_invalid_geometry_is_rejected(outer, inner):
    """A material annulus requires finite Do > Di >= 0; booleans lack units."""
    with pytest.raises(ValueError):
        PipeSection(outer, inner)


@pytest.mark.parametrize("payload", ['{"outer_diameter_m":4}', "[]", "null"])
def test_incomplete_serialization_is_rejected(payload):
    """Both diameters are required; no implicit geometry is reconstructed."""
    with pytest.raises(ValueError):
        PipeSection.from_json(payload)
