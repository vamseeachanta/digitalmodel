# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Hand-derived pipe area checks, including the JSON representation."""

import json
import math
from fractions import Fraction

import numpy as np
import pytest

from digitalmodel.data_models import pipe_section
from digitalmodel.data_models.pipe_section import AnnulusSection


def test_pipe_area_after_serialization():
    """OpenStax Precalculus 2e (2021), §5.1, Area of a Sector: A=θ*r²/2.

    With θ = 2π radians (a full circle), A=pi*r².
    Synthetic diameters Do=4 m, Di=2 m give radii 2 m and 1 m. Subtracting
    circle areas gives (4-1)*pi = 3*pi m². JSON retains both input diameters;
    the reconstructed model must retain the independently derived area.
    """
    section = AnnulusSection(outer_diameter_m=4, inner_diameter_m=2)
    restored = AnnulusSection.from_json(section.to_json())
    assert restored.outer_diameter_m == 4
    assert restored.inner_diameter_m == 2
    assert section.area_m2 == 3 * math.pi
    assert restored.area_m2 == 3 * math.pi


def test_solid_circle_is_supported():
    """The same circle formula gives pi m² for chosen radius 1 m, no bore."""
    assert AnnulusSection(outer_diameter_m=2, inner_diameter_m=0).area_m2 == math.pi


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
        (np.bool_(True), 0),
        (4, np.bool_(False)),
        pytest.param(10**400, 1, id="outer-overflow"),
        pytest.param(4, 10**400, id="inner-overflow"),
    ],
)
def test_invalid_geometry_is_rejected(outer, inner):
    """A material annulus requires finite Do > Di >= 0; booleans lack units."""
    with pytest.raises(ValueError):
        AnnulusSection(outer_diameter_m=outer, inner_diameter_m=inner)


@pytest.mark.parametrize("payload", ['{"outer_diameter_m":4}', "[]", "null"])
def test_incomplete_serialization_is_rejected(payload):
    """Both diameters are required; no implicit geometry is reconstructed."""
    with pytest.raises(ValueError):
        AnnulusSection.from_json(payload)


def test_section_has_unambiguous_name():
    """Diameter-based geometry must not shadow the wall-thickness model."""
    assert AnnulusSection.__name__ == "AnnulusSection"
    assert not hasattr(pipe_section, "PipeSection")


def test_section_requires_keyword_arguments():
    """A second positional value must not silently mean bore or thickness."""
    with pytest.raises(TypeError):
        AnnulusSection(0.3, 0.02)


@pytest.mark.parametrize("field", ["outer_diameter_m", "inner_diameter_m"])
def test_serialized_overflow_is_rejected(field):
    """Oversized JSON integers use the same ValueError contract as geometry."""
    data = {"outer_diameter_m": 4, "inner_diameter_m": 2}
    data[field] = 10**400
    with pytest.raises(ValueError, match="finite"):
        AnnulusSection.from_json(json.dumps(data))


@pytest.mark.parametrize("scalar", [np.int64, np.float32, np.float64, Fraction])
def test_real_scalar_geometry_round_trip(scalar):
    """Real table scalars retain the hand-derived 3*pi area through JSON."""
    section = AnnulusSection(outer_diameter_m=scalar(4), inner_diameter_m=scalar(2))
    restored = AnnulusSection.from_json(section.to_json())
    assert restored.outer_diameter_m == 4
    assert restored.inner_diameter_m == 2
    assert restored.area_m2 == 3 * math.pi
