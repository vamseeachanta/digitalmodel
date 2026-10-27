# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Small-strain series stiffness checked by independent hand calculations.

All inputs are synthetic arithmetic examples, not design or material data.
Lengths are in m, EA and force in kN, displacement in m, and stiffness in kN/m.
For a common force F, each segment extends by F*L/EA (uniaxial linear elasticity).
"""

from dataclasses import replace

import pytest

from digitalmodel.subsea.mooring_analysis.models import (
    AnchorProperties,
    LineType,
    MooringLine,
    MooringLineProperties,
)


@pytest.fixture
def line():
    """Use unequal segments; unused geometry and capacities are arbitrary.

    At F=100 kN the extensions are 0.025, 0.200 and 0.025 m, respectively.
    Thus the total extension is 0.250 m and F/displacement is 400 kN/m.
    The synthetic capacities give a weakest segment of 800 kN.
    """
    return MooringLine(
        line_id="series_example",
        segments=[
            MooringLineProperties(LineType.CHAIN, 100.0, 10.0, 1000.0, 1.0, 400000.0),
            MooringLineProperties(LineType.WIRE, 200.0, 10.0, 800.0, 1.0, 100000.0),
            MooringLineProperties(LineType.CHAIN, 50.0, 10.0, 900.0, 1.0, 200000.0),
        ],
        anchor=AnchorProperties("synthetic", 2000.0, (0.0, 0.0, 0.0)),
        fairlead_location=(0.0, 0.0, 0.0),
    )


def test_segmented_line_stiffness_matches_series_elongation(line):
    """Sum hand-calculated extensions under the same applied force."""
    force = 100.0
    displacement = 0.025 + 0.200 + 0.025
    assert line.axial_stiffness == pytest.approx(force / displacement)


def test_segment_split_preserves_stiffness(line):
    """Split the 200 m segment into 80+120 m: extensions are 0.080+0.120 m."""
    original_stiffness = line.axial_stiffness
    first, middle, last = line.segments
    line.segments = [
        first,
        replace(middle, length=80.0),
        replace(middle, length=120.0),
        last,
    ]
    expected = 100.0 / (0.025 + 0.080 + 0.120 + 0.025)
    assert line.axial_stiffness == pytest.approx(expected)
    assert line.axial_stiffness == pytest.approx(original_stiffness)


def test_single_segment_stiffness(line):
    """The first segment extends 0.025 m under 100 kN."""
    line.segments = line.segments[:1]
    assert line.axial_stiffness == pytest.approx(100.0 / 0.025)


def test_total_length_and_weakest_segment_capacity(line):
    """Length is 100+200+50 m; the middle segment limits capacity to 800 kN."""
    assert line.axial_stiffness > 0.0
    assert line.total_length == 350.0
    assert line.min_mbl == 800.0


def test_empty_segments_rejected_on_stiffness_access(line):
    """An empty serial line has no defined axial stiffness."""
    empty = replace(line, segments=[])
    assert empty.total_length == 0.0
    with pytest.raises(ValueError, match="at least one segment"):
        _ = empty.axial_stiffness


@pytest.mark.parametrize("position", [0, 1, 2])  # Every segment of the example.
@pytest.mark.parametrize("field", ["length", "ea"])
@pytest.mark.parametrize(
    "invalid", [0.0, -1.0, float("nan"), float("inf"), -float("inf")]
)  # Boundary, negative, undefined and unbounded inputs, not engineering data.
def test_invalid_segment_rejected_on_stiffness_access(line, position, field, invalid):
    """Validate each current segment only when stiffness is requested."""
    setattr(line.segments[position], field, invalid)
    with pytest.raises(ValueError, match=rf"{field} must be finite and positive"):
        _ = line.axial_stiffness
