# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic hand calculations for wall-shear zero crossings.

All numerical fixtures are constructed examples, not experimental data.
The reference is the straight line through two samples: its zero divides
the interval in the ratio of the two shear magnitudes. Zero denotes zero
shear throughout; no empirical constants or fitted tolerances are used.
"""

import math

import pytest

from digitalmodel.solvers.openfoam.validation.reattachment import (
    extract_reattachment,
)


@pytest.mark.parametrize(
    "coordinates, shear",
    [([11.0, 15.0], [-1.0, 1.0]), ([11.0, 13.0, 15.0], [-1.0, 0.0, 1.0])],
)
def test_linear_shear_crossing_hand_solution(coordinates, shear):
    """Equal opposite shears give the midpoint, including a sampled zero."""
    # Hand calculation: midpoint (11 + 15)/2 = 13; step = 10, height = 2.
    # Thus the downstream distance is 3 and the height ratio is 3/2 = 1.5.
    result = extract_reattachment(
        coordinates, shear, step_location=10.0, step_height=2.0
    )
    assert result.status == "found"
    assert result.crossing_coordinate == pytest.approx(13.0)
    assert result.x == pytest.approx(3.0)
    assert result.x_over_height == pytest.approx(1.5)


def test_interpolate_only_bracketing_samples_and_take_first_crossing():
    """Other samples cannot alter the local root or replace the first root."""
    # Hand calculation: on [11, 15], shears [-1, 3] give a 1:3 split.
    # The root is 11 + (15 - 11)/4 = 12, distance 2, ratio 2/2 = 1.
    result = extract_reattachment(
        [10.0, 11.0, 15.0, 16.0, 18.0, 20.0],
        [-9.0, -1.0, 3.0, 1.0, -1.0, 1.0],
        step_location=10.0,
        step_height=2.0,
    )
    assert result.crossing_coordinate == pytest.approx(12.0)
    assert result.x == pytest.approx(2.0)
    assert result.x_over_height == pytest.approx(1.0)


def test_ignore_upstream_and_wrong_direction():
    """An upstream rising crossing and a downstream falling one are ignored."""
    # Midpoint roots: -3 upstream, 2 falling, 6 rising. Step = 0, height = 2.
    result = extract_reattachment(
        [-4.0, -2.0, 1.0, 3.0, 5.0, 7.0],
        [-1.0, 1.0, 1.0, -1.0, -1.0, 1.0],
        step_location=0.0,
        step_height=2.0,
    )
    assert result.status == "found"
    assert result.x == pytest.approx(6.0)
    assert result.x_over_height == pytest.approx(3.0)


@pytest.mark.parametrize(
    "shear",
    [
        [1.0, 2.0, 3.0],  # All positive.
        [-1.0, -2.0, -3.0],  # All negative.
        [1.0, 0.0, -1.0],  # Falling crossing.
        [-1.0, 0.0, -1.0],  # Touching zero without changing sign.
        [-1.0, -1.0, 0.0],  # No positive evidence after the final zero.
        [0.0, 1.0, 1.0],  # No negative evidence before the initial zero.
        [0.0, 0.0, 0.0],  # No directional crossing.
    ],
)
def test_no_crossing_is_explicit(shear):
    """Synthetic profiles without a rising sign change have no length."""
    result = extract_reattachment(
        [1.0, 2.0, 3.0], shear, step_location=0.0, step_height=1.0
    )
    assert result.status == "not_found"
    assert result.crossing_coordinate is None
    assert result.x is None
    assert result.x_over_height is None


def test_zero_plateau_requires_positive_evidence_and_uses_first_zero():
    """The first sampled zero is the start of a resolved zero plateau."""
    # Constructed zeros at 2 and 3; negative at 1, positive at 4.
    result = extract_reattachment(
        [1.0, 2.0, 3.0, 4.0],
        [-1.0, 0.0, 0.0, 1.0],
        step_location=0.0,
        step_height=1.0,
    )
    assert result.x == pytest.approx(2.0)


@pytest.mark.parametrize("step", [2.0, 3.0])
def test_crossing_at_or_before_step_is_not_downstream(step):
    """The hand-calculated midpoint root at 2 is outside the search region."""
    result = extract_reattachment(
        [1.0, 3.0], [-1.0, 1.0], step_location=step, step_height=1.0
    )
    assert result.status == "not_found"
    assert result.x is None


def test_bracket_can_straddle_step_when_root_is_downstream():
    """Position of the interpolated root determines whether it is downstream."""
    # Hand calculation: midpoint of [-1, 3] is 1, downstream of step = 0.
    result = extract_reattachment(
        [-1.0, 3.0], [-1.0, 1.0], step_location=0.0, step_height=1.0
    )
    assert result.x == pytest.approx(1.0)


@pytest.mark.parametrize("coordinates", [[2.0, 1.0], [1.0, 1.0], [3.0, 1.0, 2.0]])
def test_nonmonotonic_coordinates_fail(coordinates):
    """Constructed reversed, repeated and unsorted positions are invalid."""
    with pytest.raises(ValueError, match="strictly increasing"):
        extract_reattachment(
            coordinates,
            [-1.0] * len(coordinates),
            step_location=0.0,
            step_height=1.0,
        )


@pytest.mark.parametrize("bad", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("field", ["coordinates", "shear", "step_location", "step_height"])
def test_nonfinite_data_fail(bad, field):
    """Each input must be finite, including data after an otherwise valid root."""
    # The [-1, 1] pair on [1, 3] has root 2; a bad final sample still fails.
    coordinates, shear = [1.0, 3.0, 5.0], [-1.0, 1.0, 1.0]
    kwargs = {"step_location": 0.0, "step_height": 1.0}
    if field == "coordinates":
        coordinates[-1] = bad
    elif field == "shear":
        shear[-1] = bad
    else:
        kwargs[field] = bad
    with pytest.raises(ValueError, match="finite"):
        extract_reattachment(coordinates, shear, **kwargs)


@pytest.mark.parametrize("height", [0.0, -1.0])
def test_invalid_height_fail(height):
    """Zero and negative constructed step heights cannot normalize length."""
    with pytest.raises(ValueError, match="positive"):
        extract_reattachment(
            [1.0, 3.0], [-1.0, 1.0], step_location=0.0, step_height=height
        )


@pytest.mark.parametrize(
    "coordinates, shear", [([], []), ([1.0], [-1.0]), ([1.0, 2.0], [-1.0])]
)
def test_insufficient_or_mismatched_samples_fail(coordinates, shear):
    """A constructed interval needs matching arrays and at least two samples."""
    with pytest.raises(ValueError, match="samples"):
        extract_reattachment(
            coordinates, shear, step_location=0.0, step_height=1.0
        )


def test_step_location_and_height_are_required():
    """Geometry must never be inferred from the sampled profile."""
    with pytest.raises(TypeError):
        extract_reattachment([1.0, 3.0], [-1.0, 1.0])
