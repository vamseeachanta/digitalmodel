# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent controls for the dimensionless canonical-card comparison."""

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.shape_comparison import (
    loop_distance,
    prepare,
)

SQUARE = np.array([[0, 0], [1, 0], [1, 1], [0, 1]], dtype=float)


def test_square_resampling_matches_hand_calculation():
    """Unit perimeter = 4; eight samples advance 1/2 along successive edges."""
    expected = [[0, 0], [0.5, 0], [1, 0], [1, 0.5],
                [1, 1], [0.5, 1], [0, 1], [0, 0.5]]
    np.testing.assert_allclose(prepare(*SQUARE.T, points=8), expected)


def test_axis_units_start_and_traversal_do_not_change_distance():
    """The same square in x'=2x+7, y'=3y-4 has distance zero.

    Integer edge rotations and reversal permute these four vertices only;
    minimising over cyclic shifts and reversal restores the same sequence.
    """
    transformed = SQUARE * [2, 3] + [7, -4]
    reference = prepare(*SQUARE.T, points=4)
    for reverse in (False, True):
        vertices = transformed[::-1] if reverse else transformed
        for shift in range(4):
            candidate = np.roll(vertices, shift, axis=0)
            assert loop_distance(reference, prepare(*candidate.T, points=4)) == 0


def test_distance_matches_independent_translation_calculation():
    """Prepared square shifted by (3,4): each separation is sqrt(9+16)=5.

    Every competing permutation has mean norm >= norm(mean displacement)=5
    by the triangle inequality. Thus the minimum is exactly five.
    """
    assert loop_distance(SQUARE, SQUARE + [3, 4]) == pytest.approx(5)


def test_duplicate_endpoint_and_stationary_samples_preserve_square():
    """Repeating a vertex adds zero arc length; it cannot move any sample."""
    repeated = np.repeat(np.vstack([SQUARE, SQUARE[0]]), 2, axis=0)
    np.testing.assert_allclose(prepare(*repeated.T, points=8),
                               prepare(*SQUARE.T, points=8))


def test_constant_axis_has_zero_normalised_range():
    """A constant coordinate maps to zero, not a division by zero."""
    loop = prepare([2, 2, 2], [0, 1, 0], points=4)
    np.testing.assert_array_equal(loop[:, 0], 0)
    assert np.isfinite(loop).all()


def test_stationary_card_has_zero_shape():
    """Identical coordinates have zero length and zero normalised extent."""
    np.testing.assert_array_equal(prepare([2, 2, 2], [5, 5, 5], points=4),
                                  np.zeros((4, 2)))


def test_large_finite_units_do_not_overflow_normalisation():
    """Changing square units to +/-1e308 still maps each axis onto [0,1].

    These are arithmetic stress inputs, not dimensioned pump operating limits.
    Scaling before subtraction avoids the unrepresentable range 2e308.
    """
    large = (SQUARE * 2 - 1) * 1e308
    np.testing.assert_allclose(prepare(*large.T, points=4), SQUARE)


@pytest.mark.parametrize("position,load", [
    ([], []), ([0, 1], [0, 1]), ([0, 1, 2], [0, 1]),
    ([0, np.nan, 1], [0, 1, 0]), ([0, 1, 0], [0, np.inf, 1]),
    ([[0, 1, 0]], [[0, 1, 0]]),
])
def test_prepare_rejects_invalid_coordinates(position, load):
    """Missing, mismatched or nonfinite coordinates cannot define a card."""
    with pytest.raises(ValueError):
        prepare(position, load)


@pytest.mark.parametrize("points", [0, 2, 3.5, True])
def test_prepare_rejects_invalid_sample_count(points):
    """A sampled polygon requires an integer count of at least three."""
    with pytest.raises(ValueError):
        prepare(*SQUARE.T, points=points)


@pytest.mark.parametrize("candidate", [SQUARE[:3], np.zeros((4, 3)),
                                       np.full((4, 2), np.nan), []])
def test_distance_rejects_incompatible_loops(candidate):
    """Pairwise distance requires equal finite arrays of planar vertices."""
    with pytest.raises(ValueError):
        loop_distance(SQUARE, candidate)
