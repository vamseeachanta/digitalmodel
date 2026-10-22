# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic and fail-closed anchors for the residual pack."""

import pytest

from digitalmodel.residual import (
    BIAS_MAX,
    MIN_PAIRS,
    SCALE_MAX,
    SCALE_MIN,
    ResidualModel,
    correlation,
    is_significant,
    rmse,
    skill_gain,
)


def test_affine_fit_and_clip():
    """Eight pairs p=1..8, m=1.5p+0.5 lie exactly on one line.

    Slope=(12.5-2)/(8-1)=1.5, intercept=2-1.5=0.5.
    Query 4 -> 6.5; query 8 -> 12.5 clipped to caller's upper bound 10.
    Basis: NumPy 2.2 polyfit, Notes, degree-one least squares. Bounds are
    synthetic inputs, not engineering limits; tolerance is roundoff only.
    """
    model = ResidualModel.fit(
        [2, 3.5, 5, 6.5, 8, 9.5, 11, 12.5],
        [1, 2, 3, 4, 5, 6, 7, 8], lo=0, hi=10,
    )
    assert model.active
    assert model.scale == pytest.approx(1.5)
    assert model.bias == pytest.approx(0.5)
    assert model.apply(4).value == pytest.approx(6.5)
    assert model.apply(8).value == 10.0
    outside = model.apply(9)
    assert outside.escalated
    assert outside.value == 9.0


def test_rmse_and_correlation():
    """Errors [3,3] -> sqrt((9+9)/2)=3; gain 3-1=2.

    Pairs [1,2,3] and [2,4,6] have covariance proportional to both variances,
    so Pearson r=1. Basis: definitions in skill.py and NumPy 2.2 corrcoef,
    Notes. Gain equal to noise 2 fails the strict greater-than rule.
    """
    assert rmse([1, 2], [4, 5]) == 3.0
    assert skill_gain(3, 1) == 2.0
    assert correlation([1, 2, 3], [2, 4, 6]) == pytest.approx(1.0)
    assert is_significant(3, 1, 2) is False
    assert is_significant(3, 1, 1) is True


@pytest.mark.parametrize("count", range(8))
def test_insufficient_pairs_never_corrects(count):
    """Implementation choice: model.py lines 24-29 sets MIN_PAIRS=8.

    For each count 0..7, a would-be m=1.5p+0.5 fit must escalate.
    Query 4 must remain raw 4, never the candidate affine answer 6.5.
    """
    assert MIN_PAIRS == 8
    predicted = list(range(1, count + 1))
    measured = [1.5 * value + 0.5 for value in predicted]
    model = ResidualModel.fit(measured, predicted)
    assert not model.active
    assert model.status.startswith("escalate:insufficient_pairs")
    correction = model.apply(4)
    assert correction.escalated
    assert correction.value == 4.0


def test_implementation_bounds():
    """Constants from model.py lines 24-29 are policy choices, not standards.

    A line m=3p exceeds the default scale maximum 2, so query 4 returns raw 4.
    """
    assert (SCALE_MIN, SCALE_MAX, BIAS_MAX) == (0.5, 2.0, 1.0)
    model = ResidualModel.fit([3, 6, 9, 12, 15, 18, 21, 24], list(range(1, 9)))
    assert not model.active
    assert model.apply(4).escalated
    assert model.apply(4).value == 4.0
