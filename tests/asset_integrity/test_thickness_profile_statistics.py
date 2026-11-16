"""Illustrative example profile: sample scatter is distinct from population scatter."""

import math
from dataclasses import FrozenInstanceError

import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment.grid_parser import GridParser
from digitalmodel.asset_integrity.thickness_profile_statistics import (
    ThicknessReadings,
    sample_thickness_statistics,
)


def test_existing_grid_reductions():
    """Illustrative example, in the existing grid API's inches."""
    frame = pd.DataFrame([[0.4], [0.5], [0.6]])
    assert GridParser.min_thickness(frame) == 0.4
    assert GridParser.mean_thickness(frame) == 0.5


def test_sample_statistics_from_hand_residuals():
    result = sample_thickness_statistics(ThicknessReadings((0.008, 0.010, 0.012)))
    expected_squares = (0.002 ** 2, 0.0, 0.002 ** 2)
    assert result.minimum_m == pytest.approx(0.008)
    assert result.mean_m == pytest.approx(0.010)
    assert result.residuals_m == pytest.approx((0.002, 0.0, -0.002), abs=1e-16)
    assert result.squared_residuals_m2 == pytest.approx(expected_squares)
    assert result.sum_squared_residuals_m2 == pytest.approx(sum(expected_squares))
    assert result.sample_variance_m2 == pytest.approx(sum(expected_squares) / (3 - 1))
    assert result.coefficient_of_variation_ratio == pytest.approx(0.2)


def test_uniform_profile_has_no_scatter():
    result = sample_thickness_statistics(ThicknessReadings((0.016,) * 4))
    assert result.mean_m == pytest.approx(result.minimum_m)
    assert result.sample_variance_m2 == 0
    assert result.coefficient_of_variation_ratio == 0


def test_unit_scaling_preserves_cov_and_scales_variance():
    a = sample_thickness_statistics(ThicknessReadings((0.009, 0.012, 0.015)))
    b = sample_thickness_statistics(ThicknessReadings((0.018, 0.024, 0.030)))
    assert b.coefficient_of_variation_ratio == pytest.approx(a.coefficient_of_variation_ratio)
    assert b.sample_variance_m2 == pytest.approx(4 * a.sample_variance_m2)
    assert b.mean_m == pytest.approx(2 * a.mean_m)


@pytest.mark.parametrize('values', [(), (0.01,), (0.01, 0.0), (0.01, -0.01),
                                    (0.01, math.nan), (0.01, math.inf),
                                    (True, 0.01), ('0.01', 0.01), [0.01, 0.02]])
def test_invalid_profile_is_rejected(values):
    with pytest.raises(ValueError):
        ThicknessReadings(values)


def test_profile_is_immutable():
    with pytest.raises(FrozenInstanceError):
        ThicknessReadings((0.01, 0.02)).thicknesses_m = (0.02, 0.03)
