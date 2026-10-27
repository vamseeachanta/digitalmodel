# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Selection, validation and life targets for design fatigue factors."""

import sys

import pandas as pd
import pytest

from digitalmodel.fatigue import (
    DFFCategory,
    required_calculated_life,
    select_design_fatigue_factor,
)
from digitalmodel.fatigue.damage import design_life_check
from digitalmodel.fatigue.sn_curves import get_sn_curve


@pytest.mark.parametrize(
    "category, factor, years",
    [
        (DFFCategory.ACCESSIBLE_NON_CRITICAL, 1.0, 25.0),
        (DFFCategory.ACCESSIBLE_CRITICAL, 2.0, 50.0),
        (DFFCategory.NOT_ACCESSIBLE, 3.0, 75.0),
        (DFFCategory.SUBSTANTIAL_CONSEQUENCE, 10.0, 250.0),
    ],
)
def test_common_practice_defaults(category, factor, years):
    assert select_design_fatigue_factor(category) == factor
    assert required_calculated_life(25.0, factor) == years
    assert select_design_fatigue_factor(category.value) == factor


def test_category_override_preserves_defaults_and_input():
    factors = {DFFCategory.NOT_ACCESSIBLE: 4.0}
    factor = select_design_fatigue_factor(DFFCategory.NOT_ACCESSIBLE, factors=factors)
    assert factor == 4.0
    assert required_calculated_life(25.0, factor) == 100.0
    assert factors == {DFFCategory.NOT_ACCESSIBLE: 4.0}
    assert (
        select_design_fatigue_factor(DFFCategory.ACCESSIBLE_CRITICAL, factors=factors)
        == 2.0
    )
    assert select_design_fatigue_factor(DFFCategory.NOT_ACCESSIBLE) == 3.0


def test_unknown_category_is_rejected():
    with pytest.raises(ValueError):
        select_design_fatigue_factor("unknown")


@pytest.mark.parametrize(
    "factor",
    [0, -1, 0.5, float("nan"), float("inf"), -float("inf"), True, "3", None],
)
def test_invalid_factors_are_rejected(factor):
    with pytest.raises(ValueError, match="factor"):
        select_design_fatigue_factor(
            DFFCategory.NOT_ACCESSIBLE, factors={DFFCategory.NOT_ACCESSIBLE: factor}
        )
    with pytest.raises(ValueError, match="factor"):
        required_calculated_life(25.0, factor)


@pytest.mark.parametrize(
    "years", [0, -1, float("nan"), float("inf"), -float("inf"), True, "25", None]
)
def test_invalid_service_life_is_rejected(years):
    with pytest.raises(ValueError, match="service_life_years"):
        required_calculated_life(years, 3.0)


def test_overflow_is_rejected():
    with pytest.raises(ValueError, match="required calculated life"):
        required_calculated_life(sys.float_info.max, 3.0)


def test_fractional_years_and_configured_factor():
    assert required_calculated_life(12.5, 1.5) == 18.75


def test_required_life_is_a_target_for_existing_life_check():
    curve = get_sn_curve("D")
    annual_cycles = float(curve.cycles(100.0)) / 50.0
    histogram = pd.DataFrame({"stress_range": [100.0], "cycles": [annual_cycles]})
    factor = select_design_fatigue_factor(DFFCategory.NOT_ACCESSIBLE)
    target = required_calculated_life(25.0, factor)

    unfactored = design_life_check(histogram, curve, target_years=25.0)
    factored = design_life_check(histogram, curve, target_years=target)

    assert unfactored["years_to_failure"] == pytest.approx(50.0)
    assert unfactored["pass_fail"] == "PASS"
    assert factored["years_to_failure"] == pytest.approx(50.0)
    assert factored["pass_fail"] == "FAIL"
