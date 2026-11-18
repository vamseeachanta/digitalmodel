# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
# ABOUTME: Tests for anchor capacity-to-demand ratios with caller criteria.
# ABOUTME: Expected ratios use independent hand division of synthetic loads.
"""Synthetic arithmetic checks; no standard acceptance factors are asserted.

Loads are invented in kN. Hand division gives 90/40 = 2.25, 100/40 = 2.5,
110/40 = 2.75 and 0/40 = 0. The threshold 2.5 is an arbitrary caller choice.
Zero is the mathematical nonnegative bound; it is excluded from positive inputs.
"""

import math

import pytest


@pytest.mark.parametrize(
    "capacity_kn, expected_fos, expected_passes",
    [(90.0, 2.25, False), (100.0, 2.5, True), (110.0, 2.75, True),
     (0.0, 0.0, False)],
    ids=["below", "equal", "above", "zero-capacity"],
)
def test_hand_division(capacity_kn, expected_fos, expected_passes):
    """Compare exact binary-representable ratios against the supplied threshold."""
    from digitalmodel.geotechnical.anchors import FoSResult, check_anchor_fos

    result = check_anchor_fos(capacity_kn, demand_kn=40.0, required_fos=2.5)

    assert isinstance(result, FoSResult)
    assert result.actual_fos == expected_fos
    assert result.required_fos == 2.5
    assert result.passes is expected_passes
    assert result == check_anchor_fos(capacity_kn, 40.0, required_fos=2.5)


@pytest.mark.parametrize(
    "required_fos, expected_passes",
    [(0.5, True), (1.25, True), (3.0, False)],
)
def test_uses_caller_criterion(required_fos, expected_passes):
    """Hand division: 50/40 = 1.25; each criterion is an arbitrary test input."""
    from digitalmodel.geotechnical.anchors import check_anchor_fos

    result = check_anchor_fos(50.0, 40.0, required_fos=required_fos)

    assert result.actual_fos == 1.25
    assert result.required_fos == required_fos
    assert result.passes is expected_passes


def test_required_fos_has_no_default():
    """The caller must choose the criterion even for the synthetic 100/40 case."""
    from digitalmodel.geotechnical.anchors import check_anchor_fos

    with pytest.raises(TypeError, match="required_fos"):
        check_anchor_fos(capacity_kn=100.0, demand_kn=40.0)


@pytest.mark.parametrize("name", ["capacity_kn", "demand_kn", "required_fos"])
@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf, -1.0])
def test_rejects_nonfinite_and_negative_inputs(name, value):
    """NaN and infinities are nonfinite; -1 is a synthetic negative probe."""
    from digitalmodel.geotechnical.anchors import check_anchor_fos

    # Otherwise passing hand case: 100/40 = 2.5 >= the chosen criterion 2.0.
    inputs = {"capacity_kn": 100.0, "demand_kn": 40.0, "required_fos": 2.0}
    inputs[name] = value
    with pytest.raises(ValueError, match=name):
        check_anchor_fos(**inputs)


@pytest.mark.parametrize("name", ["demand_kn", "required_fos"])
def test_rejects_zero_positive_inputs(name):
    """Zero demand is undefined; zero criterion would admit zero capacity."""
    from digitalmodel.geotechnical.anchors import check_anchor_fos

    inputs = {"capacity_kn": 100.0, "demand_kn": 40.0, "required_fos": 2.0}
    inputs[name] = 0.0
    with pytest.raises(ValueError, match=name):
        check_anchor_fos(**inputs)
