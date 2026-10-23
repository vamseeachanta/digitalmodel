"""Slice 4: inverse matrices and IHO S-44 ed. 6.1.0 (2022), 3.2.3/Table 1."""

from dataclasses import replace
from math import dist, sqrt

import pytest

from digitalmodel.subsea.survey_qa import (
    DepthUncertainty,
    Offsets,
    Seabed,
    apply_corrections,
    screen_overlap,
)


@pytest.mark.parametrize("direction", [1, -1])
def test_inverse_order_and_wrong_order_residual(survey, direction):
    """Inverse of T*Ry*Rx*Rz is Rz^-1*Rx^-1*Ry^-1*T^-1.
    Thus sequential latency,pitch,roll,yaw restores every coordinate. Reversing
    the sequence cannot invert noncommuting matrices. Test biases are the
    requested example: 0.5,1.0,1.5 deg, 0.08 s (issue #2955).
    """
    offsets = Offsets(0.5, 1.0, 1.5, 0.08)
    raw = survey(bed=Seabed(50, 0.2, 0.1), direction=direction, offsets=offsets)
    correct = apply_corrections(raw, offsets)
    wrong = apply_corrections(raw, offsets, order=("yaw", "roll", "pitch", "latency"))

    def rms(s):
        return sqrt(
            sum(dist(p, q) ** 2 for p, q in zip(s.xyz_m, s.truth_xyz_m)) / len(s.xyz_m)
        )

    assert rms(correct) < 1e-10  # Floating point reconstruction, no field limit.
    assert rms(wrong) > 0.001  # Hand scale: v*tau*sin(yaw) is about 0.0042 m.
    assert rms(wrong) > rms(correct)


def test_order_must_be_a_complete_permutation(survey):
    with pytest.raises(ValueError, match="permutation"):
        apply_corrections(survey(), Offsets(), order=("roll",) * 4)


def test_overlap_hand_difference_and_source(survey):
    """Special Order, S-44 6.1.0 Table 1 read values: a=0.25 m, b=0.0075.
    At mean depth 50 m, sqrt(0.25^2+(0.0075*50)^2)=sqrt(0.203125) m.
    Two planes at 49.9 and 50.1 differ by 0.2 m, below that screen.
    This comparison is not a complete 95 percent uncertainty assessment.
    """
    a, b = survey(), survey(direction=-1)
    a = replace(a, xyz_m=tuple((x, y, 49.9) for x, y, z in a.xyz_m))
    b = replace(b, xyz_m=tuple((x, y, 50.1) for x, y, z in b.xyz_m))
    budget = DepthUncertainty(
        "Special Order",
        "6.1.0 (2022)",
        0.25,
        0.0075,
        "IHO S-44, Table 1, section 3.2.3",
    )
    result = screen_overlap(a, b, budget=budget, xy_tolerance_m=0)
    assert result.status == "WITHIN_SCREEN"
    assert result.max_abs_difference_m == pytest.approx(0.2)
    assert result.min_limit_m == pytest.approx(sqrt(0.203125))
    assert result.matched_count == len(a.xyz_m)
    assert result.unmatched_first == result.unmatched_second == 0
    assert result.source == budget.table_reference


def test_overlap_exceeds_limit(survey):
    """Same Table 1 basis: depths 49.5/50.5 have 1 m disagreement >0.451 m."""
    a, b = survey(), survey(direction=-1)
    a = replace(a, xyz_m=tuple((x, y, 49.5) for x, y, z in a.xyz_m))
    b = replace(b, xyz_m=tuple((x, y, 50.5) for x, y, z in b.xyz_m))
    budget = DepthUncertainty(
        "Special Order",
        "6.1.0 (2022)",
        0.25,
        0.0075,
        "IHO S-44, Table 1, section 3.2.3",
    )
    result = screen_overlap(a, b, budget=budget, xy_tolerance_m=0)
    assert result.status == "EXCEEDS_SCREEN"
    assert result.max_ratio == pytest.approx(1 / sqrt(0.203125))


def test_missing_table_inputs_not_evaluated(survey):
    result = screen_overlap(
        survey(), survey(direction=-1), budget=None, xy_tolerance_m=0
    )
    assert result.status == "NOT_EVALUATED"
    assert result.min_limit_m is None
    assert result.max_ratio is None


def test_corrected_overlap(survey):
    offsets = Offsets(0.5, 1.0, 1.5, 0.08)
    a = apply_corrections(survey(offsets=offsets), offsets)
    b = apply_corrections(survey(direction=-1, offsets=offsets), offsets)
    result = screen_overlap(a, b, budget=None, xy_tolerance_m=1e-9)
    assert result.max_abs_difference_m < 1e-10
    assert result.matched_count == len(a.xyz_m)


def test_overlap_reports_partial_coverage(survey):
    a, b = survey(), survey(direction=-1, beams=(0.0,))
    result = screen_overlap(a, b, budget=None, xy_tolerance_m=0)
    assert result.matched_count == 21
    assert result.unmatched_first == 84
    assert result.unmatched_second == 0
