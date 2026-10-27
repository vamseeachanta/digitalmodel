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
    """Planes at 49.9/50.1 m differ by 0.2 m; counts need no standard.

    The previous table-reference string does not authorize a TVU ceiling.
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
    assert result.status == "NOT_EVALUATED"
    assert result.max_abs_difference_m == pytest.approx(0.2)
    assert result.min_limit_m is None
    assert result.max_ratio is None
    assert result.matched_count == len(a.xyz_m)
    assert result.unmatched_first == result.unmatched_second == 0
    assert "citation evidence unavailable" in result.source


def test_large_difference_does_not_authorize_screen(survey):
    """A 1 m depth disagreement still needs citation evidence for assessment."""
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
    assert result.status == "NOT_EVALUATED"
    assert result.max_abs_difference_m == pytest.approx(1)
    assert result.max_ratio is None
    assert result.min_limit_m is None


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


@pytest.mark.parametrize(
    "edition,reference",
    [
        ("6.1.0 (2022)", "IHO S-44, Table 1, section 3.2.3"),
        ("6.2.0", "IHO S-44, Table 1, section 3.2.3"),
        ("6.1.0 (2022)", "A different publisher, Table 1"),
        ("6.1.0 (2022)", "IHO S-44, section 1"),
        ("6.1.0 (2022)", "https://iho.int/"),
    ],
)
def test_reference_strings_do_not_authorize_screen(survey, edition, reference):
    """Correct-looking, mismatched and URL strings are not citation evidence."""
    budget = DepthUncertainty("Special Order", edition, 0.25, 0.0075, reference)
    result = screen_overlap(
        survey(), survey(direction=-1), budget=budget, xy_tolerance_m=0
    )
    assert result.status == "NOT_EVALUATED"
    assert result.min_limit_m is None
    assert result.max_ratio is None
    assert result.matched_count == len(survey().xyz_m)
    assert "citation" in result.source.lower()


def test_direct_tvu_calculation_requires_eligible_citation():
    """A bibliographic string must not permit bypassing the overlap guard."""
    from digitalmodel.citations import CitationResolutionError

    budget = DepthUncertainty(
        "Special Order",
        "6.1.0 (2022)",
        0.25,
        0.0075,
        "IHO S-44, Table 1, section 3.2.3",
    )
    with pytest.raises(CitationResolutionError, match="eligible_citation_unavailable"):
        budget.limit_m(50)


def test_overlap_does_not_call_unqualified_limit(survey, monkeypatch):
    """No numeric ceiling is calculated before citation evidence is eligible."""

    def unqualified(*args):
        pytest.fail("an unqualified TVU calculation was attempted")

    monkeypatch.setattr(DepthUncertainty, "limit_m", unqualified)
    budget = DepthUncertainty("Special Order", "6.1.0 (2022)", 0.25, 0.0075, "Table 1")
    result = screen_overlap(survey(), survey(), budget=budget, xy_tolerance_m=0)
    assert result.status == "NOT_EVALUATED"
