# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic arithmetic checks; these are not empirical validation cases."""

from dataclasses import replace
from math import inf, nan, nextafter

import pytest

from digitalmodel.citations.schema import Citation, CitationResolutionError, CitedValue
from digitalmodel.hydrodynamics.parametric_hull_analysis.squat import (
    ROMISCH_REFERENCE,
    clearance_budget,
    romisch_squat,
    squat_speed_sweep,
)


@pytest.fixture
def case(tmp_path):
    """Provision isolated citation metadata, never a live engineering record."""
    reference = Citation(
        code_id="SYNTHETIC-INPUTS", publisher="Test fixture", revision="1",
        section="Hand calculation in test_squat.py; not engineering input",
        wiki_path="wikis/test/inputs.md", source_sibling="generic",
    )
    depth = Citation(
        code_id="PIANC-DEPTH-BANDS", publisher="ITTC", revision="2002",
        section="Appendix A.1, p. 201",
        wiki_path="wikis/marine-engineering/wiki/standards/pianc-depth-bands.md",
    )
    for citation in (ROMISCH_REFERENCE, reference, depth):
        path = tmp_path / citation.wiki_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(
            f"---\ncode_id: {citation.code_id}\npublisher: {citation.publisher}\n"
            f"revision: {citation.revision}\n---\nSynthetic resolution fixture.\n",
            encoding="utf-8",
        )
    return dict(
        length_bp_m=100.0, beam_m=12.5, block_coefficient=0.8,
        draft_m=10.0, water_depth_m=14.4, speed_ms=4.0,
        critical_speed=CitedValue(8.0, reference, "m/s"),
        minimum_depth_ratio=CitedValue(1.1, reference, "dimensionless"),
        maximum_depth_ratio=CitedValue(2.0, reference, "dimensionless"),
        citation_root=tmp_path,
    )


def test_hand_calculation(case):
    """PIANC On Course 142 (2011), App. A4, eqs. 7-10, synthetic inputs.

    r=4/8=0.5; Cv=8*(1/4)*(0+1/16)=1/8.
    Bow Cf=(10*12.5*0.8/100)^2=1; stern Cf=1.
    K=0.155*sqrt(14.4/10)=0.155*1.2=0.186.
    Each squat=(1/8)*1*0.186*10=0.2325 m.
    The 1.1..2.0 input interval is synthetic, not a published validity limit.
    """
    result = romisch_squat(**case)
    assert result.bow_m == pytest.approx(0.2325)
    assert result.stern_m == pytest.approx(0.2325)
    assert result.maximum_m == pytest.approx(0.2325)
    assert result.depth_class.value == "shallow"
    assert result.navigation_status == "NOT_EVALUATED"
    assert result.coefficients["speed"].value == pytest.approx(0.125)
    assert result.coefficients["depth"].citation == ROMISCH_REFERENCE
    assert result.critical_speed == case["critical_speed"]


def test_stern_can_govern(case):
    """Halving B quarters Cf: bow=0.2325/4; stern remains 0.2325."""
    result = romisch_squat(**(case | {"beam_m": 6.25}))
    assert result.bow_m == pytest.approx(0.058125)
    assert result.maximum_m == result.stern_m


def test_bow_can_govern(case):
    """Doubling B quadruples Cf: bow=4*0.2325=0.93 m."""
    result = romisch_squat(**(case | {"beam_m": 25.0}))
    assert result.maximum_m == pytest.approx(0.93)
    assert result.maximum_m == result.bow_m


def test_zero_speed(case):
    result = romisch_squat(**(case | {"speed_ms": 0.0}))
    assert result.maximum_m == 0.0


@pytest.mark.parametrize("key", [
    "length_bp_m", "beam_m", "draft_m", "water_depth_m", "block_coefficient",
])
@pytest.mark.parametrize("bad", [0.0, -1.0, nan, inf])
def test_invalid_geometry(case, key, bad):
    with pytest.raises(ValueError):
        romisch_squat(**(case | {key: bad}))


@pytest.mark.parametrize("bad", [-1.0, nan, inf, 8.0, 9.0])
def test_invalid_speed(case, bad):
    with pytest.raises(ValueError):
        romisch_squat(**(case | {"speed_ms": bad}))


@pytest.mark.parametrize("depth", [9.0, 10.0, 10.9, 20.1])
def test_grounding_and_applicability_guards(case, depth):
    with pytest.raises(ValueError):
        romisch_squat(**(case | {"water_depth_m": depth}))


@pytest.mark.parametrize("ratio, expected", [(1.2, "very_shallow"), (1.5, "shallow"), (2.0, "medium")])
def test_existing_depth_boundaries(case, ratio, expected):
    result = romisch_squat(**(case | {"water_depth_m": ratio * 10}))
    assert result.depth_class.value == expected


def test_envelope_endpoints(case):
    for depth in (11.0, 20.0):
        assert romisch_squat(**(case | {"water_depth_m": depth})).maximum_m > 0
    for depth in (nextafter(11.0, 0.0), nextafter(20.0, inf)):
        with pytest.raises(ValueError):
            romisch_squat(**(case | {"water_depth_m": depth}))


@pytest.mark.parametrize("key", ["critical_speed", "minimum_depth_ratio", "maximum_depth_ratio"])
def test_input_citation_and_units(case, key):
    with pytest.raises(ValueError, match="units"):
        romisch_squat(**(case | {key: replace(case[key], units="knots")}))
    with pytest.raises(CitationResolutionError):
        romisch_squat(**(case | {key: replace(case[key], citation=replace(
            case[key].citation, revision="missing"))}))


@pytest.mark.parametrize("target", ["method", "depth"])
def test_missing_method_or_depth_record_fails_closed(case, target):
    path = (ROMISCH_REFERENCE.wiki_path if target == "method" else
            "wikis/marine-engineering/wiki/standards/pianc-depth-bands.md")
    (case["citation_root"] / path).write_text("No frontmatter", encoding="utf-8")
    with pytest.raises(CitationResolutionError):
        romisch_squat(**case)


def test_clearance_hand_calculation(case):
    """Geometric budget: 14.4-10-0.2325-0.5=3.6675 m; margin=0.6675 m.

    Allowance 0.5 m and minimum 3 m are synthetic specified inputs, not rules.
    """
    squat = romisch_squat(**case)
    minimum = CitedValue(3.0, case["critical_speed"].citation, "m")
    result = clearance_budget(squat, allowance_m=0.5, minimum_clearance=minimum,
                              citation_root=case["citation_root"])
    assert result.static_m == pytest.approx(4.4)
    assert result.residual_m == pytest.approx(3.6675)
    assert result.margin_m == pytest.approx(0.6675)
    assert result.status == "MEETS_INPUT_MINIMUM"
    assert result.navigation_status == "NOT_EVALUATED"


def test_missing_criterion_and_negative_clearance(case):
    result = clearance_budget(romisch_squat(**case), allowance_m=5.0,
                              minimum_clearance=None, citation_root=case["citation_root"])
    assert result.residual_m == pytest.approx(-0.8325)
    assert result.status == "NOT_EVALUATED"
    assert result.margin_m is None
    assert result.budget_exhausted


@pytest.mark.parametrize("allowance", [-1.0, nan, inf])
def test_invalid_allowance(case, allowance):
    with pytest.raises(ValueError):
        clearance_budget(romisch_squat(**case), allowance_m=allowance,
                         minimum_clearance=None, citation_root=case["citation_root"])


def test_speed_sweep(case):
    """At r=0.25: Cv=0.5*(1/256+1/16)=17/512.

    S=(17/512)*0.186*10=0.0617578125 m; other expectations above.
    """
    inputs = {k: v for k, v in case.items() if k != "speed_ms"}
    rows = squat_speed_sweep([4.0, 0.0, 2.0], **inputs)
    assert [r.maximum_m for r in rows] == pytest.approx([0.2325, 0, 0.0617578125])
    with pytest.raises(ValueError):
        squat_speed_sweep([0.0, 8.0], **inputs)
    with pytest.raises(ValueError):
        squat_speed_sweep([], **inputs)


@pytest.mark.parametrize("key, value", [
    ("critical_speed", 0), ("critical_speed", -1), ("critical_speed", inf),
    ("minimum_depth_ratio", 1), ("minimum_depth_ratio", 3),
    ("maximum_depth_ratio", 1), ("maximum_depth_ratio", nan),
])
def test_invalid_cited_values(case, key, value):
    with pytest.raises(ValueError):
        romisch_squat(**(case | {key: replace(case[key], value=value)}))


def test_invalid_block_coefficient_and_overflow(case):
    with pytest.raises(ValueError):
        romisch_squat(**(case | {"block_coefficient": 1.01}))
    with pytest.raises(ValueError):
        romisch_squat(**(case | {"beam_m": 1e308, "length_bp_m": 1e-308}))


def test_unverified_or_altered_result_rejected(case):
    result = romisch_squat(**case)
    with pytest.raises(ValueError, match="does not match"):
        clearance_budget(replace(result, bow_m=0), allowance_m=0,
                         minimum_clearance=None, citation_root=case["citation_root"])
    (case["citation_root"] / ROMISCH_REFERENCE.wiki_path).write_text(
        "Invalidated record", encoding="utf-8")
    with pytest.raises(CitationResolutionError):
        clearance_budget(result, allowance_m=0, minimum_clearance=None,
                         citation_root=case["citation_root"])


@pytest.mark.parametrize("minimum, status", [
    (5, "BELOW_INPUT_MINIMUM"), (0, "MEETS_INPUT_MINIMUM"),
])
def test_clearance_comparison(case, minimum, status):
    result = clearance_budget(
        romisch_squat(**case), allowance_m=0,
        minimum_clearance=CitedValue(minimum, case["critical_speed"].citation, "m"),
        citation_root=case["citation_root"],
    )
    assert result.status == status


def test_exact_minimum(case):
    """At rest, depth=15 and draft=10 give exactly 5 m static clearance."""
    result = clearance_budget(
        romisch_squat(**(case | {"speed_ms": 0, "water_depth_m": 15})),
        allowance_m=0,
        minimum_clearance=CitedValue(5, case["critical_speed"].citation, "m"),
        citation_root=case["citation_root"],
    )
    assert result.margin_m == 0
    assert result.status == "MEETS_INPUT_MINIMUM"


def test_criterion_provenance(case):
    criterion = CitedValue(1, case["critical_speed"].citation, "m")
    for invalid in (replace(criterion, units="ft"), replace(criterion, value=-1)):
        with pytest.raises(ValueError):
            clearance_budget(romisch_squat(**case), allowance_m=0,
                             minimum_clearance=invalid, citation_root=case["citation_root"])
    with pytest.raises(CitationResolutionError):
        clearance_budget(romisch_squat(**case), allowance_m=0,
                         minimum_clearance=replace(criterion, citation=replace(
                             criterion.citation, revision="missing")),
                         citation_root=case["citation_root"])
