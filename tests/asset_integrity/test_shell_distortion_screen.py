# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Part 8 limit checks from API 579-1/ASME FFS-1 June 2016 Table 8.4.

Synthetic dimensions use millimetres. Expected utilisation is calculated by
subtracting measured extreme diameters, dividing by specified diameter,
then dividing by the cited fabrication limit. No published example solution
was opened; these tests establish the individual table criteria only.
Second read confirmed on 2026-10-12 UTC against the held copy of the cited edition (independent re-read of the four limit values and their locators).
"""

from dataclasses import replace
import math

import pytest

from digitalmodel.asset_integrity.shell_distortion_screen import (
    PipingContext,
    assess_angular_misalignment,
    assess_piping_ovality,
)
from digitalmodel.citations.schema import CitationResolutionError


@pytest.fixture
def citation_root(tmp_path):
    """Use an isolated synthetic metadata fixture, never a licensed excerpt."""
    root = tmp_path / "part8-citation-fixture"
    page = root / "wikis/engineering-standards/wiki/standards/api-std-579.md"
    page.parent.mkdir(parents=True, exist_ok=True)
    page.write_text(
        '---\ncode_id: api-std-579-asme-ffs-1\n'
        'publisher: "API + ASME (joint)"\nrevision: "2016"\n---\n',
        encoding="utf-8",
    )
    return root


@pytest.fixture
def context():
    """Confirmed eligibility per 2016 clauses 8.2.4, 8.2.5.1 and 8.4.1(a)."""
    return PipingContext(
        construction_code="ASME B31.3",
        component_type="straight_pipe",
        original_tolerance_confirmed=True,
        noncyclic_service_confirmed=True,
        outside_creep_range_confirmed=True,
        original_fatigue_analysis=False,
        other_damage_present=False,
        local_shell_distortion_present=False,
    )


@pytest.mark.parametrize("standard", ["ASTM A358", "ASTM A671", "ASTM A672", "ASTM A691"])
@pytest.mark.parametrize("difference,verdict", [(9.0, "PASS"), (10.0, "PASS"), (11.0, "NEEDS_ASSESSMENT")])
def test_welded_pipe_limits(standard, difference, verdict, context, citation_root):
    """2016 Table 8.4: 10/1000 = 0.01; utilisation = 0.01/0.01 = 1."""
    result = assess_piping_ovality(
        specified_od_mm=1000.0, minimum_od_mm=1000.0,
        maximum_od_mm=1000.0 + difference, thickness_mm=20.0,
        purchase_standard=standard, context=context, citation_root=citation_root,
    )
    assert result.governing_value == pytest.approx(difference / 1000.0)
    assert result.utilisation == pytest.approx(difference / 10.0)
    assert result.verdict == verdict
    assert result.applicable
    assert result.citations[0].revision == "2016"
    assert "Table 8.4" in result.citations[0].section


@pytest.mark.parametrize("difference,verdict", [(14.0, "PASS"), (15.0, "PASS"), (16.0, "NEEDS_ASSESSMENT")])
def test_thin_pipe_ovality(difference, verdict, context, citation_root):
    """2016 Table 8.4: 15/1000 = 0.015; utilisation = 0.015/0.015 = 1."""
    result = assess_piping_ovality(
        specified_od_mm=1000.0, minimum_od_mm=1000.0,
        maximum_od_mm=1000.0 + difference, thickness_mm=30.0,
        purchase_standard="ASTM A530", context=context, citation_root=citation_root,
    )
    assert result.verdict == verdict
    assert result.utilisation == pytest.approx(difference / 15.0)


@pytest.mark.parametrize("thickness,applicable", [(29.0, True), (30.0, True), (31.0, False)])
def test_thin_wall_applicability(thickness, applicable, context, citation_root):
    """2016 Table 8.4: 30/1000 = 0.03, the inclusive thin-wall boundary."""
    result = assess_piping_ovality(
        specified_od_mm=1000.0, minimum_od_mm=1000.0,
        maximum_od_mm=1001.0, thickness_mm=thickness,
        purchase_standard="ASTM A530", context=context, citation_root=citation_root,
    )
    assert result.applicable is applicable
    if not applicable:
        assert result.utilisation is None
        assert result.verdict == "NOT_APPLICABLE"


@pytest.mark.parametrize("angle,verdict", [(2.0, "PASS"), (3.0, "PASS"), (math.nextafter(3.0, math.inf), "NEEDS_ASSESSMENT")])
def test_angular_limit(angle, verdict, context, citation_root):
    """2016 Table 8.4: angular limit 3 degrees; utilisation = angle/3."""
    result = assess_angular_misalignment(
        angle_degrees=angle, context=context, citation_root=citation_root,
    )
    assert result.utilisation == pytest.approx(angle / 3.0)
    assert result.verdict == verdict


@pytest.mark.parametrize("field,value", [
    ("construction_code", "OTHER"), ("original_tolerance_confirmed", False),
    ("component_type", "elbow"),
    ("noncyclic_service_confirmed", False), ("outside_creep_range_confirmed", False),
    ("original_fatigue_analysis", True), ("other_damage_present", True),
    ("local_shell_distortion_present", True),
])
def test_eligibility_both_screens(field, value, context, citation_root):
    """2016 clauses 8.2.1, 8.2.2, 8.2.4, 8.2.5.1 and 8.4.1(a)."""
    invalid = replace(context, **{field: value})
    angle = assess_angular_misalignment(angle_degrees=1.0, context=invalid, citation_root=citation_root)
    ovality = assess_piping_ovality(
        specified_od_mm=1000.0, minimum_od_mm=1000.0, maximum_od_mm=1001.0,
        thickness_mm=20.0, purchase_standard="ASTM A358", context=invalid,
        citation_root=citation_root,
    )
    for result in (angle, ovality):
        assert result.verdict == "NOT_APPLICABLE"
        assert result.utilisation is None
        assert result.applicability_flags


@pytest.mark.parametrize("angle", [-1.0, math.nan, math.inf, True])
def test_invalid_angle(angle, context, citation_root):
    """2016 Table 8.4 measures an angular magnitude; malformed inputs fail."""
    with pytest.raises(ValueError):
        assess_angular_misalignment(angle_degrees=angle, context=context, citation_root=citation_root)


@pytest.mark.parametrize("field,value", [
    ("specified_od_mm", 0.0), ("minimum_od_mm", -1.0),
    ("maximum_od_mm", math.inf), ("thickness_mm", math.nan),
    ("thickness_mm", 600.0), ("maximum_od_mm", 900.0),
])
def test_invalid_geometry(field, value, context, citation_root):
    """2016 Table 8.4 requires ordered, positive measured pipe dimensions."""
    inputs = dict(specified_od_mm=1000.0, minimum_od_mm=1000.0,
                  maximum_od_mm=1001.0, thickness_mm=20.0)
    inputs[field] = value
    with pytest.raises(ValueError):
        assess_piping_ovality(**inputs, purchase_standard="ASTM A358", context=context, citation_root=citation_root)


def test_unsupported_purchase_standard(context, citation_root):
    """2016 Table 8.4: other purchase specifications need their own limits."""
    result = assess_piping_ovality(
        specified_od_mm=1000.0, minimum_od_mm=1000.0, maximum_od_mm=1001.0,
        thickness_mm=20.0, purchase_standard="OTHER", context=context,
        citation_root=citation_root,
    )
    assert result.verdict == "NOT_APPLICABLE"


def test_citation_missing_fails_closed(context, citation_root):
    """The citation contract prohibits results without a matching edition."""
    with pytest.raises(CitationResolutionError, match="page_missing"):
        assess_angular_misalignment(
            angle_degrees=1.0, context=context,
            citation_root=citation_root / "missing",
        )


def test_nonboolean_confirmation(context, citation_root):
    """2016 clause 8.2.5.1 requires confirmed noncyclic eligibility."""
    with pytest.raises(ValueError, match="boolean"):
        assess_angular_misalignment(
            angle_degrees=1.0,
            context=replace(context, noncyclic_service_confirmed=1),
            citation_root=citation_root,
        )


def test_derived_ratio_overflow(context, citation_root):
    """Table 8.4 diameter normalisation must remain finite."""
    with pytest.raises(ValueError, match="ratio"):
        assess_piping_ovality(
            specified_od_mm=1e-308, minimum_od_mm=1.0,
            maximum_od_mm=1e308, thickness_mm=0.1,
            purchase_standard="ASTM A358", context=context,
            citation_root=citation_root,
        )


def test_citation_wrong_edition_fails_closed(context, citation_root):
    """The opened 2016 Table 8.4 cannot resolve against another edition."""
    page = citation_root / "wikis/engineering-standards/wiki/standards/api-std-579.md"
    page.write_text(page.read_text().replace('"2016"', '"2021"'))
    with pytest.raises(CitationResolutionError, match="frontmatter_mismatch:revision"):
        assess_angular_misalignment(
            angle_degrees=1.0, context=context, citation_root=citation_root,
        )


def test_utilisation_overflow(context, citation_root):
    """Table 8.4 utilisation normalisation must remain finite."""
    with pytest.raises(ValueError, match="utilisation"):
        assess_piping_ovality(
            specified_od_mm=1.0, minimum_od_mm=1.0,
            maximum_od_mm=1e308, thickness_mm=0.1,
            purchase_standard="ASTM A358", context=context,
            citation_root=citation_root,
        )


@pytest.mark.parametrize(
    "assess,inputs",
    [
        (assess_angular_misalignment, {"angle_degrees": 1.0}),
        (
            assess_piping_ovality,
            {
                "specified_od_mm": 1000.0,
                "minimum_od_mm": 1000.0,
                "maximum_od_mm": 1001.0,
                "thickness_mm": 20.0,
                "purchase_standard": "ASTM A358",
            },
        ),
    ],
    ids=["angular", "ovality"],
)
def test_default_citation_path_resolves(
    assess, inputs, context, citation_root, monkeypatch
):
    """Both screens resolve synthetic 2016 metadata with the default root."""
    monkeypatch.delenv("DIGITALMODEL_REPO_ROOT", raising=False)
    monkeypatch.setenv("LLM_WIKI_PATH", str(citation_root))

    result = assess(context=context, **inputs)

    assert result.verdict == "PASS"
    assert result.citations[0].revision == "2016"


@pytest.mark.parametrize(
    "assess,inputs",
    [
        (assess_angular_misalignment, {"angle_degrees": 1.0}),
        (
            assess_piping_ovality,
            {
                "specified_od_mm": 1000.0,
                "minimum_od_mm": 1000.0,
                "maximum_od_mm": 1001.0,
                "thickness_mm": 20.0,
                "purchase_standard": "ASTM A358",
            },
        ),
    ],
    ids=["angular", "ovality"],
)
def test_default_citation_path_missing_fails_closed(
    assess, inputs, context, tmp_path, monkeypatch
):
    """An empty wiki fails closed without falling back to a sibling checkout."""
    root = tmp_path / "missing-citation-fixture"
    (root / "wikis").mkdir(parents=True)
    monkeypatch.delenv("DIGITALMODEL_REPO_ROOT", raising=False)
    monkeypatch.setenv("LLM_WIKI_PATH", str(root))

    with pytest.raises(CitationResolutionError, match="page_missing"):
        assess(context=context, **inputs)
