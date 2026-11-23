# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic hand checks of CSR renewal criteria, edition 1 July 2025."""

from dataclasses import replace

import pytest

from digitalmodel.asset_integrity.hull_wastage_screen import (
    HullPlate,
    SurveyScope,
    screen_global_corrosion,
    screen_local_corrosion,
)
from digitalmodel.citations.schema import Citation, CitationResolutionError


@pytest.fixture
def citation_root(tmp_path):
    """Synthetic resolver metadata; CSR 2025 Chapter 13 supplies the method."""
    page = tmp_path / "wikis" / "test" / "csr.md"
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: IACS-CSR\npublisher: IACS\nrevision: 2025-07-01\n---\n",
        encoding="utf-8",
    )
    citation = Citation(
        code_id="IACS-CSR", publisher="IACS", revision="2025-07-01",
        section="Part 1 Chapter 13", wiki_path="wikis/test/csr.md",
        source_sibling="generic",
    )
    return citation, tmp_path


@pytest.fixture
def scope():
    """CSR 2025 Chapter 13 Sections 1, 1.1.2, and 2, 2.2.1: synthetic survey."""
    return SurveyScope(classed_under_csr=True, in_operation=True,
                       special_survey=True, age_years=11.0,
                       simple_section_confirmed=True)


@pytest.mark.parametrize("measured,verdict", [
    (7.999, "RENEW"), (8.0, "MONITOR_RESERVE"),
    (8.499, "MONITOR_RESERVE"), (8.5, "ACCEPT_LOCAL"),
])
def test_local_renewal_boundaries(citation_root, scope, measured, verdict):
    """CSR 2025 Ch 13 Sec 2 symbols and 2.1.1–2.1.3, pages 715–717.

    Synthetic thicknesses in millimetres: as-built 12, corrosion addition 3,
    voluntary addition 1. Renewal = 12 - 3 - 1 = 8; repair = 12 - 1 = 11.
    Reserve upper boundary = 8 + 0.5 = 8.5. Equality at 8 is in the reserve
    band; equality at 8.5 is outside it. Diminution = 12 - measured.
    """
    citation, root = citation_root
    result = screen_local_corrosion(
        as_built_mm=12, measured_mm=measured, corrosion_addition_mm=3,
        voluntary_addition_mm=1, scope=scope, citation=citation, wiki_root=root,
    )
    assert result.verdict == verdict
    assert result.values["renewal_thickness_mm"] == 8
    assert result.values["repair_thickness_mm"] == 11
    assert result.values["diminution_mm"] == pytest.approx(12 - measured)
    assert result.applicability_flags == ()
    assert len(result.citations) == 3


@pytest.mark.parametrize("field,value", [
    ("as_built_mm", 0), ("measured_mm", -1),
    ("measured_mm", float("nan")), ("measured_mm", float("inf")),
    ("corrosion_addition_mm", -1), ("corrosion_addition_mm", 12),
    ("voluntary_addition_mm", -1), ("reserve_mm", 0.4),
])
def test_local_invalid_inputs(citation_root, scope, field, value):
    """CSR 2025 Ch 13 Sec 2, 2.1.1: positive renewal; finite dimensions."""
    citation, root = citation_root
    kwargs = dict(as_built_mm=12, measured_mm=10, corrosion_addition_mm=3,
                  voluntary_addition_mm=0, reserve_mm=0.5)
    kwargs[field] = value
    with pytest.raises(ValueError):
        screen_local_corrosion(**kwargs, scope=scope, citation=citation,
                               wiki_root=root)


@pytest.mark.parametrize("field", ["classed_under_csr", "in_operation"])
def test_local_outside_scope(citation_root, scope, field):
    """CSR 2025 Ch 13 Sec 1, 1.1.2: operational ships classed under CSR only."""
    citation, root = citation_root
    result = screen_local_corrosion(
        as_built_mm=12, measured_mm=10, corrosion_addition_mm=3,
        voluntary_addition_mm=0, scope=replace(scope, **{field: False}),
        citation=citation, wiki_root=root,
    )
    assert result.verdict == "NOT_APPLICABLE"
    assert result.values == {}
    assert result.applicability_flags


def plates(measured_mm=9):
    """CSR 2025 Ch 13 Sec 2, 2.2.2: synthetic two-flange/one-web section."""
    return (
        HullPlate("bottom", "bottom", "horizontal", 2, 0, 10, measured_mm, 2),
        HullPlate("deck", "deck", "horizontal", 2, 2, 10, measured_mm, 2),
        HullPlate("web", "neutral", "vertical", 2, 1, 10, measured_mm, 2),
    )


def test_global_hand_calculation(citation_root, scope):
    """CSR 2025 Ch 13 Sec 2, 2.2.2(a,b); Ch 5 Sec 1, 1.4.2–1.5.1.

    Rectangle inertia and parallel-axis sums: Engineering Statics, undated
    online edition, Sections 10.2.1 and 10.4, Equation 10.4.1, opened.

    Synthetic flat plates, gross thickness 0.010 metre, measured 0.009 metre.
    Two horizontal flanges: width 2 metres, centroids 0 and 2 metres.
    One vertical web: height 2 metres, centroid 1 metre. Neutral axis = 1.
    Gross area = 3 * 2 * 0.010 = 0.060 square metre.
    Gross inertia = 2*(2*0.010**3/12 + 0.020*1**2) + 0.010*2**3/12
    = 0.046667 metre to the fourth power.
    Measured inertia = 2*(2*0.009**3/12 + 0.018*1**2) + 0.009*2**3/12
    = 0.042000243 metre to the fourth power. Both extreme-fibre distances
    are 1 metre; modulus = inertia / 1. Required modulus = 0.9*0.046667.
    Neutral-zone target area = 2*(10 - 0.5*2)/1000 = 0.018 square metre.
    Measured neutral area = 2*9/1000 = 0.018; equality passes. The very small
    flange own-inertia contribution makes the modulus fraction below 0.9;
    it is retained, not rounded away. Governing verdict is RENEW.
    """
    citation, root = citation_root
    result = screen_global_corrosion(
        plates(), depth_m=2, zones_confirmed=True, scope=scope,
        citation=citation, wiki_root=root,
    )
    assert result.values["gross_sm_deck_m3"] == pytest.approx(0.046667)
    assert result.values["measured_sm_deck_m3"] == pytest.approx(0.042000243)
    assert result.values["neutral_target_area_m2"] == pytest.approx(0.018)
    assert result.utilizations["neutral_area"] == pytest.approx(1)
    assert result.utilizations["deck_modulus"] > 1
    assert result.verdict == "RENEW"
    assert result.governing_check in ("deck_modulus", "bottom_modulus")


@pytest.mark.parametrize("thickness,verdict", [(8.999, "RENEW"),
                                                (9.001, "PASS_GLOBAL")])
def test_global_limit_sides(citation_root, scope, thickness, verdict):
    """CSR 2025 Ch 13 Sec 2, 2.2.2: flange modulus and web area limits.

    For the preceding section, web area = 2*t/1000; target = 0.018.
    Thicknesses 8.999 and 9.001 millimetres lie on opposite sides of 9.
    The modulus boundary also lies between them by the inertia formula.
    """
    citation, root = citation_root
    result = screen_global_corrosion(
        plates(thickness), depth_m=2, zones_confirmed=True, scope=scope,
        citation=citation, wiki_root=root,
    )
    assert result.verdict == verdict


@pytest.mark.parametrize("age,applicable", [(9.999, False), (10, False),
                                           (10.001, True)])
def test_global_age_limit(citation_root, scope, age, applicable):
    """CSR 2025 Ch 13 Sec 2, 2.2.1: special surveys after ten years."""
    citation, root = citation_root
    result = screen_global_corrosion(
        plates(10), depth_m=2, zones_confirmed=True,
        scope=replace(scope, age_years=age), citation=citation, wiki_root=root,
    )
    assert (result.verdict == "PASS_GLOBAL") is applicable
    if not applicable:
        assert result.values == {}


@pytest.mark.parametrize("field", ["special_survey", "classed_under_csr",
                                    "in_operation"])
def test_global_survey_scope(citation_root, scope, field):
    """CSR 2025 Ch 13 Sec 1, 1.1.2, and Sec 2, 2.2.1: scope is explicit."""
    citation, root = citation_root
    result = screen_global_corrosion(
        plates(), depth_m=2, zones_confirmed=True,
        scope=replace(scope, **{field: False}), citation=citation, wiki_root=root,
    )
    assert result.verdict == "NOT_APPLICABLE"


def test_unconfirmed_zones(citation_root, scope):
    """CSR 2025 Ch 13 Sec 2, 1.2.1–1.2.3: approved zone membership required."""
    citation, root = citation_root
    result = screen_global_corrosion(
        plates(), depth_m=2, zones_confirmed=False, scope=scope,
        citation=citation, wiki_root=root,
    )
    assert result.verdict == "NOT_APPLICABLE"
    assert "zone_membership_unconfirmed" in result.applicability_flags


def test_equivalent_deck_outside_reused_api(citation_root, scope):
    """CSR 2025 Ch 5 Sec 1, 1.4.3: simple deck-distance branch only.

    The reused API has no transverse coordinate for the elevated-member
    equivalent-deck branch. That branch is outside the implemented screen.
    """
    citation, root = citation_root
    result = screen_global_corrosion(
        plates(), depth_m=2, zones_confirmed=True,
        scope=replace(scope, simple_section_confirmed=False),
        citation=citation, wiki_root=root,
    )
    assert result.verdict == "NOT_APPLICABLE"
    assert "section_outside_supported_idealization" in result.applicability_flags


@pytest.mark.parametrize("change", [{"measured_mm": 0}, {"width_m": -1},
                                    {"corrosion_addition_mm": 20},
                                    {"orientation": "sloped"},
                                    {"centroid_m": float("inf")}])
def test_invalid_global_geometry(citation_root, scope, change):
    """CSR 2025 Ch 13 Sec 2, 2.2.2: positive finite contributing plate areas."""
    citation, root = citation_root
    items = (replace(plates()[0], **change), *plates()[1:])
    with pytest.raises(ValueError):
        screen_global_corrosion(
            items, depth_m=2, zones_confirmed=True, scope=scope,
            citation=citation, wiki_root=root,
        )


def test_missing_citation_fails_closed(citation_root, scope):
    """CSR 2025 Ch 13 local formula; unresolved provenance prevents calculation."""
    citation, root = citation_root
    citation = replace(citation, wiki_path="wikis/test/missing.md")
    with pytest.raises(CitationResolutionError):
        screen_local_corrosion(
            as_built_mm=12, measured_mm=10, corrosion_addition_mm=3,
            voluntary_addition_mm=0, scope=scope, citation=citation,
            wiki_root=root,
        )
