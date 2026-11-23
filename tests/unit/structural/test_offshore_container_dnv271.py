"""Invariant + citation tests for the DNV 2.7-1 offshore-container model."""

import math

import pytest

from digitalmodel.structural.offshore_container.dnv_2_7_1 import (
    SHS,
    DesignFactors,
    allowable_stress,
    factor_citations,
    sling_angle_to_vertical,
    utilization,
)

BASE_LIFT = 0.5 * math.hypot(6.0, 2.5)  # gives v = 45 deg


def test_shs_section_props():
    sec = SHS.from_mm(150, 10)
    bi = 0.130
    assert sec.area == pytest.approx(0.150 ** 2 - bi ** 2)
    assert sec.modulus == pytest.approx(sec.inertia / 0.075)


def test_sling_angle_to_vertical_45():
    half_diag = 0.5 * math.hypot(4.0, 4.0)
    v = sling_angle_to_vertical(4.0, 4.0, half_diag)  # lift = half-diagonal
    assert math.degrees(v) == pytest.approx(45.0)


def test_allowable_is_085_of_yield():
    # DNV §4.2.1: sigma_e <= 0.85 * Re (no separate material factor)
    f = DesignFactors()
    assert allowable_stress(355e6, f) == pytest.approx(0.85 * 355e6)


def test_dnv_factor_values():
    f = DesignFactors()
    assert f.structure_load_factor == 2.5
    assert f.padeye_load_factor == 3.0
    assert f.usage_factor == 0.85


def test_FL_is_2p5_R_g():
    u = utilization(6.0, 2.5, BASE_LIFT, 25_000, SHS.from_mm(250, 12))
    assert u.FL_kN == pytest.approx(2.5 * 25_000 * 9.80665 / 1e3, rel=1e-6)


def test_util_increases_with_rating():
    sec = SHS.from_mm(250, 12)
    low = utilization(6.0, 2.5, BASE_LIFT, 10_000, sec).governing
    high = utilization(6.0, 2.5, BASE_LIFT, 30_000, sec).governing
    assert high > low


def test_steeper_sling_reduces_padeye_and_toprail():
    sec = SHS.from_mm(250, 12)
    half_diag = 0.5 * math.hypot(6.0, 2.5)
    shallow = utilization(6.0, 2.5, half_diag / math.tan(math.radians(60)), 25_000, sec)
    steep = utilization(6.0, 2.5, half_diag / math.tan(math.radians(20)), 25_000, sec)
    # smaller angle to vertical (steeper) -> less inward pull and lower RSL
    assert steep.top_rail < shallow.top_rail
    assert steep.pad_eye < shallow.pad_eye


def test_floor_is_angle_independent():
    sec = SHS.from_mm(250, 12)
    half_diag = 0.5 * math.hypot(6.0, 2.5)
    a = utilization(6.0, 2.5, half_diag / math.tan(math.radians(50)), 25_000, sec)
    b = utilization(6.0, 2.5, half_diag / math.tan(math.radians(30)), 25_000, sec)
    assert a.floor == pytest.approx(b.floor)


def test_bigger_section_reduces_utilization():
    small = utilization(6.0, 2.5, BASE_LIFT, 25_000, SHS.from_mm(200, 10)).governing
    big = utilization(6.0, 2.5, BASE_LIFT, 25_000, SHS.from_mm(250, 12)).governing
    assert big < small


# --- citations -------------------------------------------------------------

def test_factor_citation_resolves_with_fixture(tmp_path):
    """With a valid wiki fixture, the citation resolves to the DNV factor value."""
    page = (tmp_path / "wikis" / "marine-engineering" / "wiki" / "standards"
            / "dnvgl-st-e271.md")
    page.parent.mkdir(parents=True)
    page.write_text(
        '---\ncode_id: dnvgl-st-e271\npublisher: DNV\nrevision: "2017"\n---\n# DNV 2.7-1\n')
    from digitalmodel.citations.registry import get_offshore_container_factor
    cv = get_offshore_container_factor("primary_structure_load", repo_root=tmp_path)
    assert cv.value == 2.5
    assert cv.citation.code_id == "dnvgl-st-e271"


def test_factor_citations_degrade_gracefully(tmp_path):
    """No resolvable wiki page -> empty dict + RuntimeWarning, never raises."""
    import digitalmodel.structural.offshore_container.dnv_2_7_1 as mod
    mod._CITATION_WARNED = False
    empty = tmp_path / "empty_clone"
    (empty / "wikis").mkdir(parents=True)  # valid clone, but no E271 page
    with pytest.warns(RuntimeWarning):
        out = factor_citations(repo_root=empty)
    assert out == {}
