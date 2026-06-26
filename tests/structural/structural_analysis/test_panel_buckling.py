# ABOUTME: Tests for the PRELIMINARY stiffened-panel buckling solver
# ABOUTME: (panel_buckling.py): effective section, governing mode, monotonicity.
"""Tests for digitalmodel.structural.structural_analysis.panel_buckling.

These assert *relationships and bounds* (areas summing, positivity, monotonic
trends, pass/fail logic) rather than unvalidated magic magnitudes, consistent
with the PRELIMINARY status of the solver under test.
"""

import pytest

from digitalmodel.structural.structural_analysis.models import (
    STEEL_AH36,
    STEEL_GRADE_A,
)
from digitalmodel.structural.structural_analysis.panel_buckling import (
    PRELIMINARY,
    StiffenerGeometry,
    StiffenedPanelGeometry,
    PanelBucklingResult,
    StiffenedPanelBucklingAnalyzer,
)


# --- Representative ship panel fixtures ------------------------------------
def make_tee_panel():
    """800mm spacing x 12mm plate, tee web 250x10, flange 90x12, span 2400mm."""
    stiff = StiffenerGeometry(
        web_height=250.0,
        web_thickness=10.0,
        flange_width=90.0,
        flange_thickness=12.0,
        spacing=800.0,
        section_type="tee",
    )
    return StiffenedPanelGeometry(
        plate_length=2400.0, plate_thickness=12.0, stiffener=stiff
    )


def make_flatbar_panel():
    """Same plate/spacing/span but a flat-bar (web only, no flange)."""
    stiff = StiffenerGeometry(
        web_height=250.0,
        web_thickness=10.0,
        flange_width=0.0,
        flange_thickness=0.0,
        spacing=800.0,
        section_type="flatbar",
    )
    return StiffenedPanelGeometry(
        plate_length=2400.0, plate_thickness=12.0, stiffener=stiff
    )


@pytest.fixture
def analyzer():
    return StiffenedPanelBucklingAnalyzer(STEEL_AH36)


# --- Honesty flag ----------------------------------------------------------
def test_preliminary_flag_is_true():
    assert PRELIMINARY is True


# --- effective_section -----------------------------------------------------
def test_effective_section_areas_sum(analyzer):
    panel = make_tee_panel()
    sec = analyzer.effective_section(panel)
    # Component areas computed independently.
    assert sec["area_plate"] == pytest.approx(800.0 * 12.0)
    assert sec["area_web"] == pytest.approx(250.0 * 10.0)
    assert sec["area_flange"] == pytest.approx(90.0 * 12.0)
    assert sec["area_total"] == pytest.approx(
        sec["area_plate"] + sec["area_web"] + sec["area_flange"]
    )


def test_effective_section_I_and_r_positive(analyzer):
    panel = make_tee_panel()
    sec = analyzer.effective_section(panel)
    assert sec["I"] > 0
    assert sec["r"] > 0
    # r = sqrt(I / A)
    assert sec["r"] == pytest.approx((sec["I"] / sec["area_total"]) ** 0.5)


def test_effective_plate_width_is_full_spacing(analyzer):
    panel = make_tee_panel()
    sec = analyzer.effective_section(panel)
    assert sec["effective_plate_width"] == pytest.approx(800.0)


def test_neutral_axis_between_plate_and_flange(analyzer):
    panel = make_tee_panel()
    sec = analyzer.effective_section(panel)
    # NA must lie above plate mid-thickness and below the flange top.
    flange_top = 12.0 / 2.0 + 250.0 + 12.0
    assert 0.0 < sec["neutral_axis"] < flange_top


def test_tee_has_larger_I_than_flatbar(analyzer):
    tee = analyzer.effective_section(make_tee_panel())
    flat = analyzer.effective_section(make_flatbar_panel())
    # Adding a flange raises both area and I.
    assert tee["area_total"] > flat["area_total"]
    assert tee["I"] > flat["I"]


def test_flatbar_has_no_flange_area(analyzer):
    sec = analyzer.effective_section(make_flatbar_panel())
    assert sec["area_flange"] == pytest.approx(0.0)


# --- check_panel: representative AH36 panel --------------------------------
def test_check_panel_representative(analyzer):
    panel = make_tee_panel()
    res = analyzer.check_panel(panel, sigma_x=120.0)
    assert isinstance(res, PanelBucklingResult)
    assert res.utilization > 0
    assert res.governing_mode in ("plate_induced", "column")
    # Governing utilization is the max of the two component utilizations.
    assert res.utilization == pytest.approx(
        max(res.plate_utilization, res.column_utilization)
    )
    assert res.critical_stress > 0


def test_check_panel_details_populated(analyzer):
    panel = make_tee_panel()
    res = analyzer.check_panel(panel, sigma_x=120.0)
    assert res.details["preliminary"] is True
    assert res.details["stiffener_tripping_modelled"] is False
    assert "plate_result" in res.details
    assert "column_result" in res.details
    assert "section" in res.details


def test_stiffener_utilization_is_zero_placeholder(analyzer):
    # Tripping not modelled -> reported as 0.0 (honest placeholder).
    res = analyzer.check_panel(make_tee_panel(), sigma_x=120.0)
    assert res.stiffener_utilization == pytest.approx(0.0)


# --- Monotonicity ----------------------------------------------------------
def test_thicker_plate_lowers_plate_utilization(analyzer):
    base = make_tee_panel()
    thick = make_tee_panel()
    thick.plate_thickness = 20.0
    u_thin = analyzer.plate_induced(base, sigma_x=120.0).utilization
    u_thick = analyzer.plate_induced(thick, sigma_x=120.0).utilization
    assert u_thick < u_thin


def test_higher_stress_raises_utilization(analyzer):
    panel = make_tee_panel()
    u_low = analyzer.check_panel(panel, sigma_x=60.0).utilization
    u_high = analyzer.check_panel(panel, sigma_x=180.0).utilization
    assert u_high > u_low


def test_longer_span_raises_column_utilization(analyzer):
    short = make_tee_panel()
    longp = make_tee_panel()
    longp.plate_length = 4800.0
    u_short = analyzer.column_buckling(short, sigma_x=120.0).utilization
    u_long = analyzer.column_buckling(longp, sigma_x=120.0).utilization
    assert u_long > u_short


# --- Boundary cases --------------------------------------------------------
def test_zero_stress_passes(analyzer):
    res = analyzer.check_panel(make_tee_panel(), sigma_x=0.0)
    assert res.utilization == pytest.approx(0.0)
    assert bool(res.passes) is True


def test_overloaded_panel_fails(analyzer):
    # Very thin plate + very high stress -> must fail.
    stiff = StiffenerGeometry(
        web_height=150.0,
        web_thickness=8.0,
        flange_width=60.0,
        flange_thickness=8.0,
        spacing=800.0,
        section_type="tee",
    )
    panel = StiffenedPanelGeometry(
        plate_length=3000.0, plate_thickness=5.0, stiffener=stiff
    )
    res = analyzer.check_panel(panel, sigma_x=320.0)
    assert bool(res.passes) is False
    assert res.utilization > 1.0


def test_lower_grade_steel_higher_utilization():
    # Lower yield (Grade A) should not pass more easily than AH36.
    panel = make_tee_panel()
    u_a = StiffenedPanelBucklingAnalyzer(STEEL_GRADE_A).check_panel(
        panel, sigma_x=150.0
    ).utilization
    u_ah36 = StiffenedPanelBucklingAnalyzer(STEEL_AH36).check_panel(
        panel, sigma_x=150.0
    ).utilization
    assert u_a >= u_ah36


# --- Section type validation ----------------------------------------------
def test_invalid_section_type_raises():
    with pytest.raises(ValueError):
        StiffenerGeometry(
            web_height=250.0,
            web_thickness=10.0,
            flange_width=90.0,
            flange_thickness=12.0,
            spacing=800.0,
            section_type="ibeam",
        )


def test_flatbar_zeroes_flange_dims():
    stiff = StiffenerGeometry(
        web_height=250.0,
        web_thickness=10.0,
        flange_width=90.0,   # should be zeroed
        flange_thickness=12.0,
        spacing=800.0,
        section_type="flatbar",
    )
    assert stiff.flange_width == 0.0
    assert stiff.flange_thickness == 0.0
