#!/usr/bin/env python3
# ABOUTME: Tests for the riser report-template generator (deliverable of record, #814).
"""
Deterministic tests for digitalmodel.subsea.catenary_riser.report.

No network, no solver, no live analysis -- the generator consumes a provided
sample results structure and writes branded HTML + markdown reports.
"""

from __future__ import annotations

from dataclasses import asdict

import pytest

from digitalmodel.subsea.catenary_riser.report import (
    CodeCheck,
    FatigueResult,
    Provenance,
    RiserReportData,
    SECTION_ORDER,
    StrengthResult,
    generate_riser_report,
    render_html,
    render_markdown,
    sample_riser_report_data,
)


# ---------------------------------------------------------------------------
# Sample structure
# ---------------------------------------------------------------------------

def test_sample_data_shape():
    d = sample_riser_report_data()
    assert isinstance(d, RiserReportData)
    assert d.strength_results and d.fatigue_results and d.code_checks
    # Roll-ups computed from the sample numbers.
    assert d.max_strength_utilisation == pytest.approx(0.82)
    assert d.min_fatigue_life_years == pytest.approx(41.0)
    assert d.overall_status in {"PASS", "MARGINAL", "FAIL"}


def test_status_classification():
    # FAIL: utilisation over allowable.
    assert StrengthResult("x", 0, 1, 1, 1, 1.05).status == "FAIL"
    # MARGINAL: between 0.9 and 1.0.
    assert StrengthResult("x", 0, 1, 1, 1, 0.95).status == "MARGINAL"
    # PASS: comfortably below.
    assert StrengthResult("x", 0, 1, 1, 1, 0.50).status == "PASS"
    # Fatigue: life < design life -> FAIL.
    assert FatigueResult("x", 0, 1.0 / 10.0, 25.0).status == "FAIL"
    # Fatigue: life >> design life -> PASS.
    assert FatigueResult("x", 0, 1.0 / 100.0, 25.0).status == "PASS"


# ---------------------------------------------------------------------------
# HTML rendering
# ---------------------------------------------------------------------------

def test_html_contains_all_sections_and_numbers(tmp_path):
    d = sample_riser_report_data()
    out = generate_riser_report(d, tmp_path / "riser_report.html")

    assert out["html"].exists()
    assert out["markdown"].exists()

    html = out["html"].read_text(encoding="utf-8")

    # Every standard section title is present.
    for section in SECTION_ORDER:
        assert section in html, f"missing section: {section}"

    # Branding + deliverable framing.
    assert "digitalmodel" in html
    assert "Riser Analysis Report" in html
    assert d.riser_name in html

    # Key numbers from the sample appear in the report.
    assert "0.82" in html                 # governing strength utilisation
    assert "41.0" in html                 # min fatigue life
    assert "1840.0" in html               # hang-off effective tension

    # Code citations / standards.
    assert "DNV-OS-F201" in html
    assert "DNV-RP-C203" in html
    assert "API RP 1111" in html

    # Atlas (screening) vs custom (certified) labelling + provenance stamp.
    assert "SCREENING (atlas)" in html
    assert "riser-strength-fatigue-screening v0.3.0" in html


def test_html_accepts_plain_dict():
    d = sample_riser_report_data()
    payload = asdict(d)  # plain nested dict, no dataclasses
    html = render_html(payload)
    assert "Riser Analysis Report" in html
    assert "0.82" in html


def test_custom_mode_label():
    d = sample_riser_report_data()
    d.provenance.mode = "custom"
    html = render_html(d)
    assert "CERTIFIED (custom)" in html
    assert "SCREENING (atlas)" not in html


# ---------------------------------------------------------------------------
# Markdown rendering
# ---------------------------------------------------------------------------

def test_markdown_has_sections_and_tables():
    md = render_markdown(sample_riser_report_data())
    assert "# SCR-12in-WI — Riser Analysis Report" in md
    for header in (
        "## Executive Summary",
        "## Inputs & Assumptions",
        "## Method & Standards",
        "## Strength / Utilisation Summary",
        "## Fatigue Summary",
        "## Code Checks",
        "## Critical-Location Findings",
        "## Conclusions",
        "## Provenance",
    ):
        assert header in md
    # Table rows rendered.
    assert "| Touchdown point |" in md
    assert "0.82" in md


# ---------------------------------------------------------------------------
# Edge / empty case
# ---------------------------------------------------------------------------

def test_empty_results_handled(tmp_path):
    d = RiserReportData(
        project="Empty",
        riser_name="EMPTY-RISER",
        riser_type="Steel Catenary Riser (SCR)",
        water_depth_m=1000.0,
        provenance=Provenance(mode="custom"),
    )
    # Roll-ups degrade gracefully.
    assert d.max_strength_utilisation is None
    assert d.min_fatigue_life_years is None
    assert d.overall_status == "N/A"

    out = generate_riser_report(d, tmp_path / "empty.html")
    html = out["html"].read_text(encoding="utf-8")

    # Still a complete, branded report with all section headers.
    for section in SECTION_ORDER:
        assert section in html
    assert "No strength results provided." in html
    assert "No fatigue results provided." in html
    assert "No code checks provided." in html
    assert "CERTIFIED (custom)" in html

    # Markdown twin too.
    md = out["markdown"].read_text(encoding="utf-8")
    assert "_No strength results provided._" in md


def test_zero_damage_fatigue_is_infinite_life():
    r = FatigueResult("benign", 0.0, 0.0, 25.0)
    assert r.fatigue_life_years is None
    assert r.status == "PASS"
    d = RiserReportData(
        project="P", riser_name="R", riser_type="SCR", water_depth_m=500.0,
        fatigue_results=[r],
    )
    html = render_html(d)
    assert "&infin;" in html  # infinite life rendered in HTML table
    md = render_markdown(d)
    assert "| inf |" in md
