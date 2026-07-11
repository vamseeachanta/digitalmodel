#!/usr/bin/env python3
"""ABOUTME: Tests for the OpenFOAM sloshing HTML engineering report layer (#1528).

Fixture-first: sibling sweep/extraction modules are not on origin/main, so these
tests define small synthetic, de-identified fixtures shaped like the real
``sloshing_sweep.SweepCase`` / ``time_history.SynchronizedTimeHistory`` outputs
and render the report from those plain shapes.
"""

from __future__ import annotations

import re
from html.parser import HTMLParser

import pytest

from digitalmodel.solvers.openfoam.report import (
    render_aggregate_report,
    render_case_report,
    REQUIRED_SECTIONS,
)


# ============================================================================
# Fixtures shaped like the real sibling schemas
# ============================================================================


def _channel(values, units, source_field, provenance="synthetic"):
    """Build a channel dict mirroring SynchronizedTimeHistory channel shape."""
    return {
        "values": list(values),
        "units": units,
        "source_field": source_field,
        "provenance": provenance,
    }


def make_history(
    case_id="case_00",
    *,
    mass_balance_ok=True,
    dry_out=False,
    overflow=False,
    impact=False,
    mass_balance_residual=1.0e-4,
):
    """Synthetic SynchronizedTimeHistory-like mapping (de-identified)."""
    t = [0.0, 0.5, 1.0, 1.5, 2.0]
    return {
        "case_id": case_id,
        "time": t,
        "channels": {
            "free_surface_elevation": _channel(
                [0.0, 0.12, 0.31, 0.18, 0.05], "m", "alpha.water"
            ),
            "elevation_max": _channel(
                [0.0, 0.12, 0.31, 0.31, 0.31], "m", "alpha.water"
            ),
            "wall_pressure": _channel(
                [1000.0, 1400.0, 2200.0, 1600.0, 1100.0], "Pa", "p_rgh"
            ),
            "liquid_mass": _channel(
                [500.0, 500.1, 499.9, 500.0, 500.05], "kg", "alpha.water"
            ),
            "com_x": _channel([0.0, 0.02, 0.05, 0.03, 0.01], "m", "alpha.water"),
            "com_y": _channel([0.0, 0.0, 0.0, 0.0, 0.0], "m", "alpha.water"),
            "com_z": _channel([0.4, 0.41, 0.44, 0.42, 0.40], "m", "alpha.water"),
            "inlet_flow_rate": _channel(
                [0.0, 1.2, 2.4, 1.1, 0.2], "m^3/s", "phi"
            ),
            "outlet_flow_rate": _channel(
                [0.0, 0.1, 0.9, 1.5, 0.4], "m^3/s", "phi"
            ),
            "roll_angle": _channel([0.0, 5.0, 0.0, -5.0, 0.0], "deg", "prescribed"),
        },
        "validation": {
            "synchronized_time": t,
            "mass_balance_residual": mass_balance_residual,
            "mass_balance_ok": mass_balance_ok,
            "dry_out": dry_out,
            "overflow": overflow,
            "impact": impact,
        },
        "metadata": {"solver": "gravity_conduit_surrogate", "n_samples": len(t)},
    }


def make_case(
    case_id="case_00",
    *,
    fill_fraction=0.5,
    period_ratio=1.0,
    near_resonance=True,
):
    """Synthetic SweepCase-like mapping."""
    natural_period = 2.0
    return {
        "case_id": case_id,
        "fill_fraction": fill_fraction,
        "fill_depth": fill_fraction * 1.0,
        "conduit_capacity": 3.5,
        "period_ratio": period_ratio,
        "natural_frequency_hz": 1.0 / natural_period,
        "natural_period_s": natural_period,
        "roll_frequency_hz": 1.0 / (natural_period * period_ratio),
        "roll_period_s": natural_period * period_ratio,
        "near_resonance": near_resonance,
    }


def make_manifest():
    """Synthetic manifest over a fill x period_ratio sweep."""
    fills = [0.25, 0.5, 0.75]
    ratios = [0.8, 1.0, 1.2]
    cases = []
    i = 0
    for f in fills:
        for r in ratios:
            cases.append(
                make_case(
                    case_id=f"case_{i:02d}",
                    fill_fraction=f,
                    period_ratio=r,
                    near_resonance=abs(r - 1.0) < 0.15,
                )
            )
            i += 1
    return {
        "schema_version": "1.0.0",
        "study_name": "Partial-Fill Sloshing Screening",
        "provenance": "/mnt/local-analysis/secret-client/run_2026/manifest.json",
        "cases": cases,
        "content_hash": "abc123def456",
    }


def make_histories_for(manifest, invalid_case_id="case_04"):
    """One history per case; one is flagged invalid (mass-balance fail + impact)."""
    out = {}
    for case in manifest["cases"]:
        cid = case["case_id"]
        if cid == invalid_case_id:
            out[cid] = make_history(
                case_id=cid,
                mass_balance_ok=False,
                impact=True,
                mass_balance_residual=0.42,
            )
        else:
            out[cid] = make_history(case_id=cid)
    return out


# ============================================================================
# Helpers
# ============================================================================


class _Parseable(HTMLParser):
    def error(self, message):  # pragma: no cover - defensive
        raise ValueError(message)


def _assert_parses(html: str) -> None:
    parser = _Parseable()
    parser.feed(html)
    parser.close()


def _assert_no_external_refs(html: str) -> None:
    low = html.lower()
    assert "http://" not in low, "external http reference present"
    assert "https://" not in low, "external https reference present"
    assert "<link" not in low, "external stylesheet <link> present"
    assert "cdn" not in low, "CDN reference present"
    assert "src=" not in low or 'src="data:' in low, "non-data src reference"


def _assert_deidentified(html: str) -> None:
    assert "secret-client" not in html
    assert "/mnt/" not in html
    assert "/home/" not in html
    assert not re.search(r"[A-Za-z]:\\", html), "windows absolute path leaked"


# ============================================================================
# Schema / API tests
# ============================================================================


def test_required_sections_constant():
    assert REQUIRED_SECTIONS == (
        "Inputs",
        "Methodology",
        "Visualizations",
        "Validation",
        "Outputs",
        "Limitations",
    )


def test_case_report_has_all_required_sections():
    html = render_case_report(make_history(), make_case())
    for section in REQUIRED_SECTIONS:
        assert f'id="{section.lower()}"' in html, f"missing section id {section}"
        assert section in html, f"missing section heading {section}"


def test_aggregate_report_has_all_required_sections():
    manifest = make_manifest()
    html = render_aggregate_report(manifest, make_histories_for(manifest))
    for section in REQUIRED_SECTIONS:
        assert f'id="{section.lower()}"' in html, f"missing section id {section}"
        assert section in html, f"missing section heading {section}"


# ============================================================================
# Case-report render tests
# ============================================================================


def test_case_report_is_valid_self_contained_html():
    html = render_case_report(make_history(), make_case())
    assert html.lstrip().lower().startswith("<!doctype html>")
    assert "</html>" in html
    _assert_parses(html)
    _assert_no_external_refs(html)
    _assert_deidentified(html)


def test_case_report_embeds_all_required_plots():
    html = render_case_report(make_history(), make_case())
    assert html.count("<svg") >= 5
    for title in (
        "Free-Surface Elevation",
        "Wall Pressure",
        "Liquid Mass",
        "Center of Mass",
        "Inlet / Outlet Flow",
    ):
        assert title in html, f"missing plot: {title}"


def test_case_report_shows_validation_metrics():
    html = render_case_report(make_history(), make_case())
    assert "mass_balance" in html.lower() or "mass balance" in html.lower()
    # a passing case should be marked valid
    assert "PASS" in html or "valid" in html.lower()


def test_case_report_flags_invalid_case():
    bad = make_history(case_id="case_bad", mass_balance_ok=False, impact=True)
    html = render_case_report(bad, make_case(case_id="case_bad"))
    low = html.lower()
    assert "invalid" in low or "fail" in low or "flag" in low


def test_case_report_missing_channels_raises():
    hist = make_history()
    del hist["channels"]["wall_pressure"]
    with pytest.raises(ValueError):
        render_case_report(hist, make_case())


# ============================================================================
# Aggregate-report render tests
# ============================================================================


def test_aggregate_report_is_valid_self_contained_html():
    manifest = make_manifest()
    html = render_aggregate_report(manifest, make_histories_for(manifest))
    assert html.lstrip().lower().startswith("<!doctype html>")
    assert "</html>" in html
    _assert_parses(html)
    _assert_no_external_refs(html)
    _assert_deidentified(html)


def test_aggregate_report_embeds_heatmaps():
    manifest = make_manifest()
    html = render_aggregate_report(manifest, make_histories_for(manifest))
    assert 'class="heatmap"' in html
    # heatmaps span both sweep axes
    assert "Fill Fraction" in html
    assert "Period Ratio" in html or "T_roll / T_natural" in html


def test_aggregate_report_lists_every_case_none_omitted():
    manifest = make_manifest()
    html = render_aggregate_report(manifest, make_histories_for(manifest))
    for case in manifest["cases"]:
        assert case["case_id"] in html, f"case omitted: {case['case_id']}"


def test_aggregate_report_flags_invalid_cases_visibly():
    manifest = make_manifest()
    histories = make_histories_for(manifest, invalid_case_id="case_04")
    html = render_aggregate_report(manifest, histories)
    # invalid case is present AND flagged, not silently dropped
    assert "case_04" in html
    assert "INVALID" in html or 'class="invalid"' in html
    # count of invalid cases surfaced
    assert re.search(r"1\s*(invalid|flagged|failed)", html, re.IGNORECASE)


def test_aggregate_report_without_histories_still_renders():
    manifest = make_manifest()
    html = render_aggregate_report(manifest)
    for case in manifest["cases"]:
        assert case["case_id"] in html
    _assert_no_external_refs(html)


def test_aggregate_report_empty_cases_raises():
    manifest = make_manifest()
    manifest["cases"] = []
    with pytest.raises(ValueError):
        render_aggregate_report(manifest)
