"""Tests for the vessel suitability report generator (#883)."""

from __future__ import annotations

from digitalmodel.marine_ops.installation.suitability_report import (
    build_report,
    to_html,
    to_markdown,
)
from digitalmodel.marine_ops.installation.vessel_suitability import LiftRequirement


def _report(weight=4000.0, radius=40.0, **kw):
    return build_report(
        LiftRequirement(weight_te=weight, radius_m=radius, **kw), title="Test screening"
    )


def test_report_structure_and_summary():
    rep = _report()
    assert rep.rows, "no vessels in report"
    assert rep.title == "Test screening"
    # summary counts are consistent
    assert rep.n_defensible == sum(1 for r in rep.rows if r.defensible)
    assert sum(rep.confidence_mix.values()) == len(rep.rows)
    assert sum(rep.source_mix.values()) == len(rep.rows)


def test_rows_ranked_descending():
    rep = _report()
    scores = [r.score for r in rep.rows]
    assert scores == sorted(scores, reverse=True)


def test_markdown_contains_table_and_provenance():
    rep = _report()
    md = to_markdown(rep, limit=5)
    assert "| # | Vessel | Score |" in md
    assert "defensible" in md.lower()
    assert "public-sourced" in md  # provenance note present
    # top-ranked vessel name appears
    assert rep.rows[0].vessel.split()[0] in md


def test_html_renders_badges_and_rows():
    rep = _report()
    html = to_html(rep, limit=5)
    assert html.startswith("<!doctype html>")
    assert "<table>" in html and "</table>" in html
    assert "border-radius" in html  # confidence badge styling
    assert "NOT a substitute" in html  # provenance disclaimer
    # confidence tier label of the top row shows up as a badge
    assert rep.rows[0].confidence.value in html


def test_limit_caps_rows():
    rep = _report()
    md_full = to_markdown(rep)
    md_small = to_markdown(rep, limit=3)
    assert md_full.count("\n|") > md_small.count("\n|")


def test_dp_requirement_in_header():
    rep = _report(min_dp_class=3)
    assert "min DP3" in to_markdown(rep)
