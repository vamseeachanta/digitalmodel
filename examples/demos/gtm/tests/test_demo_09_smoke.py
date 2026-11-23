# ABOUTME: Smoke test for demo_09 CP retrofit & remaining-life — runs the demo end-to-end on a small set.
# ABOUTME: Asserts sane outputs (remaining_life >= 0, retrofit_mass >= 0, monotone trends, report + JSON written).
"""demo_09 smoke test.

Run with the shared-venv PYTHONPATH command (no uv rebuild):
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest examples/demos/gtm/tests/test_demo_09_smoke.py -q
"""
from __future__ import annotations

import json
from pathlib import Path

import pytest

import demo_09_cp_retrofit_remaining_life as demo


def test_single_case_sane_outputs():
    """A single retrofit case produces non-negative remaining life and retrofit mass."""
    res = demo.run_single_case(
        structure_key="4-leg jacket",
        elapsed_years=18.0,
        measured_potential_V=-0.95,
        coating_key="moderate",
    )
    assert res["mean_current_A"] > 0
    assert res["remaining_life_years"] >= 0
    assert res["additional_mass_kg"] >= 0
    assert res["additional_anodes_needed"] >= 0
    assert 0.0 <= res["depletion_percentage"] <= 100.0
    # Depletion profile is a non-empty time series.
    assert len(res["profile_years"]) > 1
    assert len(res["profile_years"]) == len(res["profile_remaining_mass_kg"])
    # DNV-RP-B401 is the governing standard.
    assert "B401" in res["standard"]


def test_remaining_life_decreases_with_age():
    """Older structure (more consumed) => less remaining anode life (same everything else)."""
    young = demo.run_single_case("4-leg jacket", 10.0, -0.95, "moderate")
    old = demo.run_single_case("4-leg jacket", 32.0, -0.95, "moderate")
    assert old["remaining_life_years"] <= young["remaining_life_years"]
    # More age also means more depletion.
    assert old["depletion_percentage"] >= young["depletion_percentage"]


def test_underprotected_potential_triggers_retrofit():
    """A measured potential above the protection threshold flags an urgent retrofit."""
    # -0.78 V is above the -0.80 V threshold => under-protected, even when young.
    under = demo.run_single_case("4-leg jacket", 10.0, -0.78, "moderate")
    assert under["is_retrofit_needed"] is True


def test_degraded_coating_raises_current_demand():
    """More coating breakdown => higher present mean current demand."""
    sound = demo.run_single_case("monopile", 18.0, -0.95, "sound")
    degraded = demo.run_single_case("monopile", 18.0, -0.95, "degraded")
    assert degraded["mean_current_A"] > sound["mean_current_A"]


def test_pipeline_end_to_end_writes_artifacts(tmp_path):
    """The full pipeline runs the sweep and writes a non-empty HTML report + JSON results."""
    report_path = tmp_path / "report.html"
    results_path = tmp_path / "results.json"

    all_results, out_report, out_results = demo.run_pipeline(
        from_cache=False,
        output_path=report_path,
        results_path=results_path,
    )

    # 3 structures x 4 ages x 4 potentials x 3 coatings = 144 cases.
    expected = (
        len(demo.STRUCTURES)
        * len(demo.ELAPSED_AGES)
        * len(demo.MEASURED_POTENTIALS)
        * len(demo.COATING_BREAKDOWN)
    )
    assert len(all_results) == expected == 144
    assert all("error" not in r for r in all_results)
    assert all(r["remaining_life_years"] >= 0 for r in all_results)
    assert all(r["additional_mass_kg"] >= 0 for r in all_results)

    # Report file written and is a real HTML document with the branded title + a chart.
    assert out_report == report_path
    assert report_path.is_file()
    html = report_path.read_text()
    assert html.startswith("<!DOCTYPE html>")
    assert "Retrofit" in html
    assert "chart-remaining_life_vs_age" in html
    assert "DNV-RP-B401" in html

    # JSON results round-trip with metadata.
    assert out_results == results_path
    data = json.loads(results_path.read_text())
    assert data["metadata"]["total_cases"] == 144
    assert data["metadata"]["backing_module"].startswith(
        "digitalmodel.cathodic_protection.marine_structure_cp"
    )
    assert len(data["results"]) == 144


def test_from_cache_rebuilds_report(tmp_path):
    """--from-cache reloads the JSON and rebuilds the report without re-running the sweep."""
    report_path = tmp_path / "report.html"
    results_path = tmp_path / "results.json"

    # First a real run to populate the cache.
    demo.run_pipeline(from_cache=False, output_path=report_path, results_path=results_path)
    report_path.unlink()
    assert not report_path.exists()

    # Now from cache — report is rebuilt, JSON untouched.
    all_results, out_report, _ = demo.run_pipeline(
        from_cache=True, output_path=report_path, results_path=results_path
    )
    assert len(all_results) == 144
    assert out_report.is_file()
