# ABOUTME: Smoke test for demo_06 sacrificial-anode CP — runs the demo end-to-end on a small set.
# ABOUTME: Asserts sane outputs (anode mass > 0, monotone trends, report + JSON written).
"""demo_06 smoke test.

Run with the shared-venv PYTHONPATH command (no uv rebuild):
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest examples/demos/gtm/tests/test_demo_06_smoke.py -q
"""
from __future__ import annotations

import json
from pathlib import Path

import pytest

import demo_06_marine_structure_cp as demo
from digitalmodel.cathodic_protection.marine_structure_cp import ClimateRegion


def test_single_case_sane_outputs():
    """A single CP case produces strictly positive current demand and anode mass."""
    res = demo.run_single_case(
        structure_key="4-leg jacket",
        climate=ClimateRegion.TEMPERATE,
        design_life=25.0,
        coating_profile="bare",
    )
    assert res["total_mean_current_A"] > 0
    assert res["total_anode_mass_kg"] > 0
    assert res["number_of_anodes"] >= 1
    # Anode distribution covers the demand-carrying zones and sums to the count.
    assert sum(res["anode_distribution"].values()) == res["number_of_anodes"]
    # DNV-RP-B401 is the governing standard.
    assert "B401" in res["standard"]


def test_anode_mass_increases_with_design_life():
    """Longer design life => more anode mass (same structure/climate/coating)."""
    short = demo.run_single_case("monopile", ClimateRegion.TEMPERATE, 15.0, "bare")
    long = demo.run_single_case("monopile", ClimateRegion.TEMPERATE, 40.0, "bare")
    assert long["total_anode_mass_kg"] > short["total_anode_mass_kg"]


def test_arctic_demand_exceeds_tropical():
    """Colder climate => higher current density => higher mean current (Table 10-1)."""
    tropical = demo.run_single_case("4-leg jacket", ClimateRegion.TROPICAL, 25.0, "bare")
    arctic = demo.run_single_case("4-leg jacket", ClimateRegion.ARCTIC, 25.0, "bare")
    assert arctic["total_mean_current_A"] > tropical["total_mean_current_A"]


def test_pipeline_end_to_end_writes_artifacts(tmp_path):
    """The full pipeline runs the sweep and writes a non-empty HTML report + JSON results."""
    report_path = tmp_path / "report.html"
    results_path = tmp_path / "results.json"

    all_results, out_report, out_results = demo.run_pipeline(
        from_cache=False,
        output_path=report_path,
        results_path=results_path,
    )

    # 3 structures x 4 climates x 3 lives x 4 coatings = 144 cases.
    expected = (
        len(demo.STRUCTURES)
        * len(demo.CLIMATE_REGIONS)
        * len(demo.DESIGN_LIVES)
        * len(demo.COATING_PROFILES)
    )
    assert len(all_results) == expected == 144
    assert all("error" not in r for r in all_results)
    assert all(r["total_anode_mass_kg"] > 0 for r in all_results)

    # Report file written and is a real HTML document with the branded title + a chart.
    assert out_report == report_path
    assert report_path.is_file()
    html = report_path.read_text()
    assert html.startswith("<!DOCTYPE html>")
    assert "Cathodic Protection" in html
    assert "chart-zone_heatmap" in html
    assert "DNV-RP-B401" in html

    # JSON results round-trip with metadata.
    assert out_results == results_path
    data = json.loads(results_path.read_text())
    assert data["metadata"]["total_cases"] == 144
    assert data["metadata"]["backing_module"].endswith("marine_structure_cp")
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
