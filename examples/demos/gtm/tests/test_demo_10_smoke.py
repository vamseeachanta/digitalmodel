# ABOUTME: Smoke test for demo_10 CAD plate-buckling verification — runs the demo end-to-end on a small set.
# ABOUTME: Asserts sane outputs (sigma_cr > 0, usage computed, render + report + JSON written).
"""demo_10 smoke test.

Run with the shared-venv PYTHONPATH command (no uv rebuild):
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest examples/demos/gtm/tests/test_demo_10_smoke.py -q
"""
from __future__ import annotations

import json
from pathlib import Path

import demo_10_cad_plate_buckling_verification as demo
from digitalmodel.infrastructure.calculations.plate_buckling import PlateEdgeCondition


def _steel():
    return next(m for m in demo.MATERIALS if m["name"] == "Steel S355")


def test_single_case_sane_outputs():
    """A single plate case produces a positive sigma_cr and a computed usage."""
    res = demo.run_single_case(
        geometry_name="deck panel",
        a_mm=2400.0,
        b_mm=1200.0,
        t_mm=18.0,
        boundary_name="simply-supported",
        edge_condition=PlateEdgeCondition.SIMPLY_SUPPORTED,
        material=_steel(),
    )
    assert res["sigma_cr_Pa"] > 0
    assert res["sigma_cr_MPa"] > 0
    assert res["usage"] > 0
    assert res["verdict"] in ("PASS", "FAIL")
    # usage == sigma_applied / sigma_cr.
    assert abs(res["usage"] - res["sigma_applied_Pa"] / res["sigma_cr_Pa"]) < 1e-9
    assert res["slenderness_class"] in ("stocky", "normal", "slender")
    assert "C201" in res["standard"]


def test_sigma_cr_matches_closed_form():
    """sigma_cr equals the classical k pi^2 E /(12(1-nu^2)) (t/b)^2 formula."""
    import math

    mat = _steel()
    res = demo.run_single_case(
        "deck panel", 2400.0, 1200.0, 18.0,
        "simply-supported", PlateEdgeCondition.SIMPLY_SUPPORTED, mat,
    )
    t, b = 0.018, 1.2
    k = 4.0  # simply supported, longitudinal
    expected = k * math.pi**2 * mat["E"] / (12 * (1 - mat["nu"] ** 2)) * (t / b) ** 2
    assert abs(res["sigma_cr_Pa"] - expected) / expected < 1e-9


def test_clamped_capacity_exceeds_simply_supported():
    """Clamped edges (k=7) give a higher sigma_cr than simply supported (k=4)."""
    mat = _steel()
    ss = demo.run_single_case(
        "deck panel", 2400.0, 1200.0, 12.0,
        "simply-supported", PlateEdgeCondition.SIMPLY_SUPPORTED, mat,
    )
    cl = demo.run_single_case(
        "deck panel", 2400.0, 1200.0, 12.0,
        "clamped", PlateEdgeCondition.CLAMPED, mat,
    )
    assert cl["sigma_cr_Pa"] > ss["sigma_cr_Pa"]


def test_thicker_plate_higher_capacity():
    """sigma_cr grows with thickness (t/b)^2 for the same panel/material/boundary."""
    mat = _steel()
    thin = demo.run_single_case(
        "deck panel", 2400.0, 1200.0, 8.0,
        "simply-supported", PlateEdgeCondition.SIMPLY_SUPPORTED, mat,
    )
    thick = demo.run_single_case(
        "deck panel", 2400.0, 1200.0, 25.0,
        "simply-supported", PlateEdgeCondition.SIMPLY_SUPPORTED, mat,
    )
    assert thick["sigma_cr_Pa"] > thin["sigma_cr_Pa"]


def test_pipeline_end_to_end_writes_artifacts(tmp_path):
    """The full pipeline runs the sweep and writes a non-empty HTML report + JSON + render."""
    report_path = tmp_path / "report.html"
    results_path = tmp_path / "results.json"
    render_path = tmp_path / "render.png"

    all_results, out_report, out_results = demo.run_pipeline(
        from_cache=False,
        output_path=report_path,
        results_path=results_path,
        render_path=render_path,
    )

    # 4 geometries x 4 thicknesses x 2 boundaries x 3 materials = 96 cases.
    expected = (
        len(demo.GEOMETRIES_MM)
        * len(demo.THICKNESSES_MM)
        * len(demo.BOUNDARIES)
        * len(demo.MATERIALS)
    )
    assert len(all_results) == expected == 96
    assert all(r["sigma_cr_Pa"] > 0 for r in all_results)
    assert all(r["usage"] > 0 for r in all_results)
    assert all(r["verdict"] in ("PASS", "FAIL") for r in all_results)

    # Report file written and is a real HTML document with the branded title + a chart.
    assert out_report == report_path
    assert report_path.is_file()
    html = report_path.read_text()
    assert html.startswith("<!DOCTYPE html>")
    assert "Plate-Buckling" in html
    assert "chart-usage_vs_slenderness" in html
    assert "DNV-RP-C201" in html

    # Geometry render PNG written (matplotlib available in this env).
    assert render_path.is_file()
    assert render_path.stat().st_size > 0

    # JSON results round-trip with metadata.
    assert out_results == results_path
    data = json.loads(results_path.read_text())
    assert data["metadata"]["total_cases"] == 96
    assert data["metadata"]["backing_module"].endswith("plate_buckling")
    assert data["metadata"]["pass_count"] + data["metadata"]["fail_count"] == 96
    assert len(data["results"]) == 96


def test_from_cache_rebuilds_report(tmp_path):
    """--from-cache reloads the JSON and rebuilds the report without re-running the sweep."""
    report_path = tmp_path / "report.html"
    results_path = tmp_path / "results.json"
    render_path = tmp_path / "render.png"

    demo.run_pipeline(
        from_cache=False, output_path=report_path,
        results_path=results_path, render_path=render_path,
    )
    report_path.unlink()
    assert not report_path.exists()

    all_results, out_report, _ = demo.run_pipeline(
        from_cache=True, output_path=report_path,
        results_path=results_path, render_path=render_path,
    )
    assert len(all_results) == 96
    assert out_report.is_file()
