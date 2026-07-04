# ABOUTME: Smoke test for demo_08 ICCP design — runs the demo end to end on a small set.
# ABOUTME: Asserts sane sizing outputs (rectifier rating > 0, anode count >= 1, report written).
"""demo_08 ICCP design smoke test.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest examples/demos/gtm/tests/test_demo_08_smoke.py -q
"""
from __future__ import annotations

import json
from pathlib import Path

import demo_08_iccp_design as demo


def test_single_case_sane_outputs():
    """A single ICCP design case returns sane, positive sizing outputs."""
    r = demo.run_single_case(
        structure=demo.STRUCTURES[0],          # small jacket
        current_density=demo.CURRENT_DENSITIES[1],  # aged/final demand
        anode=demo.ANODE_OPTIONS[0],           # MMO distributed
        cable_length_m=demo.CABLE_LENGTHS_M[0],
    )
    # Demand chained through the iccp_design module.
    assert r["total_current_A"] > 0
    # Rectifier sizing: positive DC output and a standard rating that covers it.
    assert r["rect_dc_voltage_V"] > 0
    assert r["rect_rating_V"] > 0
    assert r["rect_rating_A"] > 0
    assert r["rect_power_W"] > 0
    assert r["rect_rating_V"] >= r["rect_dc_voltage_V"]
    # Ground-bed: at least one anode and a positive resistance.
    assert r["number_of_anodes"] >= 1
    assert r["bed_resistance_ohm"] > 0
    assert r["anode_life_years"] > 0
    # Cable sized within the voltage-drop limit.
    assert r["cable_area_mm2"] > 0
    assert r["cable_voltage_drop_V"] <= demo.MAX_VOLTAGE_DROP_V + 1e-6
    assert r["status"] in ("OK", "MARGINAL", "NO_FIT")


def test_sweep_full_cross_product():
    """The full sweep produces the expected 4x3x3x4 = 144 cases, all with sane outputs."""
    results = demo.run_parametric_sweep()
    assert len(results) == (
        len(demo.STRUCTURES) * len(demo.CURRENT_DENSITIES)
        * len(demo.ANODE_OPTIONS) * len(demo.CABLE_LENGTHS_M)
    )
    assert len(results) == 144
    for r in results:
        assert r["rect_rating_A"] > 0
        assert r["number_of_anodes"] >= 1
        assert r["bed_resistance_ohm"] > 0


def test_run_demo_end_to_end_writes_report(tmp_path):
    """run_demo writes a branded HTML report and a JSON cache; --from-cache reloads it."""
    report_path = tmp_path / "report.html"
    results_path = tmp_path / "results.json"

    results, written = demo.run_demo(
        from_cache=False, output_path=report_path, results_path=results_path,
    )
    # Report file written and non-trivial.
    assert written == report_path
    assert report_path.is_file()
    html = report_path.read_text()
    assert len(html) > 2000
    assert "ICCP" in html
    # Branded report scaffolding present.
    assert "Methodology" in html
    assert "Assumptions" in html

    # JSON cache written with metadata pointing at the backing module.
    assert results_path.is_file()
    cached = json.loads(results_path.read_text())
    assert cached["metadata"]["total_cases"] == len(results)
    assert cached["metadata"]["backing_module"] == (
        "digitalmodel.cathodic_protection.iccp_design"
    )

    # --from-cache reloads the same number of cases and re-writes the report.
    report2 = tmp_path / "report2.html"
    results2, _ = demo.run_demo(
        from_cache=True, output_path=report2, results_path=results_path,
    )
    assert len(results2) == len(results)
    assert report2.is_file()


def test_demand_monotonic_in_current():
    """Higher current-density demand on the same structure raises the total current."""
    low = demo.run_single_case(
        demo.STRUCTURES[2], demo.CURRENT_DENSITIES[0],
        demo.ANODE_OPTIONS[0], demo.CABLE_LENGTHS_M[0],
    )
    high = demo.run_single_case(
        demo.STRUCTURES[2], demo.CURRENT_DENSITIES[2],
        demo.ANODE_OPTIONS[0], demo.CABLE_LENGTHS_M[0],
    )
    assert high["total_current_A"] > low["total_current_A"]
