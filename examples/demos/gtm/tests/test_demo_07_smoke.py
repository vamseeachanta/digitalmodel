# ABOUTME: Smoke test for demo_07 subsea pipeline CP — runs the demo end to end on a small set.
# ABOUTME: Asserts sane bracelet-anode outputs (anode mass > 0, count >= 1, report written).
"""demo_07 subsea pipeline CP (bracelet-anode) smoke test.

Run with:
    PYTHONPATH=examples/demos/gtm:src \\
        .venv/bin/python -m pytest examples/demos/gtm/tests/test_demo_07_smoke.py -q
"""
from __future__ import annotations

import json
from pathlib import Path

import demo_07_pipeline_cp as demo


def test_single_case_sane_outputs():
    """A single bracelet-anode design case returns sane, positive outputs."""
    r = demo.run_single_case(
        diameter=demo.DIAMETERS[3],        # 12"
        coating=demo.COATINGS[0],          # FBE
        environment=demo.ENVIRONMENTS[0],  # exposed seawater
        resistivity_ohm_m=demo.RESISTIVITIES_OHM_M[1],  # 0.30 ohm-m
    )
    # Current demand chained through the iso_15589_2 module.
    assert r["current_density_mA_m2"] > 0
    assert r["i_mean_A"] > 0
    assert r["i_final_A"] > 0
    # Anode electricals: positive resistance and output.
    assert r["anode_resistance_ohm"] > 0
    assert r["anode_output_A"] > 0
    # Bracelet sizing: positive mass and at least one anode.
    assert r["anode_mass_kg"] > 0
    assert r["number_of_anodes"] >= 1
    assert r["anode_spacing_m"] > 0
    # Protected length is positive (DNV-RP-F103 envelope).
    assert r["protected_length_m"] > 0
    assert r["status"] in ("OK", "MARGINAL", "NO_FIT")


def test_sweep_full_cross_product():
    """The full sweep produces 6x3x3x3 = 162 cases, all with sane outputs."""
    results = demo.run_parametric_sweep()
    assert len(results) == (
        len(demo.DIAMETERS) * len(demo.COATINGS)
        * len(demo.ENVIRONMENTS) * len(demo.RESISTIVITIES_OHM_M)
    )
    assert len(results) == 162
    for r in results:
        assert r["anode_mass_kg"] > 0
        assert r["number_of_anodes"] >= 1
        assert r["i_mean_A"] > 0
        assert r["protected_length_m"] > 0


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
    assert "Bracelet" in html
    # Branded report scaffolding present.
    assert "Methodology" in html
    assert "Assumptions" in html

    # JSON cache written with metadata pointing at the backing module.
    assert results_path.is_file()
    cached = json.loads(results_path.read_text())
    assert cached["metadata"]["total_cases"] == len(results)
    assert cached["metadata"]["backing_module"] == (
        "digitalmodel.cathodic_protection.iso_15589_2"
    )

    # --from-cache reloads the same number of cases and re-writes the report.
    report2 = tmp_path / "report2.html"
    results2, _ = demo.run_demo(
        from_cache=True, output_path=report2, results_path=results_path,
    )
    assert len(results2) == len(results)
    assert report2.is_file()


def test_mass_monotonic_in_diameter():
    """Larger diameter on the same coating/environment raises the anode mass."""
    small = demo.run_single_case(
        demo.DIAMETERS[0], demo.COATINGS[0],
        demo.ENVIRONMENTS[0], demo.RESISTIVITIES_OHM_M[1],
    )
    large = demo.run_single_case(
        demo.DIAMETERS[5], demo.COATINGS[0],
        demo.ENVIRONMENTS[0], demo.RESISTIVITIES_OHM_M[1],
    )
    assert large["anode_mass_kg"] > small["anode_mass_kg"]
