# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Report checks use independent tabulated response values, not a solver oracle."""

import numpy as np
import pytest

from digitalmodel.hydrodynamics.passing_ship.analysis import AnalysisResult
from digitalmodel.hydrodynamics.passing_ship.response_report import response_sections


def test_response_sections_use_actual_envelopes_and_motion_samples():
    """Tension input [10000,12000] N gives that same envelope in N."""
    case = AnalysisResult(
        name="case<script>",
        time=np.array([0, 1]),
        forces=None,
        motions=np.array([[0, 0, 0], [1, 2, 0.1]]),
        line_tensions_n={"line<script>": np.array([10000, 12000])},
        fender_loads_n={"fender": np.array([0, 500])},
    )
    html = response_sections([case])
    assert "Tension envelopes" in html
    assert "Motion time histories" in html
    assert "10000" in html and "12000" in html
    assert "case&lt;script&gt;" in html
    assert "line&lt;script&gt;" in html
    assert "case<script>" not in html
    assert "line<script>" not in html
    assert "Yaw (mrad)" in html
    assert "Revision history" in html
    assert "Fender loads" in html
    assert "Plotly.newPlot" in html
    assert "[0, 1]" in html
    assert "Dynamic" in html


def test_unrequested_quantities_are_labelled():
    case = AnalysisResult("forces only", np.array([0]), None, None, None, None)
    html = response_sections([case])
    assert "Not requested" in html
    assert "not supplied" in response_sections([])


def test_benchmark_report_accepts_computed_response(monkeypatch):
    """Check the actual report integration while avoiding repeat force quadrature."""
    from digitalmodel.hydrodynamics.passing_ship import benchmark_report

    case = AnalysisResult(
        "computed response",
        np.array([0]),
        None,
        np.zeros((1, 3)),
        {"line": np.array([10000])},
        None,
    )
    monkeypatch.setattr(
        benchmark_report, "_run_sweep", lambda calc: ([0], [0], [1], [0])
    )
    monkeypatch.setattr(
        benchmark_report,
        "_run_checks",
        lambda calc: {
            "abeam": {"surge": 0, "sway": 1, "yaw": 0},
            "vel_ratio": 4,
            "sep_drop": 50,
        },
    )
    html = benchmark_report.generate_benchmark_report(response_results=[case])
    assert "computed response" in html
    assert "Tension envelopes" in html
    assert "Motion time histories" in html
    assert "Force Profile" in html


def test_worked_example_exports_reproducible_inputs_and_answers(tmp_path):
    """Wind-only q=(-200/2000,0,200/16000); surge-negative T=10100 N."""
    from pathlib import Path
    import runpy

    example = (
        Path(__file__).resolve().parents[3]
        / "examples/domains/hydrodynamics/passing_ship_response.py"
    )
    module = runpy.run_path(str(example))
    results = module["run_example"](tmp_path)
    assert len(results) == 5
    np.testing.assert_allclose(results[1].motions, [[-0.1, 0, 0.0125]] * 3, atol=1e-12)
    assert results[1].line_tensions_n["surge-negative"][0] == 10100
    assert (tmp_path / "spec.yml").is_file()
    assert "10100" in (tmp_path / "response.csv").read_text(encoding="utf-8")
    html = (tmp_path / "response.html").read_text(encoding="utf-8")
    assert "Tension envelopes" in html
    assert "NOT_EVALUATED" in html
    assert b"\r\n" not in (tmp_path / "response.csv").read_bytes()
    assert b"\r\n" not in (tmp_path / "response.html").read_bytes()


def test_small_response_values_remain_visible():
    """0.0125 rad = 12.5 mrad; 10000.2/1000 = 10.0002 kN.

    Reporting in N preserves the 0.6 N envelope range without kN rounding.
    A surge of 0.0001 m must also survive axis and hover formatting.
    """
    import json
    import re

    case = AnalysisResult(
        "small response", np.array([0, 1]), None,
        np.array([[0.0001, 0, 0.0125], [0.0002, 0, 0.025]]),
        {"line": np.array([10000.2, 10000.8])}, None,
    )
    html = response_sections([case])
    assert "10000.2" in html and "10000.8" in html
    assert "Minimum (N)" in html
    plots = re.findall(r"Plotly.newPlot\([^,]+,(\[.*?\]),(\{.*?\})\);", html)
    surge, surge_layout = (json.loads(value) for value in plots[0])
    yaw, yaw_layout = (json.loads(value) for value in plots[2])
    assert surge[0]["y"] == pytest.approx([0.0001, 0.0002])
    assert yaw[0]["y"] == pytest.approx([12.5, 25])
    assert surge_layout["yaxis"]["tickformat"] == ".3g"
    assert yaw_layout["yaxis"]["tickformat"] == ".3g"
    assert "%{y:.3g}" in surge[0]["hovertemplate"]


def test_revision_history_uses_supplied_records_only():
    case = AnalysisResult("still", np.array([0]), None, None, None, None)
    html = response_sections([case])
    assert "No revision record supplied" in html
    assert "Initial computed response" not in html
    history = [{"revision": "2", "date": "2026-10-22", "change": "Correct <units>"}]
    html = response_sections([case], revision_history=history)
    assert "2026-10-22" in html and "Correct &lt;units&gt;" in html


def test_applicability_report_gives_basis_and_missing_criterion():
    case = AnalysisResult(
        "passage", np.array([0]), None, None, None, None,
        surge_natural_period_s=6.283185307179586, passage_duration_s=18,
        applicability_basis={"mass_kg": 2000, "surge_stiffness_n_m": 2000,
                             "moored_length_m": 10, "passing_length_m": 8, "speed_m_s": 1},
    )
    html = response_sections([case])
    assert "6.28 s" in html and "18 s" in html and "0.349" in html
    assert "NOT_EVALUATED" in html and "No acceptance criterion supplied" in html
    assert "Added mass" in html and "15.1" in html
    assert "2e+03 kg" in html and "2e+03 N/m" in html
    assert "10 m" in html and "8 m" in html and "1 m/s" in html
