# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Report checks use independent tabulated response values, not a solver oracle."""

import numpy as np

from digitalmodel.hydrodynamics.passing_ship.analysis import AnalysisResult
from digitalmodel.hydrodynamics.passing_ship.response_report import response_sections


def test_response_sections_use_actual_envelopes_and_motion_samples():
    """Tension input [10000,12000] N gives [10.0,12.0] kN envelope exactly."""
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
    assert "10.0" in html and "12.0" in html
    assert "case&lt;script&gt;" in html
    assert "line&lt;script&gt;" in html
    assert "case<script>" not in html
    assert "line<script>" not in html
    assert "Yaw (rad)" in html
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
    assert "10100" in (tmp_path / "response.csv").read_text()
    assert "Tension envelopes" in (tmp_path / "response.html").read_text()
