# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Report evidence, incomplete coverage and executable synthetic example."""

import os
import runpy
import subprocess
import sys
from dataclasses import replace
from pathlib import Path

from digitalmodel.lifting_lug.assessment import PadeyeAllowables, assess_padeye
from digitalmodel.lifting_lug.report import render_report

ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = ROOT / "examples/workflows/lifting-lug-design/padeye_assessment.py"
SAMPLE = ROOT / "docs/reports/padeye-lift-point-analysis.html"


def synthetic():
    return runpy.run_path(str(EXAMPLE))["synthetic_assessment"]()


def test_report_has_hand_computed_answer_and_way_forward():
    """Independent arithmetic is recorded in test_assessment's hand calculation.

    P=180 kN, weld stress=112.5 MPa, limit=100 MPa, required throat=9 mm.
    """
    page = render_report(synthetic())
    for text in (
        "Padeye and lift point analysis", "Revision history", "180.0",
        "112.5", "9.0", "FAIL", "NOT_EVALUATED", "13.3.2.4",
        "assumed, based on previous experience", "effective throat",
        "Stress-Strain Curves", "Shear and Torsion",
    ):
        assert text in page
    assert (page.index("<h2>Summary") < page.index("<h2>Revision history")
            < page.index("<h2>Inputs"))
    assert SAMPLE.read_text(encoding="utf-8") == page


def test_report_escapes_input_provenance():
    initial = synthetic()
    load = replace(initial.load, basis='<script>alert("input")</script>')
    result = assess_padeye(initial.plate, load, initial.allowables, initial.weld)
    page = render_report(result)
    assert '<script>alert("input")</script>' not in page
    assert "&lt;script&gt;" in page


def test_missing_inputs_render_as_unevaluated_not_numeric_zero():
    initial = synthetic()
    result = assess_padeye(initial.plate, initial.load, PadeyeAllowables())
    page = render_report(result)
    assert "An allowable and its source are required" in page
    assert "Effective weld geometry is missing" in page
    assert "not available" in page
    assert "<td>None</td>" not in page


def test_executable_example_writes_only_requested_report(tmp_path):
    output = tmp_path / "analysis.html"
    completed = subprocess.run(
        [sys.executable, str(EXAMPLE), "--output", str(output)],
        env={**os.environ, "PYTHONPATH": str(ROOT / "src")},
        capture_output=True, text=True, timeout=20, check=True,
    )
    assert output.read_text(encoding="utf-8") == SAMPLE.read_text(encoding="utf-8")
    assert "screening=FAIL" in completed.stdout
    assert "qualification=NOT_EVALUATED" in completed.stdout
    assert sorted(path.name for path in tmp_path.iterdir()) == ["analysis.html"]
