# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Numerical screening regressions, without an external solver runtime."""

import json
from pathlib import Path

import pytest
import yaml

from digitalmodel.subsea.mooring_analysis.fpso_workflow import FPSOMooringWorkflow


def test_spread_mooring_summary_uses_vertical_span(tmp_path):
    """Exercise the registry input directly and retain its screening limits.

    These summary values pin the approximate designer's output; the independent
    geometry qualification remains in test_catenary_vertical_span.py.
    """
    root = Path(__file__).resolve().parents[3]
    input_path = root / "examples/workflows/fpso-spread-mooring/input.yml"
    cfg = yaml.safe_load(input_path.read_text())
    output_path = tmp_path / "summary.json"
    cfg["output"]["summary_json"] = str(output_path)

    result = FPSOMooringWorkflow().router(cfg)["fpso_mooring"]
    summary = result["summary"]

    assert summary["overall_status"] == "PASS"
    assert summary["n_results"] == len(result["line_results"]) == 8
    assert summary["min_safety_factor"] == pytest.approx(3.73)
    assert summary["max_utilization"] == pytest.approx(0.268)
    assert result["environmental_loads"]["total_force"] == pytest.approx(2280.94225)
    assert json.loads(output_path.read_text()) == result
