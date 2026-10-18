# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Registry routing and standalone four-point skew workflow."""

import json
import subprocess
import sys
from pathlib import Path

import pytest
import yaml


def test_workflow_cli_and_registry(tmp_path):
    root = Path(__file__).resolve().parents[2]
    registry = yaml.safe_load((root / "docs/registry/workflows.yaml").read_text())
    row = next(row for row in registry["workflows"] if row["id"] == "four-point-skew")
    assert row["test"] == "tests/workflows/test_four_point_skew_workflow.py::test_workflow_cli_and_registry"
    fixture = yaml.safe_load((root / row["input"]).read_text())
    input_path = tmp_path / "input.yml"
    input_path.write_text(yaml.safe_dump(fixture))
    completed = subprocess.run([
        sys.executable, "-m", "digitalmodel.marine_ops.installation.four_point_skew",
        str(input_path),
    ], capture_output=True, text=True)
    assert completed.returncode == 0, completed.stderr
    summary = json.loads((tmp_path / "results/skew_summary.json").read_text())
    assert summary["effective_skl"] == pytest.approx(1.529679, rel=0.001)
    assert summary["design_skl"] >= 1.33
    assert summary["qa_status"] == "pending_family_clearance"
    assert summary["engineering_use_allowed"] is False
    assert len(summary["cases"]) == 16

