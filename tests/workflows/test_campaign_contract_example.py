# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent expected counts for the issue 2298 worked example."""

import csv
import importlib.util
import json
from pathlib import Path


def test_worked_example(tmp_path):
    """a completes, b fails, c resumes and completes: 3 attempted, 2 native.

    Initially a,c pass; revised input withholds a, leaving one qualified. Both
    native completions enter the regenerated report, so report freshness is 2/2.
    a's counter advances at 20; at 31, age 11 exceeds the supplied limit 10.
    These are synthetic inputs, not engineering acceptance recommendations.
    """
    path = Path(__file__).resolve().parents[2] / "examples/workflows/campaign_contract.py"
    spec = importlib.util.spec_from_file_location("campaign_example", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    answer = module.run_example(tmp_path)
    assert answer["stalled"] == "STALLED"
    assert answer["resumed"] == ["c"]
    assert answer["status"] == (
        "native 3/3 attempted, 2 completed, 1 qualified; report 2/2 complete; "
        "next: Review withheld a and failed b")
    report = json.loads((tmp_path / "report.json").read_text())
    with (tmp_path / "workbook.csv").open(newline="") as stream:
        workbook = list(csv.DictReader(stream))
    assert report["status"] == answer["status"]
    assert [row["id"] for row in workbook] == ["a", "b", "c"]
    assert all(row["criteria_version"] == "v2" for row in workbook)
    preview = json.loads((tmp_path / "criteria-preview.json").read_text())
    assert preview["affected_ids"] == ["a", "b", "c"]
    assert preview["newly_withheld"] == ["a"]
