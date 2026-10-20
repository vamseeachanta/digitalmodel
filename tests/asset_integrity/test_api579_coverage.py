# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Coverage intake tests; no numerical acceptance criteria are asserted.

Expected results follow the explicit unimplemented-criteria contract, not a
published engineering solution. Clause identifiers come from API 579-1/ASME
FFS-1, December 2021, public contents, sections 8/11/13/14.3 and .4.
"""

import ast
import copy
import json
from pathlib import Path
import subprocess
import sys

import pytest
import yaml

from digitalmodel.asset_integrity.api579_coverage import assess_coverage, router

ROOT = Path(__file__).resolve().parents[2]
EVIDENCE = {
    "design_data": "design-record",
    "operating_history": "history-record",
    "inspection_data": "inspection-record",
}


@pytest.mark.parametrize("part", [8, 11, 13, 14])
@pytest.mark.parametrize("level", [1, 2, 3])
def test_recorded_evidence_never_becomes_engineering_acceptance(part, level):
    result = assess_coverage(part=part, level=level, evidence=EVIDENCE)
    assert result["verdict"] == "NOT_EVALUATED"
    assert result["evidence_status"] == "RECORDED"
    assert result["missing_evidence"] == []
    assert result["evidence"] == EVIDENCE
    assert result["part"] == part and result["requested_level"] == level
    assert result["criteria_implemented"] is False
    assert result["published_example_validated"] is False
    assert result["source"]["edition"] == "2021"
    assert result["source"]["assessment_clause"] == f"{part}.4"
    assert result["source"]["data_clause"] == f"{part}.3"
    assert result["limitations"] and result["next_action"]


@pytest.mark.parametrize("part", [8, 11, 13, 14])
def test_missing_evidence_remains_explicit(part):
    result = assess_coverage(part=part)
    assert result["verdict"] == "NOT_EVALUATED"
    assert result["evidence_status"] == "INCOMPLETE"
    assert set(result["missing_evidence"]) == set(EVIDENCE)


def test_partial_evidence_is_not_lost_or_mutated():
    evidence = {"design_data": "design-record"}
    before = copy.deepcopy(evidence)
    result = assess_coverage(part=8, evidence=evidence)
    assert set(result["missing_evidence"]) == {"operating_history", "inspection_data"}
    assert evidence == before
    result["evidence"]["design_data"] = "changed-output"
    assert evidence == before


@pytest.mark.parametrize("part", [True, 8.0, "8", 4, None])
def test_unsupported_parts_are_rejected(part):
    with pytest.raises(ValueError, match="part"):
        assess_coverage(part=part)


@pytest.mark.parametrize("level", [True, 1.0, "1", 0, 4, None])
def test_invalid_levels_are_rejected(level):
    with pytest.raises(ValueError, match="level"):
        assess_coverage(part=8, level=level)


@pytest.mark.parametrize("evidence", [[], "record", {"typo": "record"},
                                      {"design_data": " "}, {"design_data": None},
                                      {"design_data": True}])
def test_malformed_evidence_is_rejected(evidence):
    with pytest.raises(ValueError, match="evidence"):
        assess_coverage(part=8, evidence=evidence)


def test_preparation_routes_preserve_existing_capability_boundaries():
    shell = assess_coverage(part=8)
    fatigue = assess_coverage(part=14)
    assert "shell_distortion_screen" in shell["related_modules"][0]
    assert any("sn_curves" in name for name in fatigue["related_modules"])
    assert any("rainflow" in name for name in fatigue["related_modules"])
    assert "2016" in shell["limitations"]
    assert "DNV" in fatigue["limitations"]


def test_router_and_committed_example():
    cfg = yaml.safe_load((ROOT / "examples/workflows/api579-coverage-screen/input.yml").read_text())
    result = router(copy.deepcopy(cfg))["api579_coverage_screen"]
    assert [entry["part"] for entry in result["assessments"]] == [8, 14, 13, 11]
    assert result["verdict"] == "NOT_EVALUATED"
    assert all(entry["verdict"] == "NOT_EVALUATED" for entry in result["assessments"])
    assert cfg.get("api579_coverage_screen") is None


@pytest.mark.parametrize("requests", [None, [], {}, [{"part": 8, "typo": True}], [8]])
def test_router_rejects_malformed_requests(requests):
    with pytest.raises(ValueError):
        router({"coverage_requests": requests})


def test_standalone_cli_returns_batch_verdict():
    command = [sys.executable, "-m", "digitalmodel.asset_integrity.api579_coverage",
               str(ROOT / "examples/workflows/api579-coverage-screen/input.yml")]
    completed = subprocess.run(command, check=True, capture_output=True, text=True)
    assert json.loads(completed.stdout)["verdict"] == "NOT_EVALUATED"


def test_engine_route_is_registered_without_importing_solver_integrations():
    tree = ast.parse((ROOT / "src/digitalmodel/engine.py").read_text())
    assert any(isinstance(node, ast.Compare) and any(
        isinstance(value, ast.Constant) and value.value == "api579_coverage_screen"
        for value in node.comparators) for node in ast.walk(tree))
    registry = yaml.safe_load((ROOT / "docs/registry/workflows.yaml").read_text())
    entry = next(row for row in registry["workflows"] if row["id"] == "api579-coverage-screen")
    assert entry["basename"] == "api579_coverage_screen"
    assert entry["result"] == {"kind": "in_memory", "key": "api579_coverage_screen"}


def test_catalog_separates_available_intake_from_planned_calculations():
    from digitalmodel.asset_integrity.offering_catalog import load

    catalog = load()
    intake = catalog.ffs["engines"]["parts-8-11-13-14"]
    assert intake["status"] == "workflow"
    assert intake["workflow"] == "api579-coverage-screen"
    assert "NOT_EVALUATED" in intake["caveat"]
    rows = [row for row in catalog.rows if "parts-8-11-13-14" in row.engines]
    assert rows
    assert all(row.status == "planned" and row.issue == 2203 for row in rows)
