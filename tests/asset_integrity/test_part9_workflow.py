# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Integration contracts for the inspection-driven Part 9 readiness route."""

from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from digitalmodel.asset_integrity.part9_workflow import router

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW_ID = "api579-part9-crack-fad"


def config():
    path = ROOT / "examples/workflows" / WORKFLOW_ID / "input.yml"
    return yaml.safe_load(path.read_text())


def test_router_returns_explicit_blockers_and_readable_report(tmp_path):
    cfg = config()
    cfg["_config_dir_path"] = str(tmp_path)
    cfg["part9_inspection"]["component_id"] = '<script>alert("example")</script>'
    result = router(cfg)["api579_part9_crack_fad"]
    assert result["verdict"] == "NOT_EVALUATED"
    assert result["acceptable"] is None
    report = Path(cfg["part9_report"]["written_path"])
    assert report.parent == tmp_path / "results"
    text = report.read_text()
    assert text == result["report_html"]
    assert "&lt;script&gt;" in text and '<script>alert(' not in text
    assert "Revision history" in text
    assert text.index("Screening verdict") < text.index("Revision history")
    assert text.index("Revision history") < text.index("Inspection sufficiency")
    assert all(check["reason"] in text for check in result["checks"])
    assert "COMPLETE_FOR_REVIEW" in text and "NOT_EVALUATED" in text
    assert "Intake completeness only" in text


def test_in_memory_router_is_deterministic_and_does_not_change_inspection(tmp_path):
    cfg = config()
    cfg.pop("part9_report")
    before = deepcopy(cfg["part9_inspection"])
    first = router(deepcopy(cfg))[cfg["basename"]]
    assert first == router(deepcopy(cfg))[cfg["basename"]]
    assert cfg["part9_inspection"] == before
    assert "report_path" not in first


@pytest.mark.parametrize("value", [None, [], "inspection"])
def test_router_rejects_missing_or_malformed_inspection(value):
    with pytest.raises(ValueError, match="mapping"):
        router({"basename": "api579_part9_crack_fad", "part9_inspection": value})


def test_registry_row_and_engine_route(tmp_path):
    from digitalmodel.engine import engine

    rows = yaml.safe_load((ROOT / "docs/registry/workflows.yaml").read_text())["workflows"]
    row, = [row for row in rows if row["id"] == WORKFLOW_ID]
    assert row["basename"] == "api579_part9_crack_fad"
    assert row["result"] == {"kind": "in_memory", "key": row["basename"]}
    assert row["runtime"] == "offline"
    cfg = config()
    result = engine(cfg=cfg, embed=True, root_folder=str(tmp_path), log_to_file=False)
    value = result[row["basename"]]
    assert value["verdict"] == "NOT_EVALUATED"
    assert value["measurement_sufficiency"]["status"] == "COMPLETE_FOR_REVIEW"
    assert Path(result["part9_report"]["written_path"]).is_relative_to(tmp_path)


def test_workflow_api_route_is_repeatable():
    """Deckhand consumers use the same registered run_workflow entry point."""
    from digitalmodel.workflow_api import run_workflow

    envelope = run_workflow(WORKFLOW_ID, verify_reproducible=True)
    assert envelope.status == "ok", envelope.warnings
    assert envelope.determinism["reproducible"] is True
    value = envelope.result["value"]
    assert value["verdict"] == "NOT_EVALUATED"
    assert value["kr"] is None
    assert not envelope.provenance.get("standard_revisions")


def test_partial_intake_report_names_missing_measurements(tmp_path):
    cfg = {"basename": "api579_part9_crack_fad", "part9_inspection": {},
           "part9_report": {"issue_date": "2026-10-20", "output_dir": str(tmp_path)}}
    result = router(cfg)[cfg["basename"]]
    assert "flaw.depth_mm" in result["report_html"]
    assert "INSUFFICIENT_DATA" in result["report_html"]
