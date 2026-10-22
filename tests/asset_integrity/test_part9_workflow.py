# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Integration contracts for the inspection-driven Part 9 readiness route."""

from copy import deepcopy
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pytest
import yaml

from digitalmodel.asset_integrity.part9_workflow import router

ROOT = Path(__file__).resolve().parents[2]
WORKFLOW_ID = "api579-part9-crack-fad"


def config():
    path = ROOT / "examples/workflows" / WORKFLOW_ID / "input.yml"
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def test_router_returns_explicit_blockers_and_readable_report(tmp_path):
    cfg = config()
    cfg["_config_dir_path"] = str(tmp_path)
    cfg["part9_inspection"]["component_id"] = '<script>alert("example")</script>'
    result = router(cfg)["api579_part9_crack_fad"]
    assert result["verdict"] == "NOT_EVALUATED"
    assert result["acceptable"] is None
    report = Path(cfg["part9_report"]["written_path"])
    assert report.parent == tmp_path / "results"
    text = report.read_text(encoding="utf-8")
    assert text == cfg["part9_report"]["report_html"]
    assert "report_html" not in result
    assert "written_path" not in result
    assert cfg["part9_report"]["written_path"] == report.as_posix()
    assert b"\r\n" not in report.read_bytes()
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
    assert "written_path" not in first


@pytest.mark.parametrize("value", [None, [], "inspection"])
def test_router_rejects_missing_or_malformed_inspection(value):
    with pytest.raises(ValueError, match="mapping"):
        router({"basename": "api579_part9_crack_fad", "part9_inspection": value})


def test_registry_row_and_engine_route(tmp_path):
    from digitalmodel.engine import engine

    rows = yaml.safe_load((ROOT / "docs/registry/workflows.yaml").read_text(
        encoding="utf-8"))["workflows"]
    row, = [row for row in rows if row["id"] == WORKFLOW_ID]
    assert row["basename"] == "api579_part9_crack_fad"
    assert row["result"] == {"kind": "in_memory", "key": row["basename"]}
    assert row["runtime"] == "offline"
    assert row["outputs"] == [
        "examples/workflows/api579-part9-crack-fad/results/part9-report.html"]
    assert row["test"] == (
        "tests/workflows/test_durable_workflows.py::"
        "test_workflow_registry[api579-part9-crack-fad]")
    cfg = config()
    result = engine(cfg=cfg, embed=True, root_folder=str(tmp_path), log_to_file=False)
    value = result[row["basename"]]
    assert value["verdict"] == "NOT_EVALUATED"
    assert value["measurement_sufficiency"]["status"] == "COMPLETE_FOR_REVIEW"
    assert Path(result["part9_report"]["written_path"]).is_relative_to(tmp_path)


def test_workflow_api_route_is_repeatable(tmp_path, monkeypatch):
    """Deckhand consumers use the same registered run_workflow entry point."""
    from digitalmodel.workflow_api import run_workflow

    example = ROOT / "examples/workflows" / WORKFLOW_ID
    before = {path.relative_to(example).as_posix(): path.read_bytes()
              for path in example.rglob("*") if path.is_file()}
    monkeypatch.chdir(tmp_path)
    envelope = run_workflow(WORKFLOW_ID, verify_reproducible=True)
    assert envelope.status == "ok", envelope.warnings
    assert envelope.determinism["reproducible"] is True
    value = envelope.result["value"]
    assert value["verdict"] == "NOT_EVALUATED"
    assert value["kr"] is None
    assert not envelope.provenance.get("standard_revisions")
    assert list(tmp_path.iterdir()) == []
    assert {path.relative_to(example).as_posix(): path.read_bytes()
            for path in example.rglob("*") if path.is_file()} == before


def test_partial_intake_report_names_missing_measurements(tmp_path):
    cfg = {"basename": "api579_part9_crack_fad", "part9_inspection": {},
           "part9_report": {"issue_date": "2026-10-20", "output_dir": str(tmp_path)}}
    result = router(cfg)[cfg["basename"]]
    assert result["measurement_sufficiency"]["status"] == "INSUFFICIENT_DATA"
    assert "flaw.depth_mm" in cfg["part9_report"]["report_html"]
    assert "INSUFFICIENT_DATA" in cfg["part9_report"]["report_html"]


def test_workflow_result_hash_is_independent_of_report_generation_date(monkeypatch):
    from digitalmodel.asset_integrity.assessment import screening_report
    from digitalmodel.workflow_api import run_workflow

    class ReportClock:
        current = datetime(2026, 10, 20, tzinfo=timezone.utc)

        @classmethod
        def now(cls, tz):
            return cls.current.astimezone(tz)

    monkeypatch.setattr(screening_report, "datetime", ReportClock)
    cfg = config()
    cfg.pop("part9_report")
    first_cfg = router(deepcopy(cfg))
    first = run_workflow(WORKFLOW_ID)
    ReportClock.current += timedelta(days=1)
    second_cfg = router(deepcopy(cfg))
    second = run_workflow(WORKFLOW_ID)
    assert "<strong>Date:</strong> 2026-10-20" in first_cfg["part9_report"]["report_html"]
    assert "<strong>Date:</strong> 2026-10-21" in second_cfg["part9_report"]["report_html"]
    assert first_cfg["part9_report"]["report_html"] != second_cfg["part9_report"]["report_html"]
    assert first_cfg[cfg["basename"]] == second_cfg[cfg["basename"]]
    assert first.status == second.status == "ok"
    assert first.determinism["result_hash"] == second.determinism["result_hash"]
    assert "report_html" not in first.result["value"]


def test_report_metadata_is_available_without_persistence():
    cfg = config()
    cfg.pop("part9_report")
    result = router(cfg)[cfg["basename"]]
    assert "report_html" not in result
    assert "NOT_EVALUATED" in cfg["part9_report"]["report_html"]
    assert "written_path" not in cfg["part9_report"]
