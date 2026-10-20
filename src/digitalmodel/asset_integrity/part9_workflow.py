# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline Part 9 intake route and FFSReport adapter; calculations stay blocked."""

from __future__ import annotations

from collections.abc import Mapping
from datetime import date
from pathlib import Path

from digitalmodel.asset_integrity.part9_inspection import assess_inspection


def _report(result, issue_date):
    from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport

    sufficiency = result["measurement_sufficiency"]
    sections = {
        "Revision history": {
            f"Initial issue / {issue_date}":
                "Inspection readiness is reported to expose missing measurements "
                "and unqualified calculation methods.",
        },
        "Inspection sufficiency": {"Status": sufficiency["status"],
                                   "Criterion": sufficiency["criterion"]},
        "Measurement issues": {
            f"{item['field']} ({item['kind']})": item["reason"]
            for item in sufficiency["issues"]
        } or {"Missing or invalid fields": "None identified by the intake schema."},
        "Submitted measurements (units in field names)": result["observed_fields"],
        "Method readiness": {
            check["id"]: f"{check['status']}: {check['reason']} "
                         f"Source required: {check['standard']} ({check['edition']}), "
                         f"{check['source_required']}."
            for check in result["checks"]
        },
        "Applicability flags": {flag: "Review required" for flag in result["applicability_flags"]},
    }
    return FFSReport.generate_screening_html(
        result["component_id"], "Part 9 crack-like flaw inspection readiness",
        {"verdict": result["verdict"], "governing_criterion": sufficiency["criterion"]},
        sections, [result["next_action"],
                   "Intake completeness does not establish data authenticity, engineering "
                   "sufficiency, applicability, fitness for service or remaining life.",
                   "The intake checklist is a practitioner reference, not a numerical method."],
    )


def router(cfg: dict) -> dict:
    """Expose one fail-closed result for engine and registry/API consumers.

    An optional part9_report.output_dir requests persistence. Written paths are
    stored in report metadata outside the in-memory result so temporary engine
    roots cannot change its result hash. The reported issue date is caller-owned.
    """
    result = assess_inspection(cfg.get("part9_inspection"))
    report_cfg = cfg.get("part9_report", {})
    if not isinstance(report_cfg, Mapping):
        raise ValueError("part9_report must be a mapping")
    issue_date = report_cfg.get("issue_date")
    if issue_date is not None:
        if not isinstance(issue_date, str) or date.fromisoformat(issue_date).isoformat() != issue_date:
            raise ValueError("part9_report.issue_date must be an ISO date")
    result["report_html"] = _report(result, issue_date or "not issued")
    if report_cfg.get("output_dir"):
        output = Path(report_cfg["output_dir"])
        if not output.is_absolute():
            output = Path(cfg.get("_config_dir_path", ".")) / output
        output.mkdir(parents=True, exist_ok=True)
        path = output / "part9-report.html"
        path.write_text(result["report_html"], encoding="utf-8")
        cfg["part9_report"]["written_path"] = str(path)
    cfg["api579_part9_crack_fad"] = result
    return cfg
