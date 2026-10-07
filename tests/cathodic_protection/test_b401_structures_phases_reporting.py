"""Report integration for B401 riser-base phased assessments."""

from __future__ import annotations

import copy
from pathlib import Path
from typing import Any, cast

import yaml  # type: ignore[import-untyped]

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.report_adapters import anode_design_report
from digitalmodel.reporting import TableBlock

FIXTURE = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "cathodic_protection"
    / "workflow_inputs"
    / "riser_base_phased.yml"
)


def _cfg() -> dict[str, Any]:
    with FIXTURE.open(encoding="utf-8") as stream:
        return cast(dict[str, Any], yaml.safe_load(stream))


def test_report_exposes_components_phases_retrofit_and_owner_disposition() -> None:
    cfg = _cfg()
    first = run_cathodic_protection(copy.deepcopy(cfg))["results"]
    check = first["riser_base_assessment"]["phases"][1]["families"]["sea"][
        "output_checks"
    ]["initial_output"]
    cfg["inputs"]["riser_base_assessment"]["accepted_output_shortfalls"] = [
        {
            "family": "sea",
            "phase": "operation",
            "criterion": "initial_output",
            "demand_A": check["demand_A"],
            "output_A": check["output_A"],
            "ratio": check["ratio"],
            "owner_reason": "Temporary exception with monitoring controls.",
            "basis": {
                "source": "project_practice",
                "reason": "Owner disposition; not standard compliance.",
            },
        }
    ]
    spec = anode_design_report(run_cathodic_protection(cfg))
    tables = {
        block.title: block
        for section in spec.sections
        for block in section.blocks
        if isinstance(block, TableBlock)
    }
    assert "Riser-base components" in tables
    assert "Phase mass ledger" in tables
    assert "Retrofit assessment" in tables
    assert "Accepted output shortfalls (engineering result remains FAIL)" in tables
    assert "Initial demand [A]" in tables["Phase mass ledger"].columns
    assert "Final demand/output ratio" in tables["Phase mass ledger"].columns
    assert "Composition reason" in tables["Riser-base components"].columns
