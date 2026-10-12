"""Report contract tests for the multi-component B401 riser route."""

from pathlib import Path

import pytest
import yaml

from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
from digitalmodel.cathodic_protection.report_adapters import anode_design_report
from digitalmodel.reporting import TableBlock

FIXTURE = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "cathodic_protection"
    / "workflow_inputs"
    / "multi_component_riser.yml"
)


def _cfg() -> dict:
    with FIXTURE.open(encoding="utf-8") as stream:
        return yaml.safe_load(stream)


def _tables(cfg: dict) -> dict[str, TableBlock]:
    spec = anode_design_report(cfg)
    return {
        block.title: block
        for section in spec.sections
        for block in section.blocks
        if isinstance(block, TableBlock)
    }


def test_component_report_exposes_allocated_hosted_and_cited_rows() -> None:
    cfg = run_cathodic_protection(_cfg())
    tables = _tables(cfg)
    assert {
        "Component demand and disposition",
        "Protected-component allocations",
        "Physically hosted anode families",
        "Electrical continuity paths",
        "Overall reconciliation and governing case",
        "Where each cited table was used",
    } <= set(tables)
    allocations = tables["Protected-component allocations"]
    assert {row[0] for row in allocations.rows} >= {"buoyancy_tank", "riser_joints"}
    references = " ".join(
        str(cell)
        for row in tables["Where each cited table was used"].rows
        for cell in row
    )
    assert "Table A.1" in references and "Table 8-6" in references
    assert "Table 8-1" in references and "Table 8-4" in references
    hosts = tables["Physically hosted anode families"]
    upper = next(row for row in hosts.rows if row[1] == "upper_standoff")
    # Four installed 50 kg anodes; installed output columns are family totals.
    assert upper[6] == pytest.approx(200.0)
    family = cfg["results"]["anode_families"]["upper_standoff"]
    assert upper[8] == pytest.approx(4 * family["current_output_initial_A"])
    assert upper[9] == pytest.approx(4 * family["current_output_final_A"])
    assert {"Mass PASS", "Initial PASS", "Final PASS"} <= set(hosts.columns)
    component_table = tables["Component demand and disposition"]
    assert {
        "Coating basis",
        "Coating edition",
        "Coating applicability",
        "Concrete weight coating",
        "Field-joint infill",
        "Compatible linepipe",
        "Coverage",
        "Component status",
        "Component governing case",
    } <= set(component_table.columns)
    buoyancy = next(row for row in component_table.rows if row[0] == "buoyancy_tank")
    assert buoyancy[component_table.columns.index("Component status")] == "PASS"
    reconciliation = tables["Overall reconciliation and governing case"]
    assert any(
        row[0] == "Allocation reconciliation checks" for row in reconciliation.rows
    )
