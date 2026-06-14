"""Assertions for field-development and production durable workflows."""

from __future__ import annotations

import pytest


FIELD_DEV_PRODUCTION_WORKFLOWS = {
    "capex-estimate",
    "opex-estimate",
    "concept-selection",
    "nodal-analysis",
    "vlp-correlations",
}


def assert_field_dev_production_workflow(workflow_id: str, cfg: dict) -> None:
    if workflow_id == "capex-estimate":
        estimate = cfg["capex_estimate"]["estimate"]
        assert estimate["host_type"] == "TLP"
        assert [
            estimate["low_usd_bn"],
            estimate["base_usd_bn"],
            estimate["high_usd_bn"],
        ] == pytest.approx([2.0, 4.0, 6.0])
        assert "Mars TLP" in estimate["basis"]
    elif workflow_id == "opex-estimate":
        estimate = cfg["opex_estimate"]["estimate"]
        assert estimate["host_type"] == "Semi"
        assert [
            estimate["low_usd_mm_per_yr"],
            estimate["base_usd_mm_per_yr"],
            estimate["high_usd_mm_per_yr"],
            estimate["opex_per_bbl_usd"],
        ] == pytest.approx([146.10, 278.92, 478.15, 5.09])
    elif workflow_id == "concept-selection":
        selection = cfg["concept_selection"]["selection"]
        top = selection["ranked_options"][0]
        assert selection["selected_host"] == "TLP"
        assert top["host_type"] == "TLP"
        assert top["score"] == pytest.approx(87.0)
        assert top["capex_estimate_usd_bn"] == pytest.approx([2.0, 6.0])
    elif workflow_id == "nodal-analysis":
        block = cfg["nodal_analysis"]
        op = block["operating_point"]
        assert block["ipr_model"] == "vogel"
        assert op["q_bopd"] == pytest.approx(996.727220, rel=1e-4)
        assert op["pwf_psi"] == pytest.approx(2458.137610, rel=1e-4)
        assert op["confidence"] == "Green"
        assert op["q_uncertainty_fraction"] == pytest.approx(0.05)
    elif workflow_id == "vlp-correlations":
        block = cfg["vlp_correlations"]
        curve = block["curve"]
        assert block["correlation"] == "hagedorn_brown"
        assert block["monotonic_increasing"] is True
        assert [row["q_bopd"] for row in curve] == pytest.approx(
            [100.0, 300.0, 500.0, 800.0, 1200.0]
        )
        assert curve[0]["pwf_psi"] == pytest.approx(1816.367181, rel=1e-6)
        assert curve[-1]["pwf_psi"] == pytest.approx(1866.715924, rel=1e-6)
    else:
        raise AssertionError(f"Missing field-development assertion for {workflow_id}")
