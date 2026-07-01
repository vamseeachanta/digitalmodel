"""Golden test for the LCOE/TOTEX workflow + data sheet (issue #1226).

Runs the committed example ``input.yml`` end-to-end through the router and
checks the base case reproduces the deck's $184/MWh, that optional reliability
and sensitivity blocks populate, and that the JSON summary + HTML data sheet are
written.
"""

from pathlib import Path

import pytest
import yaml

from digitalmodel.floating_wind.economics import base_case, compute_lcoe
from digitalmodel.floating_wind.economics_report import (
    lcoe_data_sheet,
    render_lcoe_data_sheet_html,
)
from digitalmodel.floating_wind.economics_workflow import (
    FloatingWindEconomicsWorkflow,
)

_EXAMPLE_YML = (
    Path(__file__).resolve().parents[2]
    / "examples"
    / "workflows"
    / "floating-wind-economics"
    / "input.yml"
)


def _load_example() -> dict:
    return yaml.safe_load(_EXAMPLE_YML.read_text())


def test_example_input_exists():
    assert _EXAMPLE_YML.is_file()
    cfg = _load_example()
    assert cfg["basename"] == "floating_wind_economics"


def test_router_reproduces_base_case(tmp_path):
    cfg = _load_example()
    cfg["output"] = {
        "summary_json": str(tmp_path / "summary.json"),
        "data_sheet_html": str(tmp_path / "sheet.html"),
    }
    out = FloatingWindEconomicsWorkflow().router(cfg)
    result = out["floating_wind_economics"]["result"]

    assert result["farm_capacity_mw"] == pytest.approx(495.0)
    assert result["lcoe"]["lcoe_usd_per_mwh"] == pytest.approx(184.0, rel=0.02)
    # per-MWh split reconstitutes the LCOE
    split = (
        result["lcoe"]["capex_per_mwh"]
        + result["lcoe"]["opex_per_mwh"]
        + result["lcoe"]["decomex_per_mwh"]
    )
    assert split == pytest.approx(result["lcoe"]["lcoe_usd_per_mwh"], rel=1e-3)


def test_router_reliability_block(tmp_path):
    cfg = _load_example()
    cfg["output"] = {}
    out = FloatingWindEconomicsWorkflow().router(cfg)
    rel = out["floating_wind_economics"]["result"]["reliability"]
    # 50% failure-rate reduction -> ~$162/MWh (deck slide 17)
    assert rel["failure_rate_reduction"] == pytest.approx(0.50)
    assert rel["lcoe"]["lcoe_usd_per_mwh"] == pytest.approx(162.0, rel=0.02)


def test_router_sensitivities_block(tmp_path):
    cfg = _load_example()
    cfg["output"] = {}
    out = FloatingWindEconomicsWorkflow().router(cfg)
    sens = {s["name"]: s for s in out["floating_wind_economics"]["result"]["sensitivities"]}
    assert "design_life_30yr" in sens
    # every listed sensitivity lowers LCOE vs the $184 base
    for s in sens.values():
        assert s["delta_vs_base"] < 0.0


def test_router_writes_outputs(tmp_path):
    cfg = _load_example()
    summary_json = tmp_path / "out" / "summary.json"
    sheet_html = tmp_path / "out" / "sheet.html"
    cfg["output"] = {
        "summary_json": str(summary_json),
        "data_sheet_html": str(sheet_html),
    }
    FloatingWindEconomicsWorkflow().router(cfg)
    assert summary_json.is_file()
    assert sheet_html.is_file()
    assert "184" in sheet_html.read_text()


def test_router_without_output_is_noop_on_disk():
    """No output config -> still computes, writes nothing."""
    cfg = _load_example()
    cfg.pop("output", None)
    out = FloatingWindEconomicsWorkflow().router(cfg)
    res = out["floating_wind_economics"]["result"]
    assert "summary_json" not in res
    assert "data_sheet_html" not in res


def test_data_sheet_sections_and_compact_mode():
    e = base_case()
    r = compute_lcoe(e)
    full = lcoe_data_sheet(e, r)
    compact = lcoe_data_sheet(e, r, mode="compact")
    # full renders all always-sections; both are non-empty HTML fragment lists
    assert len(full) >= 5
    assert all(isinstance(s, str) and s for s in full)
    assert len(compact) <= len(full)


def test_data_sheet_html_is_standalone_document():
    e = base_case()
    r = compute_lcoe(e)
    doc = render_lcoe_data_sheet_html(e, r)
    assert doc.lstrip().lower().startswith("<!doctype") or "<html" in doc.lower()
    assert "LCOE" in doc
