"""Validation for CTV operability reference data used by GTM vessel suitability.

The dataset is source/reference material for issue #591 and should remain
traceable, deterministic, and safe for prospect-facing report generation.
"""
from __future__ import annotations

import json
from pathlib import Path

import pytest

GTM_ROOT = Path(__file__).resolve().parent.parent
DATA_FILE = GTM_ROOT / "data" / "ctv_operability_kincardine.json"


def _load_data() -> dict:
    return json.loads(DATA_FILE.read_text(encoding="utf-8"))


def test_ctv_operability_reference_has_required_traceability() -> None:
    data = _load_data()

    assert data["_source"]["publisher"] == "SeaOps Solutions"
    assert "Kincardine" in data["_source"]["title"] or data["site"]["name"] == "Kincardine"
    assert data["site"]["metocean_dataset"] == "NORA3"
    assert data["operation"]["constraints"]["daylight_only"] is True
    assert data["_references"]
    assert "GTM" in data["_description"]


def test_ctv_operability_monthly_series_is_complete_and_ordered() -> None:
    data = _load_data()
    monthly = data["monthly_operability_climatology"]

    assert [row["month"] for row in monthly] == [
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    ]
    for row in monthly:
        assert 0 <= row["as_built_n_only_pct"] <= 100
        assert 0 <= row["with_s_landing_n_plus_s_pct"] <= 100
        assert row["with_s_landing_n_plus_s_pct"] >= row["as_built_n_only_pct"]


def test_ctv_operability_headline_metrics_match_source_callouts() -> None:
    data = _load_data()
    metrics = data["headline_metrics"]

    assert metrics["as_built_annual_operability_pct"] == pytest.approx(17.8)
    assert metrics["with_s_landing_annual_operability_pct"] == pytest.approx(25.9)
    assert metrics["annual_uplift_percentage_points"] == pytest.approx(8.2)
    assert metrics["annual_uplift_relative_pct"] == pytest.approx(46)
    assert metrics["peak_absolute_uplift_month"] == "May"
    assert metrics["peak_absolute_uplift_percentage_points"] == pytest.approx(15.3)


def test_ctv_operability_supports_gtm_vessel_suitability_storyline() -> None:
    data = _load_data()
    use_cases = set(data["gtm_storyline"]["demo_use_cases"])

    assert "vessel suitability analysis" in use_cases
    assert "CTV / SOV access operability screening" in use_cases
    assert any("LIVE MODE" in use_case for use_case in use_cases)
    assert data["gtm_storyline"]["engineering_caveats"]
