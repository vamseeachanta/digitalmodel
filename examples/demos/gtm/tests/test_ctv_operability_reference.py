"""Guardrails for CTV operability reference material.

The Kincardine CTV operability dataset is internal competitive/reference
material, not GTM collateral or a design-basis engineering dataset.
"""
from __future__ import annotations

import json
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[4]
GTM_ROOT = REPO_ROOT / "examples" / "demos" / "gtm"
GTM_OUTPUT_ROOT = GTM_ROOT / "output" / "client_pdf_pack_2026-05-07"
GTM_OUTPUT_PDF_BUNDLE = (
    GTM_OUTPUT_ROOT / "digitalmodel_vessel_capability_gtm_pdf_pack_2026-05-07"
)
OLD_GTM_DATA_FILE = GTM_ROOT / "data" / "ctv_operability_kincardine.json"
OLD_GTM_OUTPUT_FILE = GTM_OUTPUT_ROOT / "06_ctv_access_operability_kincardine_digitalmodel.html"
OLD_GTM_OUTPUT_PDF = GTM_OUTPUT_ROOT / "06_ctv_access_operability_kincardine_digitalmodel.pdf"
OLD_GTM_BUNDLED_OUTPUT_PDF = (
    GTM_OUTPUT_PDF_BUNDLE / "06_ctv_access_operability_kincardine_digitalmodel.pdf"
)
GTM_OUTPUT_INDEX_FILE = GTM_OUTPUT_ROOT / "00_vessel_capability_gtm_pdf_pack_index.html"
REFERENCE_DATA_FILE = (
    REPO_ROOT
    / "references"
    / "vessel-suitability"
    / "data"
    / "ctv_operability_kincardine.json"
)
GTM_README = GTM_ROOT / "README.md"

FORBIDDEN_EXTERNAL_FRAMING = (
    "gtm_storyline",
    "demo_use_cases",
    "prospect-" + "facing",
    "LIVE" + " MODE",
)


def _load_data() -> dict:
    return json.loads(REFERENCE_DATA_FILE.read_text(encoding="utf-8"))


def _reference_text() -> str:
    return REFERENCE_DATA_FILE.read_text(encoding="utf-8")


def test_ctv_operability_reference_has_required_traceability() -> None:
    data = _load_data()

    assert data["_source"]["publisher"] == "SeaOps Solutions"
    assert "Kincardine" in data["_source"]["title"] or data["site"]["name"] == "Kincardine"
    assert data["site"]["metocean_dataset"] == "NORA3"
    assert data["operation"]["constraints"]["daylight_only"] is True
    assert data["_references"]
    assert "internal reference" in data["_description"].lower()


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


def test_ctv_operability_enforces_internal_reference_boundary() -> None:
    data = _load_data()
    boundary = data["usage_boundary"]

    assert boundary["classification"] == "internal_reference_only"
    assert "explicit user approval" in boundary["promotion_rule"]
    assert "source-rights review" in boundary["promotion_rule"]
    assert "SeaOps" in boundary["competitor_context"]
    assert "competitor" in boundary["competitor_context"].lower()
    assert data["data_confidence"]["digitized_values"] == "rights_unresolved_no_external_use"
    assert data["data_confidence"]["engineering_use"] == "not_design_basis"
    assert data["methodology_boundary"]["reference_observation"]
    assert data["methodology_boundary"]["design_basis_engineering"]
    assert "reference_insights" in data
    assert "gtm_storyline" not in data


def test_ctv_operability_promotion_guardrails_remove_gtm_surface() -> None:
    assert not OLD_GTM_DATA_FILE.exists()
    assert not OLD_GTM_OUTPUT_FILE.exists()
    assert not OLD_GTM_OUTPUT_PDF.exists()
    assert not OLD_GTM_BUNDLED_OUTPUT_PDF.exists()

    reference_text = _reference_text()
    readme_text = GTM_README.read_text(encoding="utf-8")
    output_index_text = GTM_OUTPUT_INDEX_FILE.read_text(encoding="utf-8")

    for forbidden in FORBIDDEN_EXTERNAL_FRAMING:
        assert forbidden not in reference_text

    assert "ctv_operability_kincardine.json" not in readme_text
    assert "vessel/access suitability GTM storylines" not in readme_text
    assert "06_ctv_access_operability_kincardine" not in output_index_text
    assert "CTV access-operability" not in output_index_text
