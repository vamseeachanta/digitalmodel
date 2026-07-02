# ABOUTME: Drift guard for the published casing design explorer product cases.
# ABOUTME: Re-runs every frozen product through the live design checks.
"""The capabilities page publishes an eight-product casing design study frozen
by ``scripts/capabilities/build_casing_design_explorer.py`` into
``docs/api/well/casing-design-explorer.json``. These tests re-run each frozen
product through the live :func:`check_production_casing` /
:func:`max_frac_surface_pressure` so a changed rating formula, load case or
design factor cannot silently invalidate the published page."""

import json
import math
from pathlib import Path

import pytest

from digitalmodel.well.tubulars.casing import casing
from digitalmodel.well.tubulars.casing_design import (
    ProductionCasingWell,
    api_round_force_lbf,
    api_round_pressure_psi,
    burst_external_profile,
    check_production_casing,
    max_frac_surface_pressure,
)

_REPO = Path(__file__).resolve().parents[3]
_FROZEN = _REPO / "docs" / "api" / "well" / "casing-design-explorer.json"

_MODES = ("burst", "collapse", "tension", "triaxial")


def _frozen() -> dict:
    return json.loads(_FROZEN.read_text(encoding="utf-8"))


def _frozen_products() -> list[dict]:
    return _frozen()["products"]


def _well() -> ProductionCasingWell:
    return ProductionCasingWell(**_frozen()["well"])


def test_frozen_artifact_exists_and_spans_pass_and_fail():
    products = _frozen_products()
    assert len(products) >= 5
    assert any(p["passes_all"] for p in products)
    assert any(not p["passes_all"] for p in products)


@pytest.mark.parametrize("case", _frozen_products(), ids=lambda case: case["label"])
def test_published_ratings_match_live_catalog(case):
    """API-rounded ratings on the page must match the live API 5C3 catalog."""
    product = casing(case["grade"], od_in=case["od_in"],
                     weight_ppf=case["weight_ppf"])
    ratings = case["ratings"]
    assert api_round_pressure_psi(product.burst_psi) == ratings["burst_psi"]
    assert api_round_pressure_psi(product.collapse_psi) == ratings["collapse_psi"]
    assert api_round_force_lbf(product.body_yield_lbf) == ratings["body_yield_lbf"]
    assert product.collapse_regime == ratings["collapse_regime"]
    assert product.wt_in == pytest.approx(case["wall_in"], abs=5e-4)
    assert product.id_in == pytest.approx(case["id_in"], abs=5e-4)


@pytest.mark.parametrize("case", _frozen_products(), ids=lambda case: case["label"])
def test_published_design_factors_match_live_checks(case):
    """Each published per-mode DF / verdict must match the live checks."""
    product = casing(case["grade"], od_in=case["od_in"],
                     weight_ppf=case["weight_ppf"])
    live = check_production_casing(product, case["weight_ppf"], _well())
    assert set(live) == set(_MODES)
    for mode in _MODES:
        frozen = case["checks"][mode]
        res = live[mode]
        assert not math.isinf(res.min_design_factor), mode
        assert res.min_design_factor == pytest.approx(
            frozen["min_design_factor"], abs=1e-3), (
            f"{case['label']} {mode}: live DF {res.min_design_factor:.4f} != "
            f"published {frozen['min_design_factor']:.4f}. Rebuild the page "
            "with scripts/capabilities/build_casing_design_explorer.py and "
            "re-review.")
        assert res.rating == pytest.approx(frozen["rating"], abs=0.06)
        assert res.max_load == pytest.approx(frozen["max_load"], abs=0.06)
        assert res.governing_depth_ft == pytest.approx(
            frozen["governing_depth_ft"], abs=0.06)
        assert res.required_design_factor == frozen["required_design_factor"]
        assert res.passes == frozen["passes"]
        assert res.load_case == frozen["load_case"]
    assert all(r.passes for r in live.values()) == case["passes_all"]


@pytest.mark.parametrize("case", _frozen_products(), ids=lambda case: case["label"])
def test_published_max_frac_pressure_matches_live(case):
    data = _frozen()
    well = _well()
    product = casing(case["grade"], od_in=case["od_in"],
                     weight_ppf=case["weight_ppf"])
    external = burst_external_profile(
        well.mud_ppg, well.toc_ft, well.outer_shoe_ft, well.td_ft,
        mix_water_ppg=well.mix_water_ppg, pore_ppg=well.pore_ppg)
    p_max = max_frac_surface_pressure(
        product, well.frac_fluid_ppg, external, well.td_ft,
        design_factor=data["frac_allowable_df"])
    assert p_max == pytest.approx(
        case["max_frac_surface_pressure_psi"], abs=0.06)


def test_published_barlow_golden_example():
    """The worked-example panel must reproduce 5-1/2" 23# P110 -> 14,520 psi."""
    barlow = _frozen()["barlow"]
    product = casing("P110", od_in=5.5, weight_ppf=23.0)
    assert barlow["rounded_psi"] == 14_520.0
    assert api_round_pressure_psi(product.burst_psi) == barlow["rounded_psi"]
    assert product.burst_psi == pytest.approx(barlow["raw_psi"], abs=0.06)


def test_html_page_embeds_all_products():
    html = (_FROZEN.parent / "casing-design-explorer.html").read_text(
        encoding="utf-8")
    for case in _frozen_products():
        assert json.dumps(case["label"])[1:-1] in html
