"""Tests for the ALARP risk-evaluation framework (issue #577).

Covers the three ALARP regions, the threshold boundaries, and the
gross-disproportion cost-benefit check (both justifying and ruling out a
mitigation). Hand-verified cases are noted inline and in the solution doc.
"""

import math

import pytest

from digitalmodel.asset_integrity.safety_case import (
    ALARPRegion,
    ALARPThresholds,
    classify_risk,
    gross_disproportion_check,
    risk_from_likelihood_consequence,
)


# --------------------------------------------------------------------------- #
# Region classification (default thresholds: 1e-3 / 1e-6 per year, R2P2)
# --------------------------------------------------------------------------- #
def test_broadly_acceptable_case():
    res = classify_risk(1e-7)
    assert res.region is ALARPRegion.BROADLY_ACCEPTABLE
    assert res.requires_alarp_demonstration is False


def test_tolerable_alarp_case():
    # Hand-verified: 5e-5 lies between 1e-6 and 1e-3 -> ALARP region.
    res = classify_risk(5e-5)
    assert res.region is ALARPRegion.TOLERABLE_ALARP
    assert res.requires_alarp_demonstration is True


def test_intolerable_case():
    res = classify_risk(5e-3)
    assert res.region is ALARPRegion.INTOLERABLE
    assert res.requires_alarp_demonstration is False


# --------------------------------------------------------------------------- #
# Threshold boundaries (convention: == upper -> Intolerable; == lower -> BA)
# --------------------------------------------------------------------------- #
def test_upper_boundary_is_intolerable():
    res = classify_risk(1e-3)
    assert res.region is ALARPRegion.INTOLERABLE


def test_just_below_upper_boundary_is_alarp():
    res = classify_risk(1e-3 * (1 - 1e-9))
    assert res.region is ALARPRegion.TOLERABLE_ALARP


def test_lower_boundary_is_broadly_acceptable():
    res = classify_risk(1e-6)
    assert res.region is ALARPRegion.BROADLY_ACCEPTABLE


def test_just_above_lower_boundary_is_alarp():
    res = classify_risk(1e-6 * (1 + 1e-6))
    assert res.region is ALARPRegion.TOLERABLE_ALARP


# --------------------------------------------------------------------------- #
# Configurable thresholds (e.g. public-risk line 1e-4)
# --------------------------------------------------------------------------- #
def test_custom_public_thresholds_shift_classification():
    public = ALARPThresholds(upper_intolerable=1e-4, lower_broadly_acceptable=1e-6)
    # 5e-4 is ALARP under defaults but Intolerable under the public line.
    assert classify_risk(5e-4).region is ALARPRegion.TOLERABLE_ALARP
    assert classify_risk(5e-4, public).region is ALARPRegion.INTOLERABLE


def test_invalid_thresholds_rejected():
    with pytest.raises(ValueError):
        ALARPThresholds(upper_intolerable=1e-6, lower_broadly_acceptable=1e-3)
    with pytest.raises(ValueError):
        ALARPThresholds(upper_intolerable=-1.0)


def test_negative_risk_rejected():
    with pytest.raises(ValueError):
        classify_risk(-1e-5)


# --------------------------------------------------------------------------- #
# risk = likelihood x consequence
# --------------------------------------------------------------------------- #
def test_risk_from_likelihood_consequence():
    # 1e-3 events/yr * 0.05 fatality prob = 5e-5 /yr -> ALARP region.
    risk = risk_from_likelihood_consequence(1e-3, 0.05)
    assert risk == pytest.approx(5e-5)
    assert classify_risk(risk).region is ALARPRegion.TOLERABLE_ALARP


def test_risk_from_likelihood_consequence_negative_rejected():
    with pytest.raises(ValueError):
        risk_from_likelihood_consequence(-1.0, 0.5)


# --------------------------------------------------------------------------- #
# Gross-disproportion cost-benefit check
# --------------------------------------------------------------------------- #
def test_gdf_justifies_mitigation():
    # Hand-verified: cost 2e6 < gdf(3)*benefit(1e6)=3e6 -> required, ratio 2.0.
    res = gross_disproportion_check(cost=2.0e6, benefit=1.0e6, gdf=3.0)
    assert res.measure_required is True
    assert res.is_grossly_disproportionate is False
    assert res.disproportion_ratio == pytest.approx(2.0)


def test_gdf_rules_out_mitigation():
    # cost 4e6 >= gdf(3)*benefit(1e6)=3e6 -> grossly disproportionate, ratio 4.0.
    res = gross_disproportion_check(cost=4.0e6, benefit=1.0e6, gdf=3.0)
    assert res.measure_required is False
    assert res.is_grossly_disproportionate is True
    assert res.disproportion_ratio == pytest.approx(4.0)


def test_gdf_boundary_is_grossly_disproportionate():
    # cost exactly == gdf*benefit -> ruled out (>= test).
    res = gross_disproportion_check(cost=3.0e6, benefit=1.0e6, gdf=3.0)
    assert res.is_grossly_disproportionate is True
    assert res.measure_required is False


def test_gdf_default_factor_is_three():
    res = gross_disproportion_check(cost=2.5e6, benefit=1.0e6)
    assert res.gdf == 3.0
    assert res.measure_required is True  # 2.5e6 < 3e6


def test_gdf_zero_benefit_infinite_ratio_rules_out():
    res = gross_disproportion_check(cost=1.0e6, benefit=0.0, gdf=3.0)
    assert math.isinf(res.disproportion_ratio)
    assert res.measure_required is False


def test_gdf_must_be_at_least_one():
    with pytest.raises(ValueError):
        gross_disproportion_check(cost=1.0, benefit=1.0, gdf=0.5)
