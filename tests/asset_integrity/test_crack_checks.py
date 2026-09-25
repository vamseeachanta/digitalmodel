"""Tests for crack-assessment consistency checks (#2157 P1).

Comparator classes: closed-form LEFM relations on generic inputs, energy identity and
invariants.
"""

import math

import pytest

from digitalmodel.asset_integrity.assessment.crack_checks import (
    SIGMA_RATIO_BAND,
    growth_validity,
    implied_opening_stress,
    irwin_plastic_zone_mm,
    keq_energy,
    mpa_sqrt_mm_to_mpa_sqrt_m,
    shakedown_check,
    sigma_ref_consistency,
    ssy_check,
)


def test_unit_conversion_mpa_sqrt_mm():
    assert mpa_sqrt_mm_to_mpa_sqrt_m(100.0) == pytest.approx(100.0 / math.sqrt(1000.0))


def test_implied_opening_stress():
    # K = Y sigma sqrt(pi a): K = 10, a = 5 mm, Y = 1.12
    s = implied_opening_stress(10.0, 5.0, y=1.12)
    assert s == pytest.approx(10.0 / (1.12 * math.sqrt(math.pi * 0.005)), rel=1e-12)


def test_sigma_ref_consistency_band_is_fixed():
    assert SIGMA_RATIO_BAND == (0.5, 2.0)


def test_sigma_ref_consistency_passes_when_consistent():
    k = 1.0 * 100.0 * math.sqrt(math.pi * 0.004)  # Y = 1, sigma = 100 MPa, a = 4 mm
    res = sigma_ref_consistency(sigma_ref_mpa=110.0, k_mpa_sqrt_m=k, a_mm=4.0)
    assert res.passed
    assert res.ratio_min <= 1.1 <= res.ratio_max


def test_sigma_ref_consistency_fails_when_far_apart():
    k = 1.0 * 20.0 * math.sqrt(math.pi * 0.004)  # opening stress ~20 MPa
    res = sigma_ref_consistency(sigma_ref_mpa=200.0, k_mpa_sqrt_m=k, a_mm=4.0)
    assert not res.passed
    assert res.ratio_min > 2.0


def test_irwin_plastic_zone():
    rp = irwin_plastic_zone_mm(10.0, 200.0, condition="plane_stress")
    assert rp == pytest.approx((1 / (2 * math.pi)) * (10.0 / 200.0) ** 2 * 1000.0)
    rp_e = irwin_plastic_zone_mm(10.0, 200.0, condition="plane_strain")
    assert rp_e == pytest.approx(rp / 3.0)
    with pytest.raises(ValueError):
        irwin_plastic_zone_mm(10.0, 200.0, condition="other")


def test_ssy_check_requires_explicit_limit():
    res = ssy_check(10.0, 200.0, ligament_mm=5.0, max_ratio=0.2)
    assert res.ratio == pytest.approx(irwin_plastic_zone_mm(10.0, 200.0) / 5.0)
    assert res.passed == (res.ratio <= 0.2)
    with pytest.raises(TypeError):
        ssy_check(10.0, 200.0, ligament_mm=5.0)  # no default limit


def test_shakedown_check():
    res = shakedown_check(elastic_range_mpa=300.0, sigma_y_mpa=200.0)
    assert res.ratio == pytest.approx(0.75)
    assert res.passed
    assert not shakedown_check(elastic_range_mpa=450.0, sigma_y_mpa=200.0).passed


def test_lr_above_one_marks_growth_conditional():
    assert growth_validity([0.4, 0.8, 0.95]).status == "VALID"
    res = growth_validity([0.4, 1.02, 0.9])
    assert res.status == "CONDITIONAL"
    assert "net-section" in res.reason


def test_mixed_mode_keq_energy_identity():
    ki, kii, kiii, nu, e_mpa = 10.0, 3.0, 2.0, 0.3, 200_000.0
    g = (1 - nu**2) / e_mpa * (ki**2 + kii**2) + (1 + nu) / e_mpa * kiii**2
    keq = keq_energy(ki, kii, kiii, nu)
    assert keq**2 * (1 - nu**2) / e_mpa == pytest.approx(g, rel=1e-12)


def test_keq_rejects_bad_poisson():
    with pytest.raises(ValueError):
        keq_energy(1.0, 0.0, 0.0, 0.5)


# --- Codex P1 review regressions (2026-09-25) -----------------------------------------
@pytest.mark.parametrize("ligament, limit", [(-5.0, 0.2), (0.0, 0.2), (5.0, 0.0)])
def test_ssy_check_rejects_invalid_inputs(ligament, limit):
    with pytest.raises(ValueError):
        ssy_check(10.0, 200.0, ligament_mm=ligament, max_ratio=limit)


@pytest.mark.parametrize("sy", [0.0, -200.0, float("nan")])
def test_checks_reject_non_positive_yield(sy):
    with pytest.raises(ValueError):
        irwin_plastic_zone_mm(10.0, sy)
    with pytest.raises(ValueError):
        shakedown_check(elastic_range_mpa=100.0, sigma_y_mpa=sy)


def test_shakedown_rejects_negative_range():
    with pytest.raises(ValueError):
        shakedown_check(elastic_range_mpa=-10.0, sigma_y_mpa=200.0)


def test_sigma_ref_consistency_rejects_non_positive_inputs():
    with pytest.raises(ValueError):
        sigma_ref_consistency(sigma_ref_mpa=-1.0, k_mpa_sqrt_m=1.0, a_mm=2.0)
    with pytest.raises(ValueError):
        sigma_ref_consistency(sigma_ref_mpa=100.0, k_mpa_sqrt_m=0.0, a_mm=2.0)
