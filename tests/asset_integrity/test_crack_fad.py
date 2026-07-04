"""Level-2 FAD crack-like flaw assessment (#1270).

Anchors, per repo validation convention:
- PUBLISHED-VALIDATED: the Option-1 curve reproduces BS 7910:2013 Cl. 7.3.2
  (cross-checked numerically against the repo's legacy implementation,
  ``asset_integrity/common/fad.py``, over the whole Lr grid); Newman-Raju
  limiting coefficient 1.04/sqrt(Q) for the shallow semicircular flaw
  (Newman & Raju 1981); Annex J Charpy correlation closed form.
- REGRESSION-ANCHOR: verdict cases on the real (anonymized) flaw-register
  dimensions from the 0151-derived fixtures.
"""

import math

import pytest

from digitalmodel.asset_integrity.assessment.crack_fad import (
    CrackFlawAssessment,
    assess_crack_like_flaw,
    critical_flaw_depth_mm,
    fad_curve_option1,
    kmat_from_charpy,
    lr_max,
    newman_raju_k,
    reference_stress_surface_flaw,
)

# X-80 in SI
SY, SU, E = 551.6, 620.5, 207_000.0
T_RISER = 22.225  # the real fleet's nominal main-tube wall (mm)


# ---------------------------------------------------------------------------
# FAD curve
# ---------------------------------------------------------------------------
def test_fad_curve_matches_legacy_bs7910_implementation():
    """Canonical curve == the legacy class-society-reviewed implementation."""
    from digitalmodel.asset_integrity.common.fad import FAD

    cfg = {
        "Outer_Pipe": {"Material": {"Material": "steel", "Material_Grade": "X80"}},
        "Material": {"steel": {"E": E, "Grades": {"X80": {"SMYS": SY, "SMUS": SU}}}},
    }
    legacy = FAD(cfg)
    legacy.BS7910_2013_option_1()
    df = legacy.FAD["option_1"]
    checked = 0
    for lr_val, fr_legacy in zip(df["L_r"], df["K_r"]):
        if float(fr_legacy) == 0.0:
            continue  # legacy appends a plot-convention drop-to-zero row at Lr_max
        assert fad_curve_option1(float(lr_val), SY, SU, E) == pytest.approx(
            float(fr_legacy), abs=1e-9
        ), f"divergence at Lr={lr_val}"
        checked += 1
    assert checked >= 200


def test_fad_curve_endpoints_and_cutoff():
    assert fad_curve_option1(0.0, SY, SU, E) == pytest.approx(1.0)
    cutoff = lr_max(SY, SU)
    assert cutoff == pytest.approx((SY + SU) / (2 * SY))
    assert fad_curve_option1(cutoff + 1e-6, SY, SU, E) == 0.0
    # monotone non-increasing over the valid range
    vals = [fad_curve_option1(0.02 * i, SY, SU, E) for i in range(53)]
    vals = [v for v in vals if v > 0]
    assert all(a >= b - 1e-12 for a, b in zip(vals, vals[1:]))


# ---------------------------------------------------------------------------
# Newman-Raju K
# ---------------------------------------------------------------------------
def test_newman_raju_shallow_semicircular_limit():
    """a/c=1, a/t->0, deepest point: F -> 1.04, Q = 2.464 (Newman-Raju 1981)."""
    a, c, t, sigma = 0.05, 0.05, 100.0, 100.0
    k = newman_raju_k(a, c, t, sigma)
    q = 1.0 + 1.464
    expected = 1.04 * sigma * math.sqrt(math.pi * (a / 1000.0) / q)
    assert k == pytest.approx(expected, rel=1e-3)
    # the classic ~0.663 sigma sqrt(pi a) coefficient
    assert k / (sigma * math.sqrt(math.pi * a / 1000.0)) == pytest.approx(
        0.6625, abs=0.005
    )


def test_newman_raju_deepens_with_depth_and_governing_point_switches():
    # K grows with depth at fixed length
    ks = [newman_raju_k(a, 25.0, T_RISER, 200.0) for a in (5.0, 8.0, 12.0)]
    assert ks == sorted(ks)
    # semicircular shallow flaw: surface point exceeds deepest point (g-factor)
    k_deep = newman_raju_k(0.5, 0.5, T_RISER, 200.0, phi_deg=90.0)
    k_surf = newman_raju_k(0.5, 0.5, T_RISER, 200.0, phi_deg=0.0)
    assert k_surf > k_deep


def test_newman_raju_validity_errors():
    with pytest.raises(ValueError):
        newman_raju_k(5.0, 3.0, T_RISER, 100.0)  # a/c > 1
    with pytest.raises(ValueError):
        newman_raju_k(25.0, 30.0, T_RISER, 100.0)  # a >= t


# ---------------------------------------------------------------------------
# Reference stress + Charpy correlation
# ---------------------------------------------------------------------------
def test_reference_stress_membrane_only_closed_form():
    a, c, t, pm = 4.0, 20.0, T_RISER, 150.0
    alpha = (a / t) / (1.0 + t / c)
    assert reference_stress_surface_flaw(a, c, t, pm) == pytest.approx(
        pm / (1.0 - alpha)
    )
    # bending raises the reference stress
    assert reference_stress_surface_flaw(a, c, t, pm, 50.0) > pm / (1.0 - alpha)


def test_charpy_correlation_annex_j_value():
    # CVN=27 J at B=25 mm: 12*sqrt(27) - 20 + 20 = 62.35 MPa*sqrt(m)
    assert kmat_from_charpy(27.0, 25.0) == pytest.approx(62.35, abs=0.01)
    # thicker section -> lower toughness credit
    assert kmat_from_charpy(27.0, 50.0) < kmat_from_charpy(27.0, 25.0)


# ---------------------------------------------------------------------------
# Level-2 assessment — real flaw-register dimensions (REGRESSION-ANCHOR)
# ---------------------------------------------------------------------------
def _assess(a, length, sigma_m=150.0, sigma_b=30.0, kmat=100.0):  # noqa: D103
    return assess_crack_like_flaw(
        a_mm=a, c_mm=length / 2.0, t_mm=T_RISER,
        sigma_m_mpa=sigma_m, sigma_b_mpa=sigma_b,
        sigma_y_mpa=SY, sigma_u_mpa=SU, k_mat=kmat,
    )


def test_real_register_flaw_is_acceptable():
    """The 23 mm x 2 mm external surface flaw from the real register, on the
    riser weld at operational stress with tested toughness: inside the FAD."""
    r = _assess(a=2.0, length=23.0)
    assert isinstance(r, CrackFlawAssessment)
    assert r.acceptable
    assert r.kr < r.f_lr
    assert r.margin > 1.0


def test_deep_flaw_fails_and_margin_is_monotone():
    """At meaningful stress (55% yield) margins shrink with depth and a deep
    flaw with modest toughness lands outside the envelope.  (At low stress a
    deep flaw can legitimately pass the static FAD — that is exactly why the
    practice depth caps exist on top of it.)"""
    kw = dict(sigma_m=300.0, sigma_b=60.0, kmat=60.0)
    margins = [_assess(a, length=80.0, **kw).margin for a in (2.0, 6.0, 12.0)]
    assert margins == sorted(margins, reverse=True)
    assert not _assess(15.0, length=80.0, **kw).acceptable


def test_charpy_fallback_is_more_conservative_than_tested_toughness():
    tested = _assess(3.0, length=40.0, kmat=120.0)
    fallback = assess_crack_like_flaw(
        a_mm=3.0, c_mm=20.0, t_mm=T_RISER,
        sigma_m_mpa=150.0, sigma_b_mpa=30.0,
        sigma_y_mpa=SY, sigma_u_mpa=SU, cvn_joules=40.0,
    )
    assert fallback.k_mat < tested.k_mat
    assert fallback.margin < tested.margin
    assert any("Annex J" in n for n in fallback.notes)


# ---------------------------------------------------------------------------
# Critical-depth envelope (the weld-envelope feeder)
# ---------------------------------------------------------------------------
def test_critical_depth_monotone_in_length_and_toughness():
    """At 55%-yield stress the envelope is interior (not validity-capped):
    longer flaws allow less depth; tougher material allows more."""
    kw = dict(t_mm=T_RISER, sigma_m_mpa=300.0, sigma_b_mpa=60.0,
              sigma_y_mpa=SY, sigma_u_mpa=SU)
    d_short = critical_flaw_depth_mm(length_mm=60.0, k_mat=100.0, **kw)
    d_long = critical_flaw_depth_mm(length_mm=200.0, k_mat=100.0, **kw)
    assert 0.0 < d_long <= d_short
    d_tough = critical_flaw_depth_mm(length_mm=200.0, k_mat=150.0, **kw)
    d_brittle = critical_flaw_depth_mm(length_mm=200.0, k_mat=40.0, **kw)
    assert d_brittle < d_long < d_tough
    # every returned depth is itself acceptable
    r = assess_crack_like_flaw(a_mm=d_long, c_mm=100.0,
                               t_mm=T_RISER, sigma_m_mpa=300.0,
                               sigma_b_mpa=60.0, sigma_y_mpa=SY,
                               sigma_u_mpa=SU, k_mat=100.0)
    assert r.acceptable
