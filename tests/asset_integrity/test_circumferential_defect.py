# ABOUTME: Tests for circumferential / combined-loading metal-loss FFS — API 579-1
# ABOUTME: Part 5 longitudinal RSF + circumferential net-section golden values; DNV combined-loading stub.
import math

import pytest

from digitalmodel.asset_integrity.circumferential_defect import (
    api579_part5_circumferential_netsection,
    api579_part5_longitudinal_rsf,
    dnv_f101_combined_loading,
    part5_folias_mt,
    part5_shell_parameter,
)
from digitalmodel.asset_integrity.dnv_rp_f101 import dnv_f101_single_defect


# ===========================================================================
# 1) API 579-1 Part 5 longitudinal LTA RSF — fully verified golden
#    D=39, Tc=0.5, s=6, tmm=0.30 (Rt=0.60)
#    -> lambda=1.746, M_t=1.2255, RSF=0.8907 (FAIL), MAWP_r = MAWP * 0.9897
# ===========================================================================
def test_part5_longitudinal_rsf_golden():
    res = api579_part5_longitudinal_rsf(D=39.0, Tc=0.5, s=6.0, tmm=0.30)
    assert res.shell_parameter == pytest.approx(1.746, abs=5e-4)
    assert res.folias_Mt == pytest.approx(1.2255, abs=5e-4)
    assert res.Rt == pytest.approx(0.60, rel=1e-12)
    assert res.rsf == pytest.approx(0.8907, abs=5e-4)
    assert res.acceptable is False  # 0.8907 < 0.90
    assert res.method == "API579-Part5-longitudinal"


def test_part5_longitudinal_mawp_reduction_golden():
    res = api579_part5_longitudinal_rsf(
        D=39.0, Tc=0.5, s=6.0, tmm=0.30, MAWP_psi=1000.0
    )
    # MAWP_r = MAWP * RSF/RSF_a = 1000 * 0.8907/0.90 = 989.7
    assert res.mawp_reduced_psi == pytest.approx(0.9897 * 1000.0, abs=0.5)
    assert res.mawp_reduced_psi == pytest.approx(
        res.mawp_psi * res.rsf / 0.90, rel=1e-12
    )


def test_part5_shell_parameter_golden():
    lam = part5_shell_parameter(D=39.0, Tc=0.5, s=6.0)
    assert lam == pytest.approx(1.285 * 6.0 / math.sqrt(39.0 * 0.5), rel=1e-12)
    assert lam == pytest.approx(1.746, abs=5e-4)


def test_part5_folias_mt_unit_at_zero():
    # lambda=0 -> M_t = sqrt(a0) = sqrt(1.0010) ~ 1.0005 (flat plate limit).
    assert part5_folias_mt(0.0) == pytest.approx(math.sqrt(1.0010), rel=1e-12)


def test_part5_folias_mt_monotonic_increasing():
    prev = part5_folias_mt(0.0)
    for lam in (0.5, 1.0, 2.0, 4.0, 8.0, 15.0):
        cur = part5_folias_mt(lam)
        assert cur > prev
        prev = cur


def test_part5_rsf_accepts_shallow_flaw():
    # Shallow flaw (Rt high) should pass RSF_a = 0.90.
    res = api579_part5_longitudinal_rsf(D=39.0, Tc=0.5, s=6.0, tmm=0.47)
    assert res.Rt == pytest.approx(0.94, rel=1e-12)
    assert res.rsf >= 0.90
    assert res.acceptable is True
    assert res.mawp_reduced_psi is None


def test_part5_rt_uses_fca():
    res = api579_part5_longitudinal_rsf(D=39.0, Tc=0.5, s=6.0, tmm=0.30, FCA=0.05)
    assert res.Rt == pytest.approx((0.30 - 0.05) / 0.5, rel=1e-12)


def test_part5_lambda_validity_limit():
    with pytest.raises(ValueError):
        part5_folias_mt(20.0001)
    # A flaw long enough to exceed lambda=20 should raise on RSF as well.
    with pytest.raises(ValueError):
        api579_part5_longitudinal_rsf(D=39.0, Tc=0.5, s=80.0, tmm=0.30)


# ===========================================================================
# 2) API 579-1 Part 5 circumferential extent — net-section membrane RSF
#    d/t=0.40, 90deg arc (c=31.0 in, Dm=39.5) -> RSF_circ = 0.9001
# ===========================================================================
def test_circumferential_netsection_golden():
    # d/t = 0.40 -> with t=0.5, d=0.20.
    res = api579_part5_circumferential_netsection(
        Dm=39.5, t=0.5, d=0.20, c=31.0, sigma_flow_psi=50_000.0
    )
    assert res.rsf_netsection == pytest.approx(0.9001, abs=5e-4)
    assert res.details["d_over_t"] == pytest.approx(0.40, rel=1e-12)
    assert res.allowable_axial_stress_psi == pytest.approx(0.9001 * 50_000.0, abs=30.0)
    assert res.method == "API579-Part5-circumferential-net-section"
    assert "net-section membrane" in res.details["solution_type"]


def test_circumferential_netsection_formula():
    Dm, t, d, c = 39.5, 0.5, 0.20, 31.0
    expect = 1.0 - (d / t) * (c / (math.pi * Dm))
    res = api579_part5_circumferential_netsection(
        Dm=Dm, t=t, d=d, c=c, sigma_flow_psi=1.0
    )
    assert res.rsf_netsection == pytest.approx(expect, rel=1e-12)


def test_circumferential_full_circumference_depth_limit():
    # c = full circumference, d/t=0.40 -> RSF = 1 - 0.40 = 0.60.
    Dm, t, d = 39.5, 0.5, 0.20
    res = api579_part5_circumferential_netsection(
        Dm=Dm, t=t, d=d, c=math.pi * Dm, sigma_flow_psi=1.0
    )
    assert res.rsf_netsection == pytest.approx(0.60, rel=1e-9)


def test_circumferential_zero_depth_is_full_strength():
    res = api579_part5_circumferential_netsection(
        Dm=39.5, t=0.5, d=0.0, c=31.0, sigma_flow_psi=50_000.0
    )
    assert res.rsf_netsection == pytest.approx(1.0, rel=1e-12)
    assert res.allowable_axial_stress_psi == pytest.approx(50_000.0, rel=1e-12)


def test_circumferential_level1_screen_flag():
    # Screen c <= 2 s (E_L/E_C); seamless E_L=E_C=1.
    ok = api579_part5_circumferential_netsection(
        Dm=39.5, t=0.5, d=0.20, c=31.0, sigma_flow_psi=1.0, s=20.0
    )
    assert ok.level1_screen_ok is True  # 31.0 <= 40.0
    fail = api579_part5_circumferential_netsection(
        Dm=39.5, t=0.5, d=0.20, c=31.0, sigma_flow_psi=1.0, s=10.0
    )
    assert fail.level1_screen_ok is False  # 31.0 > 20.0
    none = api579_part5_circumferential_netsection(
        Dm=39.5, t=0.5, d=0.20, c=31.0, sigma_flow_psi=1.0
    )
    assert none.level1_screen_ok is None


def test_circumferential_deeper_lowers_rsf():
    prev = 1.1
    for d in (0.05, 0.10, 0.20, 0.40, 0.49):
        res = api579_part5_circumferential_netsection(
            Dm=39.5, t=0.5, d=d, c=31.0, sigma_flow_psi=1.0
        )
        assert res.rsf_netsection < prev
        prev = res.rsf_netsection


def test_circumferential_validation():
    with pytest.raises(ValueError):
        api579_part5_circumferential_netsection(
            Dm=39.5, t=0.5, d=0.6, c=31.0, sigma_flow_psi=1.0
        )  # d > t
    with pytest.raises(ValueError):
        api579_part5_circumferential_netsection(
            Dm=39.5, t=0.5, d=0.2, c=200.0, sigma_flow_psi=1.0
        )  # c > circumference
    with pytest.raises(ValueError):
        api579_part5_circumferential_netsection(
            Dm=-1.0, t=0.5, d=0.2, c=31.0, sigma_flow_psi=1.0
        )  # Dm <= 0


# ===========================================================================
# 3) DNV-RP-F101 combined loading — STUB (no fabricated H1)
# ===========================================================================
def test_combined_loading_is_not_implemented():
    with pytest.raises(NotImplementedError):
        dnv_f101_combined_loading(
            D=30.0,
            t=0.375,
            d=0.15,
            L=8.0,
            c=2.0,
            smts_psi=66_700.0,
            sigma_L_psi=-27_557.0,
        )


def test_combined_loading_pure_pressure_base_matches_single_defect():
    # The stub computes (and the message embeds) the real pure-pressure base;
    # confirm that base equals dnv_f101_single_defect before it raises.
    D, t, d, L, smts = 30.0, 0.375, 0.15, 8.0, 66_700.0
    base = dnv_f101_single_defect(D, t, d, L, smts).capacity_pressure_psi
    with pytest.raises(NotImplementedError) as exc:
        dnv_f101_combined_loading(
            D=D, t=t, d=d, L=L, c=2.0, smts_psi=smts, sigma_L_psi=-1000.0
        )
    assert f"{base:.2f}" in str(exc.value)
