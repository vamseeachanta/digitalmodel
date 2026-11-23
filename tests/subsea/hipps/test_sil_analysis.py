"""Tests for HIPPS SIL reliability analysis (IEC 61508 / 61511)."""

import math

import pytest

from digitalmodel.subsea.hipps.sil_analysis import (
    SIL,
    classify_sil,
    pfd_avg_1oo1,
    pfd_avg_1oo2,
    risk_reduction_factor,
)


# --------------------------------------------------------------------------- #
# Worked example (hand-verified against IEC 61508-6 eq. PFDavg = lambda*ti/2)
#
#   Single HIPPS channel, lambda_du = 5e-7 /h, proof-test interval ti = 8760 h
#   PFDavg(1oo1) = 5e-7 * 8760 / 2 = 2.19e-3  -> in [1e-3, 1e-2) -> SIL 2
#   RRF = 1 / 2.19e-3 = 456.62
# --------------------------------------------------------------------------- #
def test_1oo1_worked_example_sil2():
    pfd = pfd_avg_1oo1(lambda_du=5e-7, ti=8760.0)
    assert pfd == pytest.approx(2.19e-3, rel=1e-9)

    res = classify_sil(pfd, target_sil=SIL.SIL2)
    assert res.sil is SIL.SIL2
    assert res.rrf == pytest.approx(1.0 / 2.19e-3, rel=1e-9)
    assert res.meets_target is True


def test_1oo2_worked_example_sil3():
    # Dual-redundant valves, per-channel lambda_du=5e-7, ti=8760, beta=5%.
    # PFDavg = ((0.95*5e-7)^2 * 8760^2)/3 + 0.05*5e-7*8760/2 = 1.15271e-4 -> SIL 3
    pfd = pfd_avg_1oo2(lambda_du=5e-7, ti=8760.0, beta=0.05)
    assert pfd == pytest.approx(1.15271307e-4, rel=1e-6)

    res = classify_sil(pfd)
    assert res.sil is SIL.SIL3
    # Redundancy + small beta gives a far higher RRF than the 1oo1 channel.
    assert res.rrf > risk_reduction_factor(pfd_avg_1oo1(5e-7, 8760.0))


def test_target_not_met_is_fail():
    # A weak SIF (PFDavg ~ 5e-2 = SIL 1) cannot meet a SIL 3 target.
    res = classify_sil(5e-2, target_sil=SIL.SIL3)
    assert res.sil is SIL.SIL1
    assert res.meets_target is False


def test_below_sil1_is_none():
    # PFDavg at/above 0.1 qualifies for no SIL band.
    res = classify_sil(0.1)
    assert res.sil is SIL.NONE
    res2 = classify_sil(0.5)
    assert res2.sil is SIL.NONE


@pytest.mark.parametrize(
    "pfd, expected",
    [
        (9.9e-2, SIL.SIL1),   # just inside SIL 1 (1e-2 <= PFD < 1e-1)
        (1.0e-2, SIL.SIL1),   # boundary: lower bound of SIL 1 band
        (1.0e-3, SIL.SIL2),   # boundary: lower bound of SIL 2 band
        (1.0e-4, SIL.SIL3),   # boundary: lower bound of SIL 3 band
        (1.0e-5, SIL.SIL4),   # boundary: lower bound of SIL 4 band
        (5.0e-6, SIL.SIL4),   # below SIL 4 floor -> capped at SIL 4
    ],
)
def test_band_boundaries(pfd, expected):
    assert classify_sil(pfd).sil is expected


def test_rrf_is_inverse_of_pfd():
    assert risk_reduction_factor(1e-3) == pytest.approx(1000.0)


@pytest.mark.parametrize("bad", [0.0, -1e-6, 1.5])
def test_invalid_pfd_raises(bad):
    with pytest.raises(ValueError):
        classify_sil(bad)
    with pytest.raises(ValueError):
        risk_reduction_factor(bad)


def test_invalid_rates_raise():
    with pytest.raises(ValueError):
        pfd_avg_1oo1(lambda_du=0.0, ti=8760.0)
    with pytest.raises(ValueError):
        pfd_avg_1oo1(lambda_du=5e-7, ti=0.0)
    with pytest.raises(ValueError):
        pfd_avg_1oo2(lambda_du=5e-7, ti=8760.0, beta=1.5)


def test_beta_zero_is_independent_only():
    # With beta=0 the common-cause term vanishes; result is the pure 1oo2 series.
    pfd = pfd_avg_1oo2(lambda_du=5e-7, ti=8760.0, beta=0.0)
    expected = (5e-7) ** 2 * 8760.0**2 / 3.0
    assert pfd == pytest.approx(expected, rel=1e-9)
    assert not math.isnan(pfd)
