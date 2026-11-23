"""Tests for the HIPPS overpressure-protection envelope (API 17O / IEC 61511)."""

import pytest

from digitalmodel.subsea.hipps.overpressure import overpressure_envelope


# --------------------------------------------------------------------------- #
# Worked example (hand-verified against eq. (2): p_at_close = p0 + dpdt*t_resp)
#
#   p_initial = 15 MPa, p_design = 34.5 MPa (5000 psi class), dpdt = 0.5 MPa/s
#   t_detect = 0.3 s, t_logic = 0.1 s, t_close = 2.0 s  -> t_response = 2.4 s
#   p_at_close = 15 + 0.5*2.4 = 16.2 MPa  <= 34.5  -> PASS, margin = 18.3 MPa
# --------------------------------------------------------------------------- #
def test_within_envelope_passes():
    res = overpressure_envelope(
        p_initial=15.0,
        p_design=34.5,
        dpdt=0.5,
        t_close=2.0,
        t_detect=0.3,
        t_logic=0.1,
    )
    assert res.t_response == pytest.approx(2.4)
    assert res.p_at_close == pytest.approx(16.2)
    assert res.margin == pytest.approx(18.3)
    assert res.utilisation == pytest.approx(16.2 / 34.5)
    assert res.passed is True


def test_fast_surge_breaches_envelope():
    # A 15 MPa/s surge over the same 2.4 s response window adds 36 MPa.
    res = overpressure_envelope(
        p_initial=15.0,
        p_design=34.5,
        dpdt=15.0,
        t_close=2.0,
        t_detect=0.3,
        t_logic=0.1,
    )
    assert res.p_at_close == pytest.approx(51.0)
    assert res.margin == pytest.approx(-16.5)
    assert res.utilisation > 1.0
    assert res.passed is False


def test_boundary_exactly_at_design_passes():
    # p_at_close == p_design is acceptable (<=): p0 + 0.5*2.4 = 16.2 == p_design.
    res = overpressure_envelope(
        p_initial=15.0,
        p_design=16.2,
        dpdt=0.5,
        t_close=2.0,
        t_detect=0.3,
        t_logic=0.1,
    )
    assert res.p_at_close == pytest.approx(16.2)
    assert res.utilisation == pytest.approx(1.0)
    assert res.margin == pytest.approx(0.0, abs=1e-12)
    assert res.passed is True


def test_default_delays_zero():
    # Without detect/logic delays, t_response == t_close.
    res = overpressure_envelope(p_initial=10.0, p_design=20.0, dpdt=1.0, t_close=3.0)
    assert res.t_response == pytest.approx(3.0)
    assert res.p_at_close == pytest.approx(13.0)


@pytest.mark.parametrize(
    "kwargs",
    [
        dict(p_initial=10.0, p_design=0.0, dpdt=1.0, t_close=1.0),   # p_design <= 0
        dict(p_initial=10.0, p_design=20.0, dpdt=-1.0, t_close=1.0),  # dpdt < 0
        dict(p_initial=10.0, p_design=20.0, dpdt=1.0, t_close=-1.0),  # t_close < 0
    ],
)
def test_invalid_inputs_raise(kwargs):
    with pytest.raises(ValueError):
        overpressure_envelope(**kwargs)
