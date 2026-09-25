"""Tests for FAD curves, Lr cut-off rules and the load margin to the envelope (#2157 P1).

Comparator classes (per the plan): closed-form evaluation of the published curve
equations, closed-form ray/cut-off geometry, and invariants. Inputs are generic.
"""

import math

import pytest

from digitalmodel.asset_integrity.assessment.crack_fad import fad_curve_option1
from digitalmodel.asset_integrity.assessment.fad_curves import (
    LrMax,
    api579_2016_level2,
    bs7910_2013_option1,
    envelope_margin,
    lr_max,
)


# --- API 579-1:2016 Level 2 curve [closed-form evaluation] -----------------------
@pytest.mark.parametrize(
    "lr, expected",
    [
        (0.0, 1.0),
        (0.5, 0.9581741742),
        (1.0, 0.5722715576),
        (1.5, 0.2057919530),
    ],
)
def test_api579_level2_values(lr, expected):
    assert api579_2016_level2(lr, lr_cut=2.0) == pytest.approx(expected, abs=1e-9)


def test_api579_level2_zero_beyond_cutoff():
    assert api579_2016_level2(1.81, lr_cut=1.80) == 0.0
    assert api579_2016_level2(1.80, lr_cut=1.80) > 0.0


def test_api579_level2_rejects_negative_lr():
    with pytest.raises(ValueError):
        api579_2016_level2(-0.1, lr_cut=2.0)


def test_bs7910_2013_option1_delegates_to_crack_fad():
    # One implementation per curve: the edition-named function must be crack_fad's.
    for lr in (0.3, 0.9, 1.2):
        assert bs7910_2013_option1(lr, 300.0, 500.0, 200_000.0) == fad_curve_option1(
            lr, 300.0, 500.0, 200_000.0
        )


def test_implemented_option1_differs_from_level2():
    # Closed-form comparison of the two implemented formulas; valid whatever the
    # edition attribution turns out to be (R01).
    lr = 1.2
    api = api579_2016_level2(lr, lr_cut=1.5)
    bs = bs7910_2013_option1(lr, 300.0, 500.0, 200_000.0)
    assert abs(api - bs) > 0.005


# --- Lr cut-off rules [closed-form + contract] ------------------------------------
def test_lr_max_flow_rule():
    r = lr_max("flow", sigma_y_mpa=300.0, sigma_u_mpa=500.0)
    assert isinstance(r, LrMax)
    assert r.value == pytest.approx(1.3333333333, abs=1e-9)
    assert r.rule == "flow"


def test_lr_max_fixed_requires_basis():
    with pytest.raises(ValueError):
        lr_max("fixed", value=1.8)
    with pytest.raises(ValueError):
        lr_max("fixed", value=1.8, basis="   ")
    r = lr_max("fixed", value=1.8, basis="stated by the assessment basis")
    assert r.value == 1.8 and r.basis.startswith("stated")


def test_lr_max_flow_requires_both_strengths():
    with pytest.raises(ValueError):
        lr_max("flow", sigma_y_mpa=300.0)


def test_lr_max_unknown_rule():
    with pytest.raises(ValueError):
        lr_max("guess", value=1.0, basis="x")


# --- Envelope margin along the primary-load ray [closed-form + invariant] -------
def _api(lr):
    return api579_2016_level2(lr, lr_cut=1.8)


def test_envelope_margin_curve_contact():
    res = envelope_margin(0.8, 0.5, curve=_api, lr_cut=1.8)
    assert res.mode == "curve"
    assert res.factor == pytest.approx(1.2174353, abs=1e-6)
    # the contact point lies on the curve
    assert res.contact_kr == pytest.approx(_api(res.contact_lr), abs=1e-9)


def test_envelope_margin_cutoff():
    res = envelope_margin(
        1.2, 0.05, curve=lambda x: api579_2016_level2(x, 1.5), lr_cut=1.5
    )
    assert res.mode == "cutoff"
    assert res.factor == pytest.approx(1.25, abs=1e-12)


def test_envelope_margin_holds_secondary_constant():
    # With K_S > 0 the path does not pass through the origin: only primary terms scale.
    kr_s = 0.1
    res = envelope_margin(0.8, 0.4, curve=_api, lr_cut=1.8, kr_secondary=kr_s)
    assert res.contact_kr == pytest.approx(
        res.factor * 0.4 + kr_s, abs=1e-12
    )  # primary scaled, secondary held
    assert res.contact_kr == pytest.approx(_api(res.contact_lr), abs=1e-9)
    # scaling the secondary term too would give a different (wrong) factor
    wrong = envelope_margin(0.8, 0.5, curve=_api, lr_cut=1.8)
    assert not math.isclose(res.factor, wrong.factor, rel_tol=1e-6)


def test_envelope_margin_point_already_outside_gives_factor_below_one():
    res = envelope_margin(0.8, 0.9, curve=_api, lr_cut=1.8)
    assert res.factor < 1.0


def test_envelope_margin_secondary_alone_outside_gives_zero():
    res = envelope_margin(0.5, 0.1, curve=_api, lr_cut=1.8, kr_secondary=1.05)
    assert res.factor == 0.0
