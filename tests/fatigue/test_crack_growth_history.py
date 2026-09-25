"""Tests for fatigue crack growth from a DeltaK(a) history (#2157 P1).

Comparator classes: closed-form Paris integration, invariants (power-law scaling,
threshold arrest) and contracts (required inputs). Inputs are generic, not taken from
any published case (plan card S04).
"""

import pytest

from digitalmodel.fatigue.crack_growth_history import (
    GrowthLaw,
    TabulatedDeltaK,
    Threshold,
    back_calculate_dk,
    convert_coefficient_n_mm_to_mpa_sqrt_m,
    cycles_to_extension,
    dk_envelope,
    dk_multiplier_to_demand,
    dk_sqrt_model,
    life,
    life_sqrt_closed_form,
    threshold_margin,
)

LAW = GrowthLaw(
    A=2e-8, m=3.0, basis="generic test law (A in mm/cycle, dK in MPa*sqrt(m))"
)


# --- law objects [contract] -------------------------------------------------------
def test_law_requires_basis():
    with pytest.raises(ValueError):
        GrowthLaw(A=2e-8, m=3.0, basis="")


def test_law_rejects_non_positive_constants():
    with pytest.raises(ValueError):
        GrowthLaw(A=0.0, m=3.0, basis="x")
    with pytest.raises(ValueError):
        GrowthLaw(A=1e-8, m=-1.0, basis="x")


def test_e_ratio_correction():
    law = GrowthLaw(A=1.65e-8, m=3.0, basis="b").with_e_ratio(200.0, 180.0)
    assert law.A == pytest.approx(1.65e-8 * (200.0 / 180.0) ** 3, rel=1e-12)
    assert "E-ratio" in law.basis


def test_unit_conversion_n_mm_to_mpa_sqrt_m():
    # Conversion only; no standard constant ships (G05).
    assert convert_coefficient_n_mm_to_mpa_sqrt_m(5.21e-13, 3.0) == pytest.approx(
        1.647547e-8, rel=1e-6
    )


# --- closed forms [closed-form] -----------------------------------------------------
def test_life_constant_dk():
    res = life(LAW, lambda a: 5.0, 1.0, 3.0, threshold=None)
    assert res.status == "GROWS"
    assert res.cycles == pytest.approx(800_000.0, rel=1e-9)


def test_life_sqrt_closed_form():
    n = life_sqrt_closed_form(LAW, dk0=5.0, a0_mm=1.0, af_mm=3.0)
    assert n == pytest.approx(338_119.784648, rel=1e-9)


def test_integrator_matches_closed_form():
    res = life(LAW, dk_sqrt_model(5.0, 1.0), 1.0, 3.0, threshold=None)
    assert res.cycles == pytest.approx(338_119.784648, rel=1e-7)


def test_life_scales_as_k_power_minus_m():
    base = life(LAW, dk_sqrt_model(5.0, 1.0), 1.0, 3.0, threshold=None).cycles
    k = 1.7
    scaled = life(
        LAW, lambda a: k * dk_sqrt_model(5.0, 1.0)(a), 1.0, 3.0, threshold=None
    ).cycles
    assert scaled == pytest.approx(base / k**3, rel=1e-9)


# --- threshold [invariant + contract] -----------------------------------------------
def test_threshold_rule_required():
    with pytest.raises(TypeError):
        Threshold(2.0)  # rule has no default
    with pytest.raises(ValueError):
        Threshold(2.0, temperature_rule="sometimes")


def test_threshold_e_ratio_requires_moduli():
    with pytest.raises(ValueError):
        Threshold(2.0, temperature_rule="e_ratio")
    t = Threshold(2.0, temperature_rule="e_ratio", e_ref_gpa=200.0, e_t_gpa=180.0)
    assert t.effective == pytest.approx(1.8, rel=1e-12)
    assert Threshold(2.0, temperature_rule="none").effective == 2.0


def test_below_threshold_everywhere_returns_arrested():
    res = life(
        LAW, lambda a: 1.5, 1.0, 3.0, threshold=Threshold(2.0, temperature_rule="none")
    )
    assert res.status == "ARRESTED"
    assert res.cycles is None
    assert res.a_arrest_mm == pytest.approx(1.0)


def test_threshold_crossing_inside_interval_returns_arrested():
    # dK falls linearly from 3.0 at a = 1 to 1.0 at a = 3; crosses 2.0 at a = 2.
    dk = lambda a: 3.0 - (a - 1.0)  # noqa: E731
    res = life(LAW, dk, 1.0, 3.0, threshold=Threshold(2.0, temperature_rule="none"))
    assert res.status == "ARRESTED"
    assert res.cycles is None
    assert res.a_arrest_mm == pytest.approx(2.0, abs=2e-3)


def test_non_positive_dk_without_threshold_raises_not_truncates():
    with pytest.raises(ValueError):
        life(LAW, lambda a: 0.0, 1.0, 3.0, threshold=None)


def test_threshold_margin():
    m = threshold_margin(
        dk_sqrt_model(5.0, 1.0), 1.0, 3.0, Threshold(2.0, temperature_rule="none")
    )
    assert m == pytest.approx(1.0 - 2.0 / 5.0, rel=1e-9)  # minimum dK is at a0


# --- sensitivities [closed-form + contract] -----------------------------------------
def test_multiplier_closed_form_without_threshold():
    n = life(LAW, dk_sqrt_model(5.0, 1.0), 1.0, 3.0, threshold=None).cycles
    k = dk_multiplier_to_demand(
        LAW, dk_sqrt_model(5.0, 1.0), 1.0, 3.0, n_demand=20_000.0, threshold=None
    )
    assert k == pytest.approx((n / 20_000.0) ** (1.0 / 3.0), rel=1e-9)


def test_multiplier_numerical_with_threshold_hits_demand():
    thr = Threshold(0.5, temperature_rule="none")
    k = dk_multiplier_to_demand(
        LAW, dk_sqrt_model(5.0, 1.0), 1.0, 3.0, n_demand=20_000.0, threshold=thr
    )
    n_k = life(
        LAW, lambda a: k * dk_sqrt_model(5.0, 1.0)(a), 1.0, 3.0, threshold=thr
    ).cycles
    assert n_k == pytest.approx(20_000.0, rel=1e-6)


# --- DeltaK models [contract + invariant] --------------------------------------------
def test_tabulated_interpolation_and_default_raise():
    t = TabulatedDeltaK([1.0, 2.0, 3.0], [2.0, 3.0, 5.0])
    assert t(1.5) == pytest.approx(2.5)
    assert t(2.5) == pytest.approx(4.0)
    with pytest.raises(ValueError):
        t(3.5)


def test_tabulated_explicit_extrapolation():
    hold = TabulatedDeltaK([1.0, 2.0], [2.0, 3.0], extrapolation="hold")
    lin = TabulatedDeltaK([1.0, 2.0], [2.0, 3.0], extrapolation="linear")
    assert hold(2.5) == pytest.approx(3.0)
    assert lin(2.5) == pytest.approx(3.5)


def test_tabulated_requires_increasing_depths():
    with pytest.raises(ValueError):
        TabulatedDeltaK([1.0, 1.0, 2.0], [1.0, 2.0, 3.0])


def test_envelope_is_pointwise_max():
    f = dk_envelope(lambda a: a, lambda a: 4.0 - a)
    assert f(1.0) == 3.0
    assert f(3.0) == 3.0
    assert f(2.0) == 2.0


# --- automated-growth history checks [closed-form on a synthetic history] ------------
def _history_from_constant_dk(dk, steps):
    rate = LAW.A * dk**LAW.m
    return [(n, n * rate) for n in steps]


def test_back_calculated_dk():
    hist = _history_from_constant_dk(4.0, [0, 100_000, 250_000, 400_000])
    for dk in back_calculate_dk(hist, LAW):
        assert dk == pytest.approx(4.0, rel=1e-12)


def test_cycles_to_extension():
    hist = [(0, 0.0), (1000, 0.2), (3000, 1.0)]
    assert cycles_to_extension(hist, 0.6) == pytest.approx(2000.0, rel=1e-12)
    with pytest.raises(ValueError):
        cycles_to_extension(hist, 1.5)
