# ABOUTME: Golden-row tests for the analytical tank-sloshing engine.

"""Tests for digitalmodel.hydrodynamics.sloshing (analytical sloshing screen)."""

import math

import pytest

from digitalmodel.hydrodynamics import sloshing as s


def test_rectangular_fundamental_period_golden():
    # L = 40 m, fill = 20 m; linear potential-flow first antisymmetric mode.
    periods = s.rectangular_tank_periods(length=40.0, fill_depth=20.0, n_modes=3)
    assert periods[0] == pytest.approx(7.4757, abs=1e-3)
    # modes are ordered longest-first
    assert periods[0] > periods[1] > periods[2]


def test_rectangular_deep_water_asymptote():
    # h >> L => tanh -> 1 => T1 -> 2*pi/sqrt(g*pi/L)
    t1 = s.rectangular_tank_periods(length=40.0, fill_depth=400.0, n_modes=1)[0]
    deep = 2.0 * math.pi / math.sqrt(s.G_STD * math.pi / 40.0)
    assert t1 == pytest.approx(deep, rel=1e-6)


def test_cylindrical_fundamental_period_golden():
    # D = 40 m, fill = 20 m; m=1 family, eps_11 = 1.8412.
    periods = s.cylindrical_tank_periods(diameter=40.0, fill_depth=20.0, n_modes=3)
    assert periods[0] == pytest.approx(6.7814, abs=1e-3)


def test_api650_convective_period_golden():
    # API 650 Annex E: Tc = 1.8 Ks sqrt(D), Ks = 0.578/sqrt(tanh(3.68 H/D)).
    tc = s.api650_convective_period(diameter=40.0, fill_height=20.0)
    assert tc == pytest.approx(6.7482, abs=1e-3)


def test_horizontal_cylinder_halffull_golden():
    # Equivalent-rectangle: half-full circle -> Omega^2 = omega^2 R/g = 1.325
    # (McIver benchmark; D = 40 m -> fundamental period 7.795 s).
    T = s.horizontal_cylinder_periods(diameter=40.0, fill_fraction=0.5, n_modes=1)[0]
    assert T == pytest.approx(7.795, abs=2e-3)
    omega_sq = (2 * math.pi / T) ** 2
    assert omega_sq * 20.0 / s.G_STD == pytest.approx(1.325, abs=2e-3)


def test_oval_reduces_to_horizontal_cylinder():
    # width == height == diameter must equal the horizontal-cylinder case.
    a = s.oval_tank_periods(width=40.0, height=40.0, fill_fraction=0.5, n_modes=1)[0]
    b = s.horizontal_cylinder_periods(diameter=40.0, fill_fraction=0.5, n_modes=1)[0]
    assert a == pytest.approx(b, rel=1e-9)


def test_oval_midfill_accuracy():
    # Elliptical AR=1.49 (width 0.52, height 0.35): equivalent-rectangle is
    # within a few % of the reference frequency at mid fill (F=0.7 -> ~1.209 Hz).
    T = s.oval_tank_periods(width=0.52, height=0.35, fill_fraction=0.7, n_modes=1)[0]
    assert (1.0 / T) == pytest.approx(1.209, rel=0.05)


def test_oval_fill_fraction_bounds():
    with pytest.raises(ValueError):
        s.oval_tank_periods(width=2.0, height=1.0, fill_fraction=0.0)
    with pytest.raises(ValueError):
        s.oval_tank_periods(width=2.0, height=1.0, fill_fraction=1.0)


def test_api650_agrees_with_potential_flow():
    # Cross-validation: API 650 convective period ~ potential-flow fundamental (<1%).
    tc = s.api650_convective_period(40.0, 20.0)
    t1 = s.cylindrical_tank_periods(40.0, 20.0, n_modes=1)[0]
    assert abs(tc - t1) / t1 < 0.01


def test_resonance_flag():
    # A 6.8 s excitation (near the cylindrical fundamental) must trip resonance.
    res = s.screen_tank(
        geometry="cylindrical", diameter=40.0, fill_depth=20.0,
        excitation_periods=[6.8], resonance_tolerance=0.15,
    )
    assert res.resonance is True
    assert res.resonant_periods and res.resonant_periods[0] == pytest.approx(6.7814, abs=1e-3)

    # A far-off excitation must NOT resonate.
    calm = s.screen_tank(
        geometry="cylindrical", diameter=40.0, fill_depth=20.0,
        excitation_periods=[15.0], resonance_tolerance=0.15,
    )
    assert calm.resonance is False


def test_router_serialises_plain_dict():
    out = s.router({"sloshing": {
        "geometry": "cylindrical", "diameter": 40.0, "fill_depth": 20.0,
        "excitation_periods": [12.0],
    }})
    assert out["geometry"] == "cylindrical"
    assert out["fundamental_period"] == pytest.approx(6.7814, abs=1e-3)
    assert out["convective_period"] == pytest.approx(6.7482, abs=1e-3)
    assert out["resonance"] is False


def test_input_validation():
    with pytest.raises(ValueError):
        s.rectangular_tank_periods(length=-1.0, fill_depth=20.0)
    with pytest.raises(ValueError):
        s.screen_tank(geometry="rectangular", fill_depth=20.0)  # missing length
    with pytest.raises(ValueError):
        s.screen_tank(geometry="spherical", fill_depth=20.0, diameter=10.0)
