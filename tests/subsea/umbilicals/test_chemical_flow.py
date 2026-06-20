"""Tests for the API 17E umbilical chemical-injection delivery core.

Hand-verified reference values are documented inline; see
docs solution issue-488.md for the worked laminar delivery check.
"""

import math

import pytest

from digitalmodel.subsea.umbilicals import (
    ChemicalFluid,
    UmbilicalTube,
    check_delivery,
    friction_factor,
    friction_pressure_drop,
    mean_velocity,
    reynolds_number,
    static_pressure,
)

# methanol-like inhibitor: rho=850 kg/m^3, mu=0.0012 Pa.s
FLUID = ChemicalFluid(density=850.0, viscosity=0.0012)
# 1/2" bore tube, 3 km developed route
TUBE = UmbilicalTube(inner_diameter=0.0127, length=3000.0)
Q_50LH = 50e-3 / 3600.0  # 50 L/h -> m^3/s


# ─────────────────────────── geometry / kinematics ───────────────────────


def test_bore_area():
    # A = pi/4 d^2 = pi/4 * 0.0127^2 = 1.26677e-4 m^2
    assert TUBE.bore_area == pytest.approx(1.266769e-4, rel=1e-5)


def test_mean_velocity_hand_verified():
    # v = Q/A = 1.388889e-5 / 1.266769e-4 = 0.109640 m/s
    assert mean_velocity(Q_50LH, TUBE) == pytest.approx(0.109640, rel=1e-4)


def test_reynolds_number_hand_verified():
    # Re = rho v d / mu = 850*0.109640*0.0127/0.0012 = 986.306
    assert reynolds_number(Q_50LH, TUBE, FLUID) == pytest.approx(986.306, rel=1e-4)


# ─────────────────────────────── friction factor ─────────────────────────


def test_friction_factor_laminar():
    # f = 64/Re = 64/986.306 = 0.064889
    assert friction_factor(986.306) == pytest.approx(0.064889, rel=1e-4)


def test_friction_factor_turbulent_blasius():
    # Re=11835.67 -> f = 0.3164 * Re^-0.25 = 0.030335
    assert friction_factor(11835.67) == pytest.approx(0.030335, rel=1e-4)


def test_friction_factor_transitional_holds_laminar():
    # 2300 < Re <= 4000 keeps the laminar form: f=64/3000
    assert friction_factor(3000.0) == pytest.approx(64.0 / 3000.0, rel=1e-9)


def test_friction_factor_boundary_4000_vs_4001():
    # at Re=4000 laminar value 64/4000=0.016 is held;
    # just above (4001) Blasius jumps to ~0.03978 (discontinuous by design)
    assert friction_factor(4000.0) == pytest.approx(0.016, rel=1e-9)
    assert friction_factor(4001.0) == pytest.approx(0.039783, rel=1e-4)
    assert friction_factor(4001.0) > friction_factor(4000.0)


# ───────────────────────────── pressure drop ─────────────────────────────


def test_friction_pressure_drop_hand_verified():
    # dP = f (L/d)(rho v^2/2) = 0.064889*(3000/0.0127)*(850*0.10964^2/2)
    #    = 78309.64 Pa
    assert friction_pressure_drop(Q_50LH, TUBE, FLUID) == pytest.approx(
        78309.64, rel=1e-4
    )


def test_static_pressure():
    # dP_static = rho g H = 850*9.81*1500 = 12_507_750 Pa
    assert static_pressure(FLUID, 1500.0) == pytest.approx(12_507_750.0, rel=1e-6)


# ──────────────────────────── delivery feasibility ───────────────────────


def test_check_delivery_feasible_hand_verified():
    # P_required = P_inj + dP_friction - dP_static
    #            = 200e5 + 78309.64 - 12_507_750 = 7_570_559.64 Pa
    # U = 7_570_559.64 / 250e5 = 0.302822 -> feasible
    res = check_delivery(
        flow_rate=Q_50LH,
        tube=TUBE,
        fluid=FLUID,
        available_pressure=250e5,
        injection_pressure=200e5,
        vertical_drop=1500.0,
    )
    assert res.flow_regime == "laminar"
    assert res.required_pressure == pytest.approx(7_570_559.64, rel=1e-4)
    assert res.utilisation == pytest.approx(0.302822, rel=1e-4)
    assert res.is_feasible


def test_check_delivery_infeasible_over_limit():
    # No hydrostatic assist, low available pressure, high back-pressure:
    # P_required = 100e5 + 78309.64 - 0 = 10_078_309.64 Pa
    # available 50 bar = 5e6 Pa -> U > 1 -> not feasible
    res = check_delivery(
        flow_rate=Q_50LH,
        tube=TUBE,
        fluid=FLUID,
        available_pressure=50e5,
        injection_pressure=100e5,
        vertical_drop=0.0,
    )
    assert res.utilisation == pytest.approx(10_078_309.64 / 5e6, rel=1e-4)
    assert res.utilisation > 1.0
    assert not res.is_feasible


def test_check_delivery_boundary_unit_utilisation():
    # Set available exactly equal to required -> U == 1.0 -> feasible (<=).
    res0 = check_delivery(
        flow_rate=Q_50LH,
        tube=TUBE,
        fluid=FLUID,
        available_pressure=1e7,  # placeholder, recompute against required
        injection_pressure=100e5,
        vertical_drop=0.0,
    )
    res = check_delivery(
        flow_rate=Q_50LH,
        tube=TUBE,
        fluid=FLUID,
        available_pressure=res0.required_pressure,
        injection_pressure=100e5,
        vertical_drop=0.0,
    )
    assert res.utilisation == pytest.approx(1.0, rel=1e-9)
    assert res.is_feasible


def test_turbulent_regime_classification():
    # Larger bore-rate combination pushes Re > 4000 -> turbulent regime.
    tube = UmbilicalTube(inner_diameter=0.00635, length=2000.0)
    q = 300e-3 / 3600.0  # 300 L/h
    res = check_delivery(
        flow_rate=q,
        tube=tube,
        fluid=FLUID,
        available_pressure=300e5,
        injection_pressure=150e5,
    )
    assert res.reynolds > 4000.0
    assert res.flow_regime == "turbulent"


# ──────────────────────────────── validation ─────────────────────────────


def test_invalid_fluid():
    with pytest.raises(ValueError):
        ChemicalFluid(density=0.0, viscosity=0.001)
    with pytest.raises(ValueError):
        ChemicalFluid(density=850.0, viscosity=0.0)


def test_invalid_tube():
    with pytest.raises(ValueError):
        UmbilicalTube(inner_diameter=0.0, length=100.0)
    with pytest.raises(ValueError):
        UmbilicalTube(inner_diameter=0.01, length=-1.0)


def test_invalid_available_pressure():
    with pytest.raises(ValueError):
        check_delivery(Q_50LH, TUBE, FLUID, available_pressure=0.0)
