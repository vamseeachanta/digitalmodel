"""Fender performance helpers and one-at-a-time sweeps (hand-checked values).

Fixture fenders and curves are fictional; no manufacturer data is used.
"""

from __future__ import annotations

import math

import pytest

from digitalmodel.hydrodynamics.berthing_energy import FenderRating, berthing_energy
from digitalmodel.hydrodynamics.fender_design import (
    PerformanceCurve,
    design_performance,
    parameter_sweep,
    reaction_at_energy,
    tornado,
)

FENDER = FenderRating(
    fender_id="GEN-PN-3", manufacturer="generic", fender_type="pneumatic",
    diameter_mm=2200, length_mm=4000, internal_pressure_kpa=50,
    rated_energy_knm=400.0, reaction_kn=1100.0, hull_pressure_kpa=135.0,
    rated_deflection_pct=60.0, source="fictional test row",
)
# fictional normalised curve: (deflection %, reaction %, energy %)
CURVE = PerformanceCurve(
    "fictional pneumatic",
    ((0, 0, 0), (20, 20, 10), (40, 50, 40), (60, 100, 100)),
    "fictional test curve",
)


def test_reaction_at_energy_interpolates_on_energy_fraction():
    # 160 kNm = 40 % of 400 -> reaction 50 % of 1100 = 550 kN, deflection 40 %
    r, d = reaction_at_energy(FENDER, 160.0, CURVE)
    assert r == pytest.approx(550.0)
    assert d == pytest.approx(40.0)
    # 100 kNm = 25 % energy -> between (20, 20, 10) and (40, 50, 40): halfway,
    # reaction 35 % -> 385 kN, deflection 30 %
    r, d = reaction_at_energy(FENDER, 100.0, CURVE)
    assert r == pytest.approx(385.0)
    assert d == pytest.approx(30.0)


def test_reaction_without_curve_is_rated_upper_bound():
    r, d = reaction_at_energy(FENDER, 100.0)
    assert r == 1100.0 and d is None


def test_reaction_above_capacity_is_undefined():
    assert reaction_at_energy(FENDER, 401.0, CURVE) == (None, None)
    # against a reduced capacity (after factors) 350 kNm, 360 is above it
    assert reaction_at_energy(FENDER, 360.0, CURVE, energy_capacity_knm=350.0) == (None, None)


def test_curve_validation():
    with pytest.raises(ValueError):
        PerformanceCurve("bad", ((0, 0, 0),), "x")
    with pytest.raises(ValueError):
        PerformanceCurve("bad", ((0, 0, 0), (0, 10, 10)), "x")
    with pytest.raises(ValueError):
        PerformanceCurve("bad", ((0, 0, 0), (10, 10, 20), (20, 20, 10)), "x")


def test_design_performance_factors():
    # 400 x 0.9 x 0.95 x 1.05 x 0.98 = 351.918 ; 1100 x 1.1 x 1.08 x 1.02 = 1332.936
    p = design_performance(
        FENDER, angular_factor=0.95, velocity_factor_energy=1.05,
        temperature_factor_energy=0.98, velocity_factor_reaction=1.08,
        temperature_factor_reaction=1.02,
    )
    assert p.energy_knm == pytest.approx(351.918)
    assert p.reaction_kn == pytest.approx(1332.936)
    assert p.factors["energy_tolerance"] == 0.10


def test_design_performance_rejects_bad_tolerance():
    with pytest.raises(ValueError):
        design_performance(FENDER, energy_tolerance=1.0)


def _velocity_sweep(n=1):
    def energy(v):
        return berthing_energy(60_000.0, v, ce=0.5, cm=1.8) * 1.25

    return parameter_sweep(
        "velocity_m_s", "berthing velocity", "m/s", 0.10, [0.05, 0.10, 0.15, 0.20],
        energy, FENDER, range_low=0.08, range_high=0.15, range_basis="test range",
        fenders_in_contact=n, curve=CURVE, reaction_limit_kn=1500.0,
        hull_pressure_limit_kpa=200.0,
    )


def test_sweep_points_include_base_and_range_ends():
    s = _velocity_sweep()
    assert [p.value for p in s.points] == [0.05, 0.08, 0.10, 0.15, 0.20]
    # base: 0.5 * 60000 * 0.01 * 0.5 * 1.8 * 1.25 = 337.5 kNm
    assert s.base_design_energy_knm == pytest.approx(337.5)
    assert s.at(0.10).utilisation == pytest.approx(337.5 / 400.0)
    # velocity squared: 0.15 -> x 2.25 -> +125 %
    assert s.change_pct(0.15) == pytest.approx(125.0)
    # above the rated energy the reaction is not defined
    assert s.at(0.15).reaction_kn is None
    # hull pressure scales with reaction at the rated contact area
    p = s.at(0.05)
    assert p.hull_pressure_kpa == pytest.approx(135.0 * p.reaction_kn / 1100.0)


def test_sweep_shares_energy_between_fenders():
    s = _velocity_sweep(n=2)
    assert s.at(0.10).energy_per_fender_knm == pytest.approx(337.5 / 2)
    assert s.at(0.15).reaction_kn is not None  # 759.4 / 2 = 379.7 < 400


def test_sweep_rejects_fractional_contacts():
    with pytest.raises(ValueError):
        _velocity_sweep(n=1.5)


def test_tornado_orders_by_span():
    s1 = _velocity_sweep()

    def energy_f(f):
        return berthing_energy(60_000.0, 0.10, ce=0.5, cm=1.8) * f

    s2 = parameter_sweep(
        "abnormal_factor", "abnormal factor", "-", 1.25, [1.0, 1.25, 1.5], energy_f, FENDER,
        range_low=1.25, range_high=1.5, range_basis="test",
    )
    bars = tornado([s2, s1])
    assert [b.parameter for b in bars] == ["velocity_m_s", "abnormal_factor"]
    # velocity 0.08: (0.8)^2 - 1 = -36 %; 0.15: +125 %
    assert bars[0].low_change_pct == pytest.approx(-36.0)
    assert bars[0].high_change_pct == pytest.approx(125.0)
    assert bars[1].high_change_pct == pytest.approx(20.0)
    assert math.isclose(bars[1].low_change_pct, 0.0, abs_tol=1e-12)
