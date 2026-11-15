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
    peak_reaction_pct,
    rated_peak_factor,
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


def test_curve_ending_below_rated_energy_is_refused():
    # second-provider review at e57362bb: data up to 10 % energy only. Interpolation
    # used to clamp to the last point and report 680/700 kNm as 20 % reaction
    # (1100 x 0.20 = 220 kN; 319 kN after tolerances), a reaction the data do not support.
    with pytest.raises(ValueError, match="100 %"):
        PerformanceCurve("short", ((0, 0, 0), (20, 20, 10)), "fictional")
    with pytest.raises(ValueError, match="100 %"):
        PerformanceCurve("short", ((0, 0, 0), (40, 50, 40), (58, 95, 99.5)), "fictional")


def test_curve_starting_above_zero_energy_is_refused():
    # below the first point the reaction is equally unsupported
    with pytest.raises(ValueError, match="0 %"):
        PerformanceCurve("late", ((20, 20, 10), (60, 100, 100)), "fictional")


def test_curve_reaching_rated_energy_is_accepted_and_reads_at_the_rated_point():
    c = PerformanceCurve("full", ((0, 0, 0), (20, 20, 10), (60, 100, 100)), "fictional")
    r, d = reaction_at_energy(FENDER, 400.0, c)      # 100 % of rated energy: last point
    assert r == pytest.approx(1100.0) and d == pytest.approx(60.0)
    r, d = reaction_at_energy(FENDER, 40.0, c)       # 10 % energy: second point
    assert r == pytest.approx(220.0) and d == pytest.approx(20.0)
    # a curve continuing beyond the rated point is also complete
    beyond = PerformanceCurve("beyond", ((0, 0, 0), (60, 100, 100), (65, 120, 112)), "fictional")
    assert reaction_at_energy(FENDER, 400.0, beyond)[0] == pytest.approx(1100.0)


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
    # hull pressure is the rated-deflection value (not scaled to part load)
    p = s.at(0.05)
    assert p.hull_pressure_kpa == pytest.approx(135.0)
    assert s.at(0.15).hull_pressure_kpa is None


def test_sweep_reaction_factor_raises_reaction_and_pressure():
    def energy(v):
        return berthing_energy(60_000.0, v, ce=0.5, cm=1.8) * 1.25

    kw = dict(range_low=0.08, range_high=0.15, range_basis="test", curve=CURVE, reaction_limit_kn=500.0)
    nominal = parameter_sweep("velocity_m_s", "v", "m/s", 0.10, [0.05, 0.10], energy, FENDER, **kw)
    toleranced = parameter_sweep("velocity_m_s", "v", "m/s", 0.10, [0.05, 0.10], energy, FENDER,
                                 reaction_factor=1.10, **kw)
    assert toleranced.reaction_factor == 1.10
    for v in (0.05, 0.10):
        assert toleranced.at(v).reaction_kn == pytest.approx(1.10 * nominal.at(v).reaction_kn)
        assert toleranced.at(v).hull_pressure_kpa == pytest.approx(1.10 * 135.0)
    # 0.10 m/s: 337.5 kNm = 84.375 % of 400 -> reaction 50 + 44.375 / 60 x 50 = 86.98 % of 1100
    r_nom = 1100.0 * (50.0 + (84.375 - 40.0) / 60.0 * 50.0) / 100.0
    assert nominal.at(0.10).reaction_kn == pytest.approx(r_nom)
    # 0.15 m/s is above the rated energy (reaction not rated); the factored
    # reaction reaches the 500 kN limit at a lower velocity than the nominal
    assert nominal.over_reaction_limit() == [0.08, 0.10] == toleranced.over_reaction_limit()
    assert toleranced.reaction_limit_value() < nominal.reaction_limit_value() < 0.08
    with pytest.raises(ValueError):
        parameter_sweep("velocity_m_s", "v", "m/s", 0.10, [0.10], energy, FENDER, reaction_factor=0.0, **kw)


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


# ------------------------------------------- reaction falling after buckling
# Second-provider review at 944a85c7: a curve may fall after buckling, so the
# reaction at the final energy can hide an earlier, higher compression peak.
# Every reaction compared with a limit is the largest reaction reached along
# the curve up to the demand energy (fictional curve below).
BUCKLING = PerformanceCurve(
    "fictional buckling",
    ((0, 0, 0), (30, 100, 40), (50, 60, 80), (60, 100, 100)),
    "fictional test curve",
)


def test_reaction_is_the_peak_reached_up_to_the_demand_energy():
    # 320 kNm = 80 % of 400: the end value is 60 % (660 kN), but compression
    # passed the 100 % peak (1,100 kN) at 40 % energy
    r, d = reaction_at_energy(FENDER, 320.0, BUCKLING)
    assert r == pytest.approx(1100.0)
    assert d == pytest.approx(50.0)                   # deflection is at the demand energy
    assert peak_reaction_pct(BUCKLING, 80.0) == pytest.approx(100.0)
    # before the peak the reaction is still the value at the energy: 20 % -> 50 %
    assert reaction_at_energy(FENDER, 80.0, BUCKLING)[0] == pytest.approx(550.0)
    assert peak_reaction_pct(BUCKLING, 0.0) == 0.0


def test_buckling_sweep_reports_the_peak_and_is_over_the_limit():
    # review reproduction: 320 kNm demand, factor 1.10, limit 900 kN; the end
    # value gave 660 x 1.10 = 726 kN and no exceedance
    s = parameter_sweep(
        "demand", "demand", "kNm", 320.0, [320.0], lambda v: v, FENDER,
        range_low=320.0, range_high=320.0, range_basis="review reproduction",
        curve=BUCKLING, reaction_limit_kn=900.0, reaction_factor=1.10,
    )
    p = s.at(320.0)
    assert p.reaction_kn == pytest.approx(1210.0)     # 1,100 x 1.10
    assert p.deflection_pct == pytest.approx(50.0)
    assert s.over_reaction_limit() == [320.0]


def test_buckling_sweep_reaction_does_not_fall_with_energy():
    s = parameter_sweep(
        "demand", "demand", "kNm", 200.0, [80.0, 160.0, 240.0, 320.0, 400.0], lambda v: v, FENDER,
        range_low=80.0, range_high=400.0, range_basis="test", curve=BUCKLING,
        reaction_limit_kn=900.0, reaction_factor=1.10,
    )
    r = [p.reaction_kn for p in s.points]
    assert r == sorted(r)
    assert r[0] == pytest.approx(605.0)               # 20 % energy: 50 % x 1,100 x 1.10
    assert all(x == pytest.approx(1210.0) for x in r[2:])
    assert s.over_reaction_limit() == [160.0, 200.0, 240.0, 320.0, 400.0]
    # the factored reaction reaches 900 kN between 80 and 160 kNm
    assert 80.0 < s.reaction_limit_value() < 160.0


def test_monotonic_curve_reaction_is_unchanged():
    # with a reaction that does not fall, the peak is the value at the demand energy
    for e in (0.0, 20.0, 40.0, 100.0, 160.0, 250.0, 337.5, 400.0):
        frac = 100.0 * e / 400.0
        end = None
        pts = CURVE.points
        for a, b in zip(pts, pts[1:]):
            if a[2] <= frac <= b[2]:
                end = 1100.0 * (a[1] + (frac - a[2]) / (b[2] - a[2]) * (b[1] - a[1])) / 100.0
                break
        assert reaction_at_energy(FENDER, e, CURVE)[0] == pytest.approx(end), e
    assert rated_peak_factor(CURVE) == 1.0
    s = _velocity_sweep()
    assert s.at(0.10).reaction_kn == pytest.approx(1100.0 * (50.0 + (84.375 - 40.0) / 60.0 * 50.0) / 100.0)


def test_rated_peak_factor():
    # the peak up to the rated point over the rated reaction, never below 1
    assert rated_peak_factor(BUCKLING) == 1.0
    above = PerformanceCurve("fictional above rated", ((0, 0, 0), (30, 110, 40), (60, 100, 100)), "x")
    assert rated_peak_factor(above) == pytest.approx(1.10)
    # a rise beyond the rated point (energy above 100 %) is not read
    past = PerformanceCurve("fictional past rated", ((0, 0, 0), (60, 100, 100), (70, 160, 130)), "x")
    assert rated_peak_factor(past) == 1.0
