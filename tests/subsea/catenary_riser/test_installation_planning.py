# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent synthetic hand calculations, Catenary Curve (2019), eqs (1)-(14).

Section integration uses dV/ds=w, dx/ds=H/sqrt(H²+V²), dz/ds=V/sqrt(H²+V²).
No elastic, current, bending or installation acceptance factors are assumed.
"""

import math

import pytest

from digitalmodel.subsea.catenary_riser.planning import (
    PlanningLimits,
    select_optimal,
    sweep_lazy_wave,
    sweep_simple,
)


def limits(**changes):
    # Synthetic limits supplied by this test, not prescribed engineering limits.
    values = dict(min_depth=0, max_depth=100, max_top_tension=100,
                  min_bottom_tension=0, max_curvature=10, max_span=100,
                  max_suspended_length=100, max_hang_off_angle=90)
    values.update(changes)
    return PlanningLimits(**values)


def test_simple_three_controls_share_hand_answer():
    """w=10, d=2, a=3 => H=30, V=40, T=50, s=4, x=3ln3."""
    sweep = sweep_simple(
        water_depth=2, weight=10,
        hang_off_angles=[math.degrees(math.atan(.75))],
        top_tensions=[50], suspended_lengths=[4],
    )
    assert not sweep.rejected
    assert len(sweep.cases) == 3
    for state in sweep.cases:
        assert state.bottom_tension == pytest.approx(30)
        assert state.top_tension == pytest.approx(50)
        assert state.suspended_length == pytest.approx(4)
        assert state.touchdown_point == pytest.approx((-3 * math.log(3), -2))
        assert state.max_curvature == pytest.approx(1 / 3)


@pytest.mark.parametrize("control", ["angle", "tension"])
def test_piecewise_hand_case(control):
    """w=1,H=3: lower L=4 => V=4,T=5,z=2,x=3ln3.

    Buoy L=6,w=-1 => V=-2,T=sqrt13,dz=5-sqrt13;
    upper L=6 => V=4,T=5,dz=5-sqrt13.
    Total depth=12-2sqrt13, span=9ln3+6asinh(2/3), L=16.
    Hog z=4, sag z=10-2sqrt13; kmax=1/3 at V=0.
    """
    args = dict(hang_off_angles=[], top_tensions=[])
    args["hang_off_angles" if control == "angle" else "top_tensions"] = [
        math.degrees(math.atan(.75)) if control == "angle" else 5,
    ]
    sweep = sweep_lazy_wave(
        weight=1, buoyancy_start=4, buoyancy_length=6, net_buoyancy=1,
        suspended_lengths=[16], **args,
    )
    assert not sweep.rejected
    state, = sweep.cases
    assert state.bottom_tension == pytest.approx(3)
    assert state.vertical_top_force == pytest.approx(4)
    assert state.water_depth == pytest.approx(12 - 2 * math.sqrt(13))
    assert state.horizontal_span == pytest.approx(9 * math.log(3) + 6 * math.asinh(2 / 3))
    assert state.hog_height == pytest.approx(4)
    assert state.sag_height == pytest.approx(10 - 2 * math.sqrt(13))
    assert state.max_curvature == pytest.approx(1 / 3)
    assert state.touchdown_point == pytest.approx((-state.horizontal_span, -state.water_depth))


def test_piecewise_force_and_displacement_by_independent_quadrature():
    """Midpoint integration of direction cosines; no package integration reused.

    Numerical assertion tolerance is a quadrature error target, not a design limit.
    """
    state, = sweep_lazy_wave(
        weight=1, buoyancy_start=4, buoyancy_length=6, net_buoyancy=1,
        suspended_lengths=[16], top_tensions=[5], hang_off_angles=[],
    ).cases
    x = z = v = 0
    for length, weight in [(4, 1), (6, -1), (6, 1)]:
        ds = length / 20000
        for _ in range(20000):
            mid = v + weight * ds / 2
            tension = math.hypot(3, mid)
            x += ds * 3 / tension
            z += ds * mid / tension
            v += weight * ds
    assert state.horizontal_span == pytest.approx(x, abs=1e-8)
    assert state.water_depth == pytest.approx(z, abs=1e-8)
    assert state.vertical_top_force == pytest.approx(v)


def test_analytic_objective_optimum_and_required_limits():
    """At w=10,d=2: J=T+90*k=H+20+900/H.

    J'=1-900/H² gives unique positive optimum H=30 N. Candidate top
    tensions 40,50,60 correspond to H=20,30,40; J=85,80,82.5.
    This proves the supplied grid minimum, not a continuous optimiser.
    """
    sweep = sweep_simple(water_depth=2, weight=10, top_tensions=[40, 50, 60],
                         hang_off_angles=[], suspended_lengths=[])
    selected = select_optimal(sweep, objective=lambda s: s.top_tension + 90 * s.max_curvature,
                              limits=limits())
    assert selected.feasible
    assert selected.best.bottom_tension == pytest.approx(30)
    assert selected.objective_value == pytest.approx(80)
    with pytest.raises(TypeError):
        PlanningLimits()


def test_infeasible_and_invalid_candidates_remain_visible():
    sweep = sweep_simple(water_depth=2, weight=10, top_tensions=[20, 50],
                         hang_off_angles=[0], suspended_lengths=[1])
    assert len(sweep.rejected) == 3
    selected = select_optimal(sweep, objective=lambda s: s.top_tension,
                              limits=limits(max_top_tension=49))
    assert not selected.feasible
    assert selected.best is None
    assert selected.objective_value is None
    assert len(selected.rejected) == 4
    assert "max_top_tension" in selected.rejected[-1].reason


@pytest.mark.parametrize("changes", [
    {"net_buoyancy": 0}, {"net_buoyancy": math.nan}, {"buoyancy_length": -1},
    {"suspended_lengths": [8]}, {"top_tensions": [1]},
    {"buoyancy_length": 2},  # no reversal: a buoyed catenary is not a lazy wave
    {"buoyancy_length": 9},  # upper sag would cross the seabed
])
def test_lazy_wave_negative_controls(changes):
    args = dict(weight=1, buoyancy_start=4, buoyancy_length=6, net_buoyancy=1,
                suspended_lengths=[16], top_tensions=[5], hang_off_angles=[])
    args.update(changes)
    result = sweep_lazy_wave(**args)
    assert not result.cases
    assert result.rejected


def test_no_objective_nan_or_invalid_limits_can_win():
    sweep = sweep_simple(water_depth=2, weight=10, top_tensions=[50],
                         hang_off_angles=[], suspended_lengths=[])
    result = select_optimal(sweep, objective=lambda s: math.nan, limits=limits())
    assert not result.feasible
    assert "objective" in result.rejected[0].reason
    with pytest.raises(ValueError, match="max_curvature"):
        limits(max_curvature=math.inf)
    with pytest.raises(ValueError, match="depth"):
        limits(min_depth=2, max_depth=1)
