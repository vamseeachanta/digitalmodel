# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent arithmetic controls for soft-clay lateral backbones.

Basis: OTC 1204 (1970), equations reproduced in OpenPile's public
py_curves.matlock_1970 notes. Inputs below are assumed examples, not site data.
Floating-point tolerances test numerical arithmetic, not design acceptance.
"""

import math

import pytest

from digitalmodel.geotechnical.py_curves import (
    Curve,
    CyclicDegradation,
    SoftClay,
    degradation_factor,
    soft_clay_py,
)


def soil():
    """Assumed inputs: su=20 kPa, gamma'=6 kN/m³, eps50=0.02, J=0.5."""
    return SoftClay(20.0, 6.0, 0.02, 0.5)


def make(depth=2.0, cyclic=False, degradation=None, points=(0, 0.05, 0.4)):
    return soft_clay_py(
        soil(), depth_m=depth, diameter_m=1.0, displacements_m=points,
        cyclic=cyclic, degradation=degradation,
    )


def test_static_hand_calculation():
    """pu=min((3*20+6*2)*1+0.5*20*2,9*20*1)=92 kN/m.

    yc=2.5*0.02*1=0.05 m; zr=max(6/(6/20+0.5),2.5)=7.5 m.
    OTC 1204 (1970) cubic backbone: at y/yc=1,8, p/pu=0.5,1.
    """
    result = make()
    assert result.ultimate_kn_m == pytest.approx(92)
    assert result.y50_m == pytest.approx(0.05)
    assert result.transition_depth_m == pytest.approx(7.5)
    assert result.curve.resistances == pytest.approx((0, 46, 92))
    assert result.curve.resistance_unit == "kN/m"


@pytest.mark.parametrize("ratio,expected", [(0, 0), (0.125, 0.25), (1, 0.5),
                                              (8, 1), (27, 1)])
def test_static_exact_cubic_points(ratio, expected):
    """OTC 1204 backbone: 0.5*cuberoot(r), capped at unity; exact cubes."""
    result = make(points=(0, 0.05 * ratio) if ratio else (0, 0.05))
    assert result.curve.resistance_at(0.05 * ratio) / 92 == pytest.approx(expected)


@pytest.mark.parametrize("ratio,rounded", [(0.1, 0.23), (0.3, 0.33),
                                           (1, 0.50), (3, 0.72), (8, 1.00)])
def test_public_reproduction_rounded_points(ratio, rounded):
    """OpenPile py_curves.api_clay Notes prints these rounded cubic points.

    Comparison uses the table's two-decimal precision, not a design tolerance.
    The implementation uses the exact cubic equation instead of interpolating
    this table. No licensed document was accessed to obtain these values.
    """
    result = make(points=(0, ratio * 0.05))
    assert round(result.curve.resistances[-1] / 92, 2) == rounded


def test_deep_capacity_and_zero_depth():
    """OTC 1204 limits: deep pu=9*20*1=180; mudline pu=3*20*1=60."""
    assert make(depth=30).ultimate_kn_m == pytest.approx(180)
    assert make(depth=0).ultimate_kn_m == pytest.approx(60)


def test_cyclic_shallow_hand_calculation():
    """OpenPile public cyclic notes: 0.72 pu at 3yc, 0.72 pu*z/zr at 15yc.

    At z=2, zr=7.5: residual=0.192pu; at 9yc, midpoint=0.456pu.
    """
    result = make(cyclic=True, points=(0, 0.05, 0.15, 0.45, 0.75, 1.0))
    assert result.curve.resistances == pytest.approx(
        (0, 46, 0.72 * 92, 0.456 * 92, 0.192 * 92, 0.192 * 92)
    )


@pytest.mark.parametrize("depth", [7.5, 10.0])
def test_cyclic_transition_and_deep_plateau(depth):
    """At/below zr=7.5 m, the cyclic tail remains 0.72pu (public notes)."""
    result = make(depth=depth, cyclic=True, points=(0, 0.15, 0.75, 1.0))
    assert result.curve.resistances[1:] == pytest.approx(
        [0.72 * result.ultimate_kn_m] * 3
    )


@pytest.mark.parametrize("depth", [0, 2, 7.5, 10])
@pytest.mark.parametrize("junction", [3, 15])
def test_cyclic_junction_continuity(depth, junction):
    """The rounded 0.72 plateau is capped before 3yc to avoid a jump."""
    centre = junction * 0.05
    result = make(depth, True, points=(0, centre - 1e-10, centre, centre + 1e-10))
    left, middle, right = result.curve.resistances[1:]
    assert left == pytest.approx(middle, abs=1e-6)
    assert right == pytest.approx(middle, abs=1e-6)


def test_transition_depth_continuity():
    """z/zr approaches unity from above the transition zone."""
    values = [make(d, True, points=(0, 1)).curve.resistances[-1]
              for d in (7.5 - 1e-9, 7.5, 7.5 + 1e-9)]
    assert values == pytest.approx([0.72 * 180] * 3)


def test_degradation_hand_calculation():
    """User-specified power law: 100**(-0.5)=0.1; no empirical default."""
    assert degradation_factor(1, 0.5) == 1
    assert degradation_factor(100, 0.5) == pytest.approx(0.1)
    assert [degradation_factor(n, 0.5) for n in (1, 4, 100)] == [1, 0.5, 0.1]
    assert degradation_factor(100, 0) == 1


def test_strength_and_secant_stiffness_degrade_independently():
    """N=100, bs=0.5, bk=1: su=2, pu=min(6+12+2,18)=18.

    The displacement scale (18/92)/0.01 enforces k50_N=0.01*k50_1.
    This is a modelling convention; laboratory calibration is NOT_EVALUATED.
    """
    state = CyclicDegradation(100, 0.5, 1)
    y50 = 0.05 * (18 / 92) / 0.01
    result = make(degradation=state, points=(0, y50))
    assert result.ultimate_kn_m == pytest.approx(18)
    assert result.y50_m == pytest.approx(y50)
    assert result.curve.resistances[-1] / y50 == pytest.approx(0.01 * 46 / 0.05)


def test_signed_interpolation_and_tail():
    result = make()
    assert result.curve.resistance_at(-0.05) == -46
    assert result.curve.resistance_at(10) == 92
    assert result.curve.resistance_at(0.025) == 23
    with pytest.raises(ValueError):
        result.curve.resistance_at(math.nan)


@pytest.mark.parametrize("args", [(0, 6, 0.02, 0.5), (20, -1, 0.02, 0.5),
                                  (20, 6, 0, 0.5), (20, 6, 0.02, -1),
                                  (math.nan, 6, 0.02, 0.5),
                                  (20, math.inf, 0.02, 0.5)])
def test_reject_invalid_soil(args):
    with pytest.raises(ValueError):
        SoftClay(*args)


@pytest.mark.parametrize("cycles,exponent", [(0, 0.5), (0.5, 1), (10, -1),
                                             (math.nan, 1), (10, math.inf),
                                             (1e300, 100)])
def test_reject_invalid_degradation(cycles, exponent):
    with pytest.raises(ValueError):
        degradation_factor(cycles, exponent)


@pytest.mark.parametrize("points", [(), (0,), (1, 2), (0, 0), (0, -1),
                                     (0, math.nan), (0, math.inf), (0, 2, 1)])
def test_reject_invalid_grid(points):
    with pytest.raises(ValueError):
        make(points=points)


@pytest.mark.parametrize("depth", [-1, math.inf, math.nan])
def test_reject_invalid_depth(depth):
    with pytest.raises(ValueError):
        make(depth=depth)


@pytest.mark.parametrize("diameter", [0, -1, math.inf, math.nan])
def test_reject_invalid_diameter(diameter):
    with pytest.raises(ValueError):
        soft_clay_py(soil(), depth_m=1, diameter_m=diameter, displacements_m=(0, 1))


def test_effective_stress_can_be_integrated_from_layers():
    """At z=2, supplied sigma'=30 gives shallow pu=60+30+20=110."""
    result = soft_clay_py(soil(), depth_m=2, diameter_m=1,
                         displacements_m=(0, 0.05), effective_stress_kpa=30)
    assert result.ultimate_kn_m == 110


def test_transition_depth_minimum():
    """Public transition equation: gamma'=1000 makes the 2.5D lower bound govern."""
    result = soft_clay_py(SoftClay(20, 1000, 0.02, 0.5), depth_m=2,
                         diameter_m=1, displacements_m=(0, 0.05))
    assert result.transition_depth_m == 2.5


@pytest.mark.parametrize("stress", [-1, math.nan, math.inf])
def test_reject_invalid_effective_stress(stress):
    with pytest.raises(ValueError):
        soft_clay_py(soil(), depth_m=2, diameter_m=1,
                     displacements_m=(0, 0.05), effective_stress_kpa=stress)


def test_reject_effective_stress_at_mudline():
    with pytest.raises(ValueError):
        soft_clay_py(soil(), depth_m=0, diameter_m=1,
                     displacements_m=(0, 0.05), effective_stress_kpa=1)


def test_curve_freezes_input_lists():
    displacements, resistances = [0, 1], [0, 2]
    curve = Curve(displacements, resistances, "kN/m", True)
    displacements[1], resistances[1] = 5, 10
    assert curve.displacements_m == (0, 1)
    assert curve.resistances == (0, 2)


@pytest.mark.parametrize("flag", ["cyclic", 1, None])
def test_reject_ambiguous_curve_flags(flag):
    with pytest.raises(ValueError):
        soft_clay_py(soil(), depth_m=2, diameter_m=1,
                     displacements_m=(0, 0.05), cyclic=flag)
    with pytest.raises(ValueError):
        Curve((0, 1), (0, 2), "kN/m", flag)
