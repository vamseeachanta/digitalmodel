"""Tests for DNV-OS-F101 local-buckling and external-pressure checks.

Expected values are computed by hand from the closed-form formulas with generic
round-number inputs (no client data used as an oracle). The collapse-cubic root
is verified against the implicit DNV equation it must satisfy, not against any
project result.

A consistent unit set is used throughout: length in mm, stress/pressure in MPa,
force in N (= MPa * mm^2), moment in N.mm (= MPa * mm^3).
"""

import math

import pytest

from digitalmodel.code_checks.dnv_os_f101 import (
    LocalBucklingResult,
    PropagationCheck,
    collapse_pressure,
    elastic_collapse_pressure,
    local_buckling_lcc,
    plastic_axial_capacity,
    plastic_collapse_pressure,
    plastic_moment_capacity,
    pressure_containment_resistance,
    propagation_check,
    propagation_pressure,
)

# --- plastic capacities -----------------------------------------------------


def test_plastic_axial_capacity():
    # Sp = fy * pi * (D - t) * t = 450 * pi * (500-20) * 20 = 450*pi*9600
    assert plastic_axial_capacity(500.0, 20.0, 450.0) == pytest.approx(
        450.0 * math.pi * 480.0 * 20.0
    )


def test_plastic_moment_capacity():
    # Mp = fy * (D - t)^2 * t = 450 * 480^2 * 20 = 450 * 230400 * 20
    assert plastic_moment_capacity(500.0, 20.0, 450.0) == pytest.approx(
        450.0 * 480.0**2 * 20.0
    )


def test_pressure_containment_resistance():
    # fcb = min(450, 535/1.15) = min(450, 465.2..) = 450
    # Pb = (2*20/480) * 450 * 2/sqrt(3)
    fcb = min(450.0, 535.0 / 1.15)
    expected = (2.0 * 20.0 / 480.0) * fcb * (2.0 / math.sqrt(3.0))
    assert pressure_containment_resistance(500.0, 20.0, 450.0, 535.0) == pytest.approx(
        expected
    )
    assert fcb == 450.0  # yield governs here


# --- local buckling (LCC) ---------------------------------------------------


def test_local_buckling_zero_load_passes():
    res = local_buckling_lcc(
        design_moment=0.0,
        design_effective_tension=0.0,
        pressure_internal=10.0,
        pressure_external=10.0,
        moment_capacity=1.0e9,
        axial_capacity=1.0e7,
        pressure_capacity=30.0,
    )
    assert isinstance(res, LocalBucklingResult)
    assert res.utilisation == pytest.approx(0.0)
    assert res.passes is True


def test_local_buckling_pure_pressure_term():
    # No moment/tension; net = pe - pi = 20 - 5 = 15 external.
    # g = 1.0 * 1.0 = 1.0 ; pressure_term = (1 * 15 / 30)^2 = 0.25
    res = local_buckling_lcc(
        design_moment=0.0,
        design_effective_tension=0.0,
        pressure_internal=5.0,
        pressure_external=20.0,
        moment_capacity=1.0e9,
        axial_capacity=1.0e7,
        pressure_capacity=30.0,
        gamma_m=1.0,
        gamma_sc=1.0,
    )
    assert res.utilisation == pytest.approx(0.25)
    assert res.passes is True


def test_local_buckling_combined_handcalc():
    # Choose round capacities so terms are exact.
    # g = 1.0 ; Md/Mp = 100/200 = 0.5 ; (Sd/Sp)^2 = (30/100)^2 = 0.09
    # group = (0.5 + 0.09)^2 = 0.59^2 = 0.3481
    # net = pi - pe = 25 - 5 = 20 ; (20/40)^2 = 0.25
    # UF = 0.3481 + 0.25 = 0.5981
    res = local_buckling_lcc(
        design_moment=100.0,
        design_effective_tension=30.0,
        pressure_internal=25.0,
        pressure_external=5.0,
        moment_capacity=200.0,
        axial_capacity=100.0,
        pressure_capacity=40.0,
        gamma_m=1.0,
        gamma_sc=1.0,
    )
    assert res.utilisation == pytest.approx(0.5981)
    assert res.passes is True


def test_local_buckling_fails():
    # g = 1.0 ; Md/Mp = 1.0 ; tension 0 ; group = 1.0 ; pressure (20/40)^2=0.25
    # UF = 1.25 > 1 -> fail
    res = local_buckling_lcc(
        design_moment=200.0,
        design_effective_tension=0.0,
        pressure_internal=25.0,
        pressure_external=5.0,
        moment_capacity=200.0,
        axial_capacity=100.0,
        pressure_capacity=40.0,
        gamma_m=1.0,
        gamma_sc=1.0,
    )
    assert res.utilisation == pytest.approx(1.25)
    assert res.passes is False


def test_local_buckling_safety_factors_applied():
    # g = 1.15 * 1.14 = 1.311
    # moment_term = 1.311 * 100/200 = 0.6555
    # tension_term = (1.311 * 30/100)^2 = (0.3933)^2 = 0.15468489
    # group = (0.6555 + 0.15468489)^2
    # pressure_term = (1.311 * 20/40)^2 = (0.6555)^2 = 0.42968025
    g = 1.15 * 1.14
    group = (g * 0.5 + (g * 0.3) ** 2) ** 2
    pterm = (g * 0.5) ** 2
    res = local_buckling_lcc(
        design_moment=100.0,
        design_effective_tension=30.0,
        pressure_internal=25.0,
        pressure_external=5.0,
        moment_capacity=200.0,
        axial_capacity=100.0,
        pressure_capacity=40.0,
    )
    assert res.utilisation == pytest.approx(group + pterm)


def test_local_buckling_invalid_capacity():
    with pytest.raises(ValueError):
        local_buckling_lcc(0.0, 0.0, 0.0, 0.0, -1.0, 1.0, 1.0)


# --- external-pressure collapse --------------------------------------------

# Reference geometry: D=500, t=20, fy=450, E=210000, nu=0.3.
_D, _T, _FY, _E = 500.0, 20.0, 450.0, 210000.0


def test_elastic_collapse_pressure():
    # Pel = 2E(t/D)^3/(1-nu^2) = 2*210000*0.04^3/0.91
    expected = 2.0 * _E * (0.04**3) / (1.0 - 0.3**2)
    assert elastic_collapse_pressure(_D, _T, _E) == pytest.approx(expected)
    assert expected == pytest.approx(29.538461538, rel=1e-9)


def test_plastic_collapse_pressure():
    # Pp = fy * 1.0 * 2 * (t/D) = 450 * 2 * 0.04 = 36.0
    assert plastic_collapse_pressure(_D, _T, _FY) == pytest.approx(36.0)


def test_collapse_pressure_satisfies_cubic():
    # Verify Pc satisfies the implicit DNV collapse equation, not a fixed number.
    f0 = 0.005
    pc = collapse_pressure(_D, _T, _FY, _E, ovality=f0)
    pel = elastic_collapse_pressure(_D, _T, _E)
    pp = plastic_collapse_pressure(_D, _T, _FY)
    lhs = (pc - pel) * (pc**2 - pp**2)
    rhs = pc * pel * pp * f0 * (_D / _T)
    assert lhs == pytest.approx(rhs, abs=1e-6)
    # And it is below both bounding pressures (real collapse < min(Pel, Pp)).
    assert pc < min(pel, pp)
    assert pc == pytest.approx(24.733652743, rel=1e-9)


def test_collapse_pressure_decreases_with_ovality():
    pc_low = collapse_pressure(_D, _T, _FY, _E, ovality=0.005)
    pc_high = collapse_pressure(_D, _T, _FY, _E, ovality=0.02)
    assert pc_high < pc_low


def test_collapse_pressure_clamps_minimum_ovality():
    # Ovality below 0.005 is clamped to 0.005 (code minimum).
    pc_zero = collapse_pressure(_D, _T, _FY, _E, ovality=0.0)
    pc_min = collapse_pressure(_D, _T, _FY, _E, ovality=0.005)
    assert pc_zero == pytest.approx(pc_min)


# --- propagation buckling ---------------------------------------------------


def test_propagation_pressure():
    # Ppr = 35 * fy * 1.0 * (t/D)^2.5 = 35 * 450 * 0.04^2.5
    expected = 35.0 * 450.0 * (0.04**2.5)
    assert propagation_pressure(_D, _T, _FY) == pytest.approx(expected)
    assert expected == pytest.approx(5.04, rel=1e-9)


def test_propagation_check_no_propagation():
    # resistance = Ppr / (gm*gsc) = 5.04 / (1.15*1.14) = 5.04 / 1.311
    res = propagation_check(net_external_pressure=2.0, diameter=_D, t=_T, f_y=_FY)
    expected_resistance = 5.04 / (1.15 * 1.14)
    assert isinstance(res, PropagationCheck)
    assert res.propagation_resistance == pytest.approx(expected_resistance)
    assert res.utilisation == pytest.approx(2.0 / expected_resistance)
    assert res.propagates is False  # 2.0 < ~3.845


def test_propagation_check_propagates():
    # Large net external pressure exceeds the resistance.
    res = propagation_check(net_external_pressure=10.0, diameter=_D, t=_T, f_y=_FY)
    assert res.utilisation > 1.0
    assert res.propagates is True
