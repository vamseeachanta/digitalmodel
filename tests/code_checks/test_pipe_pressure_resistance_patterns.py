"""Illustrative example regressions for existing pipe-pressure kernels.

All numerical cases below are independently chosen illustrative examples.
Inputs use SI: diameter and thickness [m], stress and pressure [Pa], moment
[N m], axial force [N]; Poisson ratio, ovality and load factors are dimensionless.
No source inputs, outputs or tabulated material/factor data are retained here.

Method and source-noted references
---------------------------------
A legacy engineering workbook cites DNV-OS-F101 with the following labels:
* equation 5.21: Mp = fy (D-t)^2 t; equation 5.20: Sp = fy pi (D-t) t.
* equation 5.8: Pb = 4 t min(fy, fu/1.15) / ((D-t) sqrt(3)).
* equation 5.11: Pel = 2 E (t/D)^3 / (1-nu^2).
* equation 5.12: Pp = 2 fy alpha_fab t/D.
* equation 13.13: collapse is obtained from a cubic. The existing kernel
  solves (Pc-Pel)(Pc^2-Pp^2) = Pc Pel Pp f0 D/t for the smallest positive root.
* equations 5.5 and 5.6: characteristic strength = (specified-derating) alpha_u.
* equations 4.5 and 4.7: design load effects are weighted sums.
* equations 5.19 a, 5.19 b, 5.28a and 5.28b label combined-loading variants.
  These labels identify source references, not equivalence of all variants
  to the existing local_buckling_lcc criterion tested below.
* Propagation pressure = 35 fy alpha_fab (t/D)^2.5: clause not stated in source.

The source edition is not established. These tests verify existing scalar
algebra and the cubic root selection, not compliance with a current standard.
This records existing kernel behavior, including its gamma factors on pressure;
it does not establish correctness of source-specific interaction variants.
Input gaps: approved characteristic strengths, temperature derating, fabrication
factor, ovality, resistance factors and factored loads remain caller inputs.
No source table is published. The existing collapse kernel imposes an ovality
floor; the low-ovality example tests that behavior, not an independent qualification
of that floor. Geometric applicability and installation/operation thickness
selection remain outside these kernels. Conversion, coefficient selection,
source-specific interaction variants, plotting and cache freshness are not
established by these tests. The existing LCC kernel omits alpha_c and alpha_p;
external loading requires the caller to supply minimum internal pressure as
pressure_internal. Existing scalar capacity kernels do not validate
all physical inputs; validation cases here target the existing LCC contract.
"""

import math

import pytest

from digitalmodel.code_checks.dnv_os_f101 import (
    MINIMUM_OVALITY,
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
from digitalmodel.structural.pipe_capacity.custom.DNV_OS_F201.dnv_os_f201_lrfd_wsd import (
    factored_load_effect,
    tensile_strength,
    yield_strength,
)


def test_plastic_capacities_illustrative_example():
    # Hand calculation of section area and plastic section modulus.
    diameter_m, thickness_m, yield_pa = 0.8, 0.024, 420e6
    mean_diameter_m = diameter_m - thickness_m
    area_m2 = math.pi * mean_diameter_m * thickness_m
    modulus_m3 = mean_diameter_m**2 * thickness_m
    assert plastic_axial_capacity(diameter_m, thickness_m, yield_pa) == pytest.approx(
        area_m2 * yield_pa, rel=1e-12
    )
    assert plastic_moment_capacity(diameter_m, thickness_m, yield_pa) == pytest.approx(
        modulus_m3 * yield_pa, rel=1e-12
    )


@pytest.mark.parametrize("tensile_pa", [460e6, 560e6])
def test_pressure_containment_both_strength_branches(tensile_pa):
    # Illustrative example: the two tensile strengths straddle the switch.
    fy_pa, diameter_m, thickness_m = 420e6, 0.8, 0.024
    governed_pa = min(fy_pa, tensile_pa / 1.15)
    expected_pa = 4 * thickness_m * governed_pa / (
        math.sqrt(3) * (diameter_m - thickness_m)
    )
    assert pressure_containment_resistance(
        diameter_m, thickness_m, fy_pa, tensile_pa
    ) == pytest.approx(expected_pa, rel=1e-12)


def test_collapse_components_illustrative_example():
    diameter_m, thickness_m, modulus_pa, nu = 0.8, 0.024, 190e9, 0.28
    expected_elastic_pa = 2 * modulus_pa * (thickness_m / diameter_m)**3 / (1 - nu**2)
    expected_plastic_pa = 2 * 420e6 * 0.88 * thickness_m / diameter_m
    assert elastic_collapse_pressure(
        diameter_m, thickness_m, modulus_pa, nu
    ) == pytest.approx(expected_elastic_pa, rel=1e-12)
    assert plastic_collapse_pressure(
        diameter_m, thickness_m, 420e6, 0.88
    ) == pytest.approx(expected_plastic_pa, rel=1e-12)


@pytest.mark.parametrize("thickness_m", [0.012, 0.024, 0.048])
@pytest.mark.parametrize("ovality", [0.012, 0.024])
def test_collapse_root_independent_bisection(thickness_m, ovality):
    # Illustrative examples. Bisection does not use the production cubic solver.
    diameter_m, fy_pa, modulus_pa, nu, fabrication = 0.8, 420e6, 190e9, 0.28, 0.88
    pel = 2 * modulus_pa * (thickness_m / diameter_m)**3 / (1 - nu**2)
    pp = 2 * fy_pa * fabrication * thickness_m / diameter_m
    coefficient = pel * pp * ovality * diameter_m / thickness_m

    def residual(pressure_pa):
        return (pressure_pa - pel) * (pressure_pa**2 - pp**2) - pressure_pa * coefficient

    lo, hi = 0.0, min(pel, pp)
    assert residual(lo) > 0 > residual(hi)
    for _ in range(100):
        mid = (lo + hi) / 2
        if residual(mid) > 0:
            lo = mid
        else:
            hi = mid
    actual = collapse_pressure(
        diameter_m, thickness_m, fy_pa, modulus_pa, ovality, nu, fabrication
    )
    assert 0 < actual < min(pel, pp)
    assert actual == pytest.approx((lo + hi) / 2, rel=1e-10)
    assert abs(residual(actual)) / (pel * pp**2) < 1e-10


def test_propagation_pressure_and_limit_illustrative_example():
    diameter_m, thickness_m, fy_pa, fabrication = 0.8, 0.024, 420e6, 0.88
    expected_pa = 35 * fy_pa * fabrication * (thickness_m / diameter_m)**2.5
    actual_pa = propagation_pressure(diameter_m, thickness_m, fy_pa, fabrication)
    assert actual_pa == pytest.approx(expected_pa, rel=1e-12)
    resistance_pa = actual_pa / (1.2 * 1.08)
    for multiplier, propagates in [(0.9, False), (1.0, False), (1.1, True)]:
        result = propagation_check(
            multiplier * resistance_pa, diameter_m, thickness_m, fy_pa,
            fabrication, 1.2, 1.08,
        )
        assert result.utilisation == pytest.approx(multiplier, rel=1e-12)
        assert result.propagates is propagates


def test_characteristic_strength_and_factored_load_illustrative_example():
    # Hand subtraction and weighted sum; these generic scalar APIs are reused
    # for algebra only, without substituting one standard's criteria for another.
    assert yield_strength(440e6, 20e6, 0.98) == pytest.approx(411.6e6, rel=1e-12)
    assert tensile_strength(580e6, 30e6, 0.98) == pytest.approx(539e6, rel=1e-12)
    assert factored_load_effect(-200e3, 80e3, 40e3, 1.2, 1.4, 1.1) == pytest.approx(-84e3, rel=1e-12)


@pytest.mark.parametrize("moment_sign", [-1, 1])
@pytest.mark.parametrize("tension_sign", [-1, 1])
@pytest.mark.parametrize("pressure_sign", [-1, 1])
def test_combined_loading_independent_ratios(moment_sign, tension_sign, pressure_sign):
    # Illustrative example: normalized M=1/5, S=1/4, net pressure=1/3.
    # M [N m], S [N], p [Pa]; capacities are chosen independently of geometry.
    g = 1.2 * 1.08
    expected = (g / 5 + (g / 4)**2)**2 + (g / 3)**2
    result = local_buckling_lcc(
        moment_sign * 160e3, tension_sign * 300e3, 8e6 + pressure_sign * 4e6,
        8e6, 800e3, 1200e3, 12e6, 1.2, 1.08,
    )
    assert result.utilisation == pytest.approx(expected, rel=1e-12)
    assert result.passes is (expected <= 1)


@pytest.mark.parametrize("mode", ["moment", "tension", "pressure"])
def test_combined_loading_exact_acceptance_boundary(mode):
    # Illustrative example: one normalized demand is unity; all others vanish.
    moment, tension, net = {"moment": (800e3, 0, 0),
                            "tension": (0, 1200e3, 0),
                            "pressure": (0, 0, 12e6)}[mode]
    result = local_buckling_lcc(moment, tension, net, 0, 800e3, 1200e3, 12e6, 1, 1)
    assert result.utilisation == 1
    assert result.passes


def test_combined_loading_zero_demand():
    result = local_buckling_lcc(0, 0, 8e6, 8e6, 800e3, 1200e3, 12e6, 1.2, 1.08)
    assert result.utilisation == 0
    assert result.passes


def test_combined_loading_overload_fails():
    # Illustrative example: normalized moment = 1.01, giving utilisation > 1.
    result = local_buckling_lcc(808e3, 0, 0, 0, 800e3, 1200e3, 12e6, 1, 1)
    assert result.utilisation == pytest.approx(1.01**2, rel=1e-12)
    assert result.passes is False


@pytest.mark.parametrize("parameter", [
    "moment_capacity", "axial_capacity", "pressure_capacity", "gamma_m", "gamma_sc",
])
@pytest.mark.parametrize("invalid", [0, -1, math.nan, math.inf])
def test_combined_loading_rejects_invalid_resistance(parameter, invalid):
    inputs = dict(design_moment=160e3, design_effective_tension=300e3,
                  pressure_internal=12e6, pressure_external=8e6,
                  moment_capacity=800e3, axial_capacity=1200e3,
                  pressure_capacity=12e6, gamma_m=1.2, gamma_sc=1.08)
    inputs[parameter] = invalid
    with pytest.raises(ValueError):
        local_buckling_lcc(**inputs)


@pytest.mark.parametrize("parameter", [
    "design_moment", "design_effective_tension", "pressure_internal", "pressure_external",
])
@pytest.mark.parametrize("invalid", [math.nan, math.inf, -math.inf])
def test_combined_loading_rejects_nonfinite_demands(parameter, invalid):
    inputs = dict(design_moment=160e3, design_effective_tension=300e3,
                  pressure_internal=12e6, pressure_external=8e6,
                  moment_capacity=800e3, axial_capacity=1200e3,
                  pressure_capacity=12e6, gamma_m=1.2, gamma_sc=1.08)
    inputs[parameter] = invalid
    with pytest.raises(ValueError):
        local_buckling_lcc(**inputs)


def test_geometric_scaling_identities_illustrative_example():
    # Illustrative example: doubling length gives 4x force and 8x moment;
    # pressure resistance is invariant when t/D and stresses remain fixed.
    base = (0.8, 0.024, 420e6)
    scaled = (1.6, 0.048, 420e6)
    assert plastic_axial_capacity(*scaled) == pytest.approx(4 * plastic_axial_capacity(*base), rel=1e-12)
    assert plastic_moment_capacity(*scaled) == pytest.approx(8 * plastic_moment_capacity(*base), rel=1e-12)
    assert propagation_pressure(*scaled, 0.88) == pytest.approx(propagation_pressure(*base, 0.88), rel=1e-12)
    assert pressure_containment_resistance(*scaled, 560e6) == pytest.approx(
        pressure_containment_resistance(*base, 560e6), rel=1e-12
    )
    assert elastic_collapse_pressure(1.6, 0.048, 190e9, 0.28) == pytest.approx(
        elastic_collapse_pressure(0.8, 0.024, 190e9, 0.28), rel=1e-12
    )
    assert plastic_collapse_pressure(*scaled, 0.88) == pytest.approx(
        plastic_collapse_pressure(*base, 0.88), rel=1e-12
    )
    assert collapse_pressure(1.6, 0.048, 420e6, 190e9, 0.012, 0.28, 0.88) == pytest.approx(
        collapse_pressure(0.8, 0.024, 420e6, 190e9, 0.012, 0.28, 0.88), rel=1e-12
    )


def test_collapse_ovality_floor_is_existing_api_policy():
    inputs = dict(diameter=0.8, t=0.024, f_y=420e6, youngs_modulus=190e9,
                  poisson_ratio=0.28, alpha_fab=0.88)
    assert collapse_pressure(**inputs, ovality=0) == collapse_pressure(
        **inputs, ovality=MINIMUM_OVALITY
    )
