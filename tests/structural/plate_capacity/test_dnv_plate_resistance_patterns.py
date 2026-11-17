"""Independent plate-resistance checks using illustrative example inputs only.

The existing functions are reused for patterns in a legacy engineering workbook.
Inputs use m and Pa; expected values use hand equations, identities and limiting
cases. Source citations and limits are in plate-buckling-method-coverage.md.
Existing numerical discrepancies remain regression tests, without production
changes. Inputs are illustrative examples; no source values or fixtures are included.
"""

import math

import pytest

from digitalmodel.infrastructure.base_solvers.structural.plate_buckling import (
    PlateBuckling,
)
from digitalmodel.infrastructure.calculations.plate_buckling import (
    BucklingCoefficients,
    ElasticBucklingCalculator,
    PlateEdgeCondition,
    SlendernessCalculator,
    UltimateStrengthCalculator,
    UsageFactorCalculator,
)


class ExpectedMethodDifference(AssertionError):
    """Only an explicitly checked numerical/validation discrepancy is expected."""


def assert_stated_equation(actual, expected):
    if not math.isclose(actual, expected, rel_tol=1e-12, abs_tol=1e-12):
        raise ExpectedMethodDifference(f"existing result {actual!r}; stated equation {expected!r}")


@pytest.fixture
def illustrative_plate():
    """Illustrative example; geometry in m and material strengths in Pa."""
    return dict(
        youngs_modulus_pa=180e9,
        yield_strength_pa=280e6,
        poisson_ratio=0.28,
        thickness_m=0.018,
        breadth_m=0.9,
        length_m=3.6,
        material_factor=1.2,
    )


@pytest.mark.parametrize("edge", [PlateEdgeCondition.SIMPLY_SUPPORTED, PlateEdgeCondition.CLAMPED])
@pytest.mark.parametrize("aspect", [0.2, 0.4, 0.6])
def test_coefficients_match_stated_polynomials(edge, aspect):
    coefficients = BucklingCoefficients()
    if edge == PlateEdgeCondition.SIMPLY_SUPPORTED:
        transverse = 1 + 2 * aspect**2 + aspect**4
        shear = 5.34 + 4 * aspect**2
    else:
        transverse = 1 + 2.5 * aspect**2 + 5 * aspect**4
        shear = 9 + 5.6 * aspect**2
    assert coefficients.get_transverse_coefficient(aspect, edge) == pytest.approx(transverse, rel=1e-12)
    assert coefficients.get_shear_coefficient(aspect, edge) == pytest.approx(shear, rel=1e-12)


@pytest.mark.parametrize("edge", [PlateEdgeCondition.SIMPLY_SUPPORTED, PlateEdgeCondition.CLAMPED])
def test_elastic_resistances_from_plate_rigidity(illustrative_plate, edge):
    p = illustrative_plate
    e, nu = p["youngs_modulus_pa"], p["poisson_ratio"]
    t, b, length = p["thickness_m"], p["breadth_m"], p["length_m"]
    rigidity = e * t**3 / (12 * (1 - nu**2))
    reference_stress = math.pi**2 * rigidity / (b**2 * t)
    aspect = b / length
    if edge == PlateEdgeCondition.SIMPLY_SUPPORTED:
        coefficients = (4, (1 + aspect**2)**2, 5.34 + 4 * aspect**2)
    else:
        coefficients = (7, 1 + 2.5 * aspect**2 + 5 * aspect**4, 9 + 5.6 * aspect**2)
    actual = ElasticBucklingCalculator().calculate_all_elastic_buckling_stresses(e, nu, t, b, length, edge)
    for direction, coefficient in zip(("longitudinal", "transverse", "shear"), coefficients):
        assert actual[direction] == pytest.approx(coefficient * reference_stress, rel=1e-12)


def test_elastic_strength_scales_with_modulus_and_thickness(illustrative_plate):
    p = illustrative_plate
    calc = ElasticBucklingCalculator()
    args = (p["youngs_modulus_pa"], p["poisson_ratio"], p["thickness_m"], p["breadth_m"])
    baseline = calc.calculate_base_factor(*args)
    assert calc.calculate_base_factor(2 * args[0], *args[1:]) == pytest.approx(2 * baseline)
    assert calc.calculate_base_factor(args[0], args[1], 2 * args[2], args[3]) == pytest.approx(4 * baseline)


@pytest.mark.parametrize("characteristic_pa,elastic_pa,expected", [(240e6, 60e6, 2), (90e6, 360e6, 0.5)])
def test_slenderness_from_resistance_ratio(characteristic_pa, elastic_pa, expected):
    assert SlendernessCalculator.calculate_slenderness_ratio(characteristic_pa, elastic_pa) == pytest.approx(expected)


@pytest.mark.parametrize("direction", ["longitudinal", "transverse", "shear"])
def test_equivalent_slenderness_reduces_to_single_direction(direction):
    """Illustrative example; a single active stress cancels from the ratio."""
    stress_pa, elastic_pa, yield_pa = 40e6, 100e6, 280e6
    vm_pa = stress_pa * (math.sqrt(3) if direction == "shear" else 1)
    actual = SlendernessCalculator.calculate_equivalent_slenderness(
        {direction: stress_pa}, {direction: elastic_pa}, yield_pa, vm_pa,
        interaction_exponent=1.6,
    )
    characteristic_pa = yield_pa / (math.sqrt(3) if direction == "shear" else 1)
    assert actual == pytest.approx(math.sqrt(characteristic_pa / elastic_pa), rel=1e-12)


def test_combined_slenderness_uses_geometry_exponent_and_is_load_scale_invariant():
    """Illustrative example: doubling every load leaves slenderness unchanged."""
    loads = dict(longitudinal=60e6, transverse=20e6, shear=15e6)
    elastic = dict(longitudinal=300e6, transverse=150e6, shear=120e6)
    exponent = 2 - 0.4
    vm = math.sqrt((60e6 - 20e6)**2 + 60e6 * 20e6 + 3 * (15e6)**2)
    interaction = ((60e6 / 300e6)**exponent + (20e6 / 150e6)**exponent + (15e6 / 120e6)**exponent)**(1 / exponent)
    expected = math.sqrt(280e6 * interaction / vm)
    baseline = SlendernessCalculator.calculate_equivalent_slenderness(loads, elastic, 280e6, vm, exponent)
    doubled = SlendernessCalculator.calculate_equivalent_slenderness(
        {k: 2 * v for k, v in loads.items()}, elastic, 280e6, 2 * vm, exponent,
    )
    assert baseline == pytest.approx(expected, rel=1e-12)
    assert doubled == pytest.approx(baseline, rel=1e-12)


@pytest.mark.parametrize("slenderness", [0, 0.5, 1, 1.5, 2])
def test_serviceability_and_ultimate_strength_limits(slenderness):
    yield_pa = 280e6
    calc = UltimateStrengthCalculator
    service = yield_pa / math.hypot(1, slenderness**2)
    ultimate = service if slenderness < 1 else yield_pa / (math.sqrt(2) * slenderness)
    assert calc.calculate_characteristic_resistance_serviceability(yield_pa, slenderness) == pytest.approx(service, rel=1e-12)
    assert calc.calculate_characteristic_resistance_ultimate(yield_pa, slenderness) == pytest.approx(ultimate, rel=1e-12)


@pytest.mark.parametrize("target_slenderness", [0.4, 0.65, 0.7, 1, 2])
def test_longitudinal_design_resistance_branches(illustrative_plate, target_slenderness):
    p = illustrative_plate
    fy, e, b, gamma = (p[k] for k in ("yield_strength_pa", "youngs_modulus_pa", "breadth_m", "material_factor"))
    t = 0.525 * b * math.sqrt(fy / e) / target_slenderness
    reduction = 1 if target_slenderness <= 0.673 else 1 / target_slenderness - 0.22 / target_slenderness**2
    actual = UltimateStrengthCalculator.calculate_dnv_longitudinal_resistance(fy, e, t, b, gamma)
    assert actual == pytest.approx(reduction * fy / gamma, rel=1e-12)


@pytest.mark.parametrize("aspect", [0.4, 1.4])
@pytest.mark.parametrize("target_slenderness,reduction", [(0.4, 1), (0.8, 1), (1, 0.875), (1.2, 0.75), (1.5, 0.6)])
def test_pure_shear_design_resistance_branches(illustrative_plate, aspect, target_slenderness, reduction):
    p = illustrative_plate
    fy, e, b, gamma = (p[k] for k in ("yield_strength_pa", "youngs_modulus_pa", "breadth_m", "material_factor"))
    length = b / aspect
    coefficient = 5.34 + 4 * aspect**2 if aspect < 1 else 5.34 * aspect**2 + 4
    t = 0.795 * b * math.sqrt(fy / (e * coefficient)) / target_slenderness
    actual = UltimateStrengthCalculator.calculate_dnv_shear_resistance(fy, e, t, b, length, gamma)
    assert actual == pytest.approx(reduction * fy / (math.sqrt(3) * gamma), rel=1e-12)


def test_transverse_stocky_limit(illustrative_plate):
    p = illustrative_plate
    fy, e, b, length, gamma = (p[k] for k in ("yield_strength_pa", "youngs_modulus_pa", "breadth_m", "length_m", "material_factor"))
    t = 1.1 * b * math.sqrt(fy / e) / 0.1
    actual = UltimateStrengthCalculator.calculate_dnv_transverse_resistance(fy, e, t, b, length, 1, 0, gamma)
    assert actual == pytest.approx(fy / gamma, rel=1e-12)


@pytest.mark.xfail(strict=True, raises=ExpectedMethodDifference, reason="Existing transverse method adds a reduction multiplier and omits the upper k branch")
@pytest.mark.parametrize("target_slenderness", [1, 3])
def test_transverse_resistance_against_stated_method(illustrative_plate, target_slenderness):
    """Expose the existing extra multiplier and omitted high-slenderness branch."""
    p = illustrative_plate
    fy, e, b, length, gamma = (p[k] for k in ("yield_strength_pa", "youngs_modulus_pa", "breadth_m", "length_m", "material_factor"))
    t = 1.1 * b * math.sqrt(fy / e) / target_slenderness
    mu = 0.21 * (target_slenderness - 0.2)
    term = 1 + mu + target_slenderness**2
    k = ((term - math.sqrt(term**2 - 4 * target_slenderness**2)) / (2 * target_slenderness**2)
         if target_slenderness < 2 else 1 / (2 * target_slenderness**2) + 0.07)
    expected = (k + (1 - k) * 1.3 * t / length * math.sqrt(e / fy)) * fy / gamma
    actual = UltimateStrengthCalculator.calculate_dnv_transverse_resistance(fy, e, t, b, length, k, 0, gamma)
    assert_stated_equation(actual, expected)


def test_transverse_pressure_multiplier_relative_to_zero_pressure(illustrative_plate):
    p = illustrative_plate
    fy, e, t, b, length, gamma = (p[k] for k in ("yield_strength_pa", "youngs_modulus_pa", "thickness_m", "breadth_m", "length_m", "material_factor"))
    threshold_pa = 2 * (t / b)**2 * fy
    pressure_pa = 3 * threshold_pa
    calc = UltimateStrengthCalculator.calculate_dnv_transverse_resistance
    zero = calc(fy, e, t, b, length, 0.4, 0, gamma)
    loaded = calc(fy, e, t, b, length, 0.4, pressure_pa, gamma)
    below_threshold = calc(fy, e, t, b, length, 0.4, threshold_pa / 2, gamma)
    expected_factor = 1 - max(0, 0.05 * b / t - 0.75) * ((pressure_pa - threshold_pa) / fy)
    assert loaded / zero == pytest.approx(expected_factor, rel=1e-12)
    assert below_threshold == pytest.approx(zero, rel=1e-12)


@pytest.mark.parametrize("applied_pa,expected", [(0, 0), (30e6, 0.25), (-30e6, 0.25), (120e6, 1)])
def test_usage_is_normalized_stress_magnitude(applied_pa, expected):
    assert UsageFactorCalculator.calculate_usage_factor(applied_pa, 120e6) == pytest.approx(expected)


@pytest.mark.parametrize("ci", [0, 0.4, 1])
@pytest.mark.parametrize("normal_x_pa,normal_y_pa,shear_pa", [(0, 0, 0), (60e6, 20e6, 15e6), (60e6, 0, 0), (0, 0, 30e6)])
def test_biaxial_usage_from_difference_of_normal_ratios(ci, normal_x_pa, normal_y_pa, shear_pa):
    x, y, z = normal_x_pa / 300e6, normal_y_pa / 200e6, shear_pa / 100e6
    expected = math.sqrt((1 - ci / 2) * (x*x + y*y) + ci / 2 * (x - y)**2 + z*z)
    actual = UsageFactorCalculator.calculate_biaxial_usage_factor(
        normal_x_pa, normal_y_pa, shear_pa, 300e6, 200e6, 100e6, ci,
    )
    assert actual == pytest.approx(expected, rel=1e-12, abs=1e-12)


@pytest.mark.xfail(strict=True, raises=ExpectedMethodDifference, reason="Existing legacy interaction substitutes longitudinal stress for transverse stress")
def test_legacy_biaxial_uses_both_normal_stress_components():
    """Illustrative example with unequal normal stresses isolates component use."""
    properties = dict(breadth=0.8, thickness=0.025, yield_strength=280e6)
    actual = PlateBuckling().get_bi_axial_direction(
        {}, properties, {"design_resistance": 300e6},
        {"design_resistance_transverse": 200e6}, {},
        {"longtudinal_stress": 60e6, "transverse_stress": 20e6, "shear_stress": 0},
    )["bi_axial_loading_shear_condition"]
    x, y = 60e6 / 300e6, 20e6 / 200e6
    ci = 1 - properties["breadth"] / (120 * properties["thickness"])
    expected = (1 - ci / 2) * (x*x + y*y) + ci / 2 * (x - y)**2
    assert_stated_equation(actual, expected)


@pytest.mark.xfail(strict=True, raises=ExpectedMethodDifference, reason="Existing legacy interaction fixes combined shear reduction at unity")
@pytest.mark.parametrize("target_slenderness", [1, 1.5])
def test_legacy_combined_shear_reduction_changes_usage(illustrative_plate, target_slenderness):
    """Resistance ratios cancel the legacy material divisor and stress units."""
    p = illustrative_plate
    fy, e, b, length = (p[k] for k in ("yield_strength_pa", "youngs_modulus_pa", "breadth_m", "length_m"))
    coefficient = 5.34 + 4 * (b / length)**2
    results = []
    for slenderness in (0.4, target_slenderness):
        thickness = 0.795 * b * math.sqrt(fy / (e * coefficient)) / slenderness
        result = PlateBuckling().get_bi_axial_direction(
            {}, dict(breadth=b, thickness=thickness, yield_strength=fy),
            {"design_resistance": 300e6}, {"design_resistance_transverse": 200e6}, {},
            {"longtudinal_stress": 0, "transverse_stress": 0, "shear_stress": 20e6},
        )["bi_axial_loading_shear_condition"]
        results.append(result)
    reduction = 1 - 0.8 * (target_slenderness - 0.8) if target_slenderness <= 1.25 else 1 / target_slenderness**2
    assert_stated_equation(results[1] / results[0], 1 / reduction**2)


@pytest.mark.parametrize("key,value", [("youngs_modulus", 0), ("poisson_ratio", 0), ("poisson_ratio", 0.5), ("thickness", 0), ("breadth", -1)])
def test_elastic_base_rejects_existing_invalid_input_cases(key, value):
    parameters = dict(youngs_modulus=180e9, poisson_ratio=0.28, thickness=0.018, breadth=0.9)
    parameters[key] = value
    with pytest.raises(ValueError):
        ElasticBucklingCalculator().calculate_base_factor(**parameters)


@pytest.mark.parametrize("method", ["calculate_characteristic_resistance_serviceability", "calculate_characteristic_resistance_ultimate"])
@pytest.mark.parametrize("yield_pa", [0, -280e6])
def test_characteristic_strength_rejects_nonpositive_yield(method, yield_pa):
    with pytest.raises(ValueError):
        getattr(UltimateStrengthCalculator, method)(yield_pa, 1)


@pytest.mark.xfail(strict=True, raises=ExpectedMethodDifference, reason="Existing elastic base does not reject nonfinite modulus")
@pytest.mark.parametrize("modulus_pa", [math.inf, math.nan])
def test_elastic_base_rejects_nonfinite_modulus(modulus_pa):
    try:
        ElasticBucklingCalculator().calculate_base_factor(modulus_pa, 0.28, 0.018, 0.9)
    except ValueError:
        return
    raise ExpectedMethodDifference("nonfinite material input requires explicit rejection")
