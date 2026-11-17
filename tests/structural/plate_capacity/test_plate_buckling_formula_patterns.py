"""Independent formulas for illustrative example plate checks.

All example geometry, material properties and loads are chosen independently.
Source basis and qualification limits are recorded in the
digitalmodel.infrastructure.calculations.plate_buckling_combined module docstring.
"""
import math

import pytest

from digitalmodel.infrastructure.calculations.plate_buckling import (
    ElasticBucklingCalculator,
    PlateEdgeCondition,
    SlendernessCalculator,
    UltimateStrengthCalculator,
    UsageFactorCalculator,
)
from digitalmodel.infrastructure.calculations.plate_buckling_combined import (
    dnv_combined_interaction_coefficient,
    dnv_combined_shear_reduction,
)
from digitalmodel.infrastructure.utils.engineering_units import get_registry
from digitalmodel.structural.structural_analysis.models import StressState


@pytest.mark.parametrize("edge", [PlateEdgeCondition.SIMPLY_SUPPORTED, PlateEdgeCondition.CLAMPED])
@pytest.mark.parametrize("breadth_m,length_m", [(1.5, 4.5), (2.5, 5.0)])
def test_elastic_resistances_from_plate_rigidity(edge, breadth_m, length_m):
    """Illustrative example: derive stresses from bending rigidity D."""
    modulus_pa, poisson, thickness_m = 180e9, 0.25, 0.015
    rigidity_nm = modulus_pa * thickness_m**3 / (12 * (1 - poisson**2))
    aspect = breadth_m / length_m
    if edge == PlateEdgeCondition.SIMPLY_SUPPORTED:
        coefficients = (4, (1 + aspect**2)**2, 5.34 + 4 * aspect**2)
    else:
        coefficients = (7, 1 + 2.5 * aspect**2 + 5 * aspect**4, 9 + 5.6 * aspect**2)
    result = ElasticBucklingCalculator().calculate_all_elastic_buckling_stresses(
        modulus_pa, poisson, thickness_m, breadth_m, length_m, edge
    )
    for direction, coefficient in zip(("longitudinal", "transverse", "shear"), coefficients):
        expected_pa = coefficient * math.pi**2 * rigidity_nm / (thickness_m * breadth_m**2)
        assert result[direction] == pytest.approx(expected_pa, rel=1e-12)


def test_elastic_geometric_similarity_and_thickness_scaling():
    """Illustrative example: uniform scaling preserves stress; doubling t gives 4x."""
    calc = ElasticBucklingCalculator()
    original = calc.calculate_all_elastic_buckling_stresses(180e9, 0.25, 0.015, 1.5, 4.5)
    scaled = calc.calculate_all_elastic_buckling_stresses(180e9, 0.25, 0.03, 3.0, 9.0)
    thicker = calc.calculate_all_elastic_buckling_stresses(180e9, 0.25, 0.03, 1.5, 4.5)
    for key in original:
        assert scaled[key] == pytest.approx(original[key], rel=1e-12)
        assert thicker[key] == pytest.approx(4 * original[key], rel=1e-12)


@pytest.mark.parametrize("changes", [
    {"youngs_modulus": 0}, {"youngs_modulus": -1},
    {"poisson_ratio": 0}, {"poisson_ratio": 0.5},
    {"thickness": 0}, {"thickness": -1}, {"breadth": 0}, {"breadth": -1},
])
def test_existing_elastic_input_validation(changes):
    args = dict(youngs_modulus=180e9, poisson_ratio=0.25, thickness=0.015, breadth=1.5)
    args.update(changes)
    with pytest.raises(ValueError):
        ElasticBucklingCalculator().calculate_base_factor(**args)


@pytest.mark.parametrize("slenderness", [0.0, 0.5, 1.0, 1.5, 3.0])
def test_characteristic_resistance_branches(slenderness):
    """Illustrative example: independent resistance formula and branch join."""
    strength_pa = 300e6
    service_pa = strength_pa / math.sqrt(1 + slenderness**4)
    ultimate_pa = service_pa if slenderness < 1 else strength_pa / (math.sqrt(2) * slenderness)
    assert UltimateStrengthCalculator.calculate_characteristic_resistance_serviceability(
        strength_pa, slenderness
    ) == pytest.approx(service_pa, rel=1e-12)
    assert UltimateStrengthCalculator.calculate_characteristic_resistance_ultimate(
        strength_pa, slenderness
    ) == pytest.approx(ultimate_pa, rel=1e-12)


@pytest.mark.parametrize("method", [
    UltimateStrengthCalculator.calculate_characteristic_resistance_serviceability,
    UltimateStrengthCalculator.calculate_characteristic_resistance_ultimate,
])
@pytest.mark.parametrize("strength_pa", [0, -1])
def test_existing_characteristic_strength_validation(method, strength_pa):
    with pytest.raises(ValueError):
        method(strength_pa, 0.5)


@pytest.mark.parametrize("slenderness", [0.5, 1.5, 3.0])
def test_reduced_slenderness_closed_form(slenderness):
    strength_pa = 300e6
    elastic_pa = strength_pa / slenderness**2
    assert SlendernessCalculator.calculate_slenderness_ratio(strength_pa, elastic_pa) == pytest.approx(slenderness)


@pytest.mark.parametrize("exponent", [1.5, 2.0])
def test_equivalent_slenderness_single_component_identity(exponent):
    """Illustrative example: uniaxial equivalent slenderness equals sqrt(fy/elastic)."""
    applied_pa, strength_pa, elastic_pa = 30e6, 300e6, 75e6
    value = SlendernessCalculator.calculate_equivalent_slenderness(
        {"longitudinal": applied_pa, "transverse": 0, "shear": 0},
        {"longitudinal": elastic_pa, "transverse": 80e6, "shear": 60e6},
        strength_pa, applied_pa, exponent,
    )
    assert value == pytest.approx(math.sqrt(strength_pa / elastic_pa), rel=1e-12)


def test_equivalent_slenderness_combined_loading():
    """Illustrative example: independent generalized mean of normalized stresses."""
    sx_pa, sy_pa, tau_pa, fy_pa = 30e6, 15e6, 12e6, 300e6
    ex_pa, ey_pa, et_pa, exponent = 75e6, 60e6, 45e6, 1.5
    equivalent_pa = math.sqrt(sx_pa**2 + sy_pa**2 - sx_pa * sy_pa + 3 * tau_pa**2)
    mean = ((sx_pa / ex_pa)**exponent + (sy_pa / ey_pa)**exponent + (tau_pa / et_pa)**exponent)**(1 / exponent)
    expected = math.sqrt(fy_pa * mean / equivalent_pa)
    actual = SlendernessCalculator.calculate_equivalent_slenderness(
        dict(longitudinal=sx_pa, transverse=sy_pa, shear=tau_pa),
        dict(longitudinal=ex_pa, transverse=ey_pa, shear=et_pa),
        fy_pa, equivalent_pa, exponent,
    )
    assert actual == pytest.approx(expected, rel=1e-12)


@pytest.mark.parametrize("slenderness", [0.5, 0.67, 0.9, 1.5])
def test_longitudinal_design_resistance_branches(slenderness):
    """Illustrative example: choose t by inversion of the slenderness definition."""
    fy_pa, modulus_pa, breadth_m, factor = 300e6, 180e9, 1.5, 1.2
    thickness_m = 0.525 * breadth_m * math.sqrt(fy_pa / modulus_pa) / slenderness
    reduction = 1 if slenderness <= 0.673 else (slenderness - 0.22) / slenderness**2
    expected_pa = reduction * fy_pa / factor
    assert UltimateStrengthCalculator.calculate_dnv_longitudinal_resistance(
        fy_pa, modulus_pa, thickness_m, breadth_m, factor
    ) == pytest.approx(expected_pa, rel=1e-12)


def test_transverse_stocky_plate_reduces_to_yield_over_factor():
    """Stocky limit gives fy/material factor; this does not check the geometric term."""
    fy_pa, modulus_pa, breadth_m, length_m, factor = 300e6, 180e9, 1.5, 4.5, 1.2
    slenderness = 0.15
    thickness_m = 1.1 * breadth_m * math.sqrt(fy_pa / modulus_pa) / slenderness
    expected_pa = fy_pa / factor
    actual_pa = UltimateStrengthCalculator.calculate_dnv_transverse_resistance(
        fy_pa, modulus_pa, thickness_m, breadth_m, length_m,
        k4_factor=1, pressure=0, material_factor=factor,
    )
    assert actual_pa == pytest.approx(expected_pa, rel=1e-12)


@pytest.mark.xfail(strict=True, reason="Existing transverse API applies an extra reduction to the geometric term")
def test_transverse_geometric_term_retains_unreduced_contribution():
    """Illustrative example: independent expression for RP C-201, Section 6.3."""
    fy_pa, modulus_pa, breadth_m, length_m, factor = 300e6, 180e9, 1.5, 4.5, 1.2
    slenderness = 1.5
    thickness_m = 1.1 * breadth_m * math.sqrt(fy_pa / modulus_pa) / slenderness
    mu = 0.21 * (slenderness - 0.2)
    q = 1 + mu + slenderness**2
    reduction = (q - math.sqrt(q*q - 4*slenderness**2)) / (2*slenderness**2)
    geometry = 1.3 * thickness_m / length_m * math.sqrt(modulus_pa / fy_pa)
    expected_pa = (geometry + reduction * (1 - geometry)) * fy_pa / factor
    actual_pa = UltimateStrengthCalculator.calculate_dnv_transverse_resistance(
        fy_pa, modulus_pa, thickness_m, breadth_m, length_m,
        k4_factor=1, pressure=0, material_factor=factor,
    )
    assert actual_pa == pytest.approx(expected_pa, rel=1e-12)


@pytest.mark.parametrize("slenderness", [2.0, 3.0])
@pytest.mark.xfail(strict=True, reason="Existing transverse API omits the high-slenderness branch")
def test_transverse_high_slenderness_branch(slenderness):
    fy_pa, modulus_pa, breadth_m, length_m, factor = 300e6, 180e9, 1.5, 4.5, 1.2
    thickness_m = 1.1 * breadth_m * math.sqrt(fy_pa / modulus_pa) / slenderness
    actual_pa = UltimateStrengthCalculator.calculate_dnv_transverse_resistance(
        fy_pa, modulus_pa, thickness_m, breadth_m, length_m,
        k4_factor=1, pressure=0, material_factor=factor,
    )
    assert actual_pa / (fy_pa / factor) == pytest.approx(
        1 / (2 * slenderness**2) + 0.07, rel=1e-12
    )


@pytest.mark.parametrize("aspect", [0.5, 1.0, 1.5])
@pytest.mark.parametrize("slenderness", [0.5, 0.8, 0.95, 1.2, 1.5])
def test_shear_design_resistance_branches(aspect, slenderness):
    """Illustrative example: invert lambda_w to exercise both geometry branches."""
    fy_pa, modulus_pa, breadth_m, factor = 300e6, 180e9, 1.5, 1.2
    length_m = breadth_m / aspect
    coefficient = 5.34 + 4 * aspect**2 if aspect < 1 else 5.34 * aspect**2 + 4
    thickness_m = 0.795 * breadth_m * math.sqrt(fy_pa / (modulus_pa * coefficient)) / slenderness
    if slenderness <= 0.8:
        reduction = 1
    elif slenderness <= 1.2:
        reduction = 1 - 0.625 * (slenderness - 0.8)
    else:
        reduction = 0.9 / slenderness
    expected_pa = reduction * fy_pa / (math.sqrt(3) * factor)
    assert UltimateStrengthCalculator.calculate_dnv_shear_resistance(
        fy_pa, modulus_pa, thickness_m, breadth_m, length_m, factor
    ) == pytest.approx(expected_pa, rel=1e-12)


@pytest.mark.parametrize("breadth_thickness_ratio,expected", [
    (60, 0.5), (120, 0.0), (121, 0.0),
])
def test_combined_interaction_coefficient_hand_values(breadth_thickness_ratio, expected):
    """Illustrative ratios give one half, zero at the limit, and zero above it."""
    thickness_m = 0.025
    breadth_m = breadth_thickness_ratio * thickness_m
    assert dnv_combined_interaction_coefficient(breadth_m, thickness_m) == pytest.approx(
        expected, rel=1e-12, abs=1e-12
    )


@pytest.mark.parametrize("invalid", [0.0, -1.0, math.inf, -math.inf, math.nan])
@pytest.mark.parametrize("parameter", ["breadth_m", "thickness_m"])
def test_combined_interaction_coefficient_rejects_invalid_dimensions(parameter, invalid):
    inputs = dict(breadth_m=1.5, thickness_m=0.025)
    inputs[parameter] = invalid
    with pytest.raises(ValueError):
        dnv_combined_interaction_coefficient(**inputs)


@pytest.mark.parametrize("slenderness,expected", [
    (0.5, 1.0), (0.8, 1.0), (1.0, 0.84), (1.25, 0.64), (1.5, 4 / 9),
])
def test_combined_shear_reduction_hand_values(slenderness, expected):
    """Illustrative hand values cover both branch joins and all three branches."""
    assert dnv_combined_shear_reduction(slenderness) == pytest.approx(expected, rel=1e-12)


def test_combined_shear_reduction_zero_slenderness():
    assert dnv_combined_shear_reduction(0.0) == 1.0


@pytest.mark.parametrize("invalid", [-1.0, math.inf, -math.inf, math.nan])
def test_combined_shear_reduction_rejects_invalid_slenderness(invalid):
    with pytest.raises(ValueError):
        dnv_combined_shear_reduction(invalid)


def test_combined_biaxial_utilization_with_existing_resistance_apis():
    """Illustrative example: replace the standalone shear reduction at lambda_w=1."""
    fy_pa, modulus_pa, breadth_m, length_m, factor = 300e6, 180e9, 1.5, 3.0, 1.2
    thickness_m = 0.795 * breadth_m * math.sqrt(fy_pa / (modulus_pa * 6.34))
    longitudinal_pa = UltimateStrengthCalculator.calculate_dnv_longitudinal_resistance(
        fy_pa, modulus_pa, thickness_m, breadth_m, factor
    )
    standalone_shear_pa = UltimateStrengthCalculator.calculate_dnv_shear_resistance(
        fy_pa, modulus_pa, thickness_m, breadth_m, length_m, factor
    )
    # At unit web slenderness the standalone reduction is 7/8, the combined is 21/25.
    combined_shear_pa = standalone_shear_pa / 0.875 * dnv_combined_shear_reduction(1.0)
    interaction = dnv_combined_interaction_coefficient(breadth_m, thickness_m)
    actual = UsageFactorCalculator.calculate_biaxial_usage_factor(
        30e6, 15e6, 12e6, longitudinal_pa, 100e6, combined_shear_pa, interaction
    )
    # Independent hand result uses the supplied transverse resistance of 100 MPa.
    assert actual == pytest.approx(0.2701556759107755, rel=1e-12)


@pytest.mark.parametrize("interaction", [0, 0.5, 1])
def test_biaxial_utilization_and_zero_load(interaction):
    """Illustrative example: the squared utilization is the quadratic form."""
    actual = UsageFactorCalculator.calculate_biaxial_usage_factor(
        30e6, 15e6, 12e6, 120e6, 75e6, 60e6, interaction
    )
    expected_squared = (30 / 120)**2 + (15 / 75)**2 - interaction * (30 / 120) * (15 / 75) + (12 / 60)**2
    assert actual**2 == pytest.approx(expected_squared, rel=1e-12)
    assert UsageFactorCalculator.calculate_biaxial_usage_factor(
        0, 0, 0, 120e6, 75e6, 60e6, interaction
    ) == 0


@pytest.mark.parametrize("applied_pa", [0, 30e6, -30e6])
def test_usage_magnitude(applied_pa):
    assert UsageFactorCalculator.calculate_usage_factor(applied_pa, 120e6) == abs(applied_pa) / 120e6


def test_existing_units_roundtrip_and_elastic_consistency():
    """Illustrative example: use the repository registry, with no copied constants."""
    registry = get_registry()
    modulus_pa, thickness_m, breadth_m = 180e9, 0.015, 1.5
    modulus_ksi = registry.Quantity(modulus_pa, "Pa").to("ksi").magnitude
    thickness_ft = registry.Quantity(thickness_m, "m").to("ft").magnitude
    breadth_ft = registry.Quantity(breadth_m, "m").to("ft").magnitude
    calc = ElasticBucklingCalculator()
    si_pa = calc.calculate_base_factor(modulus_pa, 0.25, thickness_m, breadth_m)
    legacy_ksi = calc.calculate_base_factor(modulus_ksi, 0.25, thickness_ft, breadth_ft)
    assert registry.Quantity(legacy_ksi, "ksi").to("Pa").magnitude == pytest.approx(si_pa, rel=1e-12)


@pytest.mark.parametrize("sx_pa,sy_pa,tau_pa", [(0, 0, 0), (30e6, 0, 0), (0, 0, -12e6), (30e6, 15e6, 12e6)])
def test_plane_stress_equivalent_stress(sx_pa, sy_pa, tau_pa):
    """Illustrative example: compare the stress-invariant result with plane stress."""
    expected_pa = math.sqrt(sx_pa**2 + sy_pa**2 - sx_pa * sy_pa + 3 * tau_pa**2)
    actual_pa = StressState(sigma_x=sx_pa, sigma_y=sy_pa, tau_xy=tau_pa).von_mises()
    assert actual_pa == pytest.approx(expected_pa, rel=1e-12, abs=1e-12)


@pytest.mark.parametrize("pressure_pa", [0, 1e6])
def test_transverse_pressure_factor_independently_of_geometric_term(pressure_pa):
    """Illustrative example: the ratio isolates the separately applied pressure factor."""
    fy_pa, modulus_pa, thickness_m, breadth_m, length_m = 300e6, 180e9, 0.02, 1.5, 4.5
    p_limit_pa = 2 * (thickness_m / breadth_m)**2 * fy_pa
    h_alpha = max(0, 0.05 * breadth_m / thickness_m - 0.75)
    pressure_factor = 1 if pressure_pa <= p_limit_pa else 1 - h_alpha * (pressure_pa / fy_pa - 2 * (thickness_m / breadth_m)**2)
    baseline = UltimateStrengthCalculator.calculate_dnv_transverse_resistance(
        fy_pa, modulus_pa, thickness_m, breadth_m, length_m,
        k4_factor=1, pressure=0, material_factor=1.2,
    )
    pressured = UltimateStrengthCalculator.calculate_dnv_transverse_resistance(
        fy_pa, modulus_pa, thickness_m, breadth_m, length_m,
        k4_factor=1, pressure=pressure_pa, material_factor=1.2,
    )
    assert pressured / baseline == pytest.approx(pressure_factor, rel=1e-12)
