"""Plate resistance method regressions using illustrative example inputs only.

Reuse: digitalmodel.infrastructure.calculations.plate_buckling provides elastic
coefficients, elastic stress, reduced slenderness, serviceability and ultimate
resistance, directional design resistance, and biaxial utilization. The legacy
PlateBuckling route is exercised only to expose distinct unresolved defects.
No production calculation is added or changed.

Inputs and units: elastic modulus, yield strength and applied stresses are Pa;
length, breadth and thickness are m; Poisson ratio, buckling coefficients,
slenderness, interaction and material factors are dimensionless. Magnitudes of
compressive membrane stresses and shear are used. Tensile stress applicability
is not established by these examples.

Method: elastic stress is k*pi**2*E*(t/b)**2/[12*(1-nu**2)]; reduced slenderness
is sqrt(characteristic/elastic). Serviceability resistance is characteristic /
sqrt(1+slenderness**4); the ultimate branch above unity is characteristic /
(sqrt(2)*slenderness). Usage is applied/design resistance; biaxial usage is the
square root of x**2+y**2-ci*x*y+z**2 using directional resistance ratios.

References as labelled in a legacy engineering workbook: DNV 30.1, Section
3.3.1; DNV 30.1, Section 3.3.6; DNV 30.1, Table 3.1; DNV 30.1, Table 3.2;
RP C-201, Section 6.2; RP C-201, Section 6.3; RP C-201, Section 6.4;
RP C-201, Section 6.5. These labels are provenance, not independent code
qualification. The material-factor label states RP C-201: clause not stated in
source. Input gaps: standard editions, governing load/sign convention,
applicable restraint model, factors and applicability of competing resistance
methods require engineering resolution. Material and physical properties are
example inputs, not recommended design values.

Limits and work left: strict expected failures identify existing transverse
resistance and legacy clamped/biaxial defects; none establishes an acceptable
engineering result. Source pressure-unit inconsistencies, approximate unit
conversions, chained comparisons and alternate interaction/pressure subchecks
are not repaired here. FE imports, plotting and invalid empty panels are not
covered. Existing validation is tested only for the rejection cases below;
finite-value validation across all existing APIs is not established.
"""

import math

import pytest

from digitalmodel.infrastructure.base_solvers.structural.plate_buckling import (
    PlateBuckling,
)
from digitalmodel.infrastructure.base_solvers.structural.utils.post_processing import (
    compute_von_mises,
)
from digitalmodel.infrastructure.calculations.plate_buckling import (
    BucklingCoefficients,
    ElasticBucklingCalculator,
    PlateEdgeCondition,
    SlendernessCalculator,
    UltimateStrengthCalculator,
    UsageFactorCalculator,
)


@pytest.mark.parametrize("edge", [PlateEdgeCondition.SIMPLY_SUPPORTED, PlateEdgeCondition.CLAMPED])
def test_elastic_directional_stresses_from_plate_rigidity(edge):
    """Illustrative example: independent D=E*t**3/[12*(1-nu**2)], stress=k*pi**2*D/(b**2*t)."""
    modulus_pa, nu, thickness_m, breadth_m, length_m = 180e9, 0.25, 0.018, 1.2, 3.6
    rigidity_nm = modulus_pa * thickness_m**3 / (12 * (1 - nu**2))
    aspect = breadth_m / length_m
    if edge == PlateEdgeCondition.SIMPLY_SUPPORTED:
        coefficients = (4, (1 + aspect**2)**2, 5.34 + 4 * aspect**2)
    else:
        coefficients = (7, 1 + 2.5 * aspect**2 + 5 * aspect**4, 9 + 5.6 * aspect**2)
    actual = ElasticBucklingCalculator().calculate_all_elastic_buckling_stresses(
        modulus_pa, nu, thickness_m, breadth_m, length_m, edge,
    )
    expected = {name: k * math.pi**2 * rigidity_nm / (breadth_m**2 * thickness_m)
                for name, k in zip(("longitudinal", "transverse", "shear"), coefficients)}
    assert actual == pytest.approx(expected, rel=1e-12)
    # A common dimensional scale changes rigidity and area, but leaves stress unchanged.
    scaled = ElasticBucklingCalculator().calculate_all_elastic_buckling_stresses(
        modulus_pa, nu, 10 * thickness_m, 10 * breadth_m, 10 * length_m, edge,
    )
    assert scaled == pytest.approx(expected, rel=1e-12)


@pytest.mark.parametrize("aspect", [0.5, 1.0, 1.5])
@pytest.mark.parametrize("edge", [PlateEdgeCondition.SIMPLY_SUPPORTED, PlateEdgeCondition.CLAMPED])
def test_edge_coefficients_at_round_aspect_ratios(aspect, edge):
    """Illustrative example coefficients; restraint formulas are source-stated methods."""
    if edge == PlateEdgeCondition.SIMPLY_SUPPORTED:
        transverse, shear = (1 + aspect**2)**2, 5.34 + 4 * aspect**2
    else:
        transverse, shear = 1 + 2.5 * aspect**2 + 5 * aspect**4, 9 + 5.6 * aspect**2
    coefficients = BucklingCoefficients()
    assert coefficients.get_transverse_coefficient(aspect, edge) == pytest.approx(transverse)
    assert coefficients.get_shear_coefficient(aspect, edge) == pytest.approx(shear)


@pytest.mark.parametrize("slenderness", [0.0, 0.5, 1.0, 2.0])
def test_characteristic_resistance_branches(slenderness):
    """Illustrative example: zero recovers yield; unity joins at yield/sqrt(2)."""
    yield_pa = 280e6
    service_pa = yield_pa / math.sqrt(1 + slenderness**4)
    ultimate_pa = service_pa if slenderness < 1 else yield_pa / (math.sqrt(2) * slenderness)
    calc = UltimateStrengthCalculator()
    assert calc.calculate_characteristic_resistance_serviceability(yield_pa, slenderness) == pytest.approx(service_pa)
    assert calc.calculate_characteristic_resistance_ultimate(yield_pa, slenderness) == pytest.approx(ultimate_pa)


@pytest.mark.parametrize("elastic_pa, expected", [(70e6, 2.0), (280e6, 1.0), (1120e6, 0.5)])
def test_reduced_slenderness_closed_form(elastic_pa, expected):
    """Illustrative example ratios of four, one and one quarter have known square roots."""
    assert SlendernessCalculator.calculate_slenderness_ratio(280e6, elastic_pa) == pytest.approx(expected)


@pytest.mark.parametrize("applied", [0.0, 35e6, 140e6])
def test_equivalent_slenderness_reduces_to_uniaxial(applied):
    """Illustrative example: nonzero uniaxial load cancels; no load returns zero."""
    actual = SlendernessCalculator.calculate_equivalent_slenderness(
        {"longitudinal": applied, "transverse": 0.0, "shear": 0.0},
        {"longitudinal": 70e6, "transverse": 90e6, "shear": 60e6},
        280e6, applied, interaction_exponent=1.5,
    )
    assert actual == pytest.approx(0 if applied == 0 else 2)


def test_equivalent_slenderness_combined_loading():
    """Illustrative example: equal directional stress ratios give a closed norm."""
    applied = {"longitudinal": 35e6, "transverse": 45e6, "shear": 30e6}
    elastic = {"longitudinal": 70e6, "transverse": 90e6, "shear": 60e6}
    vm_pa = math.sqrt(35e6**2 + 45e6**2 - 35e6 * 45e6 + 3 * 30e6**2)
    expected = math.sqrt(280e6 / vm_pa * (3 * 0.5**1.5)**(1 / 1.5))
    assert SlendernessCalculator.calculate_equivalent_slenderness(
        applied, elastic, 280e6, vm_pa, interaction_exponent=1.5,
    ) == pytest.approx(expected)


@pytest.mark.parametrize("slenderness", [0.5, 0.673, 0.9, 2.0])
def test_longitudinal_design_reduction(slenderness):
    """Illustrative example: thickness is chosen to place the method at each branch."""
    yield_pa, modulus_pa, breadth_m, factor = 280e6, 180e9, 1.2, 1.3
    thickness_m = 0.525 * breadth_m * math.sqrt(yield_pa / modulus_pa) / slenderness
    reduction = 1 if slenderness <= 0.673 else (slenderness - 0.22) / slenderness**2
    actual = UltimateStrengthCalculator.calculate_dnv_longitudinal_resistance(
        yield_pa, modulus_pa, thickness_m, breadth_m, material_factor=factor,
    )
    assert actual == pytest.approx(reduction * yield_pa / factor)


@pytest.mark.parametrize("aspect", [0.5, 1.0, 1.5])
@pytest.mark.parametrize("slenderness", [0.6, 0.8, 1.0, 1.2, 1.5])
def test_shear_design_reduction_branches(aspect, slenderness):
    """Illustrative example: exercise both coefficient branches and all reduction intervals."""
    yield_pa, modulus_pa, length_m, factor = 280e6, 180e9, 2.0, 1.3
    breadth_m = aspect * length_m
    k = 5.34 + 4 * aspect**2 if aspect < 1 else 5.34 * aspect**2 + 4
    thickness_m = 0.795 * breadth_m * math.sqrt(yield_pa / (modulus_pa * k)) / slenderness
    if slenderness <= 0.8:
        reduction = 1
    elif slenderness <= 1.2:
        reduction = 1 - 0.625 * (slenderness - 0.8)
    else:
        reduction = 0.9 / slenderness
    actual = UltimateStrengthCalculator.calculate_dnv_shear_resistance(
        yield_pa, modulus_pa, thickness_m, breadth_m, length_m, material_factor=factor,
    )
    assert actual == pytest.approx(reduction * yield_pa / (math.sqrt(3) * factor))


@pytest.mark.parametrize("pressure_ratio", [0.0, 0.5, 1.0, 2.0, 1e6])
def test_transverse_pressure_threshold_and_clipping(pressure_ratio):
    """Illustrative example: pressure effect is isolated by dividing by zero-pressure resistance."""
    yield_pa, modulus_pa, thickness_m, breadth_m, length_m = 280e6, 180e9, 0.018, 1.2, 3.6
    limit_pa = 2 * (thickness_m / breadth_m)**2 * yield_pa
    pressure_pa = pressure_ratio * limit_pa
    args = (yield_pa, modulus_pa, thickness_m, breadth_m, length_m)
    baseline = UltimateStrengthCalculator.calculate_dnv_transverse_resistance(
        *args, k4_factor=0.4, pressure=0, material_factor=1.3,
    )
    actual = UltimateStrengthCalculator.calculate_dnv_transverse_resistance(
        *args, k4_factor=0.4, pressure=pressure_pa, material_factor=1.3,
    )
    h = max(0, 0.05 * breadth_m / thickness_m - 0.75)
    expected_ratio = 1 if pressure_ratio <= 1 else max(0, 1 - h * (pressure_pa - limit_pa) / yield_pa)
    assert actual / baseline == pytest.approx(expected_ratio)


def test_biaxial_usage_with_unequal_normal_components():
    """Illustrative example ratios x=1/4, y=1/2, z=1/5; ci=1/2."""
    actual = UsageFactorCalculator.calculate_all_usage_factors(
        {"longitudinal": 30e6, "transverse": 90e6, "shear": 15e6},
        {"longitudinal": 120e6, "transverse": 180e6, "shear": 75e6},
        interaction_factor=0.5,
    )
    expected = math.sqrt(1 / 16 + 1 / 4 - 1 / 16 + 1 / 25)
    assert actual == pytest.approx(dict(longitudinal=0.25, transverse=0.5, shear=0.2, biaxial=expected))


@pytest.mark.parametrize("normal_x, normal_y, shear, expected", [(45e6, 0.0, 0.0, 45e6), (0.0, 0.0, 30e6, math.sqrt(3) * 30e6), (45e6, 45e6, 0.0, 45e6)])
def test_plane_stress_von_mises_identities(normal_x, normal_y, shear, expected):
    """Illustrative example: uniaxial, pure shear and equal biaxial identities."""
    assert compute_von_mises(normal_x, normal_y, tau_xy=shear) == pytest.approx(expected)


@pytest.mark.parametrize("field", ["youngs_modulus", "thickness", "breadth"])
@pytest.mark.parametrize("invalid", [0.0, -2.0])
def test_elastic_rejects_nonpositive_inputs(field, invalid):
    """Illustrative example rejection cases supported by the existing API."""
    inputs = dict(youngs_modulus=180e9, poisson_ratio=0.25, thickness=0.018, breadth=1.2)
    inputs[field] = invalid
    with pytest.raises(ValueError):
        ElasticBucklingCalculator().calculate_base_factor(**inputs)


@pytest.mark.parametrize("nu", [-0.1, 0.0, 0.5, 0.9])
def test_elastic_rejects_poisson_ratio_outside_existing_domain(nu):
    """Illustrative example rejection; existing domain is narrower than general isotropic elasticity."""
    with pytest.raises(ValueError):
        ElasticBucklingCalculator().calculate_base_factor(180e9, nu, 0.018, 1.2)


@pytest.mark.parametrize("yield_pa", [0.0, -2.0])
@pytest.mark.parametrize("method", [UltimateStrengthCalculator.calculate_characteristic_resistance_serviceability, UltimateStrengthCalculator.calculate_characteristic_resistance_ultimate])
def test_characteristic_resistance_rejects_nonpositive_yield(yield_pa, method):
    """Illustrative example rejection in both resistance branches."""
    with pytest.raises(ValueError):
        method(yield_pa, 0.5)


@pytest.mark.xfail(strict=True, raises=AssertionError, reason='Existing transverse method applies an additional column reduction; source method differs')
def test_source_pattern_transverse_resistance_has_no_extra_column_multiplier():
    """Illustrative example: source interpolation uses ki once; API also applies a separate column reduction."""
    yield_pa, modulus_pa, thickness_m, breadth_m, length_m, factor = 280e6, 180e9, 0.018, 1.2, 3.6, 1.3
    column_slenderness = 1.1 * breadth_m / thickness_m * math.sqrt(yield_pa / modulus_pa)
    ki = 1 / (2 * column_slenderness**2) + 0.07  # Source branch for slenderness above two.
    a = 1.3 * thickness_m / length_m * math.sqrt(modulus_pa / yield_pa)
    expected_pa = (a + ki * (1 - a)) * yield_pa / factor
    actual = UltimateStrengthCalculator.calculate_dnv_transverse_resistance(
        yield_pa, modulus_pa, thickness_m, breadth_m, length_m,
        k4_factor=ki, pressure=0, material_factor=factor,
    )
    assert actual == pytest.approx(expected_pa, rel=1e-12)


@pytest.mark.xfail(strict=True, raises=AssertionError, reason='Existing legacy method uses simply-supported equivalent slenderness for the clamped result')
def test_legacy_clamped_equivalent_serviceability_uses_clamped_slenderness():
    """Illustrative example: clamped equivalent lambda=1/2 differs from simple lambda=2."""
    result = PlateBuckling().get_characteristic_buckling_serviceability(
        {}, {"normal_stress_kx": 280e6, "normal_stress_tk": 280e6 / math.sqrt(3)},
        dict(ratio_longtudinal_direction=0.5, ratio_transverse_direction=0.5,
             ratio_shear_direction=0.5, ratio_equivalent_direction=0.5),
        {"yield_strength": 280e6}, {"equivalent_slenderness_ratio": 2.0},
    )
    expected = round(280e6 / math.sqrt(1 + 0.5**4), 2)
    assert result["resistance_equivalent_direction"] == pytest.approx(expected, rel=1e-12)


@pytest.mark.xfail(strict=True, raises=AssertionError, reason='Existing legacy interaction repeats the longitudinal component')
def test_legacy_biaxial_interaction_uses_transverse_component():
    """Illustrative example: unequal normal stresses expose repeated longitudinal component."""
    actual = PlateBuckling().get_bi_axial_direction(
        {}, {"breadth": 1.2, "thickness": 0.018, "yield_strength": 280e6},
        {"design_resistance": 120e6}, {"design_resistance_transverse": 180e6},
        {"shear_resistance": 75e6},
        {"longtudinal_stress": 30e6, "transverse_stress": 90e6, "shear_stress": 0.0},
    )["bi_axial_loading_shear_condition"]
    ci = 1 - 1.2 / (120 * 0.018)
    expected = 0.25**2 + 0.5**2 - ci * 0.25 * 0.5
    assert actual == pytest.approx(expected, rel=1e-12)


@pytest.mark.xfail(strict=True, raises=AssertionError, reason='Existing legacy transverse utilization uses longitudinal stress')
def test_legacy_transverse_usage_uses_transverse_component():
    """Illustrative example: transverse stress/design resistance equals one half."""
    actual = PlateBuckling().DNV_RP_C201_usage_factor(
        {}, {"longtudinal_stress": 30e6, "transverse_stress": 90e6, "shear_stress": 15e6},
        {"design_resistance": 120e6}, {"design_resistance_transverse": 180e6},
        {"shear_resistance": 75e6}, {"bi_axial_loading_shear_condition": 0.25},
    )
    assert actual["usage_transverse"] == pytest.approx(0.5)
