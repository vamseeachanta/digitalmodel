"""Illustrative example tests; expectations follow independent hand formulas."""

from dataclasses import FrozenInstanceError, replace
from math import pi, sqrt

import pytest

from digitalmodel.drilling_riser.riser_properties import (
    BuoyancyInput,
    RiserJointInput,
    SoilResistanceInput,
    TensionInput,
    apparent_mass_from_ratio,
    compute_buoyancy_diameter_m,
    compute_joint_properties,
    compute_soil_pressure_pa,
    compute_tension_balance,
    mass_from_linear_density,
)
from digitalmodel.drilling_riser.stackup import effective_tension


def illustrative_joint():
    """Neutral illustrative example with SI dimensions and mass equivalents."""
    return RiserJointInput(
        joint_count_dimensionless=3,
        joint_length_m=10.0,
        outer_diameter_m=0.8,
        wall_thickness_m=0.04,
        dry_mass_per_joint_kg=800.0,
        submerged_mass_per_joint_kg=200.0,
        youngs_modulus_pa=200e9,
        poisson_ratio_dimensionless=0.25,
    )


def test_section_properties_and_count_scaling():
    joint = illustrative_joint()
    result = compute_joint_properties(joint)
    bore = 0.8 - 2 * 0.04
    area = pi * (0.8**2 - bore**2) / 4
    inertia = pi * (0.8**4 - bore**4) / 64
    assert result.inner_diameter_m == pytest.approx(bore)
    assert result.area_m2 == pytest.approx(area)
    assert result.second_moment_m4 == pytest.approx(inertia)
    assert result.area_m2 == pytest.approx(0.0955044166, rel=1e-8)
    assert result.second_moment_m4 == pytest.approx(0.0069145198, rel=1e-6)
    assert result.polar_moment_m4 == pytest.approx(2 * inertia)
    assert result.axial_stiffness_n == pytest.approx(200e9 * area)
    assert result.bending_stiffness_n_m2 == pytest.approx(200e9 * inertia)
    assert result.torsional_stiffness_n_m2 == pytest.approx(80e9 * 2 * inertia)
    assert result.total_length_m == 30.0
    assert result.total_dry_mass_kg == 2400.0
    assert result.total_submerged_mass_kg == 600.0
    assert result.dry_mass_kg_per_m == 80.0
    assert result.submerged_mass_kg_per_m == 20.0
    single = compute_joint_properties(replace(joint, joint_count_dimensionless=1))
    assert single.area_m2 == result.area_m2
    assert single.dry_mass_kg_per_m == result.dry_mass_kg_per_m
    assert 3 * single.total_length_m == result.total_length_m


def test_exact_imperial_boundary_and_explicit_mass_coefficient():
    joint = RiserJointInput.from_imperial(
        joint_count_dimensionless=2, joint_length_ft=10.0,
        outer_diameter_in=10.0, wall_thickness_in=0.5,
        dry_mass_per_joint_lb=200.0, submerged_mass_per_joint_lb=100.0,
        youngs_modulus_kpa=200e6, poisson_ratio_dimensionless=0.25,
        mass_conversion_kg_per_lb=0.45359237,
    )
    assert joint.joint_length_m == 3.048
    assert joint.outer_diameter_m == pytest.approx(0.254)
    assert joint.wall_thickness_m == pytest.approx(0.0127)
    assert joint.dry_mass_per_joint_kg == 200 * 0.45359237
    assert joint.submerged_mass_per_joint_kg == 100 * 0.45359237
    assert joint.youngs_modulus_pa == 200e9
    with pytest.raises(FrozenInstanceError):
        joint.joint_length_m = 4.0


def test_buoyancy_inverse_and_signed_submerged_mass():
    # Illustrative example: displaced annulus per metre recovers the mass loss.
    params = BuoyancyInput(80.0, 20.0, 0.4, 1000.0)
    diameter = compute_buoyancy_diameter_m(params)
    assert diameter == pytest.approx(0.486203, rel=1e-5)
    assert diameter == pytest.approx(sqrt(4 * 60 / (pi * 1000) + 0.4**2))
    assert 1000 * pi / 4 * (diameter**2 - 0.4**2) == pytest.approx(60.0)
    neutral = replace(params, submerged_mass_kg_per_m=80.0)
    assert compute_buoyancy_diameter_m(neutral) == 0.4
    buoyant = replace(params, submerged_mass_kg_per_m=-40.0)
    assert compute_buoyancy_diameter_m(buoyant) > diameter


def test_zero_mass_and_solid_circular_section():
    joint = replace(illustrative_joint(), wall_thickness_m=0.4,
                    dry_mass_per_joint_kg=0.0, submerged_mass_per_joint_kg=0.0)
    result = compute_joint_properties(joint)
    assert result.inner_diameter_m == 0.0
    assert result.area_m2 == pytest.approx(pi * 0.8**2 / 4)
    assert result.second_moment_m4 == pytest.approx(pi * 0.8**4 / 64)
    assert result.total_dry_mass_kg == 0.0


def test_weight_and_tension_equilibrium_including_compression_and_uplift():
    params = TensionInput(10000.0, 300.0, 10.0)
    result = compute_tension_balance(params)
    assert result.inservice_weight_n == 3000.0
    assert result.bottom_tension_n == 7000.0
    assert result.bottom_tension_n + result.inservice_weight_n == params.top_tension_n
    assert compute_tension_balance(replace(params, top_tension_n=100.0)).bottom_tension_n == -2900
    assert compute_tension_balance(replace(params, inservice_mass_kg=-20.0)).bottom_tension_n == 10200
    assert compute_tension_balance(replace(params, inservice_mass_kg=0.0)).bottom_tension_n == 10000


def test_tension_balance_matches_existing_effective_tension():
    assert (
        compute_tension_balance(TensionInput(10000.0, 300.0, 10.0)).bottom_tension_n
        == effective_tension(10000.0, 3000.0, 1.0)
        == 7000.0
    )


def test_apparent_mass_ratio_requires_explicit_factor():
    assert apparent_mass_from_ratio(800.0, 0.75) == 600.0
    assert apparent_mass_from_ratio(800.0, -0.25) == -200.0
    assert apparent_mass_from_ratio(0.0, 0.75) == 0.0


def test_soil_load_is_normalized_by_reference_diameter_only():
    params = SoilResistanceInput(0.02, 12000.0, 0.6)
    assert compute_soil_pressure_pa(params) == 20000.0
    assert compute_soil_pressure_pa(replace(params, reference_diameter_m=1.2)) == 10000
    assert compute_soil_pressure_pa(replace(params, line_load_n_per_m=0.0)) == 0.0
    assert compute_soil_pressure_pa(replace(params, line_load_n_per_m=-12000)) == -20000
    converted = SoilResistanceInput.from_imperial(
        deflection_in=0.5, line_load_lbf_per_in=10.0,
        reference_diameter_in=20.0, force_conversion_n_per_lbf=4.4482216152605,
    )
    assert converted.deflection_m == pytest.approx(0.0127)
    expected = (10 * 4.4482216152605 / 0.0254) / (20 * 0.0254)
    assert compute_soil_pressure_pa(converted) == pytest.approx(expected)


@pytest.mark.parametrize('field,value', [
    ('joint_count_dimensionless', 0), ('joint_count_dimensionless', 1.5),
    ('joint_count_dimensionless', True), ('joint_length_m', 0),
    ('outer_diameter_m', -1), ('wall_thickness_m', 0),
    ('wall_thickness_m', 0.5), ('dry_mass_per_joint_kg', -1),
    ('submerged_mass_per_joint_kg', float('nan')),
    ('youngs_modulus_pa', 0), ('poisson_ratio_dimensionless', -1),
    ('poisson_ratio_dimensionless', 0.5), ('outer_diameter_m', float('inf')),
    ('joint_length_m', True), ('poisson_ratio_dimensionless', '0.25'),
])
def test_invalid_joint_input(field, value):
    with pytest.raises(ValueError):
        replace(illustrative_joint(), **{field: value})


@pytest.mark.parametrize('params', [
    (0, 0, 0, 0), (10, 20, 0.4, 1000), (-1, 0, 0.4, 1000),
    (10, 0, -0.4, 1000), (10, 0, 0.4, float('nan')),
])
def test_invalid_buoyancy_input(params):
    with pytest.raises(ValueError):
        BuoyancyInput(*params)


@pytest.mark.parametrize('params', [
    (10, 2, 0), (float('inf'), 2, 10), (10, float('nan'), 10),
])
def test_invalid_tension_input(params):
    with pytest.raises(ValueError):
        TensionInput(*params)


@pytest.mark.parametrize('params', [(0, 1, 0), (-0.1, 1, 1), (0, float('inf'), 1)])
def test_invalid_soil_input(params):
    with pytest.raises(ValueError):
        SoilResistanceInput(*params)


@pytest.mark.parametrize('mass,ratio', [(-1, 1), (1, 2), (1, float('nan'))])
def test_invalid_apparent_mass_ratio(mass, ratio):
    with pytest.raises(ValueError):
        apparent_mass_from_ratio(mass, ratio)


@pytest.mark.parametrize('scale', [0, -1, float('nan'), True])
def test_invalid_boundary_coefficient(scale):
    with pytest.raises(ValueError):
        SoilResistanceInput.from_imperial(
            deflection_in=0.5, line_load_lbf_per_in=10,
            reference_diameter_in=20, force_conversion_n_per_lbf=scale,
        )


def test_mass_from_required_linear_density():
    assert mass_from_linear_density(25.0, 8.0) == 200.0
    assert mass_from_linear_density(0.0, 8.0) == 0.0
    for density, length in [(-1, 8), (25, 0), (float('nan'), 8), (25, True)]:
        with pytest.raises(ValueError):
            mass_from_linear_density(density, length)
