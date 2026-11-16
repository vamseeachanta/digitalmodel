"""Neutral regression cases for methods used by a legacy engineering workbook.

All numeric inputs below are an illustrative example, chosen independently of
source data. Existing functions are reused; no parallel mechanics is introduced.
Lengths are m, modulus Pa, mass per length kg/m, area m2, inertia m4, stiffness
N or N.m2, and tension kN. The imperial boundary returns SI properties.

Method: A = pi*(R**2-r**2), I = pi*(R**4-r**4)/4, J = 2*I;
EA = E*A, EI = E*I, GJ = E*J/(2*(1+nu)); a supplied displaced volume per
length is inverted to a circular diameter. Component service force is subtracted
from the preceding tension. These are geometry and static mechanics checks.

Source citation: API-1111; clause not stated in source. The title alone does
not establish a standards basis for these formulas or pressure wall sizing.
Input gaps: water density, gravity, wet/dry mass basis and empirical mass factors
require caller qualification; no values are inferred here. A flooded-bore mass
convention requires reconciliation before supplying displaced volume. Historical
solver tension, environmental loads and soil resistance tables are not generated.
Limits: geometry and force arithmetic do not establish strength or dynamic
response. The low-level section kernels do not validate their full input domain;
existing tube validation is geometric, and finite-number validation is tested
only at the existing explicit length-conversion boundary.
"""

import math

import pytest

from digitalmodel.common.units import length_in_metres
from digitalmodel.drilling_riser.global_model.build import hydrodynamic_od_m
from digitalmodel.drilling_riser.global_model.spec import tube_section
from digitalmodel.drilling_riser.section import (
    section_properties,
    section_properties_imperial,
)
from digitalmodel.drilling_riser.stackup import effective_tension
from digitalmodel.units import Q_


def test_annulus_section_properties_match_radius_identities():
    # Illustrative example: thick annulus with chosen modulus and Poisson ratio.
    outer_radius_m, inner_radius_m = 0.4, 0.2
    elastic_modulus_pa, poisson_ratio = 180e9, 0.25
    geometry = tube_section(
        od_m=2 * outer_radius_m,
        wall_m=outer_radius_m - inner_radius_m,
        youngs_modulus_pa=elastic_modulus_pa,
    )
    actual = section_properties(
        2 * outer_radius_m, geometry['id_m'], elastic_modulus_pa, poisson_ratio
    )
    # Closed forms for these chosen radii; independent of production helpers.
    area_m2 = 0.12 * math.pi
    inertia_m4 = 0.006 * math.pi
    assert geometry['id_m'] == pytest.approx(2 * inner_radius_m)
    assert actual.area == pytest.approx(area_m2, rel=1e-9, abs=1e-12)
    assert actual.moment_of_inertia == pytest.approx(inertia_m4, rel=1e-9, abs=1e-12)
    assert actual.polar_moment == pytest.approx(2 * inertia_m4, rel=1e-9, abs=1e-12)
    assert actual.shear_modulus == pytest.approx(72e9, rel=1e-9, abs=1e-12)
    assert actual.axial_stiffness == pytest.approx(21.6e9 * math.pi, rel=1e-9)
    assert actual.bending_stiffness == pytest.approx(1.08e9 * math.pi, rel=1e-9)
    assert actual.torsional_stiffness == pytest.approx(0.864e9 * math.pi, rel=1e-9)
    assert geometry['ea_n'] == pytest.approx(elastic_modulus_pa * area_m2, rel=1e-9)
    assert geometry['ei_nm2'] == pytest.approx(
        elastic_modulus_pa * inertia_m4, rel=1e-9
    )


def test_length_scaling_preserves_area_and_stiffness_powers():
    # Illustrative example: a geometric similarity identity, with fixed material.
    base = section_properties(0.8, 0.4, 180e9, 0.25)
    scaled = section_properties(2.4, 1.2, 180e9, 0.25)
    assert scaled.area / base.area == pytest.approx(9.0, rel=1e-9)
    assert scaled.axial_stiffness / base.axial_stiffness == pytest.approx(9.0, rel=1e-9)
    assert scaled.moment_of_inertia / base.moment_of_inertia == pytest.approx(
        81.0, rel=1e-9
    )
    assert scaled.bending_stiffness / base.bending_stiffness == pytest.approx(
        81.0, rel=1e-9
    )
    assert scaled.torsional_stiffness / base.torsional_stiffness == pytest.approx(
        81.0, rel=1e-9
    )


def test_imperial_boundary_against_quantity_conversion_and_radius_formulas():
    # Illustrative example. Conversion is delegated to the existing registry.
    outer_in, inner_in, modulus_ksi, nu = 16.0, 14.0, 28000.0, 0.25
    actual = section_properties_imperial(outer_in, inner_in, modulus_ksi, nu)
    radius_m = Q_(outer_in, 'inch').to('m').magnitude / 2
    bore_radius_m = Q_(inner_in, 'inch').to('m').magnitude / 2
    modulus_pa = Q_(modulus_ksi, 'ksi').to('Pa').magnitude
    area_m2 = math.pi * (radius_m**2 - bore_radius_m**2)
    inertia_m4 = math.pi * (radius_m**4 - bore_radius_m**4) / 4
    assert actual.area == pytest.approx(area_m2, rel=1e-9, abs=1e-12)
    assert actual.moment_of_inertia == pytest.approx(inertia_m4, rel=1e-9, abs=1e-12)
    assert actual.axial_stiffness == pytest.approx(modulus_pa * area_m2, rel=1e-9)
    assert actual.bending_stiffness == pytest.approx(modulus_pa * inertia_m4, rel=1e-9)
    assert actual.torsional_stiffness == pytest.approx(
        modulus_pa * inertia_m4 / (1 + nu), rel=1e-9
    )


def test_diameter_inverts_supplied_displaced_volume_with_flooded_bore():
    # Illustrative example: apparent mass difference represents annulus buoyancy.
    density_kg_m3 = 1000.0
    dry_mass_kg_m = 600.0
    wet_mass_kg_m = dry_mass_kg_m - density_kg_m3 * 0.12 * math.pi
    bore_area_m2 = math.pi * 0.2**2
    annulus_volume_m3_m = (dry_mass_kg_m - wet_mass_kg_m) / density_kg_m3
    displaced_volume_m3_m = annulus_volume_m3_m + bore_area_m2
    assert hydrodynamic_od_m(displaced_volume_m3_m) == pytest.approx(
        0.8, rel=1e-9, abs=1e-12
    )


@pytest.mark.parametrize('diameter_m', [0.2, 0.8, 1.6])
def test_diameter_inverse_area_identity(diameter_m):
    # Illustrative example: area from radius is the independent input.
    volume_m3_m = math.pi * (diameter_m / 2)**2
    assert hydrodynamic_od_m(volume_m3_m) == pytest.approx(
        diameter_m, rel=1e-9, abs=1e-12
    )


def test_tension_chain_accumulates_weight_and_signed_uplift():
    # effective_tension is T - W*f; here f=1.0 and W is a signed per-component
    # force, i.e. plain subtraction outside the documented whole-string meaning.
    # Illustrative example: no undocumented gravity conversion is required.
    service_forces_kn = (125.0, -40.0, 75.0)
    tension_kn = 800.0
    for force_kn in service_forces_kn:
        tension_kn = effective_tension(tension_kn, force_kn, 1.0)
    assert tension_kn == pytest.approx(640.0)
    assert effective_tension(800.0, 125.0, 1.0) == pytest.approx(675.0)
    assert effective_tension(800.0, 125.0, 0.0) == pytest.approx(800.0)
    assert effective_tension(800.0, 0.0, 1.0) == pytest.approx(800.0)
    assert effective_tension(20.0, 80.0, 1.0) == pytest.approx(-60.0)


@pytest.mark.parametrize(
    'od_m,wall_m',
    [
        (0.0, 0.1),
        (-0.8, 0.1),
        (0.8, 0.0),
        (0.8, -0.1),
        (0.8, 0.4),
        (0.8, 0.5),
    ],
)
def test_existing_tube_boundary_rejects_impossible_geometry(od_m, wall_m):
    with pytest.raises(ValueError, match='invalid tube'):
        tube_section(od_m=od_m, wall_m=wall_m, youngs_modulus_pa=180e9)


@pytest.mark.parametrize(
    'value',
    [-1.0, math.inf, -math.inf, math.nan, True, '8', None],
)
def test_existing_length_boundary_rejects_invalid_numeric_values(value):
    with pytest.raises(ValueError, match='finite nonnegative'):
        length_in_metres(value, 'ft')


@pytest.mark.parametrize('unit', ['inch', 'survey_ft', '', None])
def test_existing_length_boundary_requires_supported_explicit_unit(unit):
    with pytest.raises(ValueError, match='explicitly'):
        length_in_metres(8.0, unit)


def test_existing_length_boundary_matches_quantity_registry():
    # Illustrative example: round length; international foot, no inferred unit.
    assert length_in_metres(8.0, 'ft') == pytest.approx(
        Q_(8.0, 'ft').to('m').magnitude, rel=1e-9
    )
    assert length_in_metres(0.0, 'm') == 0.0
