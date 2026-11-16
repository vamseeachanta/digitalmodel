"""Illustrative example inputs; expected values follow independent identities."""

from dataclasses import FrozenInstanceError, replace
from math import pi, sqrt

import pytest

from digitalmodel.drilling_riser.section import cross_sectional_area, moment_of_inertia
from digitalmodel.hydrostatics.fluid_column import ft_to_m
from digitalmodel.installation.pipelay.integrity import displaced_weight_per_length
from digitalmodel.subsea.flexible_pipes.cross_section import (
    GRAVITY,
    FlexiblePipeSection,
    Layer,
    LayerType,
)
from digitalmodel.subsea.hose_properties import (
    AnnularGeometry,
    BendRadiusInput,
    DiameterSegment,
    FloatBuoyancyInput,
    HoseReferenceProperties,
    SubmergedHoseInput,
    _annular_power,
    compute_float_mass,
    compute_submerged_hose_mass,
    equivalent_buoyancy_diameter_m,
    feet_to_metres,
    millimetres_to_metres,
    minimum_bend_radius_m,
    scale_hose_properties,
    total_hose_length_m,
)


def reference_properties():
    """Illustrative example, independent of any supplier or measured hose."""
    return HoseReferenceProperties(
        geometry=AnnularGeometry(0.8, 0.4),
        bending_empty_n_m2=16000.0,
        bending_pressurized_n_m2=32000.0,
        torsional_n_m2=48000.0,
        axial_empty_n=800000.0,
        axial_pressurized_n=1200000.0,
        breaking_strength_n=200000.0,
    )


def test_equivalent_diameter_preserves_cylinder_volume():
    segments = (DiameterSegment(2.0, 0.2), DiameterSegment(6.0, 0.4))
    diameter = equivalent_buoyancy_diameter_m(segments)
    # Sum the separate cylinder volumes using radii, not the RMS expression.
    volume_m3 = pi * 0.1**2 * 2.0 + pi * 0.2**2 * 6.0
    assert total_hose_length_m(segments) == 8.0
    assert pi * (diameter / 2.0) ** 2 * 8.0 == pytest.approx(volume_m3)
    assert diameter == pytest.approx(sqrt(13) / 10, rel=1e-12)


def test_equivalent_diameter_is_independent_of_subdivision_and_order():
    one = (DiameterSegment(8.0, 0.4),)
    split = (DiameterSegment(3.0, 0.4), DiameterSegment(5.0, 0.4))
    assert equivalent_buoyancy_diameter_m(one) == pytest.approx(0.4)
    assert equivalent_buoyancy_diameter_m(split) == pytest.approx(0.4)
    assert equivalent_buoyancy_diameter_m(split[::-1]) == pytest.approx(0.4)
    assert equivalent_buoyancy_diameter_m((DiameterSegment(2.0, 0.0),)) == 0


@pytest.mark.parametrize("function", [total_hose_length_m, equivalent_buoyancy_diameter_m])
def test_empty_profile_is_rejected(function):
    with pytest.raises(ValueError):
        function(())


def test_uniform_geometric_scale_gives_fourth_and_second_power_laws():
    reference = reference_properties()
    result = scale_hose_properties(reference, AnnularGeometry(0.4, 0.2))
    # Halving every diameter divides I and J by 16, and area by 4.
    assert result.bending_empty_n_m2 == pytest.approx(1000.0)
    assert result.bending_pressurized_n_m2 == pytest.approx(2000.0)
    assert result.torsional_n_m2 == pytest.approx(3000.0)
    assert result.axial_empty_n == pytest.approx(200000.0)
    assert result.axial_pressurized_n == pytest.approx(300000.0)
    assert result.breaking_strength_n == pytest.approx(50000.0)


def test_identity_and_different_bore_scaling():
    reference = reference_properties()
    assert scale_hose_properties(reference, reference.geometry) == reference
    # Same outside diameter, solid circular section: area ratio 4/3,
    # second-moment ratio 16/15 relative to a bore half the outside diameter.
    solid = scale_hose_properties(reference, AnnularGeometry(0.8, 0.0))
    assert solid.axial_empty_n == pytest.approx(reference.axial_empty_n * 4 / 3)
    assert solid.bending_empty_n_m2 == pytest.approx(
        reference.bending_empty_n_m2 * 16 / 15
    )


def test_scaling_ratios_match_existing_section_properties():
    reference = reference_properties()
    result = scale_hose_properties(reference, AnnularGeometry(0.5, 0.3))
    assert result.axial_empty_n / reference.axial_empty_n == pytest.approx(
        cross_sectional_area(0.5, 0.3) / cross_sectional_area(0.8, 0.4),
        rel=1e-12,
    )
    assert result.bending_empty_n_m2 / reference.bending_empty_n_m2 == pytest.approx(
        moment_of_inertia(0.5, 0.3) / moment_of_inertia(0.8, 0.4),
        rel=1e-12,
    )


@pytest.mark.parametrize("power", [0, 3, 6])
def test_unsupported_annular_powers_are_rejected(power):
    with pytest.raises(ValueError):
        _annular_power(AnnularGeometry(0.4, 0.2), power)


def test_submerged_hose_mass_by_volume_balance():
    inputs = SubmergedHoseInput(
        length_m=8.0,
        dry_mass_kg=120.0,
        geometry=AnnularGeometry(0.4, 0.2),
        internal_density_kg_m3=800.0,
        external_density_kg_m3=1000.0,
    )
    result = compute_submerged_hose_mass(inputs)
    fluid = pi * 0.1**2 * 8 * 800
    displacement = pi * 0.2**2 * 8 * 1000
    assert result.dry_mass_per_length_kg_m == 15.0
    assert result.internal_fluid_mass_kg == pytest.approx(fluid)
    assert result.displaced_fluid_mass_kg == pytest.approx(displacement)
    assert result.submerged_mass_kg == pytest.approx(120 + fluid - displacement)
    assert result.submerged_mass_kg == pytest.approx(120 - 256 * pi)
    assert result.submerged_mass_kg < 0  # Buoyant mass-equivalent is signed.


def test_submerged_hose_mass_matches_existing_flexible_pipe_section():
    section = FlexiblePipeSection([
        Layer(
            layer_type=LayerType.PRESSURE_SHEATH,
            inner_diameter=0.2,
            thickness=0.1,
            density=15 / (pi * (0.2**2 - 0.1**2)),
        )
    ])
    result = compute_submerged_hose_mass(
        SubmergedHoseInput(8, 120, AnnularGeometry(0.4, 0.2), 800, 1000)
    )
    assert section.dry_mass_per_length() == pytest.approx(15)
    assert result.submerged_mass_kg / 8 * GRAVITY == pytest.approx(
        section.submerged_weight_per_length(800, 1000)
    )


def test_displaced_hose_mass_matches_existing_pipelay_displacement():
    result = compute_submerged_hose_mass(
        SubmergedHoseInput(8, 120, AnnularGeometry(0.4, 0.2), 800, 1000)
    )
    assert result.displaced_fluid_mass_kg / 8 * 9.80665 == pytest.approx(
        displaced_weight_per_length(0.4, 1000)
    )


def test_zero_density_and_neutral_buoyancy_limits():
    base = SubmergedHoseInput(5.0, 0.0, AnnularGeometry(0.4, 0.0), 0.0, 0.0)
    assert compute_submerged_hose_mass(base).submerged_mass_kg == 0
    assert compute_submerged_hose_mass(replace(base, dry_mass_kg=80)).submerged_mass_kg == 80
    displaced = 1000 * pi * 0.2**2 * 5
    neutral = replace(base, dry_mass_kg=displaced, external_density_kg_m3=1000)
    assert compute_submerged_hose_mass(neutral).submerged_mass_kg == pytest.approx(0)


def test_float_mass_and_signed_net_buoyancy():
    result = compute_float_mass(FloatBuoyancyInput(0.8, 500.0, 1000.0))
    assert result.displaced_fluid_mass_kg == 800
    assert result.dry_mass_kg == 300
    heavy = compute_float_mass(FloatBuoyancyInput(0.8, -100.0, 1000.0))
    assert heavy.dry_mass_kg == 900


def test_bend_radius_requires_an_explicit_dimensionless_factor():
    assert minimum_bend_radius_m(BendRadiusInput(0.2, 7.0)) == pytest.approx(1.4)


def test_exact_boundary_unit_definitions():
    # Illustrative example: 1250 international feet are exactly 381 metres;
    # 1250 millimetres are exactly 1.25 metres. No supplier dimensions are used.
    assert feet_to_metres(1250.0) == 381.0
    assert millimetres_to_metres(1250.0) == 1.25


@pytest.mark.parametrize("length_ft", [0.0, 1.0, 1250.0])
def test_feet_conversion_matches_existing_converter(length_ft):
    assert feet_to_metres(length_ft) == ft_to_m(length_ft)


@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -float("inf"), True, "2"])
def test_nonphysical_numeric_types_are_rejected(bad):
    constructors = [
        lambda: DiameterSegment(bad, 0.2),
        lambda: DiameterSegment(2.0, bad),
        lambda: AnnularGeometry(bad, 0.1),
        lambda: AnnularGeometry(0.4, bad),
        lambda: BendRadiusInput(0.2, bad),
        lambda: FloatBuoyancyInput(0.8, bad, 1000),
        lambda: replace(reference_properties(), axial_empty_n=bad),
        lambda: SubmergedHoseInput(8, bad, AnnularGeometry(0.4, 0.2), 800, 1000),
        lambda: feet_to_metres(bad),
        lambda: millimetres_to_metres(bad),
    ]
    for construct in constructors:
        with pytest.raises(ValueError):
            construct()


@pytest.mark.parametrize(
    "construct",
    [
        lambda: DiameterSegment(0, 0.2),
        lambda: DiameterSegment(-2, 0.2),
        lambda: DiameterSegment(2, -0.2),
        lambda: AnnularGeometry(0, 0),
        lambda: AnnularGeometry(0.4, 0.4),
        lambda: AnnularGeometry(0.4, 0.5),
        lambda: AnnularGeometry(0.4, -0.1),
        lambda: BendRadiusInput(0.2, 0),
        lambda: BendRadiusInput(0, 7),
        lambda: FloatBuoyancyInput(0, 0, 1000),
        lambda: FloatBuoyancyInput(0.8, 900, 1000),
        lambda: FloatBuoyancyInput(0.8, 0, 0),
        lambda: replace(reference_properties(), breaking_strength_n=-1),
        lambda: SubmergedHoseInput(0, 80, AnnularGeometry(0.4, 0.2), 800, 1000),
        lambda: SubmergedHoseInput(8, -1, AnnularGeometry(0.4, 0.2), 800, 1000),
        lambda: SubmergedHoseInput(8, 80, AnnularGeometry(0.4, 0.2), -1, 1000),
        lambda: SubmergedHoseInput(8, 80, AnnularGeometry(0.4, 0.2), 800, -1),
        lambda: feet_to_metres(-1),
        lambda: millimetres_to_metres(-1),
    ],
)
def test_invalid_physical_domains_are_rejected(construct):
    with pytest.raises(ValueError):
        construct()


def test_inputs_are_frozen_and_coefficients_have_no_defaults():
    segment = DiameterSegment(2.0, 0.2)
    with pytest.raises(FrozenInstanceError):
        segment.length_m = 3.0
    with pytest.raises(TypeError):
        BendRadiusInput(0.2)
    with pytest.raises(TypeError):
        FloatBuoyancyInput(0.8, 500)
