"""Illustrative inputs exercising validation and limiting cases."""

from dataclasses import FrozenInstanceError, replace
import math
import pytest

from digitalmodel.drilling_riser.stackup_static_weights import (
    JointWeightInput, HydrostaticSectionInput, TensionInput, TubularCapacityInput,
    adjusted_joint_weights, section_weights, minimum_tension, axial_wall_capacity_n,
)


def section():
    return HydrostaticSectionInput(10.0, 30.0, 20000.0, 15000.0,
        0.2, 0.0, 0.0, 0.0, 0.0, 1000.0, 1000.0, 10.0)


def tension():
    return TensionInput(200000.0, 1.0, 0.0, 50000.0, 1.0,
        100000.0, 100000.0, 8, 0, 0.0, 0.0, 0.0, 500000.0, 1.0, 1.0, 1.0)


def test_equal_fluids_recover_supplied_flooded_wet_weight():
    result = section_weights(section())
    assert result.filled_submerged_weight_n == pytest.approx(3.0 * 15000.0)
    empty = section_weights(replace(section(), installed_length_m=0.0))
    assert empty.filled_submerged_weight_n == 0.0


def test_neutral_tension_factors_and_no_failures():
    result = minimum_tension(tension())
    assert result.fluid_correction_n == 0.0
    assert result.slip_ring_tension_n == 200000.0 - 50000.0
    assert result.minimum_top_tension_n == result.slip_ring_tension_n
    assert result.additional_overpull_n == 0.0
    assert result.overall_top_tension_n == result.minimum_top_tension_n


def test_uplift_and_combined_assembly_remain_signed():
    p = JointWeightInput(0.0, 0.0, 8000.0, -2000.0, 0.0, 1.0)
    assert adjusted_joint_weights(p) == (8000.0, -2000.0)
    with pytest.raises(FrozenInstanceError):
        p.dry_weight_n = 10.0


@pytest.mark.parametrize('bad', [math.nan, math.inf, -math.inf, True, '3'])
def test_nonfinite_and_nonreal_inputs_are_rejected(bad):
    with pytest.raises(ValueError):
        replace(section(), installed_length_m=bad)
    with pytest.raises(ValueError):
        replace(tension(), internal_fluid_weight_n=bad)
    with pytest.raises(ValueError):
        JointWeightInput(bad, 0.0, 0.0, 0.0, 0.0, 1.0)
    with pytest.raises(ValueError):
        TubularCapacityInput(0.6, 0.03, bad, 0.6)


@pytest.mark.parametrize('field,value', [
    ('joint_length_m', 0.0), ('joint_length_m', -1.0),
    ('installed_length_m', -1.0), ('bore_area_m2', -0.1),
    ('auxiliary_bore_area_m2', -0.1), ('gravity_m_s2', 0.0),
    ('water_density_kg_m3', 0.0), ('fluid_density_kg_m3', -1.0),
])
def test_section_physical_domains(field, value):
    with pytest.raises(ValueError):
        replace(section(), **{field:value})


@pytest.mark.parametrize('wet_joint_weight_n,auxiliary_wet_weight_n_m', [
    (40000.0, 0.0), (18000.0, 2200.0),
])
def test_section_rejects_negative_exterior_displacement(
    wet_joint_weight_n, auxiliary_wet_weight_n_m,
):
    inputs = replace(
        section(), installed_length_m=10.0, dry_joint_weight_n=18000.0,
        wet_joint_weight_n=wet_joint_weight_n,
        auxiliary_wet_weight_n_m=auxiliary_wet_weight_n_m,
    )
    with pytest.raises(ValueError, match='displaced water mass'):
        section_weights(inputs)


def test_section_allows_zero_exterior_displacement():
    inputs = replace(
        section(), dry_joint_weight_n=18000.0, wet_joint_weight_n=38000.0,
    )
    result = section_weights(inputs)
    assert result.displaced_water_mass_kg_m == 0.0
    assert result.filled_submerged_weight_n == result.filled_air_weight_n


@pytest.mark.parametrize('field,value', [
    ('tensioner_count', 0), ('tensioner_count', 2.5),
    ('tensioner_count', True), ('failed_tensioner_count', -1),
    ('failed_tensioner_count', 8), ('failed_tensioner_count', 1.5),
    ('failed_tensioner_count', False), ('steel_weight_factor_1', 0.8),
    ('buoyancy_factor_1', 1.1), ('buoyancy_factor_1', -0.1),
    ('setting_fraction', 1.1), ('setting_fraction', -0.1),
    ('minimum_lower_package_overpull_n', -1.0),
])
def test_tension_physical_domains(field, value):
    with pytest.raises(ValueError):
        replace(tension(), **{field:value})


@pytest.mark.parametrize('args', [
    (0.0, 0.03, 400000000.0, 0.6), (0.6, 0.0, 400000000.0, 0.6),
    (0.6, 0.3, 400000000.0, 0.6), (0.6, 0.4, 400000000.0, 0.6),
    (0.6, 0.03, 0.0, 0.6), (0.6, 0.03, 400000000.0, 1.1),
])
def test_capacity_physical_domains(args):
    with pytest.raises(ValueError):
        axial_wall_capacity_n(TubularCapacityInput(*args))


def test_overflow_does_not_produce_a_usable_weight_result():
    with pytest.raises(ValueError, match='finite'):
        adjusted_joint_weights(JointWeightInput(1e308, 1e308, 0.0, 0.0, 1.0, 1.0))


def test_tension_factors_are_required_inputs():
    with pytest.raises(TypeError):
        TensionInput(200000.0, 1.0, 0.0, 50000.0, 1.0,
            100000.0, 100000.0, 8, 0, 0.0, 0.0, 0.0, 500000.0, 1.0)


@pytest.mark.parametrize("efficiency", [0.0, -0.1, 1.01, math.nan, True])
def test_tensioner_efficiency_out_of_range_is_rejected(efficiency):
    with pytest.raises(ValueError):
        replace(tension(), tensioner_efficiency_ratio=efficiency)


@pytest.mark.parametrize("factor", [0.99, math.inf, math.nan])
def test_fleet_angle_factor_below_unity_is_rejected(factor):
    with pytest.raises(ValueError):
        replace(tension(), fleet_angle_factor=factor)
