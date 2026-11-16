"""Illustrative examples; expected values use independent hand formulas."""

import pytest

from digitalmodel.drilling_riser.stackup_static_weights import (
    JointWeightInput,
    adjusted_joint_weights,
)


def test_joint_weight_allowances_and_signed_buoyancy():
    inputs = JointWeightInput(20000.0, 17000.0, 3000.0, -12000.0, 0.04, 0.9)
    dry_n, wet_n = adjusted_joint_weights(inputs)
    assert dry_n == pytest.approx(20000.0 * 1.04 + 3000.0)
    assert wet_n == pytest.approx(17000.0 * 1.04 - 12000.0 * 0.9)


def test_installed_section_includes_fluid_and_auxiliaries_once():
    from digitalmodel.drilling_riser.stackup_static_weights import (
        HydrostaticSectionInput, section_weights,
    )
    inputs = HydrostaticSectionInput(
        joint_length_m=10.0, installed_length_m=25.0,
        dry_joint_weight_n=18000.0, wet_joint_weight_n=14000.0,
        bore_area_m2=0.2, auxiliary_bore_area_m2=0.02,
        auxiliary_dry_weight_n_m=60.0, auxiliary_wet_weight_n_m=40.0,
        auxiliary_fluid_mass_kg_m=35.0, fluid_density_kg_m3=1400.0,
        water_density_kg_m3=1000.0, gravity_m_s2=10.0,
    )
    result = section_weights(inputs)
    dry_n_m = 18000.0 / 10.0 + 60.0
    wet_n_m = 14000.0 / 10.0 + 40.0
    water_kg_m = (0.2 + 0.02) * 1000.0
    displacement_kg_m = (dry_n_m - wet_n_m) / 10.0 + water_kg_m
    fluid_kg_m = 0.2 * 1400.0 + 35.0
    assert result.dry_weight_n_m == pytest.approx(dry_n_m)
    assert result.reference_wet_weight_n_m == pytest.approx(wet_n_m)
    assert result.displaced_water_mass_kg_m == pytest.approx(displacement_kg_m)
    assert result.internal_fluid_mass_kg_m == pytest.approx(fluid_kg_m)
    assert result.filled_air_weight_n == pytest.approx(25.0 * (dry_n_m + fluid_kg_m * 10.0))
    assert result.filled_submerged_weight_n == pytest.approx(
        25.0 * (dry_n_m + (fluid_kg_m - displacement_kg_m) * 10.0)
    )
