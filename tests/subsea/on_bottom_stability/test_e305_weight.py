# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand-derived synthetic geometry examples; no standards table data."""

import math

import pytest


def test_weight_build_up_synthetic_annuli_and_gravity():
    from digitalmodel.subsea.on_bottom_stability.e305_simplified import Pipe, weight_build_up

    # Synthetic geometry: remaining wall 0.1, IDs and layer ODs are exact.
    pipe = Pipe(
        steel_od_m=1.0, wall_m=0.2, corrosion_allowance_m=0.1,
        steel_density_kg_m3=8.0, corrosion_coating_m=0.1,
        corrosion_coating_density_kg_m3=3.0, concrete_m=0.1,
        concrete_density_kg_m3=4.0, absorption_mass_fraction=0.5,
        marine_growth_m=0.1, marine_growth_density_kg_m3=2.0,
        field_joint_fraction=0.25, field_joint_m=0.05,
        field_joint_density_kg_m3=5.0, contents_density_kg_m3=1.0,
        added_mass_kg_m=2.0, water_density_kg_m3=1.0,
    )
    result = weight_build_up(pipe, g=10.0)
    area = lambda outer, inner: math.pi * (outer**2 - inner**2) / 4
    masses = {
        'steel': area(1.0, 0.8)*8, 'corrosion_coating': area(1.2, 1.0)*3,
        'concrete': area(1.4, 1.2)*6*0.75,
        'field_joint': area(1.3, 1.2)*5*0.25,
        'marine_growth': (area(1.6, 1.4)*0.75 + area(1.5, 1.3)*0.25)*2,
        'contents': area(0.8, 0)*1, 'added': 2.0,
    }
    displaced_area = (area(1.6, 0)*0.75 + area(1.5, 0)*0.25)
    assert result.mass_kg_m == pytest.approx(sum(masses.values()))
    assert dict(result.component_mass_kg_m) == pytest.approx(masses)
    assert result.buoyancy_n_m == pytest.approx(displaced_area*10)
    assert result.submerged_weight_n_m == pytest.approx((sum(masses.values())-displaced_area)*10)
    assert weight_build_up(pipe, g=5).submerged_weight_n_m == pytest.approx(result.submerged_weight_n_m/2)
