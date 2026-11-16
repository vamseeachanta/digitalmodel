"""Illustrative example geometry and exact unit-definition identities."""

import math
import pytest


def test_axial_capacity_uses_caller_supplied_factor():
    from digitalmodel.drilling_riser.stackup_calculation import (
        TubularCapacityInput, axial_wall_capacity_n,
    )
    p = TubularCapacityInput(0.6, 0.03, 400000000.0, 0.6)
    expected_n = 0.6 * 400000000.0 * math.pi / 4.0 * (0.6**2 - 0.54**2)
    assert axial_wall_capacity_n(p) == pytest.approx(expected_n)


def test_exact_imperial_boundary_conversions():
    from digitalmodel.drilling_riser.stackup_units import (
        feet_to_m, inches_to_m, kips_to_n, n_to_kips,
        pounds_mass_to_kg, ppg_to_kg_m3, ksi_to_pa,
    )
    assert feet_to_m(1.0) == 0.3048
    assert inches_to_m(12.0) == pytest.approx(0.3048)
    assert pounds_mass_to_kg(1.0) == 0.45359237
    assert kips_to_n(1.0) == pytest.approx(1000.0 * 0.45359237 * 9.80665)
    assert n_to_kips(kips_to_n(3.0)) == pytest.approx(3.0)
    assert ksi_to_pa(1.0) == pytest.approx(1000.0 * 0.45359237 * 9.80665 / 0.0254**2)
    # A US liquid gallon is defined as 231 cubic inches.
    assert ppg_to_kg_m3(1.0) == pytest.approx(0.45359237 / (231.0 * 0.0254**3))


def test_flooded_material_weight_uses_density_ratio():
    from digitalmodel.drilling_riser.stackup_calculation import flooded_material_weight_n
    assert flooded_material_weight_n(16000.0, 8000.0, 1000.0) == pytest.approx(
        16000.0 * (1.0 - 1000.0 / 8000.0)
    )


def test_conversion_overflow_is_rejected():
    from digitalmodel.drilling_riser.stackup_units import kips_to_n
    with pytest.raises(ValueError, match='finite'):
        kips_to_n(1e308)
