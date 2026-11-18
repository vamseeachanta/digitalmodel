"""Illustrative example geometry."""

import math
import pytest


def test_axial_capacity_uses_caller_supplied_factor():
    from digitalmodel.drilling_riser.stackup_static_weights import (
        TubularCapacityInput, axial_wall_capacity_n,
    )
    p = TubularCapacityInput(0.6, 0.03, 400000000.0, 0.6)
    expected_n = 0.6 * 400000000.0 * math.pi / 4.0 * (0.6**2 - 0.54**2)
    assert axial_wall_capacity_n(p) == pytest.approx(expected_n)
    assert axial_wall_capacity_n(p) == pytest.approx(12893096.25)


def test_flooded_material_weight_uses_density_ratio():
    from digitalmodel.drilling_riser.stackup_static_weights import flooded_material_weight_n
    assert flooded_material_weight_n(16000.0, 8000.0, 1000.0) == pytest.approx(
        16000.0 * (1.0 - 1000.0 / 8000.0)
    )
    assert flooded_material_weight_n(16000.0, 8000.0, 1000.0) == pytest.approx(14000.0)


def test_axial_capacity_rejects_nonfinite_result():
    from digitalmodel.drilling_riser.stackup_static_weights import (
        TubularCapacityInput, axial_wall_capacity_n,
    )
    with pytest.raises(ValueError, match='finite'):
        axial_wall_capacity_n(TubularCapacityInput(2.0, 0.5, 1e308, 1.0))


def test_flooded_material_weight_rejects_nonfinite_result():
    from digitalmodel.drilling_riser.stackup_static_weights import flooded_material_weight_n
    with pytest.raises(ValueError, match='finite'):
        flooded_material_weight_n(1e308, 1.0, 3.0)
