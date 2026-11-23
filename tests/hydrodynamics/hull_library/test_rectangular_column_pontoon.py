# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rectangular column spacing and labelled generic proportions."""

from decimal import Decimal
import numpy as np
import pytest

from digitalmodel.hydrodynamics.hull_library.column_pontoon_form import (
    ColumnPontoonParameters,
    generate_column_pontoon,
)

# Existing square and OC4 controls from test_column_pontoon_form.py.
LEGACY_CASES = [
    dict(count=4, spacing=20, square_side=4, draft=10,
         pontoon_layout="twin", pontoon_width=2, pontoon_height=3,
         panel_target_size=5, lid=True),
    dict(count=3, spacing=50, diameter=12, draft=20, center_diameter=6.5,
         heave_plate_diameter=24, heave_plate_thickness=6,
         pontoon_layout="ring", pontoon_width=1.6, pontoon_height=1.6,
         pontoon_corner_radius=0.8, pontoon_center_z=-17,
         panel_target_size=5, bracing=True, lid=True,
         comparator_class="published-geometry"),
]


@pytest.mark.parametrize("values", LEGACY_CASES, ids=["square", "oc4"])
def test_none_spacing_preserves_default_arrays(values):
    explicit = ColumnPontoonParameters(**values, transverse_spacing=None)
    actual, _ = generate_column_pontoon(explicit)
    default, _ = generate_column_pontoon(ColumnPontoonParameters(**values))
    np.testing.assert_array_equal(actual.vertices, default.vertices)
    np.testing.assert_array_equal(actual.panels, default.panels)


def test_rectangular_waterplane_and_metacentric_radii():
    # Existing controls: side 4 m, draft 10 m and spacings 20/50 m.
    params = ColumnPontoonParameters(
        count=4, spacing=20, transverse_spacing=50,
        square_side=4, draft=10, panel_target_size=5,
    )
    _, report = generate_column_pontoon(params)
    area = 4 * 4**2
    volume = area * 10
    # Integral of a square section plus A*d^2 (parallel-axis theorem).
    # OpenStax, Moebs et al. (2016), University Physics 1, Eq. 10.20.
    inertia_roll = 4 * (4**4 / 12 + 4**2 * (50 / 2)**2)
    inertia_pitch = 4 * (4**4 / 12 + 4**2 * (20 / 2)**2)
    assert report.waterplane_area == pytest.approx(area, rel=0.005)
    assert report.bm[0] == pytest.approx(inertia_roll / volume, rel=0.01)
    assert report.bm[1] == pytest.approx(inertia_pitch / volume, rel=0.01)


@pytest.mark.parametrize("axis", ["longitudinal", "transverse"])
def test_rectangular_overlap_is_rejected(axis):
    spacing, transverse = (2, 50) if axis == "longitudinal" else (20, 2)
    with pytest.raises(ValueError, match="overlap"):
        ColumnPontoonParameters(
            count=4, spacing=spacing, transverse_spacing=transverse,
            square_side=4, draft=10,
        )


def test_transverse_spacing_requires_four_columns():
    with pytest.raises(ValueError, match="four"):
        ColumnPontoonParameters(
            count=3, spacing=20, transverse_spacing=50,
            square_side=4, draft=10,
        )


def test_generic_semi_parameters_match_prescribed_values():
    from digitalmodel.hydrodynamics.hull_library.generic_semi import (
        generic_semi_parameters,
    )

    params = generic_semi_parameters(96.2, 90.0, 22.75, 4.0)
    actual = (params.square_side, params.corner_radius, params.pontoon_width,
              params.pontoon_height, params.pontoon_corner_radius,
              params.spacing, params.transverse_spacing)
    assert actual == (17.0, 1.5, 13.5, 10.0, 1.0, 79.2, 73.0)
    assert params.count == 4
    assert params.pontoon_layout == "twin"
    assert params.lid is False
    assert params.draft == 22.75
    assert params.panel_target_size == 4.0


def test_generic_pontoon_width_is_capped_to_flat_face():
    from digitalmodel.hydrodynamics.hull_library.generic_semi import (
        generic_semi_parameters,
    )

    # Synthetic negative control: side 0.6 m is 0.4 times the specified
    # 1.5 m radius; LOA follows the prescribed c=0.177*LOA relation.
    params = generic_semi_parameters(0.4 * 1.5 / 0.177, 4.0, 1.0, 4.0)
    side = Decimal(str(params.square_side))
    radius = Decimal(str(params.corner_radius))
    width = Decimal(str(params.pontoon_width))
    assert width <= side - 2 * radius
    assert width == Decimal("0.4")
