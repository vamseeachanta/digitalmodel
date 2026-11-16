"""Independent hand checks using illustrative example inputs only."""

from dataclasses import FrozenInstanceError, replace
from math import inf, nan

import pytest

from digitalmodel.structural.mass_properties import (
    MassItem,
    MassTotals,
    RectangularPlate,
    centre_of_gravity_m,
    combine_mass_items,
    first_mass_moments_kg_m,
    plate_areal_mass_kg_m2,
    plate_mass_kg,
)


def illustrative_plate():
    """Illustrative example: uniform rectangular material, not a design case."""
    return RectangularPlate(
        length_m=2.4,
        width_m=0.4,
        thickness_m=0.02,
        quantity_1=3,
        density_kg_m3=7500.0,
    )


def test_geometry_mass_and_first_moments_hand_formula():
    plate = illustrative_plate()
    expected_areal = 0.02 * 7500.0
    expected_mass = (2.4 * 0.4 * 0.02) * 7500.0 * 3
    assert plate_areal_mass_kg_m2(plate) == pytest.approx(expected_areal)
    assert plate_mass_kg(plate) == pytest.approx(expected_mass)
    assert plate_areal_mass_kg_m2(plate) == pytest.approx(150.0)
    assert plate_mass_kg(plate) == pytest.approx(432.0)
    item = MassItem(expected_mass, -4.0, 2.0, 6.0)
    assert first_mass_moments_kg_m(item) == pytest.approx(
        (expected_mass * -4.0, expected_mass * 2.0, expected_mass * 6.0)
    )


def test_factored_aggregate_and_three_axis_centre_hand_formula():
    # Illustrative example; the factor is supplied, with no normative meaning.
    items = [MassItem(100.0, -2.0, 2.0, 4.0), MassItem(300.0, 6.0, -2.0, 8.0)]
    totals = combine_mass_items(iter(items), mass_factor_1=1.25)
    mass = (100.0 + 300.0) * 1.25
    moments = (
        (-2.0 * 100.0 + 6.0 * 300.0) * 1.25,
        (2.0 * 100.0 - 2.0 * 300.0) * 1.25,
        (4.0 * 100.0 + 8.0 * 300.0) * 1.25,
    )
    assert totals.mass_kg == mass
    assert (
        totals.first_moment_x_kg_m,
        totals.first_moment_y_kg_m,
        totals.first_moment_z_kg_m,
    ) == moments
    assert centre_of_gravity_m(totals) == pytest.approx(tuple(m / mass for m in moments))
    assert centre_of_gravity_m(totals) == pytest.approx((4.0, -1.0, 7.0))
    assert centre_of_gravity_m(totals) == pytest.approx(
        centre_of_gravity_m(combine_mass_items(items, mass_factor_1=1.0))
    )


def test_mass_and_centre_match_existing_aggregators():
    import numpy as np

    from digitalmodel.floating_wind.fowt_flexible_body import combine_mass_properties
    from digitalmodel.naval_architecture.loading_computer import (
        WeightItem,
        total_weight_and_centroids,
    )

    items = [MassItem(100.0, -2.0, 2.0, 4.0), MassItem(300.0, 6.0, -2.0, 8.0)]
    mass, centre, _ = combine_mass_properties([
        (item.mass_kg, (item.x_m, item.y_m, item.z_m), np.zeros((3, 3)))
        for item in items
    ])
    totals = combine_mass_items(items, mass_factor_1=1.0)
    assert totals.mass_kg == pytest.approx(mass)
    assert centre_of_gravity_m(totals) == pytest.approx(centre)
    weight_t, lcg_m, vcg_m = total_weight_and_centroids([
        WeightItem(
            name=f"item_{index}",
            weight_t=item.mass_kg / 1000.0,
            lcg_m=item.x_m,
            vcg_m=item.z_m,
        )
        for index, item in enumerate(items)
    ])
    assert totals.mass_kg == pytest.approx(weight_t * 1000.0)
    x_m, _, z_m = centre_of_gravity_m(totals)
    assert (x_m, z_m) == pytest.approx((lcg_m, vcg_m))


def test_centre_delegates_source_items_to_canonical_aggregation(monkeypatch):
    from digitalmodel.floating_wind import fowt_flexible_body

    canonical = fowt_flexible_body.combine_mass_properties
    parts_seen = []

    def record_parts(parts):
        parts_seen.extend(parts)
        return canonical(parts)

    monkeypatch.setattr(fowt_flexible_body, "combine_mass_properties", record_parts)
    items = [MassItem(100.0, -2.0, 2.0, 4.0), MassItem(300.0, 6.0, -2.0, 8.0)]
    totals = combine_mass_items(iter(items), mass_factor_1=1.25)
    assert centre_of_gravity_m(totals) == pytest.approx((4.0, -1.0, 7.0))
    assert [(mass, tuple(centre)) for mass, centre, _ in parts_seen] == [
        (100.0, (-2.0, 2.0, 4.0)),
        (300.0, (6.0, -2.0, 8.0)),
    ]


def test_centre_from_explicit_totals():
    assert centre_of_gravity_m(MassTotals(400.0, 1600.0, -400.0, 2800.0)) == (
        4.0, -1.0, 7.0
    )


def test_quantity_keyword_accepts_dimensionless_count():
    plate = RectangularPlate(
        length_m=2.4,
        width_m=0.4,
        thickness_m=0.02,
        quantity_1=3,
        density_kg_m3=7500.0,
    )
    assert plate_mass_kg(plate) == pytest.approx(432.0)


def test_category_selection_precedes_aggregation():
    # Illustrative example: callers own the category names and group selection.
    categories = {
        "group_a": [MassItem(40.0, 2.0, -4.0, 6.0)],
        "group_b": [MassItem(60.0, -2.0, 4.0, 2.0)],
    }
    selected = combine_mass_items(categories["group_a"], mass_factor_1=1.5)
    assert selected.mass_kg == 40.0 * 1.5
    assert centre_of_gravity_m(selected) == (2.0, -4.0, 6.0)


def test_translation_of_datum_translates_the_centre():
    items = [MassItem(40.0, -2.0, 4.0, 6.0), MassItem(60.0, 2.0, -4.0, 2.0)]
    shift = (20.0, -10.0, 4.0)
    shifted = [
        MassItem(p.mass_kg, p.x_m + shift[0], p.y_m + shift[1], p.z_m + shift[2])
        for p in items
    ]
    original = centre_of_gravity_m(combine_mass_items(items, mass_factor_1=1.0))
    result = centre_of_gravity_m(combine_mass_items(shifted, mass_factor_1=1.0))
    assert result == pytest.approx(tuple(c + d for c, d in zip(original, shift)))


def test_symmetric_masses_cancel_signed_moments():
    totals = combine_mass_items(
        [MassItem(40.0, -2.0, -4.0, -6.0), MassItem(40.0, 2.0, 4.0, 6.0)],
        mass_factor_1=1.0,
    )
    assert centre_of_gravity_m(totals) == (0.0, 0.0, 0.0)


@pytest.mark.parametrize("items", [[], [MassItem(0.0, 2.0, -4.0, 6.0)]])
def test_zero_mass_has_zero_moments_but_undefined_centre(items):
    totals = combine_mass_items(items, mass_factor_1=1.0)
    assert totals == MassTotals(0.0, 0.0, 0.0, 0.0)
    with pytest.raises(ValueError, match="positive mass"):
        centre_of_gravity_m(totals)


def test_zero_quantity_is_zero_mass():
    assert plate_mass_kg(replace(illustrative_plate(), quantity_1=0)) == 0.0


@pytest.mark.parametrize("field", ["length_m", "width_m", "thickness_m", "density_kg_m3"])
@pytest.mark.parametrize("bad", [0.0, -2.0, nan, inf, -inf, True, "2", None])
def test_plate_rejects_invalid_physical_inputs(field, bad):
    with pytest.raises(ValueError, match=field):
        replace(illustrative_plate(), **{field: bad})


@pytest.mark.parametrize("bad", [-1, 2.5, True, "2", None, nan, inf])
def test_quantity_must_be_nonnegative_integer(bad):
    with pytest.raises(ValueError, match="quantity_1"):
        replace(illustrative_plate(), quantity_1=bad)


@pytest.mark.parametrize("bad", [-2.0, nan, inf, -inf, True, "2", None])
def test_mass_item_rejects_invalid_mass(bad):
    with pytest.raises(ValueError, match="mass_kg"):
        MassItem(bad, 0.0, 0.0, 0.0)


@pytest.mark.parametrize("field", ["x_m", "y_m", "z_m"])
@pytest.mark.parametrize("bad", [nan, inf, -inf, True, "2", None])
def test_mass_item_requires_finite_coordinates(field, bad):
    with pytest.raises(ValueError, match=field):
        replace(MassItem(40.0, 0.0, 0.0, 0.0), **{field: bad})


@pytest.mark.parametrize("bad", [0.0, -2.0, nan, inf, True, "2", None])
def test_factor_must_be_positive_and_finite(bad):
    with pytest.raises(ValueError, match="mass_factor_1"):
        combine_mass_items([], mass_factor_1=bad)


def test_density_and_mass_factor_have_no_defaults():
    with pytest.raises(TypeError):
        RectangularPlate(length_m=2.0, width_m=0.4, thickness_m=0.02, quantity_1=1)
    with pytest.raises(TypeError):
        combine_mass_items([])


@pytest.mark.parametrize(
    "instance,field",
    [(illustrative_plate(), "quantity_1"), (MassItem(40.0, 2.0, 4.0, 6.0), "mass_kg")],
)
def test_inputs_are_frozen(instance, field):
    with pytest.raises(FrozenInstanceError):
        setattr(instance, field, 2)


def test_nonfinite_arithmetic_is_rejected():
    with pytest.raises(ValueError, match="finite"):
        plate_areal_mass_kg_m2(replace(illustrative_plate(), thickness_m=1e308))
    with pytest.raises(ValueError, match="finite"):
        plate_mass_kg(replace(illustrative_plate(), length_m=1e308))
    with pytest.raises(ValueError, match="finite"):
        first_mass_moments_kg_m(MassItem(1e308, 2.0, 0.0, 0.0))
    with pytest.raises(ValueError, match="finite"):
        combine_mass_items([MassItem(1e308, 0.0, 0.0, 0.0)] * 2, mass_factor_1=1.0)
    with pytest.raises(ValueError, match="finite"):
        combine_mass_items([MassItem(1e308, 0.0, 0.0, 0.0)], mass_factor_1=2.0)
    with pytest.raises(ValueError, match="finite"):
        centre_of_gravity_m(MassTotals(1e-308, 1e308, 0.0, 0.0))


def test_zero_mass_totals_cannot_have_nonzero_moments():
    with pytest.raises(ValueError, match="zero mass"):
        MassTotals(0.0, 2.0, 0.0, 0.0)


@pytest.mark.parametrize("bad", [-2.0, nan, inf])
def test_totals_reject_invalid_mass(bad):
    with pytest.raises(ValueError, match="mass_kg"):
        MassTotals(bad, 0.0, 0.0, 0.0)


def test_totals_reject_nonfinite_moments():
    with pytest.raises(ValueError, match="first_moment_y_kg_m"):
        MassTotals(40.0, 0.0, inf, 0.0)
