# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand volume balances for a synthetic, uniform cementing interval."""

import math
from dataclasses import replace

import pytest

from digitalmodel.well.drilling.cementing import PrimaryCementing


@pytest.fixture
def geometry():
    return PrimaryCementing(
        hole_diameter_in=10.0,
        casing_od_in=8.0,
        casing_id_in=6.0,
        shoe_depth_ft=1000.0,
        float_collar_depth_ft=900.0,
        annular_excess_fraction=0.25,
    )


def test_hand_checked_slurry_spacer_and_bump_volumes(geometry):
    # Both nominal annular and bore areas are 9*pi square inches.
    # A foot holds 108*pi cubic inches = 6*pi/539 barrels (9702 in^3/bbl).
    # Cement: 600 nominal annular feet + 150 excess feet + 100 bore feet.
    # Spacer: 100 nominal annular feet + 25 excess feet.
    # Displacement from casing entrance to collar: 900 bore feet.
    volumes = geometry.plan(target_top_of_cement_ft=400.0, spacer_length_ft=100.0)
    assert volumes.nominal_annular_slurry_bbl == pytest.approx(3600 * math.pi / 539)
    assert volumes.annular_excess_bbl == pytest.approx(900 * math.pi / 539)
    assert volumes.shoe_track_bbl == pytest.approx(600 * math.pi / 539)
    assert volumes.total_slurry_bbl == pytest.approx(5100 * math.pi / 539)
    assert volumes.spacer_bbl == pytest.approx(750 * math.pi / 539)
    assert volumes.displacement_to_bump_bbl == pytest.approx(5400 * math.pi / 539)


@pytest.mark.parametrize("excess, expected_slurry", [
    (0.0, 4200 * math.pi / 539),
    (0.25, 5100 * math.pi / 539),
    (1.0, 7800 * math.pi / 539),
])
def test_excess_changes_annular_budget_only(geometry, excess, expected_slurry):
    volumes = replace(geometry, annular_excess_fraction=excess).plan(
        target_top_of_cement_ft=400.0, spacer_length_ft=100.0,
    )
    assert volumes.total_slurry_bbl == pytest.approx(expected_slurry)
    assert volumes.shoe_track_bbl == pytest.approx(600 * math.pi / 539)
    assert volumes.displacement_to_bump_bbl == pytest.approx(5400 * math.pi / 539)


def test_top_of_cement_from_independently_supplied_volume(geometry):
    assert geometry.top_of_cement_ft(5100 * math.pi / 539) == pytest.approx(400.0)
    # Same slurry with nominal annular capacity reaches 250 ft instead.
    nominal = replace(geometry, annular_excess_fraction=0.0)
    assert nominal.top_of_cement_ft(5100 * math.pi / 539) == pytest.approx(250.0)


@pytest.mark.parametrize("target", [0.0, 250.0, 1000.0])
def test_planned_slurry_recovers_requested_top(geometry, target):
    volumes = geometry.plan(target_top_of_cement_ft=target, spacer_length_ft=0.0)
    assert geometry.top_of_cement_ft(volumes.total_slurry_bbl) == pytest.approx(target)
    assert volumes.spacer_bbl == 0.0


def test_shoe_track_only_and_full_annulus_boundaries(geometry):
    assert geometry.top_of_cement_ft(600 * math.pi / 539) == pytest.approx(1000.0)
    assert geometry.top_of_cement_ft(8100 * math.pi / 539) == pytest.approx(0.0)


def test_zero_shoe_track(geometry):
    case = replace(geometry, float_collar_depth_ft=1000.0)
    volumes = case.plan(target_top_of_cement_ft=1000.0, spacer_length_ft=0.0)
    assert volumes.shoe_track_bbl == 0.0
    assert volumes.total_slurry_bbl == 0.0
    assert volumes.displacement_to_bump_bbl == pytest.approx(6000 * math.pi / 539)
    assert case.top_of_cement_ft(0.0) == 1000.0


@pytest.mark.parametrize("collar", [0.0, 500.0, 1000.0])
def test_casing_volume_partitions_at_collar(geometry, collar):
    volumes = replace(geometry, float_collar_depth_ft=collar).plan(
        target_top_of_cement_ft=400.0, spacer_length_ft=0.0,
    )
    assert volumes.shoe_track_bbl + volumes.displacement_to_bump_bbl == pytest.approx(
        6000 * math.pi / 539,
    )


@pytest.mark.parametrize("field", [
    "hole_diameter_in", "casing_od_in", "casing_id_in", "shoe_depth_ft",
    "float_collar_depth_ft", "annular_excess_fraction",
])
@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf, -1.0, True])
def test_invalid_geometry_scalars_are_rejected(geometry, field, value):
    with pytest.raises(ValueError, match=field):
        replace(geometry, **{field: value})


@pytest.mark.parametrize("field", [
    "hole_diameter_in", "casing_od_in", "casing_id_in", "shoe_depth_ft",
])
def test_positive_dimensions_cannot_be_zero(geometry, field):
    with pytest.raises(ValueError, match=field):
        replace(geometry, **{field: 0.0})


@pytest.mark.parametrize("changes, message", [
    ({"casing_id_in": 8.0}, "casing_id_in"),
    ({"casing_id_in": 9.0}, "casing_id_in"),
    ({"hole_diameter_in": 8.0}, "hole_diameter_in"),
    ({"hole_diameter_in": 7.0}, "hole_diameter_in"),
    ({"float_collar_depth_ft": 1001.0}, "float_collar_depth_ft"),
])
def test_geometry_ordering_is_checked(geometry, changes, message):
    with pytest.raises(ValueError, match=message):
        replace(geometry, **changes)


@pytest.mark.parametrize("field", ["target_top_of_cement_ft", "spacer_length_ft"])
@pytest.mark.parametrize("value", [-1.0, 1001.0, math.nan, math.inf, True])
def test_plan_rejects_invalid_lengths(geometry, field, value):
    arguments = {"target_top_of_cement_ft": 400.0, "spacer_length_ft": 100.0}
    arguments[field] = value
    with pytest.raises(ValueError, match=field):
        geometry.plan(**arguments)


@pytest.mark.parametrize("volume", [-1.0, 0.0, 100.0, math.nan, math.inf, True])
def test_volume_outside_model_is_rejected(geometry, volume):
    with pytest.raises(ValueError, match="slurry_volume_bbl"):
        geometry.top_of_cement_ft(volume)


def test_smaller_bore_changes_displacement_without_changing_annulus(geometry):
    volumes = replace(geometry, casing_id_in=3.0).plan(
        target_top_of_cement_ft=400.0, spacer_length_ft=100.0,
    )
    assert volumes.nominal_annular_slurry_bbl == pytest.approx(3600 * math.pi / 539)
    assert volumes.shoe_track_bbl == pytest.approx(150 * math.pi / 539)
    assert volumes.displacement_to_bump_bbl == pytest.approx(1350 * math.pi / 539)
