# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic hand checks for the vertical, static screen in issue #3842."""

from dataclasses import replace
import math

import pytest

from digitalmodel.well.drilling.casing_seats import (
    CasingPressureProfile,
    kick_tolerance,
    select_casing_seats,
)


def profile(**overrides):
    inputs = dict(
        depths_ft=(0.0, 6000.0, 12000.0),
        pore_ppg=(9.0, 9.0, 15.0),
        fracture_ppg=(11.0, 14.6, 17.6),
        basis="Synthetic piecewise-linear example from issue #3842",
    )
    inputs.update(overrides)
    return CasingPressureProfile(**inputs)


def selection(pressure_profile=None, **overrides):
    inputs = dict(
        td_ft=12000.0,
        trip_margin_ppg=0.5,
        kick_margin_ppg=0.5,
        trip_margin_basis="Synthetic trip allowance",
        kick_margin_basis="Synthetic shoe allowance",
    )
    inputs.update(overrides)
    return select_casing_seats(pressure_profile or profile(), **inputs)


def tolerance(section=None, **overrides):
    inputs = dict(
        influx_density_ppg=2.0,
        hole_diameter_in=8.5,
        pipe_diameter_in=5.0,
        design_kick_intensity_ppg=0.0,
        minimum_volume_bbl=10.0,
        kick_intensity_basis="Synthetic swabbed influx",
        minimum_volume_basis="Synthetic acceptance threshold",
    )
    inputs.update(overrides)
    return kick_tolerance(section or selection().sections[0], **inputs)


def test_issue_hand_check_seats_and_governing_criteria():
    result = selection()
    assert result.seat_depths_ft == pytest.approx((8800.0, 3000.0))
    assert [s.bottom_ft for s in result.sections] == pytest.approx(
        [12000.0, 8800.0, 3000.0]
    )
    assert [s.mud_weight_ppg for s in result.sections] == pytest.approx(
        [15.5, 12.3, 9.5]
    )
    assert [s.required_fracture_ppg for s in result.sections] == pytest.approx(
        [16.0, 12.8, 10.0]
    )
    assert [s.criterion for s in result.sections] == [
        "fracture_gradient", "fracture_gradient", "profile_top"
    ]
    assert result.profile.basis == profile().basis
    assert result.trip_margin_basis == "Synthetic trip allowance"
    assert result.kick_margin_basis == "Synthetic shoe allowance"


def test_issue_swabbed_kick_hand_check():
    result = tolerance()
    assert result.shoe_margin_psi == pytest.approx(228.8)
    assert result.zero_volume_kick_intensity_ppg == pytest.approx(11 / 30)
    assert result.influx_height_ft == pytest.approx(8800 / 27)
    assert result.annular_capacity_bbl_ft == pytest.approx(47.25 / 1029.4)
    # The hand example states volume to one decimal place.
    assert result.volume_bbl == pytest.approx(15.0, abs=0.05)
    assert result.status == "acceptable"
    assert result.passes
    assert result.kick_intensity_basis == "Synthetic swabbed influx"
    assert result.minimum_volume_basis == "Synthetic acceptance threshold"
    assert result.design_kick_intensity_ppg == 0.0
    assert result.minimum_volume_bbl == 10.0


def test_issue_design_kick_has_no_tolerance_even_at_zero_minimum():
    result = tolerance(design_kick_intensity_ppg=0.5, minimum_volume_bbl=0.0)
    assert result.influx_height_ft == 0.0
    assert result.volume_bbl == 0.0
    assert result.status == "no tolerance"
    assert not result.passes


def test_zero_height_boundary_and_volume_acceptance_are_distinct():
    zero = tolerance(design_kick_intensity_ppg=11 / 30)
    assert zero.status == "no tolerance"
    below = tolerance(minimum_volume_bbl=20.0)
    assert below.volume_bbl > 0.0
    assert below.status == "below minimum volume"
    assert not below.passes
    assert tolerance(minimum_volume_bbl=below.volume_bbl).passes


def test_terminal_surface_interval_has_no_shoe_pressure_margin():
    result = tolerance(selection().sections[-1])
    assert result.shoe_margin_psi == 0.0
    assert result.status == "no tolerance"


def test_all_selected_sections_can_be_checked_independently():
    checks = [tolerance(section) for section in selection().sections]
    assert len(checks) == 3
    assert checks[1].zero_volume_kick_intensity_ppg == pytest.approx(15 / 88)


def test_fracture_trough_controls_even_when_top_exceeds_requirement():
    p = profile(
        depths_ft=(0.0, 2000.0, 4000.0, 6000.0),
        pore_ppg=(9.0, 9.0, 9.0, 12.0),
        fracture_ppg=(15.0, 15.0, 11.0, 17.0),
    )
    result = selection(p, td_ft=6000.0)
    assert result.seat_depths_ft == pytest.approx((4000 + 2000 / 3,))
    assert result.sections[0].required_fracture_ppg == 13.0


def test_last_upward_crossing_controls_multiple_fracture_troughs():
    p = profile(
        depths_ft=(0.0, 1000.0, 2000.0, 3000.0, 4000.0),
        pore_ppg=(9.0, 9.0, 9.0, 9.0, 12.0),
        fracture_ppg=(11.0, 15.0, 11.0, 15.0, 17.0),
    )
    assert selection(p, td_ft=4000.0).seat_depths_ft == pytest.approx((2500.0,))


def test_td_inside_a_segment_and_top_equality():
    result = selection(td_ft=10000.0)
    assert result.sections[0].shoe_ft == pytest.approx(5000.0)
    assert result.sections[0].mud_weight_ppg == pytest.approx(13.5)
    p = profile(pore_ppg=(10.0, 10.0, 10.0))
    assert selection(p).seat_depths_ft == ()
    assert selection(p).sections[0].criterion == "profile_top"


def test_profile_copies_lists_and_does_not_extrapolate():
    depths = [0.0, 6000.0, 12000.0]
    p = profile(depths_ft=depths)
    depths[1] = 7000.0
    assert p.depths_ft == (0.0, 6000.0, 12000.0)
    assert p.pore_at(8800.0) == pytest.approx(11.8)
    assert p.fracture_at(8800.0) == pytest.approx(16.0)
    for depth in (-1.0, 12001.0, math.nan, math.inf):
        with pytest.raises(ValueError):
            p.pore_at(depth)
        with pytest.raises(ValueError):
            p.fracture_at(depth)


@pytest.mark.parametrize("overrides", [
    {"depths_ft": ()},
    {"depths_ft": (0.0,)},
    {"depths_ft": (0.0, 6000.0, 6000.0)},
    {"depths_ft": (0.0, 12000.0, 6000.0)},
    {"depths_ft": (-1.0, 6000.0, 12000.0)},
    {"depths_ft": (0.0, math.nan, 12000.0)},
    {"pore_ppg": (9.0, 10.0)},
    {"pore_ppg": (9.0, -1.0, 15.0)},
    {"pore_ppg": (9.0, math.inf, 15.0)},
    {"fracture_ppg": (11.0, 8.0, 17.6)},
    {"fracture_ppg": (11.0, math.nan, 17.6)},
    {"basis": " "},
    {"basis": None},
])
def test_invalid_profiles_are_rejected(overrides):
    with pytest.raises(ValueError):
        profile(**overrides)


@pytest.mark.parametrize("overrides", [
    {"td_ft": 0.0},
    {"td_ft": 13000.0},
    {"td_ft": math.nan},
    {"trip_margin_ppg": -0.1},
    {"kick_margin_ppg": -0.1},
    {"trip_margin_ppg": math.inf},
    {"kick_margin_ppg": math.nan},
    {"trip_margin_basis": ""},
    {"kick_margin_basis": " "},
])
def test_invalid_selection_inputs_are_rejected(overrides):
    with pytest.raises(ValueError):
        selection(**overrides)


def test_impossible_bottom_window_is_rejected():
    with pytest.raises(ValueError, match="bottom"):
        selection(kick_margin_ppg=3.0)


def test_zero_length_section_is_rejected_instead_of_repeating_forever():
    p = profile(pore_ppg=(9.0, 9.0, 16.6))
    with pytest.raises(ValueError, match="progress"):
        selection(p)


def test_nonmonotone_pore_profile_cannot_silently_underbalance_a_section():
    p = profile(pore_ppg=(9.0, 14.5, 10.0))
    with pytest.raises(ValueError, match="pore"):
        selection(p)


def test_near_parallel_profiles_stop_at_explicit_computation_limit():
    p = profile(
        depths_ft=(0.0, 12000.0),
        pore_ppg=(9.0, 21.0),
        fracture_ppg=(9.999999, 22.000011),
    )
    with pytest.raises(ValueError, match="max_sections"):
        selection(p, max_sections=20)


@pytest.mark.parametrize("limit", [0, -1, 1.5, True])
def test_section_computation_limit_must_be_a_positive_integer(limit):
    with pytest.raises(ValueError, match="max_sections"):
        selection(max_sections=limit)


@pytest.mark.parametrize("overrides", [
    {"influx_density_ppg": 15.5},
    {"influx_density_ppg": 16.0},
    {"influx_density_ppg": -1.0},
    {"influx_density_ppg": math.nan},
    {"hole_diameter_in": 5.0},
    {"hole_diameter_in": math.inf},
    {"pipe_diameter_in": 0.0},
    {"pipe_diameter_in": -1.0},
    {"design_kick_intensity_ppg": -0.1},
    {"design_kick_intensity_ppg": math.nan},
    {"minimum_volume_bbl": -1.0},
    {"minimum_volume_bbl": math.inf},
    {"kick_intensity_basis": " "},
    {"minimum_volume_basis": None},
])
def test_invalid_kick_inputs_are_rejected(overrides):
    with pytest.raises(ValueError):
        tolerance(**overrides)


@pytest.mark.parametrize("overrides", [
    {"shoe_ft": -1.0},
    {"shoe_ft": 12000.0},
    {"bottom_ft": math.inf},
    {"mud_weight_ppg": 0.0},
    {"fracture_at_shoe_ppg": math.nan},
])
def test_invalid_manual_sections_are_rejected(overrides):
    with pytest.raises(ValueError):
        tolerance(replace(selection().sections[0], **overrides))


def test_bubble_height_is_limited_to_the_open_section():
    section = replace(selection().sections[0], shoe_ft=11900.0)
    result = tolerance(section)
    assert result.influx_height_ft == 100.0
    assert result.limited_by_section_length
    assert result.volume_bbl == pytest.approx(100 * 47.25 / 1029.4)


def test_overweight_mud_reports_no_tolerance():
    section = replace(selection().sections[0], mud_weight_ppg=17.0)
    result = tolerance(section)
    assert result.shoe_margin_psi < 0.0
    assert result.status == "no tolerance"


def test_conversion_rounding_against_geometric_unit_definitions():
    # 12 in/ft, 231 in3/US gal and 42 gal/bbl independently define capacity.
    exact_capacity = math.pi / 4 * (8.5 ** 2 - 5 ** 2) * 12 / (231 * 42)
    assert tolerance().annular_capacity_bbl_ft == pytest.approx(
        exact_capacity, rel=5e-5
    )
    # Weight density / 144 in2/ft2 gives 12/231 psi/ft per lb/gal.
    exact_margin = 12 / 231 * 8800 * 0.5
    assert tolerance().shoe_margin_psi == pytest.approx(exact_margin, rel=0.0011)
