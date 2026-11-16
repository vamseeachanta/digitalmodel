"""Illustrative example inputs; expected values follow independent hand formulas."""

from dataclasses import FrozenInstanceError, replace
from math import pi, sqrt

import pytest

from digitalmodel.asset_integrity.air_gap import (
    AirGapBasis, Elevation, calculate_air_gap,
)
from digitalmodel.common.units import length_in_metres
from digitalmodel.drilling_riser.heave_response import (
    PeriodInput, StretchInput, StrokeAmplificationInput, StrokeInput,
    WaterplaneInput, WaveMotionInput,
    amplified_stroke, buoyancy_stretch, deck_heave_clearance, heave_period_with_risers,
    stroke_budget, waterplane_response, wave_motion,
)
from digitalmodel.naval_architecture.seakeeping import natural_heave_period
from digitalmodel.orcawave.hydro_coefficients import create_hydrostatic_restoring


@pytest.fixture
def waterplane():
    return WaterplaneInput(12.0, 16.0, 6, 1000.0, 10.0)


@pytest.fixture
def stroke():
    return StrokeInput(
        wave_upstroke_m=12.0, wave_downstroke_m=16.0,
        reference_rao_ratio=0.6, target_rao_ratio=0.2,
        wave_divisor_ratio=2.0, tide_upstroke_m=0.2,
        offset_downstroke_m=2.0, current_downstroke_m=0.6,
        surge_downstroke_m=0.8, tide_downstroke_m=0.2,
    )


def test_waterplane_reuses_restoring_stiffness(waterplane):
    result = waterplane_response(waterplane)
    area = 12.0 * 16.0
    stiffness = 1000.0 * 10.0 * area
    assert result.column_area_m2 == area
    assert result.total_area_m2 == 6 * area
    assert result.column_stiffness_n_m == stiffness
    assert result.total_stiffness_n_m == 6 * stiffness
    existing = create_hydrostatic_restoring(0, area, 0, 0, rho=1000, g=10)
    assert result.column_stiffness_n_m == existing[2][2]


@pytest.mark.parametrize("count", [0, 1, 3, 6])
def test_period_scaling_at_fixed_effective_mass(count):
    inputs = PeriodInput(24.0, 8e6, 2e6, count)
    result = heave_period_with_risers(inputs)
    expected = 24.0 * sqrt(8e6 / (8e6 + count * 2e6))
    assert result.period_s == pytest.approx(expected, rel=1e-13)
    assert result.total_riser_stiffness_n_m == count * 2e6
    assert result.waterplane_to_riser_ratio == (
        pytest.approx(8e6 / (count * 2e6)) if count else None
    )


def test_existing_heave_period_matches_independent_oscillator():
    # Illustrative example: mass is supplied in tonnes to the existing API.
    result = natural_heave_period(18000.0, 1200.0)
    expected = 2 * pi * sqrt(18000.0 * 1000 / (1025.0 * 9.81 * 1200.0))
    assert result == pytest.approx(expected, rel=1e-13)


def test_zero_riser_stiffness_recovers_baseline():
    result = heave_period_with_risers(PeriodInput(24.0, 8e6, 0.0, 3))
    assert result.period_s == 24.0
    assert result.waterplane_to_riser_ratio is None


def test_stroke_budget_scales_only_wave_terms(stroke):
    result = stroke_budget(stroke)
    up_wave = 12.0 * (0.2 / 0.6) / 2.0
    down_wave = 16.0 * (0.2 / 0.6) / 2.0
    static_down = 2.0 + 0.6 + 0.8 + 0.2
    assert result.wave_upstroke_m == pytest.approx(up_wave)
    assert result.wave_downstroke_m == pytest.approx(down_wave)
    assert result.upstroke_m == pytest.approx(up_wave + 0.2)
    assert result.downstroke_m == pytest.approx(down_wave + static_down)
    assert result.total_stroke_m == pytest.approx(up_wave + down_wave + 0.2 + static_down)
    assert result.net_upstroke_m == pytest.approx(up_wave + 0.2 - static_down)


def test_zero_rao_preserves_static_terms(stroke):
    result = stroke_budget(replace(stroke, target_rao_ratio=0.0))
    assert result.wave_upstroke_m == result.wave_downstroke_m == 0
    assert result.total_stroke_m == pytest.approx(0.2 + 2.0 + 0.6 + 0.8 + 0.2)
    assert result.net_upstroke_m < 0


def test_wave_height_and_amplitude_are_distinct():
    result = wave_motion(WaveMotionInput(12.0, 0.2, 2.0))
    assert result.significant_total_stroke_m == pytest.approx(12.0 * 0.2)
    assert result.significant_amplitude_m == pytest.approx(12.0 * 0.2 / 2)
    assert result.maximum_total_stroke_m == pytest.approx(12.0 * 0.2 * 2.0)
    assert result.maximum_amplitude_m == pytest.approx(12.0 * 0.2)


def test_whole_stroke_amplification_is_explicit(stroke):
    budget = stroke_budget(stroke)
    result = amplified_stroke(StrokeAmplificationInput(budget.downstroke_m, 1.5))
    hand_downstroke = 16.0 * (0.2 / 0.6) / 2.0 + 2.0 + 0.6 + 0.8 + 0.2
    assert result == pytest.approx(hand_downstroke * 1.5)
    assert amplified_stroke(StrokeAmplificationInput(0.0, 1.5)) == 0.0


@pytest.mark.parametrize("stroke_m,factor", [
    (-1.0, 1.5), (True, 1.5), (float("nan"), 1.5),
    (2.0, 0.0), (2.0, -1.0), (2.0, True), (2.0, float("inf")),
    (1e300, 1e100),
])
def test_invalid_whole_stroke_amplification_is_rejected(stroke_m, factor):
    with pytest.raises(ValueError):
        amplified_stroke(StrokeAmplificationInput(stroke_m, factor))


def test_parallel_risers_share_load_and_extension():
    inputs = StretchInput(8e6, 2.0, 2e6, 6, 1.5)
    result = buoyancy_stretch(inputs)
    force = 8e6 * 2.0
    extension = force / (6 * 2e6)
    assert result.buoyancy_force_n == force
    assert result.quasistatic_stretch_m == pytest.approx(extension)
    assert result.dynamic_stretch_m == pytest.approx(1.5 * extension)
    assert result.dynamic_load_per_riser_n == pytest.approx(1.5 * force / 6)
    assert result.dynamic_load_per_riser_n == pytest.approx(
        result.dynamic_stretch_m * inputs.riser_stiffness_n_m
    )
    doubled = buoyancy_stretch(replace(inputs, riser_count=12))
    assert doubled.dynamic_stretch_m == pytest.approx(result.dynamic_stretch_m / 2)


def test_zero_imposed_heave_gives_zero_stretch():
    result = buoyancy_stretch(StretchInput(8e6, 0.0, 2e6, 6, 1.5))
    assert result.buoyancy_force_n == result.dynamic_stretch_m == 0


@pytest.mark.parametrize("heave_m", [-2.0, 0.0, 2.0])
def test_deck_clearance_reuses_existing_air_gap(heave_m):
    basis = AirGapBasis(Elevation(32.0, "datum"), Elevation(12.0, "datum"), 18.0, 2.0)
    result = deck_heave_clearance(basis, heave_m)
    assert result.clearance_m == 32.0 + heave_m - 12.0 - 18.0
    assert result.margin_m == result.clearance_m - 2.0
    shifted = replace(basis, deck_underside=Elevation(32.0 + heave_m, "datum"))
    assert result == calculate_air_gap(shifted, 0.0)
    assert result.meets_required_clearance == (result.clearance_m >= 2.0)
    assert basis.deck_underside.value_m == 32.0


def test_exact_length_conversion_is_reused():
    # Illustrative example, international feet; not a rounded conversion factor.
    assert length_in_metres(25.0, "ft") == 7.62
    assert length_in_metres(7.62, "m") == 7.62


def test_frozen_input_contract(waterplane):
    with pytest.raises(FrozenInstanceError):
        waterplane.column_length_m = 1.0


@pytest.mark.parametrize("bad", [True, "12", None, float("nan"), float("inf"), -1.0])
def test_invalid_physical_inputs_are_rejected(waterplane, stroke, bad):
    cases = [
        (waterplane_response, replace(waterplane, column_length_m=bad)),
        (heave_period_with_risers, PeriodInput(bad, 8e6, 2e6, 3)),
        (stroke_budget, replace(stroke, wave_upstroke_m=bad)),
        (wave_motion, WaveMotionInput(bad, 0.2, 2.0)),
        (buoyancy_stretch, StretchInput(8e6, bad, 2e6, 6, 1.5)),
    ]
    for function, inputs in cases:
        with pytest.raises(ValueError):
            function(inputs)


@pytest.mark.parametrize("bad_count", [True, 2.5, "3", -1, 10**400])
def test_invalid_counts_are_rejected(waterplane, bad_count):
    for function, inputs in [
        (waterplane_response, replace(waterplane, column_count=bad_count)),
        (heave_period_with_risers, PeriodInput(24.0, 8e6, 2e6, bad_count)),
        (buoyancy_stretch, StretchInput(8e6, 2.0, 2e6, bad_count, 1.5)),
    ]:
        with pytest.raises(ValueError):
            function(inputs)


@pytest.mark.parametrize("field", ["reference_rao_ratio", "wave_divisor_ratio"])
def test_zero_divisors_are_rejected(stroke, field):
    with pytest.raises(ValueError):
        stroke_budget(replace(stroke, **{field: 0.0}))


@pytest.mark.parametrize("factor", [0.0, -2.0, True, float("nan")])
def test_invalid_amplification_factors_are_rejected(factor):
    with pytest.raises(ValueError):
        wave_motion(WaveMotionInput(12.0, 0.2, factor))
    with pytest.raises(ValueError):
        buoyancy_stretch(StretchInput(8e6, 2.0, 2e6, 6, factor))


def test_maximum_wave_ratio_cannot_be_less_than_one():
    with pytest.raises(ValueError):
        wave_motion(WaveMotionInput(12.0, 0.2, 0.5))


def test_invalid_clearance_datum_and_heave_are_rejected():
    basis = AirGapBasis(Elevation(32.0, "a"), Elevation(12.0, "b"), 18.0, 2.0)
    with pytest.raises(ValueError, match="datum"):
        deck_heave_clearance(basis, 2.0)
    basis = replace(basis, still_water=Elevation(12.0, "a"))
    for bad in (True, "2", float("nan"), float("inf")):
        with pytest.raises(ValueError):
            deck_heave_clearance(basis, bad)


def test_derived_overflow_is_rejected(waterplane, stroke):
    cases = [
        (waterplane_response, replace(waterplane, column_length_m=1e308)),
        (heave_period_with_risers, PeriodInput(1e300, 8e6, 2e6, 3)),
        (stroke_budget, replace(stroke, target_rao_ratio=1e308)),
        (wave_motion, WaveMotionInput(1e300, 1e100, 2.0)),
        (buoyancy_stretch, StretchInput(1e300, 1e100, 2e6, 6, 1.5)),
    ]
    for function, inputs in cases:
        with pytest.raises(ValueError):
            function(inputs)
