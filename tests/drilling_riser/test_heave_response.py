"""Illustrative example inputs; no retained source data.

Independent expectations catch half-range errors, reversed offsets, changed
mass in the period adapter, omitted parallel springs and misplaced amplification.
"""

from dataclasses import FrozenInstanceError, replace
from math import isclose, pi, sqrt

import pytest

from digitalmodel.asset_integrity.air_gap import AirGapBasis, Elevation, calculate_air_gap
from digitalmodel.common.units import length_in_metres
from digitalmodel.drilling_riser import heave_response as response
from digitalmodel.solvers.openfoam.validation import wave_tank


def test_wave_range_and_amplitude_have_distinct_meanings():
    # Illustrative example: multiply height by response and a supplied peak ratio.
    data = response.WaveHeaveInput(12.0, 0.25, 2.0)
    assert response.wave_heave_range_m(data) == 6.0
    assert response.wave_heave_amplitude_m(data) == 3.0
    assert response.wave_heave_amplitude_m(replace(data, heave_rao_ratio=0.0)) == 0.0
    assert response.wave_heave_range_m(replace(data, wave_height_m=0.0)) == 0.0


def test_stroke_budget_scales_only_wave_motion():
    data = response.StrokeBudgetInput(
        wave_upstroke_m=6.0, wave_downstroke_m=8.0, wave_scale_ratio=0.25,
        tide_upstroke_m=0.6, offset_downstroke_m=1.2,
        current_downstroke_m=0.2, surge_downstroke_m=0.8, tide_downstroke_m=0.4,
    )
    result = response.stroke_budget(data)
    # Hand sum: up = 6/4 + 0.6; down = 8/4 + 1.2 + 0.2 + 0.8 + 0.4.
    assert result.upstroke_m == pytest.approx(2.1)
    assert result.downstroke_m == pytest.approx(4.6)
    assert result.total_stroke_m == pytest.approx(6.7)
    assert result.net_upstroke_m == pytest.approx(-0.5)
    assert response.stop_deflection_m(response.StopDeflectionInput(1.0, result.net_upstroke_m)) == pytest.approx(1.5)
    assert response.stop_deflection_m(response.StopDeflectionInput(1.0, 3.0)) == -2.0


def test_zero_wave_preserves_quasi_static_allowances():
    data = response.StrokeBudgetInput(0.0, 0.0, 1.0, 0.0, 2.0, 0.0, 1.0, 0.0)
    result = response.stroke_budget(data)
    assert result.total_stroke_m == 3.0
    assert result.net_upstroke_m == -3.0


def test_waterplane_stiffness_reuses_weight_density():
    # Illustrative example: 80 m2 at 10000 N/m3 gives 800000 N/m.
    data = response.WaterplaneInput(80.0, 10000.0)
    assert response.waterplane_stiffness_n_m(data) == pytest.approx(800000.0)
    assert response.waterplane_stiffness_n_m(replace(data, waterplane_area_m2=160.0)) == pytest.approx(1600000.0)


def test_period_adapter_preserves_effective_mass():
    data = response.PeriodShiftInput(24.0, 900000.0, 2700000.0)
    # Total stiffness quadruples at fixed mass, so period halves.
    assert response.heave_period_with_axial_springs_s(data) == pytest.approx(12.0)
    assert response.heave_period_with_axial_springs_s(replace(data, added_axial_stiffness_n_m=0.0)) == pytest.approx(24.0)
    mass = 900000.0 * (24.0 / (2.0 * pi)) ** 2
    expected = 2.0 * pi * sqrt(mass / 1800000.0)
    assert response.heave_period_with_axial_springs_s(replace(data, added_axial_stiffness_n_m=900000.0)) == pytest.approx(expected)


def test_buoyancy_load_and_elastic_stretch_use_parallel_springs():
    data = response.BuoyancyStretchInput(80.0, 10000.0, 2.0, 200000.0, 5, 1.5)
    result = response.buoyancy_stretch(data)
    # Hand force = 80 * 2 * 10000; spring displacement = F/(5*200000).
    assert result.buoyancy_force_n == pytest.approx(1600000.0)
    assert result.quasi_static_stretch_m == pytest.approx(1.6)
    assert result.amplified_stretch_m == pytest.approx(2.4)
    assert result.amplified_load_per_spring_n == pytest.approx(480000.0)
    zero = response.buoyancy_stretch(replace(data, immersion_change_m=0.0))
    assert zero.amplified_stretch_m == 0.0
    twice = response.buoyancy_stretch(replace(data, spring_count=10))
    assert twice.amplified_stretch_m == pytest.approx(1.2)


def test_moving_deck_clearance_delegates_to_existing_air_gap():
    data = response.DeckClearanceInput(32.0, 16.0, 18.0, 0.25, 11.0, 0.8, "illustrative datum")
    result = response.moving_deck_clearance(data)
    # Deck 32 + upward response 18*0.25/2 - still water 16 - crest 11 = 7.25 m.
    assert result.clearance_m == pytest.approx(7.25)
    assert result.margin_m == pytest.approx(6.45)
    assert result.meets_required_clearance
    assert response.moving_deck_clearance(replace(data, crest_height_m=20.0)).clearance_m == pytest.approx(-1.75)
    equality = response.moving_deck_clearance(replace(data, required_clearance_m=7.25))
    assert equality.meets_required_clearance


def test_moving_deck_clearance_includes_downward_heave():
    data = response.DeckClearanceInput(
        32.0, 16.0, 18.0, 0.25, 11.0, 0.8, "illustrative datum", -1,
    )
    # Deck 32 - downward response 2.25 - still water 16 - crest 11.
    assert response.moving_deck_clearance(data).clearance_m == pytest.approx(2.75)


@pytest.mark.parametrize("bad", [0, 2, -2, 0.5, True, False, "1", None])
def test_moving_deck_clearance_rejects_invalid_heave_direction(bad):
    data = response.DeckClearanceInput(
        32.0, 16.0, 18.0, 0.25, 11.0, 0.8, "illustrative datum",
        heave_direction=bad,
    )
    with pytest.raises(ValueError):
        response.moving_deck_clearance(data)


def test_existing_clearance_and_length_conversion_neutral_regressions():
    basis = AirGapBasis(Elevation(20.0, "illustrative datum"), Elevation(5.0, "illustrative datum"), 8.0, 2.0)
    assert calculate_air_gap(basis, 0.0).clearance_m == 7.0
    assert isclose(length_in_metres(25.0, "ft"), 7.62, rel_tol=1e-14)
    with pytest.raises(ValueError):
        length_in_metres(-2.0, "ft")
    with pytest.raises(ValueError):
        calculate_air_gap(replace(basis, still_water=Elevation(5.0, "other datum")), 0.0)


def test_wavelength_keeps_unsourced_coefficient_explicit():
    # Illustrative coefficient, not a standard-derived or site value.
    assert response.period_squared_wavelength_m(response.WavelengthInput(8.0, 1.5)) == 96.0


def test_period_squared_wavelength_agrees_with_deep_water_dispersion():
    data = response.WavelengthInput(8.0, 9.81 / (2.0 * pi))
    assert response.period_squared_wavelength_m(data) == pytest.approx(
        wave_tank.wavelength(8.0, 1000.0), rel=1e-3,
    )


@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -1.0, True, "2", None])
def test_wave_inputs_reject_invalid_magnitudes(bad):
    for field in ("wave_height_m", "heave_rao_ratio", "height_multiplier_ratio"):
        with pytest.raises(ValueError):
            response.wave_heave_range_m(replace(response.WaveHeaveInput(12.0, 0.25, 2.0), **{field: bad}))


@pytest.mark.parametrize("bad", [0.0, -1.0, float("nan"), float("inf"), True])
def test_positive_stiffness_and_period_inputs_are_required(bad):
    for field in ("baseline_period_s", "waterplane_stiffness_n_m"):
        with pytest.raises(ValueError):
            response.heave_period_with_axial_springs_s(replace(response.PeriodShiftInput(24.0, 900000.0, 0.0), **{field: bad}))
    for field in ("waterplane_area_m2", "fluid_weight_density_n_m3"):
        with pytest.raises(ValueError):
            response.waterplane_stiffness_n_m(replace(response.WaterplaneInput(80.0, 10000.0), **{field: bad}))


@pytest.mark.parametrize("bad", [0, -1, 2.5, True])
def test_parallel_spring_count_is_a_positive_integer(bad):
    with pytest.raises(ValueError):
        response.buoyancy_stretch(response.BuoyancyStretchInput(80.0, 10000.0, 2.0, 200000.0, bad, 1.5))


def test_invalid_offsets_factors_and_derived_overflow_are_rejected():
    data = response.StrokeBudgetInput(6.0, 8.0, 0.25, 0.6, 1.2, 0.2, 0.8, 0.4)
    with pytest.raises(ValueError):
        response.stroke_budget(replace(data, surge_downstroke_m=-0.1))
    with pytest.raises(ValueError):
        response.heave_period_with_axial_springs_s(response.PeriodShiftInput(24.0, 900000.0, -1.0))
    with pytest.raises(ValueError):
        response.buoyancy_stretch(response.BuoyancyStretchInput(80.0, 10000.0, 2.0, 200000.0, 5, -1.0))
    with pytest.raises(ValueError):
        response.wave_heave_range_m(response.WaveHeaveInput(1e300, 1e300, 1.0))
    with pytest.raises(ValueError):
        response.stop_deflection_m(response.StopDeflectionInput(float("nan"), 1.0))
    with pytest.raises(ValueError):
        response.period_squared_wavelength_m(response.WavelengthInput(0.0, 1.5))


def test_inputs_are_immutable():
    data = response.WaveHeaveInput(12.0, 0.25, 2.0)
    with pytest.raises(FrozenInstanceError):
        data.wave_height_m = 18.0


def test_stroke_integer_overflow_is_a_validation_error():
    # Individually representable integers whose sum exceeds binary64 range.
    data = response.StrokeBudgetInput(6.0, 8.0, 1.0, 0.0, 10**308, 10**308, 0.0, 0.0)
    with pytest.raises(ValueError):
        response.stroke_budget(data)


def test_unrepresentable_spring_count_is_a_validation_error():
    data = response.BuoyancyStretchInput(80.0, 10000.0, 2.0, 200000.0, 10**400, 1.5)
    with pytest.raises(ValueError):
        response.buoyancy_stretch(data)


def test_stop_deflection_inputs_are_immutable_and_unit_named():
    data = response.StopDeflectionInput(1.0, -2.0)
    assert response.stop_deflection_m(data) == 3.0
    with pytest.raises(FrozenInstanceError):
        data.up_stop_m = 3.0
