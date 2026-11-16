"""Independent illustrative example checks for motion and clearance methods."""

from dataclasses import FrozenInstanceError, replace
import math

import pytest

from digitalmodel.asset_integrity.air_gap import (
    AirGapBasis, Elevation, calculate_air_gap,
)
from digitalmodel.common.units import length_in_metres
from digitalmodel.drilling_riser.motion_clearance import (
    BuoyancyStretchInput,
    DeckClearanceInput,
    MotionScaleInput,
    RestoringInput,
    StrokeBudgetInput,
    WaveMotionInput,
    buoyancy_stretch,
    clearance_with_heave,
    restoring_response,
    scale_motion,
    stroke_budget,
    wave_motion,
)
from digitalmodel.marine_ops.marine_analysis.catenary.utils import calculate_elongation
from digitalmodel.solvers.openfoam.validation.floating_body import (
    GRAVITY, heave_stiffness, hydrostatic_heave_period,
)


def restoring_example():
    """Illustrative example, independent of source geometry and stiffness."""
    return RestoringInput(12.0, 8.0, 3, 10000.0, 720000.0, 2, 24.0)


def stroke_example():
    """Illustrative example of distinct upward and downward allowances."""
    return StrokeBudgetInput(0.25, 4.5, 2.0, 5.5, 0.75, 1.25, 0.35, 2.25)


def stretch_example():
    """Illustrative example with a supplied dynamic factor."""
    return BuoyancyStretchInput(180.0, 10000.0, 2.5, 900000.0, 3, 1.4)


def test_existing_reference_functions_with_independent_formulas():
    area, density, mass = 150.0, 1010.0, 2400000.0
    stiffness = density * GRAVITY * area
    assert heave_stiffness(area, rho=density) == pytest.approx(stiffness)
    assert hydrostatic_heave_period(mass, stiffness) == pytest.approx(
        2 * math.pi * math.sqrt(mass / stiffness)
    )
    assert calculate_elongation(360000.0, 250.0, 90000000.0) == pytest.approx(1.0)
    basis = AirGapBasis(Elevation(32.0, "datum"), Elevation(18.0, "datum"), 7.0, 2.0)
    assert calculate_air_gap(basis, 0.0).clearance_m == pytest.approx(7.0)
    assert length_in_metres(25.0, "ft") == pytest.approx(25.0 * 0.3048)


def test_parallel_restoring_and_period_ratio():
    args = restoring_example()
    result = restoring_response(args)
    area = 12.0 * 8.0 * 3
    waterplane = area * 10000.0
    restraints = 720000.0 * 2
    assert result.waterplane_area_m2 == area
    assert result.waterplane_stiffness_n_m == pytest.approx(waterplane)
    assert result.restraint_stiffness_n_m == restraints
    assert result.waterplane_to_restraint_ratio == pytest.approx(waterplane / restraints)
    assert result.period_s == pytest.approx(24.0 / math.sqrt(1 + restraints / waterplane))
    assert restoring_response(replace(args, restraint_count=6)).period_s < result.period_s


def test_no_restraints_retains_baseline_period_and_undefined_ratio():
    result = restoring_response(replace(restoring_example(), restraint_count=0))
    assert result.period_s == pytest.approx(24.0)
    assert result.waterplane_to_restraint_ratio is None


def test_wave_response_and_height_factor_are_independent():
    result = wave_motion(WaveMotionInput(12.0, 0.25, 1.6))
    assert result.total_stroke_m == pytest.approx(12.0 * 0.25 * 1.6)
    assert result.amplitude_m == pytest.approx(result.total_stroke_m / 2)
    assert wave_motion(WaveMotionInput(12.0, 0.0, 1.6)).amplitude_m == 0


def test_response_scaling_and_significant_motion():
    result = scale_motion(MotionScaleInput(12.0, 0.6, 0.25, 1.6))
    assert result == pytest.approx(12.0 * (0.25 / 0.6) / 1.6)
    assert scale_motion(MotionScaleInput(12.0, 0.6, 0.0, 1.6)) == 0


def test_stroke_budget_and_signed_stop_margin():
    result = stroke_budget(stroke_example())
    assert result.upstroke_m == pytest.approx(0.25 + 4.5)
    assert result.downstroke_m == pytest.approx(2.0 + 5.5 + 0.75 + 1.25 + 0.35)
    assert result.total_stroke_m == pytest.approx(0.25 + 4.5 + 2 + 5.5 + 0.75 + 1.25 + 0.35)
    assert result.net_upstroke_m == pytest.approx(0.25 + 4.5 - 2 - 0.75 - 1.25 - 0.35)
    assert result.stop_margin_m == pytest.approx(2.25 - result.net_upstroke_m)
    assert stroke_budget(replace(stroke_example(), upper_stop_m=0.1)).stop_margin_m < 0


@pytest.mark.parametrize("crest", [6.0, 18.0])
def test_clearance_adapter_preserves_negative_clearance(crest):
    args = DeckClearanceInput(32.0, 18.0, crest, 2.5, 1.0, "datum")
    result = clearance_with_heave(args)
    expected = (32.0 - 18.0) + 2.5 - crest
    assert result.clearance_m == pytest.approx(expected)
    assert result.margin_m == pytest.approx(expected - 1)
    assert result.meets_required_clearance == (expected >= 1)


def test_buoyancy_load_extension_and_shared_load():
    result = buoyancy_stretch(stretch_example())
    force = 180.0 * 10000.0 * 2.5
    total_stiffness = 900000.0 * 3
    assert result.buoyancy_force_n == pytest.approx(force)
    assert result.quasistatic_extension_m == pytest.approx(force / total_stiffness)
    assert result.dynamic_extension_m == pytest.approx(1.4 * force / total_stiffness)
    assert result.dynamic_load_per_restraint_n == pytest.approx(1.4 * force / 3)
    zero = buoyancy_stretch(replace(stretch_example(), immersion_m=0.0))
    assert zero.dynamic_extension_m == zero.buoyancy_force_n == 0


@pytest.mark.parametrize("value", [True, "12", float("nan"), float("inf"), -1.0])
def test_invalid_motion_inputs(value):
    with pytest.raises(ValueError):
        wave_motion(WaveMotionInput(value, 0.25, 1.6))


@pytest.mark.parametrize("field", ["reference_response_ratio", "height_ratio"])
def test_scaling_rejects_zero_divisors(field):
    with pytest.raises(ValueError):
        scale_motion(replace(MotionScaleInput(12.0, 0.6, 0.25, 1.6), **{field: 0}))


@pytest.mark.parametrize("count", [True, 1.5, -1])
def test_invalid_restraint_count(count):
    with pytest.raises(ValueError):
        restoring_response(replace(restoring_example(), restraint_count=count))


@pytest.mark.parametrize("field", ["column_length_m", "column_width_m", "column_count",
                                   "weight_density_n_m3", "restraint_stiffness_n_m",
                                   "baseline_period_s"])
def test_restoring_rejects_zero_physical_properties(field):
    with pytest.raises(ValueError):
        restoring_response(replace(restoring_example(), **{field: 0}))


def test_invalid_budget_clearance_and_stretch():
    with pytest.raises(ValueError):
        stroke_budget(replace(stroke_example(), surge_downstroke_m=-0.2))
    with pytest.raises(ValueError):
        clearance_with_heave(DeckClearanceInput(32.0, 18.0, 6.0, 2.5, 1.0, ""))
    with pytest.raises(ValueError):
        buoyancy_stretch(replace(stretch_example(), restraint_count=0))
    with pytest.raises(ValueError):
        buoyancy_stretch(replace(stretch_example(), dynamic_load_factor=0))


@pytest.mark.parametrize("call", [
    lambda: restoring_response(replace(restoring_example(), column_length_m=1e308)),
    lambda: wave_motion(WaveMotionInput(1e308, 8.0, 2.0)),
    lambda: scale_motion(MotionScaleInput(1e308, 1e-308, 1.0, 1.0)),
    lambda: stroke_budget(replace(stroke_example(), tide_upstroke_m=1e308,
                                wave_upstroke_m=1e308)),
    lambda: buoyancy_stretch(replace(stretch_example(), waterplane_area_m2=1e308)),
])
def test_nonfinite_derived_values_are_rejected(call):
    with pytest.raises(ValueError):
        call()


def test_input_records_are_frozen():
    args = restoring_example()
    with pytest.raises(FrozenInstanceError):
        args.column_length_m = 16.0
