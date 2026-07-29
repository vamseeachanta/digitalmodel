# ABOUTME: CFD-validated tests for the analytical U-tube anti-roll tank model.

"""Tests for digitalmodel.hydrodynamics.utube_tank."""

import math

import pytest

from digitalmodel.hydrodynamics import utube_tank as u


LEG_LENGTH = 20.0
LEG_WIDTH = 6.0
LEG_AREA = LEG_LENGTH * LEG_WIDTH
CENTROID_OFFSET = 5.0
FILL_DEPTH = 5.0
RHO = 1025.0
GRAVITY = 9.81
EFFECTIVE_CONDUIT_LENGTH = 5.2004
LOSS_COEFFICIENT = 2.3894
LOSS_AREA_EXPONENT = 2.138


def reference_geometry():
    return u.UTubeGeometry(
        leg_length=LEG_LENGTH,
        leg_width=LEG_WIDTH,
        fill_depth=FILL_DEPTH,
        centroid_offset=CENTROID_OFFSET,
        effective_conduit_length=EFFECTIVE_CONDUIT_LENGTH,
    )


@pytest.mark.parametrize(
    ("conduit_area", "fill_depth", "predicted_period", "cfd_period"),
    [
        (3.4, 5.0, 19.73, 19.27),
        (13.5, 5.0, 10.64, 10.97),
        (6.8, 2.5, 13.95, 13.45),
        (6.8, 7.0, 14.59, 14.38),
    ],
)
def test_natural_period_matches_cfd(
    conduit_area, fill_depth, predicted_period, cfd_period
):
    period = u.natural_period(
        leg_area=LEG_AREA,
        conduit_area=conduit_area,
        fill_depth=fill_depth,
        effective_conduit_length=EFFECTIVE_CONDUIT_LENGTH,
        gravity=GRAVITY,
    )
    assert period == pytest.approx(predicted_period, rel=0.004)
    assert period == pytest.approx(cfd_period, rel=0.04)


def test_effective_conduit_length_calibration_round_trips():
    calibrated = u.calibrate_effective_conduit_length(
        measured_period=14.31,
        leg_area=LEG_AREA,
        conduit_area=6.8,
        fill_depth=FILL_DEPTH,
        gravity=GRAVITY,
    )
    assert calibrated == pytest.approx(EFFECTIVE_CONDUIT_LENGTH, rel=1e-3)
    period = u.natural_period(
        leg_area=LEG_AREA,
        conduit_area=6.8,
        fill_depth=FILL_DEPTH,
        effective_conduit_length=calibrated,
        gravity=GRAVITY,
    )
    assert period == pytest.approx(14.31, rel=1e-3)


def test_equivalent_damping_rises_with_roll_amplitude():
    common = dict(
        geometry=reference_geometry(),
        conduit_area=6.8,
        forcing_period=20.0,
        loss_coefficient=LOSS_COEFFICIENT,
        area_exponent=LOSS_AREA_EXPONENT,
        gravity=GRAVITY,
    )
    low = u.equivalent_damping(level_difference_amplitude=0.10, **common)
    high = u.equivalent_damping(level_difference_amplitude=0.50, **common)
    assert high > low
    # Quadratic head loss produces equivalent linear damping proportional to amplitude.
    assert high / low == pytest.approx(5.0)


def test_equivalent_damping_falls_with_conduit_area():
    common = dict(
        geometry=reference_geometry(),
        forcing_period=20.0,
        level_difference_amplitude=0.10,
        loss_coefficient=LOSS_COEFFICIENT,
        area_exponent=LOSS_AREA_EXPONENT,
        gravity=GRAVITY,
    )
    small = u.equivalent_damping(conduit_area=3.4, **common)
    large = u.equivalent_damping(conduit_area=13.5, **common)
    assert large < small


@pytest.mark.parametrize(
    ("conduit_area", "cfd_damping"),
    [(3.4, 0.13598), (13.5, 0.01323)],
)
def test_linear_area_loss_scaling_is_materially_worse(conduit_area, cfd_damping):
    common = dict(
        geometry=reference_geometry(),
        conduit_area=conduit_area,
        forcing_period=20.0,
        level_difference_amplitude=0.10,
        loss_coefficient=LOSS_COEFFICIENT,
        gravity=GRAVITY,
    )
    calibrated = u.equivalent_damping(area_exponent=LOSS_AREA_EXPONENT, **common)
    linear_area = u.equivalent_damping(area_exponent=1.0, **common)
    calibrated_error = abs(calibrated - cfd_damping)
    linear_error = abs(linear_area - cfd_damping)
    assert calibrated == pytest.approx(cfd_damping, rel=0.01)
    assert linear_error > 10.0 * calibrated_error


@pytest.mark.parametrize(
    ("forcing_period", "cfd_moment_mnm"),
    [
        (60.0, 8.807),
        (40.0, 9.206),
        (26.0, 9.683),
        (24.0, 9.673),
        (22.0, 9.498),
        (20.0, 9.103),
    ],
)
def test_in_phase_tank_moment_matches_cfd(forcing_period, cfd_moment_mnm):
    moment = u.tank_moment(
        geometry=reference_geometry(),
        conduit_area=6.8,
        forcing_period=forcing_period,
        roll_amplitude=math.radians(5.0),
        density=RHO,
        gravity=GRAVITY,
        loss_coefficient=LOSS_COEFFICIENT,
        area_exponent=LOSS_AREA_EXPONENT,
    )
    assert moment.in_phase / 1e6 == pytest.approx(cfd_moment_mnm, rel=0.07)


def test_static_moment_terms_cannot_be_dropped():
    moment = u.tank_moment(
        geometry=reference_geometry(),
        conduit_area=6.8,
        forcing_period=60.0,
        roll_amplitude=math.radians(5.0),
        density=RHO,
        gravity=GRAVITY,
        loss_coefficient=LOSS_COEFFICIENT,
        area_exponent=LOSS_AREA_EXPONENT,
    )
    redistribution_only = 2.0 * RHO * GRAVITY * CENTROID_OFFSET * moment.q_in_phase
    assert moment.in_phase / redistribution_only == pytest.approx(1.45, rel=0.08)


def test_tank_reduces_peak_roll_near_resonance():
    common = dict(
        vessel_roll_period=20.0,
        hull_damping_ratio=0.05,
        tank_authority=0.12,
        geometry=reference_geometry(),
        conduit_area=4.8,
    )
    bare = u.coupled_roll_response(forcing_period=20.0, tank_enabled=False, **common)
    coupled = u.coupled_roll_response(forcing_period=20.0, tank_enabled=True, **common)
    assert abs(coupled) < abs(bare)


def test_tank_increases_long_period_roll_from_free_surface_penalty():
    common = dict(
        vessel_roll_period=20.0,
        hull_damping_ratio=0.05,
        tank_authority=0.12,
        geometry=reference_geometry(),
        conduit_area=4.8,
    )
    bare = u.coupled_roll_response(forcing_period=60.0, tank_enabled=False, **common)
    coupled = u.coupled_roll_response(forcing_period=60.0, tank_enabled=True, **common)
    assert abs(coupled) > abs(bare)


@pytest.mark.parametrize("vessel_roll_period", [18.0, 19.5, 21.0])
def test_optimum_conduit_is_bounded_and_detuned(vessel_roll_period):
    optimum = u.optimize_conduit_area(
        vessel_roll_period=vessel_roll_period,
        hull_damping_ratio=0.05,
        tank_authority=0.12,
        geometry=reference_geometry(),
        area_bounds=(2.0, 14.0),
    )
    assert 4.0 <= optimum.conduit_area <= 6.0
    tuning_ratio = optimum.tank_natural_period / vessel_roll_period
    assert 0.75 <= tuning_ratio <= 0.95
