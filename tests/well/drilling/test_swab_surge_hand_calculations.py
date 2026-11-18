"""Independent geometry values and unqualified-pressure invariants.

JPT (1961), "Wellbore Pressure Surges Produced by Pipe Movement":
https://doi.org/10.2118/1546-G-PA (public DOI metadata confirmed the title/year;
full text returned HTTP 403).
Absolute nonzero pressure, the clinging calibration and the independent
nonzero inverse-speed benchmark are NOT_EVALUATED. No empirical coefficient
is copied from the implementation. Inputs and margins are synthetic.
"""

import pytest

from digitalmodel.well.drilling.swab_surge import SwabSurge


def _section(**overrides):
    inputs = dict(
        pipe_od=6.0,
        pipe_id=4.0,
        hole_diameter=10.0,
        mud_weight=12.0,
        tvd=2_000.0,
        plastic_viscosity=20.0,
        yield_point=0.0,
        clinging_constant=0.25,
        pipe_config="closed",
    )
    inputs.update(overrides)
    return SwabSurge(**inputs)


@pytest.mark.parametrize(
    "config, factor, velocity", [("closed", 0.5625, 130.0), ("open", 0.25, 80.0)],
)
@pytest.mark.parametrize("speed", [160.0, -160.0])
def test_displacement_and_velocity_from_independent_area_balance(
    config, factor, velocity, speed,
):
    """Circular area and displaced-volume conservation, not a fitted law.

    Hole radius 5 in: area = 25*pi; OD radius 3: area = 9*pi;
    bore radius 2: area = 4*pi. Annular flow area = 16*pi in2.
    Closed pipe displaces 9*pi, giving 9/16 = 0.5625.
    Open pipe displaces steel area 5*pi into 16*pi + 4*pi,
    giving 5/20 = 0.25. At specified clinging input 0.25 and
    speed magnitude 160 ft/min, velocities are 160*(13/16) = 130
    and 160*(1/2) = 80 ft/min. This does not qualify a clinging law.
    Relative 1e-12 covers binary arithmetic; both ratios are exact dyadics.
    """
    calc = _section(pipe_config=config)
    assert calc.displacement_factor() == pytest.approx(factor, rel=1e-12)
    assert calc.effective_velocity(speed) == pytest.approx(velocity, rel=1e-12)


def test_newtonian_pressure_is_linear_in_speed_viscosity_and_length():
    """Newtonian limiting invariant; nonzero absolute pressure is unqualified.

    Doubling speed, viscosity or column length separately doubles friction
    when supplied yield point is zero. Relative 1e-12 covers arithmetic.
    """
    calc = _section()
    baseline = calc.pressure(160.0)
    assert baseline > 0
    assert calc.pressure(320.0) == pytest.approx(2 * baseline, rel=1e-12)
    assert _section(plastic_viscosity=40.0).pressure(160.0) == pytest.approx(
        2 * baseline, rel=1e-12,
    )
    assert _section(tvd=4_000.0).pressure(160.0) == pytest.approx(
        2 * baseline, rel=1e-12,
    )


def test_zero_rheology_evaluation_has_exact_hand_limit():
    """No viscosity or yield stress gives zero friction at finite motion.

    The open-pipe area balance above gives 80 ft/min. Pressure is exactly
    zero, hence no EMW change from the specified 12 ppg. No friction or
    hydrostatic conversion coefficient is needed for this limiting case.
    """
    result = _section(
        pipe_config="open", plastic_viscosity=0.0,
    ).evaluate(160.0).as_dict()
    assert result == {
        "trip_speed_ft_min": 160.0,
        "effective_velocity_ft_min": 80.0,
        "pressure_psi": 0.0,
        "surge_emw_ppg": 12.0,
        "swab_emw_ppg": 12.0,
    }


def test_inverse_newtonian_speed_scales_with_available_margin():
    """Twofold margins give twofold speed in a zero-yield linear model.

    This inverse/scaling invariant is not an independent hand-pressure
    benchmark. Its dimensional normalization remains NOT_EVALUATED.
    Relative 1e-12 covers operations in the inverse calculation.
    """
    calc = _section()
    first = calc.max_safe_trip_speed(11.5, 13.0)
    doubled = calc.max_safe_trip_speed(11.0, 14.0)
    assert first.governing_ft_min > 0
    assert doubled.swab_limited_ft_min == pytest.approx(
        2 * first.swab_limited_ft_min, rel=1e-12,
    )
    assert doubled.surge_limited_ft_min == pytest.approx(
        2 * first.surge_limited_ft_min, rel=1e-12,
    )
    assert first.surge_limited_ft_min == pytest.approx(
        2 * first.swab_limited_ft_min, rel=1e-12,
    )
    evaluated = calc.evaluate(first.governing_ft_min)
    assert evaluated.swab_emw_ppg == pytest.approx(11.5, rel=1e-12)
    assert evaluated.surge_emw_ppg == pytest.approx(12.5, rel=1e-12)
    assert calc.max_safe_trip_speed(12.0, 13.0).governing_ft_min == 0.0


@pytest.mark.parametrize("diameter", ["pipe_od", "pipe_id", "hole_diameter"])
@pytest.mark.parametrize("value", [0.0, -1.0])
def test_nonpositive_diameters_are_refused(diameter, value):
    with pytest.raises(ValueError):
        _section(**{diameter: value})
