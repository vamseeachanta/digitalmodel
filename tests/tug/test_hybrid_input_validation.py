# ABOUTME: Duty-cycle input validation and independent hybrid energy arithmetic
# ABOUTME: Synthetic boundary cases preserve zero load and zero-duration phases
"""Validate shared duty-cycle inputs without changing the emissions model.

All finite numeric inputs are synthetic test choices, not measured data or
empirical coefficients. Invalid cases follow the input contract: installed power
is positive, durations are nonnegative, and power fractions lie in [0, 1].
"""

import math

import pytest

from digitalmodel.tug.emissions import (
    OperatingPhase,
    cycle_energy,
    fuel_and_emissions,
    size_hybrid,
)


@pytest.mark.parametrize("evaluate", [cycle_energy, size_hybrid, fuel_and_emissions])
@pytest.mark.parametrize(
    ("field", "value"),
    [
        pytest.param("duration_h", -1.0, id="negative-duration"),
        pytest.param("duration_h", math.nan, id="nan-duration"),
        pytest.param("duration_h", math.inf, id="infinite-duration"),
        pytest.param("duration_h", -math.inf, id="negative-infinite-duration"),
        pytest.param("power_fraction", math.nan, id="nan-fraction"),
        pytest.param("power_fraction", math.inf, id="infinite-fraction"),
        pytest.param("power_fraction", -math.inf, id="negative-infinite-fraction"),
        pytest.param("power_fraction", -0.01, id="negative-fraction"),
        pytest.param("power_fraction", 1.01, id="fraction-above-one"),
    ],
)
def test_hybrid_rejects_invalid_phase(evaluate, field, value):
    """Every caller rejects an invalid field even after a valid first phase."""
    invalid = OperatingPhase("invalid", power_fraction=0.5, duration_h=1.0)
    setattr(invalid, field, value)
    profile = [OperatingPhase("valid", 0.25, 2.0), invalid]

    with pytest.raises(ValueError, match=rf"profile\[1\]\.{field}"):
        evaluate(1000.0, profile)


@pytest.mark.parametrize("evaluate", [cycle_energy, size_hybrid, fuel_and_emissions])
@pytest.mark.parametrize("installed_power_kw", [math.nan, math.inf, -math.inf])
def test_cycle_energy_rejects_nonfinite_power(evaluate, installed_power_kw):
    """Nonfinite installed power cannot enter any shared duty-cycle caller."""
    with pytest.raises(ValueError, match="installed_power_kw"):
        evaluate(installed_power_kw, [OperatingPhase("valid", 0.5, 1.0)])


@pytest.mark.parametrize("evaluate", [cycle_energy, size_hybrid, fuel_and_emissions])
@pytest.mark.parametrize("installed_power_kw", [0.0, -1.0])
def test_duty_cycle_rejects_nonpositive_power(evaluate, installed_power_kw):
    """Preserve the existing requirement for positive installed power."""
    with pytest.raises(ValueError, match="installed_power_kw"):
        evaluate(installed_power_kw, [OperatingPhase("valid", 0.5, 1.0)])


@pytest.mark.parametrize("evaluate", [cycle_energy, size_hybrid, fuel_and_emissions])
def test_duty_cycle_rejects_empty_profile(evaluate):
    """An empty duty cycle is invalid for every shared-boundary caller."""
    with pytest.raises(ValueError, match="profile must contain"):
        evaluate(1000.0, [])


@pytest.mark.parametrize("power_fraction, duration_h", [(0.0, 2.0), (1.0, 0.0), (0.0, 0.0)])
def test_zero_load_or_duration_is_permitted(power_fraction, duration_h):
    """E = P*f*t = 0 kWh when either load or duration is zero."""
    profile = [OperatingPhase("zero-energy", power_fraction, duration_h)]

    assert cycle_energy(1000.0, profile) == 0.0
    result = size_hybrid(1000.0, profile)
    assert result.total_energy_kwh == 0.0
    assert result.battery_energy_kwh == 0.0
    assert result.peak_energy_fraction == 0.0
    emissions = fuel_and_emissions(1000.0, profile)
    assert emissions.fuel_mass_t == 0.0
    assert emissions.co2_t == 0.0


def test_fraction_endpoints_are_permitted():
    """Hand calculation: 1000*0*2 + 1000*1*1 = 1000 kWh."""
    profile = [OperatingPhase("unloaded", 0.0, 2.0), OperatingPhase("full", 1.0, 1.0)]

    assert cycle_energy(1000.0, profile) == pytest.approx(1000.0)
    result = size_hybrid(1000.0, profile, baseline_fraction=0.5)
    # Battery: max(0 - 500, 0)*2 + max(1000 - 500, 0)*1 = 500 kWh.
    assert result.battery_energy_kwh == pytest.approx(500.0)


def test_two_phase_energy_and_peak_shaving():
    """Check independent hand arithmetic using dimensional energy identities.

    Basis: energy E = integral(P dt); for constant power, E = P*t, with
    [kWh] = [kW]*[h] and dimensionless load fraction f. Thus cycle energy is
    sum(P*f*t), and battery energy is sum(max(P*f - Pbase, 0)*t).

    Synthetic choices: P = 1000 kW, baseline fraction = 0.5, low phase
    f = 0.25 for 2 h, high phase f = 0.75 for 3 h. Hand calculation:
    total = 250*2 + 750*3 = 2750 kWh; Pbase = 500 kW;
    battery = 0*2 + (750 - 500)*3 = 750 kWh;
    peak = 750 kW; battery power = 250 kW; energy share = 750/2750 = 3/11.
    """
    profile = [OperatingPhase("low", 0.25, 2.0), OperatingPhase("high", 0.75, 3.0)]

    assert cycle_energy(1000.0, profile) == pytest.approx(2750.0)
    result = size_hybrid(1000.0, profile, baseline_fraction=0.5)
    assert result.total_energy_kwh == pytest.approx(2750.0)
    assert result.peak_power_kw == pytest.approx(750.0)
    assert result.baseline_engine_kw == pytest.approx(500.0)
    assert result.battery_power_kw == pytest.approx(250.0)
    assert result.battery_energy_kwh == pytest.approx(750.0)
    assert result.peak_energy_fraction == pytest.approx(3.0 / 11.0)
