# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic and input contracts for stored-energy screening."""

from dataclasses import FrozenInstanceError, asdict
import math

import pytest

from digitalmodel.code_checks.stored_energy import stored_energy


def test_gas_hand_calculation():
    """P1/P0=4, gamma=2: W=400000*0.01*(1-1/2)=2000 J."""
    result = stored_energy(
        0.01, 300000, "gas", specific_heat_ratio=2, ambient_pressure_pa=100000
    )
    assert result.energy_j == pytest.approx(2000)
    assert result.tnt_equivalent_kg == pytest.approx(2000 / 4184000)
    assert result.formula == "P1*V/(gamma-1)*(1-(P0/P1)**((gamma-1)/gamma))"


def test_default_gas_ratio_hand_calculation_and_custom_tnt():
    """P1/P0=128, gamma=7/5: pressure factor=1/4, W=24000 J."""
    result = stored_energy(
        0.001, 12.7, "gas", pressure_unit="MPa", ambient_pressure_pa=100000,
        tnt_energy_j_per_kg=2000000,
    )
    assert result.energy_j == pytest.approx(24000)
    assert result.tnt_equivalent_kg == pytest.approx(0.012)


def test_liquid_hand_calculation_and_default_water_modulus():
    """(10 MPa)^2 * 0.044 m3 / (2 * 2.2 GPa) = 1000 J."""
    result = stored_energy(0.044, 10, "liquid", pressure_unit="MPa")
    assert result.energy_j == pytest.approx(1000)
    assert result.tnt_equivalent_kg == pytest.approx(1000 / 4184000)
    assert result.inputs.bulk_modulus_pa == 2200000000
    assert result.formula == "p_gauge**2*V/(2*K)"
    custom = stored_energy(
        0.04, 10, "liquid", pressure_unit="MPa", bulk_modulus_pa=2000000000
    )
    assert custom.energy_j == pytest.approx(1000)


@pytest.mark.parametrize("fluid_type", ["gas", "liquid"])
@pytest.mark.parametrize(
    ("pressure", "unit"),
    [(1000000, "Pa"), (10, "bar"), (1, "MPa"), (145.03773773020923, "psi")],
)
def test_gauge_pressure_units(fluid_type, pressure, unit):
    result = stored_energy(0.01, pressure, fluid_type, pressure_unit=unit)
    reference = stored_energy(0.01, 1000000, fluid_type)
    assert result.energy_j == pytest.approx(reference.energy_j, rel=1e-12)
    assert result.inputs.pressure_gauge_pa == pytest.approx(1000000, rel=1e-12)
    assert result.inputs.pressure_gauge == pressure
    assert result.inputs.pressure_unit == unit


@pytest.mark.parametrize("fluid_type", ["gas", "liquid"])
def test_pressure_increases_energy_and_volume_scales_energy(fluid_type):
    energies = [
        stored_energy(0.01, p, fluid_type, pressure_unit="bar").energy_j
        for p in (0, 1, 10, 100)
    ]
    assert all(a < b for a, b in zip(energies, energies[1:]))
    small = stored_energy(0.01, 10, fluid_type, pressure_unit="bar")
    large = stored_energy(0.02, 10, fluid_type, pressure_unit="bar")
    assert large.energy_j == pytest.approx(2 * small.energy_j)
    assert large.tnt_equivalent_kg == pytest.approx(2 * small.tnt_equivalent_kg)


@pytest.mark.parametrize("fluid_type", ["gas", "liquid"])
def test_zero_gauge_pressure(fluid_type):
    result = stored_energy(1, 0, fluid_type, threshold_j=0)
    assert result.energy_j == 0
    assert result.tnt_equivalent_kg == 0
    assert result.exceeds_threshold is False


def test_threshold_is_optional_and_strictly_exceeded():
    options = {"bulk_modulus_pa": 2000000000}
    result = stored_energy(0.04, 10000000, "liquid", **options)
    assert result.exceeds_threshold is None
    assert result.inputs.threshold_j is None
    for threshold, expected in [(999, True), (1000, False), (1001, False)]:
        result = stored_energy(
            0.04, 10000000, "liquid", threshold_j=threshold, **options
        )
        assert result.exceeds_threshold is expected


def test_small_high_pressure_gas_is_flagged_with_supplied_threshold():
    result = stored_energy(
        0.001, 300, "gas", pressure_unit="bar", threshold_j=10000
    )
    assert result.exceeds_threshold is True
    assert result.energy_j > result.inputs.threshold_j


def test_ambient_pressure_is_absolute_pa_and_affects_only_gas():
    # At fixed P1, V and gamma=2, raising P0 from P1/4 to P1/2 reduces work.
    low = stored_energy(
        0.01, 300000, "gas", specific_heat_ratio=2, ambient_pressure_pa=100000
    )
    high = stored_energy(
        0.01, 200000, "gas", specific_heat_ratio=2, ambient_pressure_pa=200000
    )
    assert high.energy_j == pytest.approx(4000 * (1 - math.sqrt(0.5)))
    assert high.energy_j < low.energy_j
    assert stored_energy(0.01, 1, "liquid", ambient_pressure_pa=200000).energy_j == (
        stored_energy(0.01, 1, "liquid").energy_j
    )


def test_result_records_inputs_and_is_immutable():
    result = stored_energy(
        0.02, 7, "gas", pressure_unit="bar", specific_heat_ratio=1.6,
        bulk_modulus_pa=2000000000, ambient_pressure_pa=90000,
        tnt_energy_j_per_kg=4000000, threshold_j=20,
    )
    assert asdict(result.inputs) == {
        "volume_m3": 0.02,
        "pressure_gauge": 7,
        "pressure_unit": "bar",
        "pressure_gauge_pa": 700000,
        "fluid_type": "gas",
        "specific_heat_ratio": 1.6,
        "bulk_modulus_pa": 2000000000,
        "ambient_pressure_pa": 90000,
        "tnt_energy_j_per_kg": 4000000,
        "threshold_j": 20,
    }
    with pytest.raises(FrozenInstanceError):
        result.energy_j = 0
    with pytest.raises(FrozenInstanceError):
        result.inputs.volume_m3 = 0


@pytest.mark.parametrize(
    ("name", "bad_values"),
    [
        ("volume_m3", [0, -1, math.nan, math.inf, -math.inf, True, "1", None]),
        ("pressure_gauge", [-1, math.nan, math.inf, -math.inf, False, "1", None]),
        ("specific_heat_ratio", [1, 0, -1, math.nan, math.inf, True, "1.4", None]),
        ("bulk_modulus_pa", [0, -1, math.nan, math.inf, False, "2e9", None]),
        ("ambient_pressure_pa", [0, -1, math.nan, math.inf, True, "1e5", None]),
        ("tnt_energy_j_per_kg", [0, -1, math.nan, math.inf, True, "4e6", None]),
        ("threshold_j", [-1, math.nan, math.inf, True, "1"]),
    ],
)
def test_invalid_numeric_inputs(name, bad_values):
    for value in bad_values + [10**1000]:
        inputs = {"volume_m3": 0.01, "pressure_gauge": 1, "fluid_type": "gas"}
        inputs[name] = value
        with pytest.raises(ValueError, match=name):
            stored_energy(**inputs)


@pytest.mark.parametrize("fluid_type", ["steam", "Gas", "", None, 1, []])
def test_invalid_fluid_type(fluid_type):
    with pytest.raises(ValueError, match="fluid_type"):
        stored_energy(1, 1, fluid_type)


@pytest.mark.parametrize("unit", ["barg", "psig", "mPa", "atm", "", None, 1, []])
def test_invalid_pressure_unit(unit):
    with pytest.raises(ValueError, match="pressure_unit"):
        stored_energy(1, 1, "gas", pressure_unit=unit)


def test_small_gauge_pressure_does_not_cancel_to_zero():
    result = stored_energy(1, 1e-12, "gas")
    # First-order total boundary work is p_gauge*V/gamma near ambient.
    assert result.energy_j == pytest.approx(1e-12 / 1.4, rel=1e-12, abs=0)


def test_specific_heat_ratio_near_one_has_isothermal_limit():
    result = stored_energy(
        0.01, 300000, "gas", specific_heat_ratio=1.000000000001,
        ambient_pressure_pa=100000,
    )
    assert result.energy_j == pytest.approx(4000 * math.log(4), rel=1e-10)


@pytest.mark.parametrize(
    "overrides",
    [
        {"pressure_gauge": 1e308, "pressure_unit": "MPa"},
        {"pressure_gauge": 1e308, "ambient_pressure_pa": 1e308},
        {"volume_m3": 1e308},
        {"tnt_energy_j_per_kg": 1e-308},
        {"pressure_gauge": 1e308, "fluid_type": "liquid"},
    ],
)
def test_nonfinite_calculated_values_are_rejected(overrides):
    inputs = {"volume_m3": 0.01, "pressure_gauge": 1e6, "fluid_type": "gas"}
    inputs.update(overrides)
    with pytest.raises(ValueError, match="finite numeric range"):
        stored_energy(**inputs)
