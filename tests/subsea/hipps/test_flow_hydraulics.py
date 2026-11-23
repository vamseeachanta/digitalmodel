# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Numerical tests for caller-coefficient valve sizing and liquid surge."""

import math

import pytest


def test_liquid_manufacturer_worked_arithmetic():
    """Use handbook section 5.8.4 inputs and its rounded piping factor."""
    from digitalmodel.subsea.hipps.valve_sizing import liquid_capacity

    result = liquid_capacity(
        coefficient=125.7, p_upstream=314.7, p_downstream=289.7,
        specific_gravity=0.5, unit_constant=1.0, piping_factor=0.90,
        recovery_factor=0.81, critical_pressure_ratio=0.83,
        vapor_pressure=124.3,
    )
    # q = N1*Fp*Cv*sqrt(25/0.5); handbook rounds Cv and Fp.
    assert result.rate == pytest.approx(800, rel=0.002)
    assert not result.choked


def test_liquid_choking_and_inverse():
    from digitalmodel.subsea.hipps.valve_sizing import (
        liquid_capacity, required_liquid_coefficient,
    )

    inputs = dict(p_upstream=100.0, p_downstream=0.0,
                  specific_gravity=1.0, unit_constant=1.0,
                  piping_factor=1.0, recovery_factor=0.5,
                  critical_pressure_ratio=1.0, vapor_pressure=0.0)
    result = liquid_capacity(coefficient=2.0, **inputs)
    assert result.choked
    assert result.effective_drop == 25.0
    assert result.rate == 10.0
    assert required_liquid_coefficient(rate=10.0, **inputs) == 2.0


def test_gas_choking_is_independent_of_downstream_pressure():
    from digitalmodel.subsea.hipps.valve_sizing import gas_mass_capacity

    inputs = dict(coefficient=2.0, p_upstream=100.0, density=1.0,
                  unit_constant=1.0, piping_factor=1.0, choked_ratio=0.5)
    first = gas_mass_capacity(p_downstream=40.0, **inputs)
    second = gas_mass_capacity(p_downstream=0.0, **inputs)
    assert first.choked and second.choked
    assert first.rate == pytest.approx(2 * (2 / 3) * math.sqrt(50))
    assert second.rate == first.rate


def test_closure_laws_and_actuator_time():
    from digitalmodel.subsea.hipps.valve_sizing import closure_fraction, stroke_time

    assert stroke_time(stroke_length=0.3, actuator_speed=0.1) == pytest.approx(3)
    assert closure_fraction(1, 2, "linear") == 0.5
    assert closure_fraction(1, 2, "equal_percentage", rangeability=100) == 0.1
    assert closure_fraction(2, 2, "equal_percentage", rangeability=100) == 0
    assert closure_fraction(0, 0, "linear") == 0
    with pytest.raises(ValueError):
        closure_fraction(1, 2, "equal_percentage")


@pytest.mark.parametrize("value", [-1.0, float("nan"), float("inf")])
def test_valve_rejects_invalid_physical_inputs(value):
    from digitalmodel.subsea.hipps.valve_sizing import gas_mass_capacity

    with pytest.raises(ValueError):
        gas_mass_capacity(coefficient=value, p_upstream=100, p_downstream=50,
                          density=1, unit_constant=1, piping_factor=1,
                          choked_ratio=0.5)


def test_wave_speed_wall_compliance_and_joukowsky():
    from digitalmodel.subsea.hipps.pressure_surge import wave_speed, joukowsky_rise

    # Required elastic inputs; the expected value follows the stated equation.
    speed = wave_speed(density=1000, bulk_modulus=2e9, diameter=0.2,
                       wall_thickness=0.01, elastic_modulus=2e11,
                       restraint_factor=1.0)
    expected = math.sqrt(1 / (1000 * (1 / 2e9 + 0.2 / (2e11 * 0.01))))
    assert speed == pytest.approx(expected)
    assert joukowsky_rise(1000, speed, 1.0) == pytest.approx(1000 * expected)


def surge_inputs():
    """Return an explicitly constructed SI arithmetic case, not a design."""
    return dict(length=1000.0, diameter=0.2, density=1000.0,
                acoustic_speed=1000.0, initial_flow=math.pi * 0.2**2 / 4,
                reservoir_pressure=5e6, downstream_pressure=1e6,
                duration=2.0, cells=20)


def test_moc_step_closure_reproduces_joukowsky_bound():
    from digitalmodel.subsea.hipps.pressure_surge import simulate_surge
    from digitalmodel.subsea.hipps import overpressure_envelope

    result = simulate_surge(closure_time=0.0, **surge_inputs())
    assert result.peak_pressure_pa - 5e6 == pytest.approx(1e6, rel=0.05)
    assert result.dpdt_pa_s > 0
    envelope = overpressure_envelope(5e6, 7e6, result.dpdt_pa_s, 0.01)
    assert envelope.passed


def test_moc_slow_closure_and_grid_refinement():
    from digitalmodel.subsea.hipps.pressure_surge import simulate_surge

    inputs = surge_inputs()
    inputs["duration"] = 10.0
    coarse = simulate_surge(closure_time=8.0, **inputs)
    inputs["cells"] = 40
    fine = simulate_surge(closure_time=8.0, **inputs)
    assert 5e6 < coarse.peak_pressure_pa < 6e6
    assert fine.peak_pressure_pa == pytest.approx(coarse.peak_pressure_pa, rel=0.02)
    assert fine.valve_flow_m3_s[-1] == 0


@pytest.mark.parametrize("field,value", [("density", float("nan")),
                                         ("cells", 1), ("closure_time", -1)])
def test_moc_rejects_invalid_inputs(field, value):
    from digitalmodel.subsea.hipps.pressure_surge import simulate_surge

    inputs = dict(surge_inputs(), closure_time=1.0)
    inputs[field] = value
    with pytest.raises(ValueError):
        simulate_surge(**inputs)


def test_hipps_workflow_setpoint_screen_preserves_wall_sizing():
    from digitalmodel.subsea.pipeline.pipeline_pressure import (
        WallThicknessSizingWorkflow,
    )

    cfg = dict(outer_diameter_m=0.3239, design_pressure_MPa=20.0,
               depth_m=500.0, material="X65")
    base = WallThicknessSizingWorkflow({"pipeline": cfg}).run()
    protected = WallThicknessSizingWorkflow(
        {"pipeline": dict(cfg, hipps_setpoint_MPa=18.0)}
    ).run()
    assert protected["hipps_setpoint_check"]["pass"]
    assert protected["hipps_setpoint_check"]["design_pressure_MPa"] == 20.0
    assert protected["hipps_setpoint_check"]["margin_MPa"] == 2.0
    assert protected["t_min_m"] == base["t_min_m"]
    excessive = WallThicknessSizingWorkflow(
        {"pipeline": dict(cfg, hipps_setpoint_MPa=21.0)}
    ).run()
    assert not excessive["hipps_setpoint_check"]["pass"]
