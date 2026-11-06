# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic checks of water-injection pump power.

The duty values below are synthetic inputs, not equipment data or defaults.
Hand calculation: pressure work is 20,000,000 J per m3; at 0.02 m3/s
this is 400,000 W = 400 kW. At efficiency 0.8, shaft power is 500 kW.
At efficiencies 1, 0.5 and 0.25, shaft power is 400, 800 and 1,600 kW.
Zero flow transfers zero pressure work per second.

Basis: DOE, Improving Pumping System Performance, Second Edition,
"System Operating Costs" (fluid power and pump shaft efficiency):
https://www.energy.gov/sites/prod/files/2014/05/f16/pump.pdf
SI conversion: NIST, Metric (SI) Prefixes, kilo = 1000:
https://www.nist.gov/pml/owm/metric-si-prefixes
Invalid values are synthetic boundary probes of the stated input domain.
"""

import math

import pytest

from digitalmodel.field_development.facility_sizing import water_injection_pump_power


def test_pump_power_hand_calculation():
    result = water_injection_pump_power(
        flow_m3_s=0.02, pressure_rise_pa=20_000_000.0, efficiency=0.8
    )

    assert result.hydraulic_power_kw == pytest.approx(400.0)
    assert result.shaft_power_kw == pytest.approx(500.0)


def test_zero_flow_power():
    result = water_injection_pump_power(
        flow_m3_s=0.0, pressure_rise_pa=20_000_000.0, efficiency=0.8
    )

    assert result.hydraulic_power_kw == 0.0
    assert result.shaft_power_kw == 0.0


@pytest.mark.parametrize(
    ("efficiency", "expected_shaft_kw"),
    [(1.0, 400.0), (0.5, 800.0), (0.25, 1600.0)],
)
def test_efficiency_scaling(efficiency, expected_shaft_kw):
    result = water_injection_pump_power(0.02, 20_000_000.0, efficiency)

    assert result.hydraulic_power_kw == pytest.approx(400.0)
    assert result.shaft_power_kw == pytest.approx(expected_shaft_kw)


@pytest.mark.parametrize(
    ("parameter", "value"),
    [
        ("flow_m3_s", -0.02),
        ("flow_m3_s", math.nan),
        ("flow_m3_s", math.inf),
        ("flow_m3_s", -math.inf),
        ("pressure_rise_pa", 0.0),
        ("pressure_rise_pa", -20_000_000.0),
        ("pressure_rise_pa", math.nan),
        ("pressure_rise_pa", math.inf),
        ("pressure_rise_pa", -math.inf),
        ("efficiency", 0.0),
        ("efficiency", -0.8),
        ("efficiency", 1.01),
        ("efficiency", math.nan),
        ("efficiency", math.inf),
        ("efficiency", -math.inf),
    ],
)
@pytest.mark.parametrize("flow_m3_s", [0.0, 0.02])
def test_invalid_pump_inputs(parameter, value, flow_m3_s):
    inputs = {
        "flow_m3_s": flow_m3_s,
        "pressure_rise_pa": 20_000_000.0,
        "efficiency": 0.8,
    }
    inputs[parameter] = value

    with pytest.raises(ValueError, match=parameter):
        water_injection_pump_power(**inputs)
