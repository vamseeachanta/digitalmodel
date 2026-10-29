# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Water-injection hydraulic and shaft power from an explicit pump duty.

For steady incompressible flow, pressure work is force times displacement,
or pressure rise times displaced volume: dE = delta_p * dV. Thus hydraulic
power is delta_p * Q. Since Pa = N/m2, Pa * m3/s = N*m/s = W.
The SI prefix kilo gives 1000 W per kW. Pump efficiency is hydraulic power
divided by shaft power, so shaft power is hydraulic power / efficiency.

References opened on 2026-10-26:
    DOE, Improving Pumping System Performance, Second Edition,
    Section 1, "System Operating Costs" (fluid power and pump efficiency):
    https://www.energy.gov/sites/prod/files/2014/05/f16/pump.pdf
    NIST, Metric (SI) Prefixes (kilo = 1000):
    https://www.nist.gov/pml/owm/metric-si-prefixes

Only pressure work is included; changes in kinetic and potential energy
are neglected. The caller supplies the required pump pressure rise and
pump efficiency at the duty point. Motor and drive losses are excluded.
"""

from dataclasses import dataclass
from math import isfinite


@dataclass(frozen=True)
class WaterInjectionPumpPower:
    """Hydraulic output and mechanical shaft input, both in kW."""

    hydraulic_power_kw: float
    shaft_power_kw: float


def water_injection_pump_power(
    flow_m3_s: float,
    pressure_rise_pa: float,
    efficiency: float,
) -> WaterInjectionPumpPower:
    """Calculate pump hydraulic and shaft power without equipment selection.

    Args:
        flow_m3_s: Finite volumetric water flow in m3/s, nonnegative.
        pressure_rise_pa: Finite pump discharge minus suction pressure in Pa,
            strictly positive.
        efficiency: Finite hydraulic-to-shaft power ratio, in (0, 1].
            Required explicitly; excludes motor and drive efficiency.

    Returns:
        Hydraulic power and required shaft power in kW. Both are zero when
        flow is zero; pressure rise and efficiency must still be valid.

    Raises:
        ValueError: An input is nonfinite or outside its permitted range.
    """
    if not isfinite(flow_m3_s) or flow_m3_s < 0:
        raise ValueError("flow_m3_s must be finite and nonnegative")
    if not isfinite(pressure_rise_pa) or pressure_rise_pa <= 0:
        raise ValueError("pressure_rise_pa must be finite and positive")
    if not isfinite(efficiency) or not 0 < efficiency <= 1:
        raise ValueError("efficiency must be finite and in (0, 1]")

    # NIST SI prefix kilo: 1000 W per kW (reference above).
    hydraulic_power_kw = pressure_rise_pa * flow_m3_s / 1000.0
    return WaterInjectionPumpPower(
        hydraulic_power_kw=hydraulic_power_kw,
        shaft_power_kw=hydraulic_power_kw / efficiency,
    )
