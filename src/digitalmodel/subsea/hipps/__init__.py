# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""digitalmodel.subsea.hipps — HIPPS (High Integrity Pressure Protection System).

HIPPS is the SIS (Safety Instrumented System) that protects a subsea/topside
flowline from wellhead overpressure by closing fast-acting valves before the
downstream pressure exceeds the design (MAOP) of the protected segment.

API 17O ("Subsea High Integrity Pressure Protection Systems") adopts the
functional-safety framework of IEC 61508 / IEC 61511 for the Safety Instrumented
Function (SIF). This package implements the deterministic, public-standard core:

- ``sil_analysis``     — PFDavg, Risk Reduction Factor, and SIL classification
                         (IEC 61508-1 / 61511-1).
- ``overpressure``     — overpressure-protection envelope: does the HIPPS close
                         (valve stroke + transmitter delay) before the protected
                         segment reaches its design pressure during a surge.

- ``pressure_surge`` — frictionless single-reach liquid characteristics solver;
  sampled pressure-rise rate with explicit time-grid limitations.
- ``valve_sizing`` — caller-coefficient liquid/gas sizing, stroke-time and
  idealized conductance closure laws. Standard compliance remains unqualified.

The pipeline pressure-sizing workflow accepts an optional HIPPS setpoint screen
without wall-sizing credit. Deferred: FMEDA failure-mode roll-up, coupled
multiphase transients, standards qualification and licensed solver integration.
"""

from digitalmodel.subsea.hipps.sil_analysis import (
    SIL,
    SILResult,
    classify_sil,
    pfd_avg_1oo1,
    pfd_avg_1oo2,
    risk_reduction_factor,
)
from digitalmodel.subsea.hipps.overpressure import (
    OverpressureResult,
    overpressure_envelope,
)
from .pressure_surge import SurgeResult, wave_speed, joukowsky_rise, simulate_surge
from .valve_sizing import (
    ValveCapacity, liquid_capacity, required_liquid_coefficient,
    gas_mass_capacity, stroke_time, closure_fraction,
)

__all__ = [
    "SIL",
    "SILResult",
    "classify_sil",
    "pfd_avg_1oo1",
    "pfd_avg_1oo2",
    "risk_reduction_factor",
    "OverpressureResult",
    "overpressure_envelope",
    "SurgeResult",
    "wave_speed",
    "joukowsky_rise",
    "simulate_surge",
    "ValveCapacity",
    "liquid_capacity",
    "required_liquid_coefficient",
    "gas_mass_capacity",
    "stroke_time",
    "closure_fraction",
]
