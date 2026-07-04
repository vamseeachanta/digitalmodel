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

Deferred (integration / requires solver or proprietary data): transient method-of-
characteristics surge solver, detailed valve Cv flow sizing, FMEDA failure-mode
roll-up, and OrcaFlex / pipeline-pressure workflow wiring.
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

__all__ = [
    "SIL",
    "SILResult",
    "classify_sil",
    "pfd_avg_1oo1",
    "pfd_avg_1oo2",
    "risk_reduction_factor",
    "OverpressureResult",
    "overpressure_envelope",
]
