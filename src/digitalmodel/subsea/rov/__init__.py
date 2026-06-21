"""digitalmodel.subsea.rov — ROV intervention modeling (API 17H).

Subsea production systems (API 17 family) rely on work-class ROVs (API 17H) for
inspection, maintenance, and emergency intervention. Whether an intervention dive
can proceed is gated by a deterministic, public-standard constraint: the sea state
at the splash zone during Launch And Recovery System (LARS) operations.

This package implements that bounded, deterministic core:

- ``seastate_limits`` — LARS operability: single-sea-state check of significant
                        wave height against a rated limit with an optional
                        weather-forecast allowance (alpha factor), and the
                        probability-weighted operability percentage over a wave
                        scatter diagram (API 17H / IMCA R 004; weather-restricted
                        operation framework of DNV-ST-N001).

Deferred (integration / requires solver, RAO data, or proprietary catalogues):
spectral vessel-motion / snatch-load LARS dynamics (``hydrodynamics/seakeeping``),
the ROV tool catalogue and hot-stab / valve / capstan interface compatibility
checks, the API 17H interface-geometry verification, and intervention task
sequencing.
"""

from digitalmodel.subsea.rov.seastate_limits import (
    OperabilityResult,
    SeaStateResult,
    scatter_operability,
    seastate_operability,
)

__all__ = [
    "OperabilityResult",
    "SeaStateResult",
    "scatter_operability",
    "seastate_operability",
]
