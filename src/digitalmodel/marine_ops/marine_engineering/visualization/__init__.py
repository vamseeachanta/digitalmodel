"""Polar plot with vessel silhouette and on-body force vectors (digitalmodel#616).

Reusable visualization for OCIMF coefficient explorer + moored-vessel force
reviews + generic force/moment studies.

See `polar_force_overlay.polar_force_overlay()` for the main entrypoint and
`_convention.OCIMF_CONVENTION_AUTHORITY` for the OCIMF MEG3/MEG4 Annex A
sign-convention citation.
"""
from .polar_force_overlay import polar_force_overlay
from .types import (
    ForceArrowKind,
    FrameConvention,
    RadialAxisMode,
    VesselSilhouetteSpec,
)

__all__ = [
    "polar_force_overlay",
    "VesselSilhouetteSpec",
    "FrameConvention",
    "ForceArrowKind",
    "RadialAxisMode",
]
