"""Synthetic multibeam patch-test geometry and estimators; SI units."""

from .calibration import PatchTestLines, estimate_patch
from .estimators import estimate_latency, estimate_pitch, estimate_roll, estimate_yaw
from .geometry import Acquisition, Line, Offsets, Seabed, Swath, generate_swath
from .verification import (
    DepthUncertainty,
    OverlapResult,
    apply_corrections,
    screen_overlap,
)

__all__ = [
    "Acquisition",
    "Line",
    "Offsets",
    "Seabed",
    "Swath",
    "generate_swath",
    "PatchTestLines",
    "estimate_patch",
    "estimate_latency",
    "estimate_pitch",
    "estimate_roll",
    "estimate_yaw",
    "DepthUncertainty",
    "OverlapResult",
    "apply_corrections",
    "screen_overlap",
]
