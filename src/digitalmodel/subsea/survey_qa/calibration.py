"""Sequential patch-test estimation using four explicitly selected line pairs.

NOAA FPM (2020), section 1.5.6.2, pp. 26-27 gives the pitch, roll, yaw
sequence. Latency is evaluated first here as the requested synthetic diagnostic
(issue #2955), removing a translational contaminant before angular estimation.
"""

from dataclasses import dataclass

from .estimators import estimate_latency, estimate_pitch, estimate_roll, estimate_yaw
from .geometry import Offsets, Swath
from .verification import apply_corrections


@dataclass(frozen=True)
class PatchTestLines:
    """Pairs: two-speed slope, reciprocal slope, reciprocal flat, parallel target."""

    latency: tuple[Swath, Swath]
    pitch: tuple[Swath, Swath]
    roll: tuple[Swath, Swath]
    yaw: tuple[Swath, Swath]

    def __post_init__(self):
        for pair in (self.latency, self.pitch, self.roll, self.yaw):
            if len(pair) != 2 or any(not isinstance(s, Swath) for s in pair):
                raise ValueError("each patch test requires exactly two swaths")


def estimate_patch(lines: PatchTestLines) -> Offsets:
    """Estimate L/P/R/Y in order; each stage removes all earlier estimates.

    This is profile-based synthetic calibration, not nonlinear bundle adjustment.
    Constant biases across all pairs and explicitly associated target returns
    are assumptions. No injected parameters or truth coordinates are consulted.
    """
    latency = estimate_latency(*lines.latency)
    partial = Offsets(latency_s=latency)
    pitch = estimate_pitch(*(apply_corrections(s, partial) for s in lines.pitch))
    partial = Offsets(pitch_deg=pitch, latency_s=latency)
    roll = estimate_roll(*(apply_corrections(s, partial) for s in lines.roll))
    partial = Offsets(roll_deg=roll, pitch_deg=pitch, latency_s=latency)
    yaw = estimate_yaw(*(apply_corrections(s, partial) for s in lines.yaw))
    return Offsets(roll, pitch, yaw, latency)
