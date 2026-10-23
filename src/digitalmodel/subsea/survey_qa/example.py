"""One synthetic calibration campaign; all geometry is chosen test input.

Injected offsets are prescribed by issue #2955. Grid dimensions, slopes, lever
arm, sound velocity, noise and seed are illustrative choices, not published
instrument characteristics or standard acceptance limits. The SVP is constant.
"""

from .calibration import PatchTestLines
from .geometry import Acquisition, Line, Offsets, Seabed, generate_swath

EXAMPLE_OFFSETS = Offsets(roll_deg=0.5, pitch_deg=1.0, yaw_deg=1.5, latency_s=0.08)


def make_example(*, noise_std_m: float, seed: int) -> PatchTestLines:
    """Eight lines, common biases, flat/slope patches and one associated point.

    Chosen inputs: 50 m reference depth; along-track slope 0.2; target height
    5 m; beams -30 to +30 m; tracks -30/+30 m for yaw; speeds 2 and 4 m/s;
    pings every 1 s over 40 s. These values are synthetic, not sourced limits.
    """
    times = tuple(float(t) for t in range(41))
    acquisition = Acquisition((0, 0, 0), ((0, 1500), (100, 1500)), times, times, 0)
    beams = (-30.0, -20.0, -10.0, 0.0, 10.0, 20.0, 30.0)
    slope, flat = Seabed(50, 0.2, 0), Seabed(50, 0, 0)
    target = Seabed(50, 0.2, 0, (0, 0, 5))

    def run(bed, direction, speed, y, seed_offset):
        return generate_swath(
            bed,
            Line(-40 if direction == 1 else 40, y, direction, speed),
            acquisition,
            beams,
            EXAMPLE_OFFSETS,
            noise_std_m=noise_std_m,
            seed=seed + seed_offset,
        )

    return PatchTestLines(
        latency=(run(slope, 1, 2, 0, 0), run(slope, 1, 4, 0, 1)),
        pitch=(run(slope, 1, 2, 0, 2), run(slope, -1, 2, 0, 3)),
        roll=(run(flat, 1, 2, 0, 4), run(flat, -1, 2, 0, 5)),
        yaw=(run(target, 1, 2, -30, 6), run(target, 1, 2, 30, 7)),
    )
