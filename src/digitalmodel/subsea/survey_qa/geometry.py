"""Synthetic MBES geometry; constant sound velocity, no travel-time ray tracing.

Ground frame: X along the forward test line, Y to its starboard, Z positive
DOWN. A reciprocal line has direction -1: vessel forward/starboard axes are
(-X, -Y), with the same down axis. Only these parallel headings are supported.
Each ping origin is the navigation position plus the supplied sensor lever arm.
A beam is the vector (0, b, depth) from that origin; b is positive starboard.

Positive right-handed Rx (roll) moves starboard returns deeper; Ry (pitch)
moves nadir returns forward; Rz (yaw) moves starboard returns aft. Positive
latency advances the recorded origin along the direction of travel by v*tau.
Angles are degrees, time seconds, lengths metres, speeds metres/second.

Derivation (this module): Rx(y,z)=(cy-sz,sy+cz), Ry(x,z)=(cx+sz,-sx+cz),
Rz(x,y)=(cx-sy,sx+cy), with c=cos(angle), s=sin(angle). Orthogonal matrices
have inverse R(-angle). Synthetic injection is T_latency Ry Rx Rz q, so the
inverse acts in order latency, pitch, roll, yaw. This is an explicitly chosen
boresight convention, not an assertion about an acquisition system's Euler
convention. Matrix noncommutation supplies the order-dependence example.
Trigonometric ratios: OpenStax, Precalculus 2e (2021), section 5.4.
Geometric basis: NOAA Field Procedures Manual (2020), section 1.5.6.2,
pp. 26-27 (reciprocal/parallel line geometry); see docs/agent-pack.md for URL.
Numerical zeros, ones and matrix dimensions below are mathematical identities,
not empirical coefficients or acceptance limits. All survey data are inputs.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import cos, isfinite, radians, sin
from random import Random

Point = tuple[float, float, float]
STANDARD_ORDER = ("latency", "pitch", "roll", "yaw")


def finite(value: float, name: str) -> float:
    if not isfinite(value):
        raise ValueError(f"{name} must be finite")
    return value


@dataclass(frozen=True)
class Offsets:
    """Signed injected biases; corrections subtract these using inverse matrices."""

    roll_deg: float = 0.0
    pitch_deg: float = 0.0
    yaw_deg: float = 0.0
    latency_s: float = 0.0

    def __post_init__(self):
        for name in ("roll_deg", "pitch_deg", "yaw_deg"):
            # At vertical, the profile tangent is singular; no field limit implied.
            if abs(finite(getattr(self, name), name)) >= 90:
                raise ValueError(f"{name} must lie strictly between -90 and 90")
        finite(self.latency_s, "latency_s")


@dataclass(frozen=True)
class Acquisition:
    """Required sensor lever arm, constant SVP, and paired ping/navigation clocks.

    The tolerance is caller supplied, not an instrument synchronisation limit.
    The SVP is checked as metadata; coordinates are prescribed geometrically.
    """

    sensor_offsets_m: Point
    svp: tuple[tuple[float, float], ...]
    sonar_times_s: tuple[float, ...]
    navigation_times_s: tuple[float, ...]
    sync_tolerance_s: float

    def __post_init__(self):
        if self.sensor_offsets_m is None or len(self.sensor_offsets_m) != 3:
            raise ValueError("sensor offsets must contain three components")
        for value in self.sensor_offsets_m:
            finite(value, "sensor offsets")
        if not self.svp:
            raise ValueError("sound-velocity profile must be present")
        for row in self.svp:
            if len(row) != 2:
                raise ValueError("sound-velocity profile rows must be (depth, speed)")
            depth, speed = row
            if finite(depth, "SVP depth") < 0 or finite(speed, "sound velocity") <= 0:
                raise ValueError("SVP depths must be nonnegative and speeds positive")
        if any(b[0] <= a[0] for a, b in zip(self.svp, self.svp[1:])):
            raise ValueError("SVP depths must increase strictly")
        if any(row[1] != self.svp[0][1] for row in self.svp):
            raise ValueError("only a constant sound-velocity profile is supported")
        if finite(self.sync_tolerance_s, "sync tolerance") < 0:
            raise ValueError("sync tolerance must be nonnegative")
        if (
            self.sonar_times_s is None
            or self.navigation_times_s is None
            or len(self.sonar_times_s) < 2
            or len(self.sonar_times_s) != len(self.navigation_times_s)
        ):
            raise ValueError(
                "paired sonar/navigation timestamps require at least two pings"
            )
        for times in (self.sonar_times_s, self.navigation_times_s):
            for value in times:
                finite(value, "timestamp")
            if any(b <= a for a, b in zip(times, times[1:])):
                raise ValueError("timestamps must be strictly monotonic")
        if any(
            abs(a - b) > self.sync_tolerance_s
            for a, b in zip(self.sonar_times_s, self.navigation_times_s)
        ):
            raise ValueError(
                "timestamps must be synchronised within supplied tolerance"
            )


@dataclass(frozen=True)
class Line:
    """Navigation start (X,Y), parallel heading sign, and positive speed."""

    start_x_m: float
    track_y_m: float
    direction: int
    speed_m_s: float

    def __post_init__(self):
        finite(self.start_x_m, "start X")
        finite(self.track_y_m, "track Y")
        if self.direction not in (-1, 1):
            raise ValueError("direction must be +1 or -1")
        if finite(self.speed_m_s, "speed") <= 0:
            raise ValueError("speed must be positive")


@dataclass(frozen=True)
class Seabed:
    """Plane Z=d0+sx*X+sy*Y, with optional one-point (X,Y,height) shoaling.

    A point target is returned only if the specified ping/beam grid hits it
    exactly. There is no footprint or feature-detection algorithm.
    """

    depth_m: float
    slope_x: float
    slope_y: float
    feature: Point | None = None

    def __post_init__(self):
        if finite(self.depth_m, "depth") <= 0:
            raise ValueError("depth must be positive")
        finite(self.slope_x, "slope X")
        finite(self.slope_y, "slope Y")
        if self.feature is not None:
            if len(self.feature) != 3:
                raise ValueError("feature must contain X, Y and height")
            for value in self.feature:
                finite(value, "feature")
            if self.feature[2] <= 0:
                raise ValueError("feature height must be positive")

    def depth_at(self, x: float, y: float) -> float:
        depth = self.depth_m + self.slope_x * x + self.slope_y * y
        if self.feature is not None and (x, y) == self.feature[:2]:
            depth -= self.feature[2]
        return depth


@dataclass(frozen=True)
class Swath:
    """Soundings plus explicit acquisition/line metadata, with synthetic truth.

    Estimators use only xyz_m and acquisition geometry, never truth_xyz_m.
    beam_offsets_m identifies the nadir samples; feature_index is an explicitly
    associated target return, not a target picked from the truth coordinates.
    """

    xyz_m: tuple[Point, ...]
    origins_m: tuple[Point, ...]
    beam_offsets_m: tuple[float, ...]
    truth_xyz_m: tuple[Point, ...]
    line: Line
    seabed: Seabed
    acquisition: Acquisition
    feature_index: int | None

    def __post_init__(self):
        if not self.xyz_m:
            raise ValueError("swath soundings must be nonempty")
        if len(self.xyz_m) != len(self.origins_m) or len(self.xyz_m) != len(
            self.beam_offsets_m
        ):
            raise ValueError(
                "soundings, origins and beam offsets must have equal length"
            )
        if self.truth_xyz_m and len(self.truth_xyz_m) != len(self.xyz_m):
            raise ValueError("synthetic truth must be absent or have equal length")
        for collection in (self.xyz_m, self.origins_m, self.truth_xyz_m):
            for point in collection:
                if len(point) != 3:
                    raise ValueError("coordinates must contain three components")
                for value in point:
                    finite(value, "coordinate")
        for beam in self.beam_offsets_m:
            finite(beam, "beam offset")
        if self.feature_index is not None and (
            not isinstance(self.feature_index, int)
            or not 0 <= self.feature_index < len(self.xyz_m)
        ):
            raise ValueError("feature index must identify a sounding")


def rotate(point: Point, axis: str, angle_deg: float) -> Point:
    """Right-handed active rotation; see the module's matrix derivation."""
    x, y, z = point
    c, s = cos(radians(angle_deg)), sin(radians(angle_deg))
    if axis == "roll":
        return x, c * y - s * z, s * y + c * z
    if axis == "pitch":
        return c * x + s * z, y, -s * x + c * z
    if axis == "yaw":
        return c * x - s * y, s * x + c * y, z
    raise ValueError("unknown rotation axis")


def generate_swath(
    seabed: Seabed,
    line: Line,
    acquisition: Acquisition,
    beam_offsets_m: tuple[float, ...],
    offsets: Offsets,
    *,
    noise_std_m: float,
    seed: int,
) -> Swath:
    """Generate prescribed beam footprints; seeded independent depth noise.

    Reciprocal lines use direction=-1; two-speed runs use different positive
    speeds with direction unchanged. Sensor offsets are applied, not discarded.
    Sensor Z must remain above the prescribed seabed at every footprint.
    """
    if not beam_offsets_m or len(set(beam_offsets_m)) != len(beam_offsets_m):
        raise ValueError("beam offsets must be present and unique")
    for beam in beam_offsets_m:
        finite(beam, "beam offset")
    if finite(noise_std_m, "noise standard deviation") < 0:
        raise ValueError("noise standard deviation must be nonnegative")
    if not isinstance(seed, int) or isinstance(seed, bool):
        raise ValueError("noise seed must be an explicit integer")
    rng = Random(seed)
    xyz, truth, origins, beams = [], [], [], []
    feature_index = None
    direction = line.direction
    lx, ly, lz = acquisition.sensor_offsets_m
    for time in acquisition.sonar_times_s:
        ox = line.start_x_m + direction * (
            line.speed_m_s * (time - acquisition.sonar_times_s[0]) + lx
        )
        oy = line.track_y_m + direction * ly
        for beam in beam_offsets_m:
            y = oy + direction * beam
            depth = seabed.depth_at(ox, y)
            if depth <= lz or depth <= 0:
                raise ValueError("seabed must be below the sensor and depth datum")
            if seabed.feature is not None and (ox, y) == seabed.feature[:2]:
                feature_index = len(xyz)
            q = (0.0, beam, depth - lz)
            for axis in ("yaw", "roll", "pitch"):
                q = rotate(q, axis, getattr(offsets, f"{axis}_deg"))
            xyz.append(
                (
                    ox + direction * (q[0] + line.speed_m_s * offsets.latency_s),
                    oy + direction * q[1],
                    lz + q[2] + rng.gauss(0, noise_std_m),
                )
            )
            truth.append((ox, y, depth))
            origins.append((ox, oy, lz))
            beams.append(beam)
    return Swath(
        tuple(xyz),
        tuple(origins),
        tuple(beams),
        tuple(truth),
        line,
        seabed,
        acquisition,
        feature_index,
    )
