"""Inverse boresight corrections and paired overlap depth screening.

IHO S-44 edition 6.1.0 (October 2022), section 3.2.3 and Table 1:
TVU_max(d)=sqrt(a**2+(b*d)**2). a [m], b [dimensionless] and order/edition
are REQUIRED caller inputs transcribed from the applicable table. This module
has no built-in order coefficients. Missing evidence yields NOT_EVALUATED.

An overlap difference is a consistency diagnostic, not an estimate of total
vertical uncertainty at the standard's confidence level. Comparing it directly
to the individual-sounding TVU ceiling is an explicitly conservative screening
choice, not an IHO crossline acceptance rule or S-44 certification. No sqrt(2)
allowance or assumed independence is introduced. The user must qualify all
other error sources, datum reduction, coverage and order requirements.
"""

from dataclasses import dataclass, replace
from math import hypot

from .geometry import STANDARD_ORDER, Offsets, Swath, finite, rotate


def apply_corrections(
    swath: Swath,
    offsets: Offsets,
    *,
    order: tuple[str, ...] = STANDARD_ORDER,
) -> Swath:
    """Remove signed biases in the stated vessel frame (see geometry.py).

    The default inverse is latency,pitch,roll,yaw. Another complete permutation
    is allowed ONLY to demonstrate noncommutation; it changes the result.
    No refitting or access to synthetic truth occurs during correction.
    """
    if len(order) != len(STANDARD_ORDER) or set(order) != set(STANDARD_ORDER):
        raise ValueError(
            "correction order must be a permutation of latency,pitch,roll,yaw"
        )
    corrected = []
    d = swath.line.direction
    for (x, y, z), (ox, oy, oz) in zip(swath.xyz_m, swath.origins_m):
        q = (d * (x - ox), d * (y - oy), z - oz)
        for axis in order:
            if axis == "latency":
                q = (q[0] - swath.line.speed_m_s * offsets.latency_s, q[1], q[2])
            else:
                q = rotate(q, axis, -getattr(offsets, f"{axis}_deg"))
        corrected.append((ox + d * q[0], oy + d * q[1], oz + q[2]))
    return replace(swath, xyz_m=tuple(corrected))


@dataclass(frozen=True)
class DepthUncertainty:
    """User-selected S-44 table inputs and their evidence; no inferred defaults."""

    order: str
    edition: str
    a_m: float
    b: float
    table_reference: str

    def __post_init__(self):
        if any(
            not isinstance(v, str) or not v.strip()
            for v in (self.order, self.edition, self.table_reference)
        ):
            raise ValueError("order, edition and table reference must be present")
        if finite(self.a_m, "a") < 0 or finite(self.b, "b") < 0:
            raise ValueError("uncertainty coefficients must be nonnegative")
        if self.a_m == self.b == 0:
            raise ValueError("at least one uncertainty coefficient must be positive")

    def limit_m(self, depth_m: float) -> float:
        """S-44 6.1.0 (2022), 3.2.3: root sum of squared terms, at depth d."""
        if finite(depth_m, "depth") <= 0:
            raise ValueError("screen depth must be positive")
        return hypot(self.a_m, self.b * depth_m)


@dataclass(frozen=True)
class OverlapResult:
    """Matched points only; counts expose missing overlap coverage."""

    status: str
    matched_count: int
    unmatched_first: int
    unmatched_second: int
    max_abs_difference_m: float
    min_limit_m: float | None
    max_ratio: float | None
    source: str


def screen_overlap(
    a: Swath,
    b: Swath,
    *,
    budget: DepthUncertainty | None,
    xy_tolerance_m: float,
) -> OverlapResult:
    """One-to-one nearest XY matches within a REQUIRED user tolerance [m].

    No interpolation or extrapolation. For each matched pair, compare absolute
    depth difference to the TVU ceiling at its mean depth, and report the worst
    ratio over all pairs. Counts limit the conclusion to the matched subset.
    This quadratic reference matcher is for small synthetic swaths only.
    """
    if finite(xy_tolerance_m, "XY tolerance") < 0:
        raise ValueError("XY tolerance must be nonnegative")
    used, differences, limits = set(), [], []
    for x, y, z in a.xyz_m:
        candidates = [
            (hypot(x - p[0], y - p[1]), i)
            for i, p in enumerate(b.xyz_m)
            if i not in used
        ]
        if not candidates:
            continue
        distance, index = min(candidates)
        if distance > xy_tolerance_m:
            continue
        other = b.xyz_m[index]
        if finite(z, "sounding depth") <= 0 or finite(other[2], "sounding depth") <= 0:
            raise ValueError("sounding depths must be positive")
        used.add(index)
        differences.append(abs(z - other[2]))
        if budget is not None:
            limits.append(budget.limit_m((z + other[2]) / 2))
    if not differences:
        raise ValueError("no overlap matches within supplied XY tolerance")
    ratio = (
        max(difference / limit for difference, limit in zip(differences, limits))
        if limits
        else None
    )
    status = (
        "NOT_EVALUATED"
        if ratio is None
        else ("WITHIN_SCREEN" if ratio <= 1 else "EXCEEDS_SCREEN")
    )
    return OverlapResult(
        status,
        len(differences),
        len(a.xyz_m) - len(used),
        len(b.xyz_m) - len(used),
        max(differences),
        min(limits) if limits else None,
        ratio,
        budget.table_reference if budget else "S-44 table inputs not supplied",
    )
