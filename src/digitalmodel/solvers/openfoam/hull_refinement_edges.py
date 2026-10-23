"""Place horizontal refinement transitions using the actual vertical cells.

Sources: OpenFOAM Foundation, OpenFOAM 13 (2025),
``meshCheck::faceOrthogonality`` and ``boxToCell::combine``:
https://cpp.openfoam.org/v13/primitiveMeshCheck_8C_source.html
https://cpp.openfoam.org/v13/boxToCell_8C_source.html

This is a Cartesian, pre-snap geometry calculation for x-y-only bisection.
It cannot certify the quality of a snapped mesh. No acceptance limit or
vertical grading is inferred: callers supply both the limit and cell faces.
"""

from __future__ import annotations

import math
from bisect import bisect_left, bisect_right
from dataclasses import dataclass
from typing import Sequence

Vec3 = tuple[float, float, float]
Box = tuple[Vec3, Vec3]


def _positive_lengths(*values: float) -> None:
    if any(not math.isfinite(value) or value <= 0 for value in values):
        raise ValueError("cell dimensions must be finite and positive")


def xy_transition_angle_deg(
    dx: float, dy: float, dz_below: float, dz_above: float
) -> float:
    """Angle at a horizontal face with one x-y bisection across it.

    OpenFOAM 13 (2025), meshCheck::faceOrthogonality defines cos(theta)
    from the centre-to-centre vector and the face normal. Hand derivation:
    bisection puts a child centre dx/4 and dy/4 from the parent centre in
    plane; vertical centre separation is (dz_below + dz_above)/2. Thus
    theta = atan2(hypot(dx/4, dy/4), (dz_below + dz_above)/2).
    The divisors are exact centroid geometry, not empirical coefficients.
    """
    _positive_lengths(dx, dy, dz_below, dz_above)
    offset = math.hypot(dx / 4, dy / 4)
    separation = dz_below / 2 + dz_above / 2
    return math.degrees(math.atan2(offset, separation))


@dataclass(frozen=True)
class RefinementZEdgePolicy:
    """Required geometry and caller-selected acceptance criterion.

    ``z_faces`` are all background cell faces, including both domain walls,
    in ascending order; block boundaries or average block dz are insufficient.
    ``background_dx`` and ``background_dy`` are achieved cell sizes, after
    rounding horizontal counts. These remain uniform before the x-y ladder.
    ``max_angle_deg`` is a required input, not a recommended solver setting.
    """

    z_faces: tuple[float, ...]
    background_dx: float
    background_dy: float
    max_angle_deg: float

    def __post_init__(self) -> None:
        # Store a stable copy even if a caller passed a mutable sequence.
        object.__setattr__(self, "z_faces", tuple(self.z_faces))
        z = self.z_faces
        if (len(z) < 2 or any(not math.isfinite(v) for v in z)
                or any(a >= b for a, b in zip(z, z[1:]))):
            raise ValueError("z_faces must be finite and strictly increasing")
        _positive_lengths(self.background_dx, self.background_dy)
        # Positive vertical separation makes the geometric angle acute;
        # 90 degrees is the perpendicular-vector boundary, not a quality limit.
        if not math.isfinite(self.max_angle_deg) or not 0 < self.max_angle_deg < 90:
            raise ValueError("max_angle_deg must be finite and between 0 and 90")


def place_refinement_z_edges(
    boxes: Sequence[Box], policy: RefinementZEdgePolicy
) -> list[Box]:
    """Expand nested boxes outward to the nearest safe vertical cell faces.

    OpenFOAM 13 (2025), boxToCell::combine selects cell centres. Placing box
    edges on actual faces makes the selected rows unambiguous. Only z changes.
    Stages bisect x and y in order, outermost first, so the parent size at
    stage k (zero-based) is the background size divided by 2**k.

    Work from inside out and leave at least one parent row beyond an inner
    edge: coincident internal transitions could jump several levels and
    invalidate the single-bisection angle. Domain walls need no such buffer
    because there is no coarse neighbouring cell beyond a boundary face.
    Expansion to a wall can increase cell counts substantially. All boxes
    must initially be nested and inside the supplied vertical domain.
    """
    z = policy.z_faces
    last = len(z) - 1
    for k, (lo, hi) in enumerate(boxes):
        if (any(not math.isfinite(v) for v in (*lo, *hi))
                or any(a >= b for a, b in zip(lo, hi))
                or not z[0] <= lo[2] < hi[2] <= z[-1]):
            raise ValueError(f"box {k} must be finite, ordered and inside z_faces")
        if k:
            parent_lo, parent_hi = boxes[k - 1]
            if any(a < p or b > q for a, b, p, q in
                   zip(lo, hi, parent_lo, parent_hi)):
                raise ValueError(f"box {k} must be nested inside its parent")

    result = list(boxes)
    inner_lower, inner_upper = None, None
    for k in reversed(range(len(boxes))):
        lo, hi = boxes[k]
        lower = bisect_right(z, lo[2]) - 1
        upper = bisect_left(z, hi[2])
        if inner_lower is not None:
            lower = min(lower, max(0, inner_lower - 1))
            upper = max(upper, min(last, inner_upper + 1))

        # Each stage is a bisection, hence the power of two (derivation above).
        dx = math.ldexp(policy.background_dx, -k)
        dy = math.ldexp(policy.background_dy, -k)

        def safe(face: int) -> bool:
            if face in (0, last):
                return True
            return xy_transition_angle_deg(
                dx, dy, z[face] - z[face - 1], z[face + 1] - z[face]
            ) <= policy.max_angle_deg

        while not safe(lower):
            lower -= 1
        while not safe(upper):
            upper += 1
        result[k] = ((lo[0], lo[1], z[lower]), (hi[0], hi[1], z[upper]))
        inner_lower, inner_upper = lower, upper
    return result
