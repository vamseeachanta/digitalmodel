#!/usr/bin/env python3
"""
ABOUTME: Does each appendage stay under water across the whole loading-condition
matrix (#2023)? A rudder that pierces the free surface at one condition and not
at the next puts a DISCONTINUITY in the middle of the resistance curve, and the
solver reports both sides of it without comment.

WHY THIS IS NOT OBVIOUS FROM THE MEAN DRAFT
-------------------------------------------
A resistance matrix is quoted as mean draft and trim. Appendages sit at the
STERN, and the stern's local draft is not the mean draft -- it is the mean plus
half the trim, with the sign depending on which end goes down. A matrix whose
mean drafts all comfortably clear an appendage can still expose it at the
trimmed conditions, and the exposure is largest exactly where the mean draft is
smallest, because ballast conditions carry the biggest trims.

So the question is asked per condition, against the LOCAL waterline at the
appendage's own longitudinal position, and answered with a clearance in metres
and an exposed area in square metres rather than a yes/no.

WHAT CHANGES WHEN AN APPENDAGE PIERCES
--------------------------------------
Everything about the case type. A fully submerged appendage is a wall inside
the water phase; a piercing one carries its own waterline, its own local wave
system and its own air-water interface that the free-surface refinement was
never sized for. It also breaks the comparison between conditions: the
appendage's contribution to resistance stops being a smooth function of draft.
This module does not decide what to do about it -- it makes sure nobody finds
out from the results.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Dict, List, Sequence, Tuple

__all__ = [
    "Attitude",
    "SubmergenceResult",
    "check_submergence",
    "submergence_report",
]

Vec3 = Tuple[float, float, float]
Tri = Tuple[Vec3, Vec3, Vec3]


@dataclass(frozen=True)
class Attitude:
    """One (draft, trim) condition, as a TILTED water plane over a fixed hull.

    Tilting the water rather than rotating the hull is the same geometry and a
    much smaller claim: a rigid rotation of the hull about the midship
    waterline point maps the horizontal water plane onto exactly this plane in
    hull coordinates, and doing it this way means the appendage triangles never
    move, so a clearance is measured against the surface that was actually
    ingested.

    ``draft_fwd_m`` and ``draft_aft_m`` are the drafts at the forward and aft
    ends of ``reference_length_m``, which for the ingested frame starts at
    x = 0 (the aft extremity of the wetted surface, this stage's origin).
    """

    name: str
    draft_fwd_m: float
    draft_aft_m: float
    reference_length_m: float
    x_aft_m: float = 0.0

    @property
    def draft_mean_m(self) -> float:
        return 0.5 * (self.draft_fwd_m + self.draft_aft_m)

    @property
    def trim_m(self) -> float:
        """Positive means the bow floats deeper than the stern."""
        return self.draft_fwd_m - self.draft_aft_m

    def waterline_z(self, x: float) -> float:
        """Free-surface height at longitudinal station ``x``."""
        if self.reference_length_m <= 0:
            raise ValueError(
                f"reference_length_m must be positive, got "
                f"{self.reference_length_m}"
            )
        t = (x - self.x_aft_m) / self.reference_length_m
        return self.draft_aft_m + t * self.trim_m

    def to_dict(self) -> Dict[str, float]:
        return {
            "name": self.name,
            "draft_mean_m": self.draft_mean_m,
            "trim_m": self.trim_m,
            "draft_fwd_m": self.draft_fwd_m,
            "draft_aft_m": self.draft_aft_m,
            "reference_length_m": self.reference_length_m,
        }


@dataclass(frozen=True)
class SubmergenceResult:
    """One region at one attitude."""

    region: str
    attitude: str
    min_clearance_m: float
    clearance_at_x_m: float
    total_area_m2: float
    exposed_area_m2: float

    @property
    def fully_submerged(self) -> bool:
        return self.min_clearance_m > 0.0

    def to_dict(self) -> Dict[str, object]:
        return {
            "region": self.region,
            "attitude": self.attitude,
            "fully_submerged": self.fully_submerged,
            "min_clearance_m": self.min_clearance_m,
            "clearance_at_x_m": self.clearance_at_x_m,
            "total_area_m2": self.total_area_m2,
            "exposed_area_m2": self.exposed_area_m2,
        }


def check_submergence(
    regions: Sequence[Tuple[str, Sequence[Tri]]],
    attitudes: Sequence[Attitude],
) -> List[SubmergenceResult]:
    """Clearance and exposed area for every region at every attitude.

    ``min_clearance_m`` is the smallest depth of submergence over the whole
    body -- the top of it, at the local waterline over its own station.
    Negative means the body breaks the surface, and by how much.
    """
    out: List[SubmergenceResult] = []
    for name, tris in regions:
        for attitude in attitudes:
            out.append(_one(name, tris, attitude))
    return out


def _one(
    name: str, tris: Sequence[Tri], attitude: Attitude
) -> SubmergenceResult:
    depth = lambda p: attitude.waterline_z(p[0]) - p[2]  # noqa: E731
    worst = min(
        ((depth(p), p[0]) for tri in tris for p in tri),
        key=lambda pair: pair[0],
    )
    total = sum(_area(*tri) for tri in tris)
    return SubmergenceResult(
        region=name,
        attitude=attitude.name,
        min_clearance_m=worst[0],
        clearance_at_x_m=worst[1],
        total_area_m2=total,
        exposed_area_m2=total - sum(_area_submerged(tri, depth) for tri in tris),
    )


def submergence_report(
    results: Sequence[SubmergenceResult],
) -> Dict[str, object]:
    """The matrix-wide verdict, with the exposures named."""
    exposed = [r for r in results if not r.fully_submerged]
    return {
        "n_checks": len(results),
        "all_fully_submerged": not exposed,
        "results": [r.to_dict() for r in results],
        "exposures": [
            f"{r.region} pierces the free surface at {r.attitude} by "
            f"{-r.min_clearance_m:.3f} m ({r.exposed_area_m2:.2f} m2 exposed)"
            for r in exposed
        ],
        "note": (
            "clearance is measured against the LOCAL waterline over each "
            "point's own longitudinal station, not against the mean draft: "
            "an appendage is at the stern and the stern's draft is the mean "
            "plus half the trim"
        ),
    }


# --------------------------------------------------------------------------- #
#  Area below an arbitrary plane
# --------------------------------------------------------------------------- #

def _area_submerged(tri: Tri, depth: Callable[[Vec3], float]) -> float:
    """Area of ``tri`` where ``depth`` is non-negative.

    A general linear scalar, not a z-threshold: the water plane is tilted, and
    shearing the triangles flat to reuse the horizontal clipper would change
    their areas by the shear factor -- a silent few per cent on exactly the
    quantity being reported.
    """
    d = [depth(p) for p in tri]
    if all(v >= 0.0 for v in d):
        return _area(*tri)
    if all(v <= 0.0 for v in d):
        return 0.0
    poly: List[Vec3] = []
    for i in range(3):
        a, b = tri[i], tri[(i + 1) % 3]
        da, db = d[i], d[(i + 1) % 3]
        if da >= 0.0:
            poly.append(a)
        if (da >= 0.0) != (db >= 0.0):
            t = da / (da - db)
            poly.append(
                tuple(a[k] + t * (b[k] - a[k]) for k in range(3))  # type: ignore[misc]
            )
    return sum(
        _area(poly[0], poly[i], poly[i + 1]) for i in range(1, len(poly) - 1)
    )


def _area(a: Vec3, b: Vec3, c: Vec3) -> float:
    ux, uy, uz = b[0] - a[0], b[1] - a[1], b[2] - a[2]
    vx, vy, vz = c[0] - a[0], c[1] - a[1], c[2] - a[2]
    nx, ny, nz = uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx
    return 0.5 * (nx * nx + ny * ny + nz * nz) ** 0.5
