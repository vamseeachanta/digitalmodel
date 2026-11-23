#!/usr/bin/env python3
"""
ABOUTME: Verification that a body straddling the centreplane is cut CLEANLY by
it (#2023) -- the check that stands between a half-domain hull-plus-appendage
case and a leaking or doubled mesh.

THE HAZARD
----------
The resistance case is a half domain: the background mesh stops at y = 0 and
that face is a symmetry plane. A hull symmetric about the centreplane is cut
there by the background mesh itself -- snappyHexMesh never sees the starboard
half, because there are no cells for it to intersect.

Appendages break the easy version of that argument. A rudder and a propeller
boss STRADDLE the plane. Each is its own closed body, and snappy decides which
cells to remove from a per-surface inside/outside test. That test is meaningful
only if the body is closed, if the half kept by the domain is a well-defined
solid lidded by the symmetry face, and if ``locationInMesh`` is outside every
body -- otherwise the mesher keeps the inside of a rudder and deletes the water.

WHY THIS IS A VOLUME TEST AND NOT A LOOP TEST
---------------------------------------------
The obvious check is to intersect the surface with the plane and demand closed
loops. It is the wrong check, and measurably so.

A rudder is a symmetric foil: its leading and trailing edges lie EXACTLY on the
centreplane, so the plane is TANGENT to the surface there and no triangle
crosses it. Same for a hull at its stem, keel and sternpost. The section curve
is real but part of it has no crossings to find, and a loop test reports loose
ends on geometry that is perfectly sound -- measured here on the client hull:
6 phantom loose ends on a surface with 0 open and 0 non-manifold edges.

What is asked instead is the question that actually matters, and it is exact.
Clip the closed surface to ``y <= 0``. The half-body is that clipped surface
plus a flat lid at y = 0. Now apply the divergence theorem THREE ways -- with
the fields ``(x,0,0)``, ``(0,y,0)`` and ``(0,0,z)``. Every one of them gives
the enclosed volume, and every one of them integrates to ZERO over the lid
(the first and third because the lid's normal has no x or z component, the
second because y = 0 on it). So all three can be evaluated on the clipped
triangles ALONE, without ever constructing the lid, and all three must agree.

They agree only if the clipped surface really is closed by the plane. A hole
anywhere makes them disagree, because each field weights the missing patch
differently. Tangency contributes nothing to any of them, which is exactly
right: a tangent contact encloses no volume.

The lid's AREA falls out of the same argument with the field ``(0,1,0)``: it
integrates to zero over any closed surface, so the lid area is minus the
y-projected area of the clipped triangles.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Sequence, Tuple

__all__ = [
    "HalfBody",
    "PlaneSection",
    "SymmetryCutReport",
    "check_symmetry_cut",
    "half_body",
    "plane_section",
]

Vec3 = Tuple[float, float, float]
Tri = Tuple[Vec3, Vec3, Vec3]

#: Points within this of the plane are ON it. Section points are found by
#: linear interpolation along shared edges, so the two triangles either side of
#: an edge produce the same point only to within rounding.
DEFAULT_TOLERANCE_M = 1e-9

#: How far the three divergence-theorem volumes may disagree, relative to the
#: volume itself, before the clip is called unclosed. Set well above float
#: noise on a 30,000 m3 hull and far below any real hole: a missing patch of
#: 1 m2 on a rudder moves this by percent, not by parts per billion.
VOLUME_CONSISTENCY_TOLERANCE = 1e-6


@dataclass(frozen=True)
class HalfBody:
    """One side of a closed body, as cut by a plane and lidded by it."""

    volume_x_m3: float
    volume_y_m3: float
    volume_z_m3: float
    lid_area_m2: float
    n_triangles: int

    @property
    def volume_m3(self) -> float:
        return (self.volume_x_m3 + self.volume_y_m3 + self.volume_z_m3) / 3.0

    @property
    def consistency(self) -> float:
        """Relative spread of the three volumes. Zero for a clean cut."""
        volumes = (self.volume_x_m3, self.volume_y_m3, self.volume_z_m3)
        scale = max(abs(self.volume_m3), 1e-12)
        return (max(volumes) - min(volumes)) / scale

    def to_dict(self) -> Dict[str, float]:
        return {
            "volume_m3": self.volume_m3,
            "volume_consistency": self.consistency,
            "lid_area_m2": self.lid_area_m2,
            "n_clipped_triangles": self.n_triangles,
        }


def half_body(
    triangles: Sequence[Tri],
    value: float = 0.0,
    *,
    axis: int = 1,
    keep_below: bool = True,
) -> HalfBody:
    """Clip to one side of the plane and measure the solid that remains."""
    vx = vy = vz = area = 0.0
    count = 0
    for tri in triangles:
        for piece in _clip(tri, axis, value, keep_below):
            count += 1
            vx += _flux(piece, 0)
            vy += _flux(piece, 1)
            vz += _flux(piece, 2)
            area += _projected(piece, axis)
    sign = -1.0 if keep_below else 1.0
    return HalfBody(
        volume_x_m3=vx,
        volume_y_m3=vy,
        volume_z_m3=vz,
        lid_area_m2=sign * area,
        n_triangles=count,
    )


def _clip(tri: Tri, axis: int, value: float, keep_below: bool) -> List[Tri]:
    """The part of ``tri`` on the kept side, fan-triangulated.

    Degenerate output is fine and is not filtered: a triangle merely TOUCHING
    the plane clips to a sliver of zero area, and every integral below weights
    it by that area. Filtering would only move the arithmetic.
    """
    sign = 1.0 if keep_below else -1.0
    d = [sign * (p[axis] - value) for p in tri]
    if all(v <= 0.0 for v in d):
        return [tri]
    if all(v >= 0.0 for v in d):
        return []
    poly: List[Vec3] = []
    for i in range(3):
        a, b = tri[i], tri[(i + 1) % 3]
        da, db = d[i], d[(i + 1) % 3]
        if da <= 0.0:
            poly.append(a)
        if (da <= 0.0) != (db <= 0.0):
            t = da / (da - db)
            poly.append(
                tuple(a[k] + t * (b[k] - a[k]) for k in range(3))  # type: ignore[misc]
            )
    return [
        (poly[0], poly[i], poly[i + 1]) for i in range(1, len(poly) - 1)
    ]


def _cross(tri: Tri) -> Vec3:
    """Twice the area vector: the un-normalised outward normal."""
    a, b, c = tri
    u = (b[0] - a[0], b[1] - a[1], b[2] - a[2])
    v = (c[0] - a[0], c[1] - a[1], c[2] - a[2])
    return (
        u[1] * v[2] - u[2] * v[1],
        u[2] * v[0] - u[0] * v[2],
        u[0] * v[1] - u[1] * v[0],
    )


def _flux(tri: Tri, k: int) -> float:
    """``integral of x_k n_k dA``, which is the enclosed volume when summed."""
    mean = (tri[0][k] + tri[1][k] + tri[2][k]) / 3.0
    return mean * _cross(tri)[k] / 2.0


def _projected(tri: Tri, k: int) -> float:
    """Signed area projected onto the plane's normal direction."""
    return _cross(tri)[k] / 2.0


# --------------------------------------------------------------------------- #
#  The section, reported but not used as the verdict
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class PlaneSection:
    """One closed body measured against one plane."""

    axis: int
    value: float
    straddles: bool
    total_volume_m3: float
    kept: HalfBody
    discarded: HalfBody

    @property
    def closure_error(self) -> float:
        """How much volume the two halves lose or gain against the whole."""
        scale = max(abs(self.total_volume_m3), 1e-12)
        return abs(
            self.kept.volume_m3 + self.discarded.volume_m3
            - self.total_volume_m3
        ) / scale

    @property
    def symmetry_error(self) -> float:
        """Relative difference between the halves. ~0 means truly symmetric."""
        scale = max(abs(self.total_volume_m3), 1e-12)
        return abs(self.kept.volume_m3 - self.discarded.volume_m3) / scale

    @property
    def clean(self) -> bool:
        """The plane cuts this body into two well-defined lidded solids."""
        return (
            self.kept.consistency <= VOLUME_CONSISTENCY_TOLERANCE
            and self.discarded.consistency <= VOLUME_CONSISTENCY_TOLERANCE
            and self.closure_error <= VOLUME_CONSISTENCY_TOLERANCE
        )

    def to_dict(self) -> Dict[str, object]:
        return {
            "axis": "xyz"[self.axis],
            "value": self.value,
            "straddles": self.straddles,
            "cut_is_clean": self.clean,
            "total_volume_m3": self.total_volume_m3,
            "kept_half": self.kept.to_dict(),
            "discarded_half": self.discarded.to_dict(),
            "section_area_m2": self.kept.lid_area_m2,
            "volume_closure_error": self.closure_error,
            "volume_symmetry_error": self.symmetry_error,
        }


def plane_section(
    triangles: Sequence[Tri],
    value: float = 0.0,
    *,
    axis: int = 1,
    tolerance: float = DEFAULT_TOLERANCE_M,
) -> PlaneSection:
    """Measure how the plane ``axis = value`` divides a closed body."""
    lo = min(p[axis] for tri in triangles for p in tri)
    hi = max(p[axis] for tri in triangles for p in tri)
    total = sum(_flux(tri, axis) for tri in triangles)
    return PlaneSection(
        axis=axis,
        value=value,
        straddles=lo < value - tolerance and hi > value + tolerance,
        total_volume_m3=total,
        kept=half_body(triangles, value, axis=axis, keep_below=True),
        discarded=half_body(triangles, value, axis=axis, keep_below=False),
    )


# --------------------------------------------------------------------------- #
#  The whole-case verdict
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class SymmetryCutReport:
    """Per-region verdict on the centreplane cut, plus the keep-point check."""

    sections: Dict[str, PlaneSection]
    location_in_mesh: Tuple[float, float, float]
    location_outside_all: bool
    location_inside: Tuple[str, ...]

    @property
    def ok(self) -> bool:
        return self.location_outside_all and all(
            section.clean for section in self.sections.values()
        )

    def failures(self) -> List[str]:
        out = [
            f"{name} is not cut cleanly at the centreplane: the three "
            f"divergence-theorem volumes of the kept half disagree by "
            f"{section.kept.consistency:.3e} and the two halves miss the whole "
            f"by {section.closure_error:.3e} (tolerance "
            f"{VOLUME_CONSISTENCY_TOLERANCE:g}); the surface has a hole at or "
            "near the plane and snappyHexMesh cannot tell its inside from its "
            "outside there"
            for name, section in sorted(self.sections.items())
            if not section.clean
        ]
        if not self.location_outside_all:
            out.append(
                f"locationInMesh {self.location_in_mesh} is INSIDE "
                f"{list(self.location_inside)}; snappyHexMesh would keep the "
                "inside of that body and delete the fluid"
            )
        return out

    def to_dict(self) -> Dict[str, object]:
        return {
            "centreplane_cut_ok": self.ok,
            "location_in_mesh": list(self.location_in_mesh),
            "location_in_mesh_outside_all_regions": self.location_outside_all,
            "regions": {
                name: section.to_dict()
                for name, section in sorted(self.sections.items())
            },
            "failures": self.failures(),
        }


def check_symmetry_cut(
    regions: Sequence[Tuple[str, Sequence[Tri]]],
    *,
    location_in_mesh: Tuple[float, float, float],
    centreplane_y: float = 0.0,
    tolerance: float = DEFAULT_TOLERANCE_M,
) -> SymmetryCutReport:
    """Both halves of the half-domain premise, measured on real triangles."""
    # Imported lazily so the section arithmetic stays usable without the
    # containment index, which is the expensive half of this module's imports.
    from digitalmodel.naval_architecture.solid_occlusion import (  # noqa: PLC0415
        SolidIndex,
    )

    sections = {
        name: plane_section(tris, centreplane_y, axis=1, tolerance=tolerance)
        for name, tris in regions
    }
    inside = tuple(
        name
        for name, tris in regions
        if SolidIndex(tris).contains(location_in_mesh)
    )
    return SymmetryCutReport(
        sections=sections,
        location_in_mesh=tuple(location_in_mesh),  # type: ignore[arg-type]
        location_outside_all=not inside,
        location_inside=inside,
    )
