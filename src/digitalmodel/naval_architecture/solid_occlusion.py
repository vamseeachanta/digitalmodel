#!/usr/bin/env python3
"""
ABOUTME: Inside/outside queries against a closed triangle soup, and the
occlusion-aware wetted-area accounting that multi-region hull ingestion
(#2023) needs so it does not report a surface area that does not exist.

WHY THIS EXISTS
---------------
A hull with appendages is NOT one surface. A rudder and a propeller boss are
modelled as their own closed bodies and they INTERPENETRATE the hull; merging
the three triangle soups produces hundreds of non-manifold edges, so they are
carried to the mesher as separate closed regions and snappyHexMesh forms the
union implicitly from its own per-surface inside/outside tests.

That leaves an accounting problem the mesher solves and the manifest does not.
The union's external area is NOT the sum of the parts:

    * the part of an appendage that lies INSIDE the hull is not wetted, and
    * the part of the hull that lies INSIDE an appendage is not wetted either.

Summing per-region wetted areas therefore OVERSTATES the union. That matters
because ``Aref`` in ``forceCoeffs`` is a wetted area: an inflated Aref deflates
every reported coefficient by exactly its own error, silently, in a case that
converges perfectly.

WHAT IS COMPUTED, AND WHAT IS ONLY BOUNDED
------------------------------------------
The exact external area of a union of interpenetrating solids needs a surface
intersection, which is the same boolean this lane refuses to run. What IS
affordable is a classification of the existing triangles:

    external   below the waterline and outside every other region
    occluded   below the waterline and inside some other region
    undecided  a sliver that straddles another region's boundary at the finest
               subdivision this routine went to

``external`` is the estimate, ``undecided`` is its error bar, and both are
reported. A quantity whose error is stated is usable; a sum that is quietly
wrong is not.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Sequence, Tuple

from digitalmodel.naval_architecture.kcs_geometry import wetted_surface_area

__all__ = [
    "AreaSplit",
    "SolidIndex",
    "classify_wetted_area",
]

Vec3 = Tuple[float, float, float]
Tri = Tuple[Vec3, Vec3, Vec3]

#: Barycentric distance from an edge below which a ray hit is called ambiguous
#: and the query is retried from a jittered origin. A ray that grazes an edge
#: is counted once by one of the two triangles sharing it, twice, or not at
#: all, and the parity answer flips. Retrying is cheap; guessing is not.
_EDGE_TOLERANCE = 1e-9

#: Jittered retries before a containment query returns its best parity answer
#: anyway. Three independent jitters all landing on an edge is not a geometry
#: this routine can be trusted on.
_MAX_RETRIES = 4

#: Subdivision depth for a triangle that straddles another region's boundary.
#: Each level quarters the straddling area, so depth 3 bounds the undecided
#: residue at roughly 1/8 of the triangles that touch a boundary at all.
DEFAULT_SUBDIVISION_DEPTH = 3

#: A triangle longer than this fraction of the smallest feature it could
#: intersect is subdivided WITHOUT consulting its corners. Sampling four points
#: on a triangle far larger than the body it overlaps is how a small inclusion
#: disappears: all four land outside, the shortcut declares the whole triangle
#: external, and the hole under the boss is never subtracted. Measured on the
#: synthetic fixture, the shortcut alone reported 0.0 m2 occluded where the
#: closed-form answer is 2.0.
SAMPLE_SAFE_FRACTION = 0.5

#: How many extra levels a COARSE triangle may subdivide through before the
#: four-point vote is trusted anyway. Separate from ``max_depth``, which buys
#: accuracy once the triangle is already small enough to sample: this one buys
#: the right to sample at all, and a triangle that reaches the cap still coarse
#: has its whole area reported as undecided rather than quietly assigned. Six
#: levels shrink an edge 64-fold, which takes a 6 m hull panel well below the
#: half-metre feature size of a propeller boss.
MAX_COARSE_DEPTH = 6


# --------------------------------------------------------------------------- #
#  Point-in-solid
# --------------------------------------------------------------------------- #

class SolidIndex:
    """Containment queries against ONE closed triangle soup.

    A ray is cast along +x and its crossings counted; odd means inside. The
    triangles are bucketed by their (y, z) footprint so a query examines the
    handful of triangles the ray can possibly meet rather than all of them --
    without that, classifying 8,000 appendage triangles against a 32,000
    triangle hull is a quarter of a billion ray-triangle tests.

    The soup MUST be closed. Parity is meaningless on an open surface, and
    this class does not check: the caller has already gated on closure, and
    re-deriving that here would hide which stage failed.
    """

    def __init__(self, triangles: Sequence[Tri], *, resolution: int = 48) -> None:
        self._tris: Tuple[Tri, ...] = tuple(triangles)
        if not self._tris:
            raise ValueError("SolidIndex needs at least one triangle")
        lo, hi = _bounds(self._tris)
        self.bbox_min: Vec3 = lo
        self.bbox_max: Vec3 = hi
        span_y = max(hi[1] - lo[1], 1e-12)
        span_z = max(hi[2] - lo[2], 1e-12)
        self._n = max(1, int(resolution))
        self._y0, self._z0 = lo[1], lo[2]
        self._dy = span_y / self._n
        self._dz = span_z / self._n
        self._grid: Dict[Tuple[int, int], List[int]] = {}
        for i, tri in enumerate(self._tris):
            ys = [p[1] for p in tri]
            zs = [p[2] for p in tri]
            for gy in range(self._gy(min(ys)), self._gy(max(ys)) + 1):
                for gz in range(self._gz(min(zs)), self._gz(max(zs)) + 1):
                    self._grid.setdefault((gy, gz), []).append(i)

    def _gy(self, y: float) -> int:
        return min(self._n - 1, max(0, int((y - self._y0) / self._dy)))

    def _gz(self, z: float) -> int:
        return min(self._n - 1, max(0, int((z - self._z0) / self._dz)))

    def bbox_contains(self, point: Vec3, *, pad: float = 0.0) -> bool:
        """Cheap reject. A point outside the box is outside the solid."""
        return all(
            self.bbox_min[k] - pad <= point[k] <= self.bbox_max[k] + pad
            for k in range(3)
        )

    def contains(self, point: Vec3) -> bool:
        """Is ``point`` strictly inside this closed surface?"""
        if not self.bbox_contains(point):
            return False
        jitter = max(self._dy, self._dz) * 1e-6
        py, pz = point[1], point[2]
        parity = False
        for attempt in range(_MAX_RETRIES):
            offset = 0.0 if attempt == 0 else jitter * attempt
            qy = py + offset
            qz = pz - offset * 0.6180339887
            crossings, ambiguous = self._cast(point[0], qy, qz)
            parity = crossings % 2 == 1
            if not ambiguous:
                return parity
        return parity

    def _cast(self, px: float, py: float, pz: float) -> Tuple[int, bool]:
        """Crossings of the +x ray from ``(px, py, pz)``, and whether any hit
        grazed a triangle edge closely enough to make the parity unreliable."""
        bucket = self._grid.get((self._gy(py), self._gz(pz)))
        if not bucket:
            return 0, False
        crossings = 0
        ambiguous = False
        for i in bucket:
            a, b, c = self._tris[i]
            hit, x_hit, grazing = _ray_x_hit(a, b, c, py, pz)
            if not hit:
                continue
            if grazing:
                ambiguous = True
            if x_hit > px:
                crossings += 1
        return crossings, ambiguous


def _ray_x_hit(
    a: Vec3, b: Vec3, c: Vec3, py: float, pz: float
) -> Tuple[bool, float, bool]:
    """Where the +x line through ``(py, pz)`` meets triangle ``abc``.

    Solved in the (y, z) projection, which turns the intersection into a plain
    barycentric test and makes the degenerate case -- a triangle seen edge-on,
    zero projected area -- explicit rather than a division by almost zero.
    """
    y1, z1 = b[1] - a[1], b[2] - a[2]
    y2, z2 = c[1] - a[1], c[2] - a[2]
    det = y1 * z2 - y2 * z1
    if abs(det) < 1e-18:
        return False, 0.0, False
    ry, rz = py - a[1], pz - a[2]
    u = (ry * z2 - y2 * rz) / det
    v = (y1 * rz - ry * z1) / det
    w = 1.0 - u - v
    if u < 0.0 or v < 0.0 or w < 0.0:
        return False, 0.0, False
    grazing = min(u, v, w) < _EDGE_TOLERANCE
    return True, w * a[0] + u * b[0] + v * c[0], grazing


def _bounds(tris: Sequence[Tri]) -> Tuple[Vec3, Vec3]:
    xs = [p[0] for t in tris for p in t]
    ys = [p[1] for t in tris for p in t]
    zs = [p[2] for t in tris for p in t]
    return (min(xs), min(ys), min(zs)), (max(xs), max(ys), max(zs))


# --------------------------------------------------------------------------- #
#  Occlusion-aware wetted area
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class AreaSplit:
    """One region's wetted area, split by whether it is on the union's skin."""

    total_m2: float
    external_m2: float
    occluded_m2: float
    undecided_m2: float

    @property
    def occluded_fraction(self) -> float:
        return self.occluded_m2 / self.total_m2 if self.total_m2 > 0 else 0.0

    def to_dict(self) -> Dict[str, float]:
        return {
            "wetted_area_m2": self.total_m2,
            "wetted_area_external_m2": self.external_m2,
            "wetted_area_occluded_m2": self.occluded_m2,
            "wetted_area_undecided_m2": self.undecided_m2,
        }


def classify_wetted_area(
    triangles: Sequence[Tri],
    waterline_z: float,
    others: Sequence[SolidIndex],
    *,
    max_depth: int = DEFAULT_SUBDIVISION_DEPTH,
) -> AreaSplit:
    """Split one region's wetted area by containment in the OTHER regions.

    ``others`` must exclude this region's own solid: every point of a surface
    lies on its own boundary, where a parity test is a coin toss.

    With no other regions the answer is the plain wetted area and no work is
    done -- the single-region case must cost nothing, because it is the case
    that was already correct.
    """
    total = wetted_surface_area(triangles, waterline_z)
    if not others:
        return AreaSplit(total, total, 0.0, 0.0)

    feature = _feature_size(others)
    external = occluded = undecided = 0.0
    for tri in triangles:
        wet = wetted_surface_area((tri,), waterline_z)
        if wet <= 0.0:
            continue
        if not _may_touch(tri, others):
            external += wet
            continue
        ext, occ, und = _split_triangle(
            tri, waterline_z, others, max_depth, feature
        )
        external += ext
        occluded += occ
        undecided += und
    return AreaSplit(total, external, occluded, undecided)


def _feature_size(others: Sequence[SolidIndex]) -> float:
    """The smallest thing a triangle could be hiding inside itself.

    The narrowest bounding-box extent of the narrowest other region: a
    triangle appreciably larger than that can contain the whole of it between
    its sample points.
    """
    extents = [
        solid.bbox_max[k] - solid.bbox_min[k]
        for solid in others
        for k in range(3)
    ]
    return min((e for e in extents if e > 0.0), default=float("inf"))


def _longest_edge(tri: Tri) -> float:
    return max(
        max(abs(tri[i][k] - tri[(i + 1) % 3][k]) for k in range(3))
        for i in range(3)
    )


def _may_touch(tri: Tri, others: Sequence[SolidIndex]) -> bool:
    """Bounding-box prefilter. Most of a hull is nowhere near a rudder."""
    lo = tuple(min(p[k] for p in tri) for k in range(3))
    hi = tuple(max(p[k] for p in tri) for k in range(3))
    for solid in others:
        if all(
            lo[k] <= solid.bbox_max[k] and hi[k] >= solid.bbox_min[k]
            for k in range(3)
        ):
            return True
    return False


def _inside_any(point: Vec3, others: Sequence[SolidIndex]) -> bool:
    return any(solid.contains(point) for solid in others)


def _split_triangle(
    tri: Tri,
    waterline_z: float,
    others: Sequence[SolidIndex],
    depth: int,
    feature: float,
    coarse_budget: int = MAX_COARSE_DEPTH,
) -> Tuple[float, float, float]:
    """``(external, occluded, undecided)`` area below the waterline for ``tri``.

    The three corners and the centroid are tested; agreement decides the whole
    triangle, disagreement subdivides it into four. At depth zero a still-mixed
    triangle is assigned by its centroid AND counted as undecided, so the
    reported area carries its own error bar rather than an assumption.

    The four-point vote is only trusted once the triangle is SMALL relative to
    the bodies it could be overlapping. Above that size it is subdivided first
    whatever the vote says -- otherwise a hull panel spanning several metres
    votes "all outside" over a boss half a metre across sitting in the middle
    of it, and the occluded patch is never found. Those levels come out of
    ``coarse_budget``, not out of ``depth``, so asking for more accuracy is not
    also silently asking to sample a panel four times and hope.
    """
    wet = wetted_surface_area((tri,), waterline_z)
    if wet <= 0.0:
        return 0.0, 0.0, 0.0

    centroid = _centroid(tri)
    if _longest_edge(tri) > SAMPLE_SAFE_FRACTION * feature:
        if coarse_budget <= 0:
            return (
                (0.0, wet, wet)
                if _inside_any(centroid, others)
                else (wet, 0.0, wet)
            )
        return _recurse(
            tri, waterline_z, others, depth, feature, coarse_budget - 1
        )

    flags = [_inside_any(p, others) for p in (*tri, centroid)]
    if not any(flags):
        return wet, 0.0, 0.0
    if all(flags):
        return 0.0, wet, 0.0
    if depth <= 0:
        return (0.0, wet, wet) if flags[3] else (wet, 0.0, wet)
    return _recurse(tri, waterline_z, others, depth - 1, feature, coarse_budget)


def _recurse(
    tri: Tri,
    waterline_z: float,
    others: Sequence[SolidIndex],
    depth: int,
    feature: float,
    coarse_budget: int,
) -> Tuple[float, float, float]:
    ext = occ = und = 0.0
    for sub in _subdivide(tri):
        e, o, u = _split_triangle(
            sub, waterline_z, others, depth, feature, coarse_budget
        )
        ext += e
        occ += o
        und += u
    return ext, occ, und


def _centroid(tri: Tri) -> Vec3:
    a, b, c = tri
    return (
        (a[0] + b[0] + c[0]) / 3.0,
        (a[1] + b[1] + c[1]) / 3.0,
        (a[2] + b[2] + c[2]) / 3.0,
    )


def _subdivide(tri: Tri) -> Tuple[Tri, Tri, Tri, Tri]:
    a, b, c = tri
    ab = _mid(a, b)
    bc = _mid(b, c)
    ca = _mid(c, a)
    return ((a, ab, ca), (ab, b, bc), (ca, bc, c), (ab, bc, ca))


def _mid(p: Vec3, q: Vec3) -> Vec3:
    return ((p[0] + q[0]) / 2.0, (p[1] + q[1]) / 2.0, (p[2] + q[2]) / 2.0)
