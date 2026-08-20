"""
ABOUTME: Open-boundary capping for client hull surfaces (#2023). Finds the
boundary loops of an open triangle soup, verifies each is a genuine cycle, and
closes it with a triangulation that is checked rather than assumed -- but only
above the waterline unless the caller explicitly says otherwise.

WHY THIS EXISTS
---------------
Client hull surfaces routinely arrive as OPEN shells: the moulded surface stops
at deck level and there is no lid. ``snappyHexMesh`` cannot tell inside from
outside through such an opening, so it leaks into the hull interior and the
solver returns a confident wrong answer. ``hull_ingest`` therefore refuses an
open surface -- which is right, and which also means a perfectly ordinary
client hull cannot be ingested at all.

Capping closes the opening. It is also the most dangerous repair this stage can
make, so it is bounded by three rules, each of which exists because breaking it
produces a wrong number rather than an error:

1. AN OPEN BOUNDARY IS ONLY CAPPED IF IT IS A CYCLE.
   Edges used by exactly one triangle are assembled into components, and every
   vertex of a component must have degree exactly two. A component that is not
   a simple cycle means the surface is TORN -- a hole meeting another hole at a
   point, a missing patch mid-surface -- and capping it would hide a modelling
   defect behind a watertight verdict. That case raises.

2. A CAP IS ONLY PLACED ABOVE THE WATERLINE.
   A lid below the waterline adds area to the reported wetted surface and
   volume to the reported displacement: it silently alters the two quantities
   the analysis exists to produce. A deck lid ABOVE the waterline contributes
   nothing to either, so it is free. A loop that dips below the waterline
   raises, naming its z range, and needs an explicit opt-in to proceed.

3. THE TRIANGULATION IS VERIFIED, NOT ASSUMED.
   See ``triangulate_loop`` for why a centroid fan is not good enough.

WHY EAR CLIPPING IN THE DOMINANT PLANE, AND NOT A CENTROID FAN
--------------------------------------------------------------
The obvious cap -- a fan from the loop's centroid to each boundary edge -- is
correct only while the loop is CONVEX in plan. It is not, on real hulls: a
transom notch, a stern cutout or a well deck makes the plan-view outline
concave, and then fan triangles reach across the concavity and cover ground
that is outside the polygon. The result still closes every open edge, so the
closure check passes, while the lid folds through itself and encloses the wrong
region. That is precisely the class of failure this whole ingestion stage is
built to refuse.

Ear clipping partitions the polygon exactly: every point of the region is
covered once and no triangle leaves it. The proof available at runtime is
cheap and is asserted by the tests -- the cap's area equals the polygon's plan
area, which a self-intersecting fan cannot manage.

Ear clipping is a 2D method, and the loops here are NOT planar (the motivating
hull's deck edge spans half a metre of z over 150 m of ship). So the loop is
projected onto the plane it is most nearly parallel to -- the one normal to the
dominant component of its own Newell normal -- and clipped there. The resulting
triangles are then lifted back to the original 3D vertices, so the cap is a
ruled, generally non-planar surface whose EDGES are exactly the loop's edges.
Closure is therefore topological and exact: it does not depend on the loop
being flat, and no merge tolerance is involved anywhere.

The projection is legitimate only while it is injective, i.e. while the loop
does not overlap itself in plan view. That is checked, not assumed: the
projected polygon is tested for self-intersection before any clipping happens,
and a loop that fails is refused rather than capped with a folded lid.

Winding is deliberately NOT decided here. The cap triangles are consistently
wound among themselves; the caller closes the surface and then runs
``kcs_geometry.orient_consistently``, which propagates one orientation across
the whole closed manifold and fixes the global sense from the enclosed volume.
Guessing an outward direction for an arbitrary opening would be a heuristic
where an exact method already exists downstream.
"""

from __future__ import annotations

import math
from collections import defaultdict
from dataclasses import dataclass, field
from typing import Dict, List, Sequence, Tuple

from digitalmodel.naval_architecture.hull_ingest import HullIngestError

Vec3 = Tuple[float, float, float]
Vec2 = Tuple[float, float]
Tri = Tuple[Vec3, Vec3, Vec3]

__all__ = [
    "DEFAULT_QUANTISE",
    "BoundaryLoop",
    "CapBelowWaterlineError",
    "CapResult",
    "CapTriangulationError",
    "TornBoundaryError",
    "cap_boundary_loops",
    "find_boundary_loops",
    "triangulate_loop",
]

#: Decimal places vertices are quantised to when edges are matched. Kept equal
#: to ``kcs_geometry.check_surface``'s default on purpose: a capper that
#: identified vertices on a different rule from the checker could close every
#: loop it found and still leave the checker reporting open edges.
DEFAULT_QUANTISE = 9


# --------------------------------------------------------------------------- #
#  Errors - each names the action that resolves it
# --------------------------------------------------------------------------- #

class TornBoundaryError(HullIngestError):
    """An open boundary is not a closed loop: the surface is torn, not open."""


class CapBelowWaterlineError(HullIngestError):
    """A boundary loop reaches the waterline or below it."""


class CapTriangulationError(HullIngestError):
    """A boundary loop could not be triangulated into a valid cap."""


# --------------------------------------------------------------------------- #
#  Small geometry helpers (3D area is the only one duplicated from elsewhere,
#  and only because importing it would drag a module cycle behind it)
# --------------------------------------------------------------------------- #

def _area3(a: Vec3, b: Vec3, c: Vec3) -> float:
    ux, uy, uz = b[0] - a[0], b[1] - a[1], b[2] - a[2]
    vx, vy, vz = c[0] - a[0], c[1] - a[1], c[2] - a[2]
    nx, ny, nz = uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx
    return 0.5 * math.sqrt(nx * nx + ny * ny + nz * nz)


def _cross2(o: Vec2, a: Vec2, b: Vec2) -> float:
    return (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])


# --------------------------------------------------------------------------- #
#  Boundary loops
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class BoundaryLoop:
    """One closed cycle of edges used by exactly one triangle each.

    ``vertices`` is the cycle in walk order and does NOT repeat the first
    vertex at the end; the closing edge is implicit.
    """

    vertices: List[Vec3]

    @property
    def n_vertices(self) -> int:
        return len(self.vertices)

    @property
    def z_min(self) -> float:
        return min(p[2] for p in self.vertices)

    @property
    def z_max(self) -> float:
        return max(p[2] for p in self.vertices)

    @property
    def bbox(self) -> Tuple[Vec3, Vec3]:
        xs = [p[0] for p in self.vertices]
        ys = [p[1] for p in self.vertices]
        zs = [p[2] for p in self.vertices]
        return (min(xs), min(ys), min(zs)), (max(xs), max(ys), max(zs))

    def describe(self) -> str:
        lo, hi = self.bbox
        return (
            f"{self.n_vertices} vertices, z {self.z_min:g}..{self.z_max:g} m, "
            f"plan extent {hi[0] - lo[0]:g} x {hi[1] - lo[1]:g} m"
        )


def find_boundary_loops(
    tris: Sequence[Tri], *, quantise: int = DEFAULT_QUANTISE
) -> List[BoundaryLoop]:
    """Assemble the singly-used edges of ``tris`` into closed loops.

    Raises ``TornBoundaryError`` when any boundary component is not a simple
    cycle. That check is the whole reason this is not three lines of edge
    walking: an open surface and a torn one look identical to an edge counter,
    and only one of them may be capped.

    Degenerate triangles are skipped exactly as ``check_surface`` skips them,
    so the two agree on which edges are open.
    """
    verts: Dict[Tuple[float, ...], int] = {}
    coords: List[Vec3] = []

    def vid(p: Vec3) -> int:
        key = tuple(round(c, quantise) + 0.0 for c in p)
        got = verts.get(key)
        if got is None:
            got = len(coords)
            verts[key] = got
            coords.append(p)
        return got

    counts: Dict[Tuple[int, int], int] = defaultdict(int)
    for tri in tris:
        if _area3(*tri) <= 0.0:
            continue
        ids = [vid(p) for p in tri]
        for a, b in ((ids[0], ids[1]), (ids[1], ids[2]), (ids[2], ids[0])):
            counts[(a, b) if a < b else (b, a)] += 1

    open_edges = [edge for edge, n in counts.items() if n == 1]
    if not open_edges:
        return []

    adjacency: Dict[int, List[int]] = defaultdict(list)
    for a, b in open_edges:
        adjacency[a].append(b)
        adjacency[b].append(a)

    bad = {v: len(nbrs) for v, nbrs in adjacency.items() if len(nbrs) != 2}
    if bad:
        worst = sorted(bad.items(), key=lambda item: -item[1])[:5]
        detail = "; ".join(
            f"({coords[v][0]:g}, {coords[v][1]:g}, {coords[v][2]:g}) has "
            f"degree {degree}"
            for v, degree in worst
        )
        raise TornBoundaryError(
            f"the open boundary of this surface is not a set of closed loops: "
            f"{len(bad)} of {len(adjacency)} boundary vertices have a degree "
            f"other than 2 ({detail}).\n"
            "A boundary vertex of degree 1 means a dangling edge and a degree "
            "of 4 or more means two holes meeting at a point, so the surface "
            "is TORN rather than merely open. Capping it would close the hole "
            "and hide the tear behind a watertight verdict, so this stage "
            "stops here.\n"
            "Options: repair the surface in CAD; raise weld_tolerance= if the "
            "tear is numerical rather than real; or ingest with force=True to "
            "emit the STL with watertight=false recorded."
        )

    loops: List[BoundaryLoop] = []
    seen: set = set()
    for start in adjacency:
        if start in seen:
            continue
        cycle = [start]
        seen.add(start)
        previous, current = None, start
        while True:
            nxt = next(
                (v for v in adjacency[current] if v != previous), None
            )
            # Degree is exactly 2 everywhere, so the walk closes; the guard is
            # against a doubled edge, which would otherwise loop forever.
            if nxt is None or nxt == start:
                break
            if nxt in seen:  # pragma: no cover - excluded by the degree check
                raise TornBoundaryError(
                    "boundary walk revisited a vertex before closing its "
                    "loop; the boundary is not a set of simple cycles"
                )
            cycle.append(nxt)
            seen.add(nxt)
            previous, current = current, nxt
        if len(cycle) < 3:
            raise TornBoundaryError(
                f"a boundary component holds only {len(cycle)} vertices, which "
                "cannot bound an area; the surface is torn, not open"
            )
        loops.append(BoundaryLoop(vertices=[coords[v] for v in cycle]))

    loops.sort(key=lambda loop: (-loop.n_vertices, loop.z_min))
    return loops


# --------------------------------------------------------------------------- #
#  Triangulating one loop
# --------------------------------------------------------------------------- #

def _newell_normal(points: Sequence[Vec3]) -> Vec3:
    """Area-weighted normal of a (possibly non-planar) closed polygon.

    Newell's method rather than a three-point cross product: three consecutive
    vertices of a deck edge are very nearly collinear, so a local cross product
    is dominated by round-off, whereas Newell's sum is an integral over the
    whole loop and is stable.
    """
    nx = ny = nz = 0.0
    n = len(points)
    for i in range(n):
        a, b = points[i], points[(i + 1) % n]
        nx += (a[1] - b[1]) * (a[2] + b[2])
        ny += (a[2] - b[2]) * (a[0] + b[0])
        nz += (a[0] - b[0]) * (a[1] + b[1])
    return (0.5 * nx, 0.5 * ny, 0.5 * nz)


def _segments_cross(p1: Vec2, p2: Vec2, p3: Vec2, p4: Vec2) -> bool:
    """True if segments p1p2 and p3p4 intersect at all, touching included."""
    d1 = _cross2(p3, p4, p1)
    d2 = _cross2(p3, p4, p2)
    d3 = _cross2(p1, p2, p3)
    d4 = _cross2(p1, p2, p4)
    if ((d1 > 0.0) != (d2 > 0.0)) and ((d3 > 0.0) != (d4 > 0.0)):
        if d1 != 0.0 and d2 != 0.0 and d3 != 0.0 and d4 != 0.0:
            return True

    def on(a: Vec2, b: Vec2, p: Vec2) -> bool:
        return (
            _cross2(a, b, p) == 0.0
            and min(a[0], b[0]) <= p[0] <= max(a[0], b[0])
            and min(a[1], b[1]) <= p[1] <= max(a[1], b[1])
        )

    return on(p3, p4, p1) or on(p3, p4, p2) or on(p1, p2, p3) or on(p1, p2, p4)


def _is_simple(poly: Sequence[Vec2]) -> bool:
    """Whether a closed polygon has no self-intersection.

    O(n^2) in the number of vertices. Boundary loops are the rim of an opening,
    which is hundreds of vertices at most on a hull tessellated finely enough
    to solve, so the quadratic cost is a few milliseconds and buys an exact
    answer. A sweep line would be faster and much easier to get subtly wrong.
    """
    n = len(poly)
    if len({(round(p[0], 12), round(p[1], 12)) for p in poly}) != n:
        return False                       # two vertices project onto one point
    for i in range(n):
        a1, a2 = poly[i], poly[(i + 1) % n]
        for j in range(i + 1, n):
            if j == i or (j + 1) % n == i or (i + 1) % n == j:
                continue                   # edges sharing a vertex
            b1, b2 = poly[j], poly[(j + 1) % n]
            if _segments_cross(a1, a2, b1, b2):
                return False
    return True


def _point_in_triangle(
    p: Vec2, a: Vec2, b: Vec2, c: Vec2, eps: float
) -> bool:
    """Inside or ON the boundary of a counter-clockwise triangle.

    Boundary counts as inside deliberately: a vertex sitting exactly on a
    candidate ear's diagonal makes that ear unsafe, so the conservative answer
    is the one that rejects it and tries a different ear.
    """
    return (
        _cross2(a, b, p) >= -eps
        and _cross2(b, c, p) >= -eps
        and _cross2(c, a, p) >= -eps
    )


def triangulate_loop(loop: BoundaryLoop) -> List[Tri]:
    """Cap one boundary loop with a valid, non-self-intersecting fan of ears.

    See the module docstring for why this is ear clipping in the loop's
    dominant plane rather than a centroid fan.

    The returned triangles use the loop's own 3D vertices, so their edges are
    exactly the loop's edges and closure is exact rather than tolerance-based.
    Every triangle is verified to have positive area in 3D: a zero-area cap
    triangle is skipped by ``check_surface`` when it counts edges, so it would
    reopen the very edge it was meant to close.
    """
    points = list(loop.vertices)
    n = len(points)
    if n < 3:
        raise CapTriangulationError(
            f"a boundary loop of {n} vertices cannot bound an area"
        )

    normal = _newell_normal(points)
    magnitude = math.sqrt(sum(c * c for c in normal))
    if magnitude <= 0.0:
        raise CapTriangulationError(
            "this boundary loop has zero projected area in every plane, so it "
            f"has no interior to cap ({loop.describe()}). The loop is "
            "degenerate: repair the surface in CAD."
        )
    axis = max(range(3), key=lambda k: abs(normal[k]))
    u_axis, v_axis = (axis + 1) % 3, (axis + 2) % 3
    poly: List[Vec2] = [(p[u_axis], p[v_axis]) for p in points]

    # Right-handed drop of ``axis``: the 2D signed area then carries the sign
    # of normal[axis], so reversing to counter-clockwise below is a statement
    # about the projection and not about which way the loop was walked.
    signed = 0.5 * sum(
        poly[i][0] * poly[(i + 1) % n][1] - poly[(i + 1) % n][0] * poly[i][1]
        for i in range(n)
    )
    if signed < 0.0:
        points.reverse()
        poly.reverse()
        signed = -signed

    extent = max(
        max(p[0] for p in poly) - min(p[0] for p in poly),
        max(p[1] for p in poly) - min(p[1] for p in poly),
    )
    eps = 1e-12 * max(extent * extent, 1.0)

    if signed <= eps:
        raise CapTriangulationError(
            "this boundary loop encloses no area in the plane it is most "
            f"nearly parallel to ({loop.describe()}); it cannot be capped."
        )

    if not _is_simple(poly):
        raise CapTriangulationError(
            "this boundary loop self-intersects when projected onto the plane "
            f"it is most nearly parallel to ({loop.describe()}), so it is not "
            "a simple polygon there and ear clipping is not valid on it. "
            "Capping it anyway would produce a lid folded through itself, "
            "which closes every open edge while enclosing the wrong region. "
            "Repair or split the opening in CAD."
        )

    indices = list(range(n))
    ears: List[Tuple[int, int, int]] = []

    def is_ear(prev: int, cur: int, nxt: int, *, strict: bool) -> bool:
        a, b, c = poly[prev], poly[cur], poly[nxt]
        turn = _cross2(a, b, c)
        if turn < (eps if strict else -eps):
            return False
        if _area3(points[prev], points[cur], points[nxt]) <= 0.0:
            # Collinear in projection is fine -- the loop is not planar, so the
            # lifted triangle may still have area -- but collinear in 3D is a
            # degenerate triangle and must never be emitted.
            return False
        for k in indices:
            if k in (prev, cur, nxt):
                continue
            if _point_in_triangle(poly[k], a, b, c, eps):
                return False
        return True

    while len(indices) > 3:
        clipped = False
        for strict in (True, False):
            for k in range(len(indices)):
                prev = indices[k - 1]
                cur = indices[k]
                nxt = indices[(k + 1) % len(indices)]
                if is_ear(prev, cur, nxt, strict=strict):
                    ears.append((prev, cur, nxt))
                    indices.pop(k)
                    clipped = True
                    break
            if clipped:
                break
        if not clipped:
            raise CapTriangulationError(
                "ear clipping stalled with "
                f"{len(indices)} vertices of this boundary loop left "
                f"({loop.describe()}). Every remaining candidate is either "
                "reflex, contains another boundary vertex, or is degenerate in "
                "3D. The loop is not a simple polygon in its own dominant "
                "plane; repair or split the opening in CAD rather than "
                "capping it."
            )
    ears.append((indices[0], indices[1], indices[2]))

    cap: List[Tri] = [
        (points[i], points[j], points[k]) for i, j, k in ears
    ]
    for tri in cap:
        if _area3(*tri) <= 0.0:  # pragma: no cover - guarded inside is_ear
            raise CapTriangulationError(
                "the cap for this boundary loop contains a zero-area triangle "
                f"({loop.describe()}); it would be skipped by the closure "
                "check and reopen the edge it was meant to close."
            )
    return cap


# --------------------------------------------------------------------------- #
#  The capper
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class CapResult:
    """A capped surface plus everything needed to audit the repair."""

    triangles: List[Tri]
    cap_triangles: List[Tri]
    loops: List[BoundaryLoop] = field(default_factory=list)
    loop_triangles: List[int] = field(default_factory=list)
    below_waterline: bool = False
    waterline_z: float = float("nan")

    @property
    def n_caps(self) -> int:
        return len(self.loops)

    @property
    def n_cap_triangles(self) -> int:
        return len(self.cap_triangles)

    @property
    def cap_area(self) -> float:
        return sum(_area3(*tri) for tri in self.cap_triangles)

    def loop_records(self, *, z_datum: float = 0.0) -> List[Dict[str, object]]:
        """Per-loop summaries for the manifest.

        ``z_datum`` is subtracted from the recorded heights so the manifest can
        report them in the frame it reports everything else in (keel at z=0),
        rather than in whatever frame the capper happened to run in.
        """
        records: List[Dict[str, object]] = []
        start = 0
        # Each loop's triangles occupy a contiguous run of ``cap_triangles``,
        # in loop order, because that is the order they were appended in.
        for loop, count in zip(self.loops, self.loop_triangles):
            lo, hi = loop.bbox
            records.append(
                {
                    "n_vertices": loop.n_vertices,
                    "n_triangles": count,
                    "z_min_m": loop.z_min - z_datum,
                    "z_max_m": loop.z_max - z_datum,
                    "plan_extent_m": [hi[0] - lo[0], hi[1] - lo[1]],
                    "area_m2": sum(
                        _area3(*tri)
                        for tri in self.cap_triangles[start:start + count]
                    ),
                }
            )
            start += count
        return records


def cap_boundary_loops(
    tris: Sequence[Tri],
    *,
    waterline_z: float,
    allow_below_waterline: bool = False,
    quantise: int = DEFAULT_QUANTISE,
) -> CapResult:
    """Close every open boundary of ``tris``, above the waterline only.

    ``waterline_z`` is required, not optional. A capper that did not know where
    the water is could not enforce the one rule that keeps the reported
    displacement honest, and defaulting it to zero would make that rule depend
    on whatever frame the caller happened to be in.

    ``allow_below_waterline`` is the explicit opt-in for a submerged opening --
    a hull whose bottom is genuinely missing, say. The manifest records that it
    was used, because from that point on the reported displacement includes
    volume the client's surface did not describe.
    """
    tris = [tuple(tri) for tri in tris]  # type: ignore[misc]
    loops = find_boundary_loops(tris, quantise=quantise)
    if not loops:
        return CapResult(
            triangles=list(tris),
            cap_triangles=[],
            waterline_z=waterline_z,
        )

    # ``<=`` and not ``<``: ``wetted_surface_area`` counts a triangle whose
    # vertices all lie AT or below the plane as fully wetted, so a lid exactly
    # on the waterline would add its whole area to the reported wetted surface.
    submerged = [loop for loop in loops if loop.z_min <= waterline_z]
    if submerged and not allow_below_waterline:
        detail = "; ".join(loop.describe() for loop in submerged)
        raise CapBelowWaterlineError(
            f"{len(submerged)} of {len(loops)} open boundary loops reach the "
            f"waterline at z = {waterline_z:g} m or pass below it ({detail}).\n"
            "A cap below the waterline adds area to the reported wetted "
            "surface and volume to the reported displacement, which are "
            "exactly the quantities this pipeline exists to produce, and it "
            "does so silently: the case still meshes and still solves.\n"
            "Options: check the draft; check that the layer filter did not "
            "drop the hull's bottom; repair the opening in CAD; or pass "
            "allow_below_waterline=True to accept a hull whose displacement "
            "includes volume the source surface did not describe."
        )

    cap: List[Tri] = []
    per_loop: List[int] = []
    for loop in loops:
        piece = triangulate_loop(loop)
        cap.extend(piece)
        per_loop.append(len(piece))

    return CapResult(
        triangles=list(tris) + cap,
        cap_triangles=cap,
        loops=loops,
        loop_triangles=per_loop,
        below_waterline=bool(submerged),
        waterline_z=waterline_z,
    )
