"""
ABOUTME: KCS (KRISO Container Ship) hull surface generation for #1173. Reads the
Gothenburg 2000 / Tokyo 2005 workshop's published structured surface grid and
tessellates it into a watertight STL at model scale, with no CAD dependency.

WHY THIS ROUTE, AND NOT IGES
----------------------------
The #1173 plan flags KCS geometry acquisition as the top schedule risk: the
workshops publish IGES/STEP, those files are NURBS surface entities (IGES type
128), and this repository has no CAD kernel to evaluate them.

That risk does not bind, because the Tokyo 2005 archive also publishes a
*structured surface grid* in Tecplot ASCII covering the hull's starboard half.
A structured grid is already a tessellation waiting to happen: every quad is
four known points, and the blocks join point-for-point.

The choice of source is not merely convenient, it is condition-matched. The
referent this case gates against is Gothenburg 2000 / Tokyo 2005 Case 1.1, and
this grid is that campaign's own geometry. The T2015 and SIMMAN mirrors publish
a later re-fairing of the hull by a different institution; using it would
introduce a geometry-vintage mismatch into a comparison whose entire discipline
is condition matching.

THE TOPOLOGY, VERIFIED RATHER THAN ASSUMED
------------------------------------------
Four structured blocks, all conformal at their seams to machine zero:

    bow2 block 1  (41 x 31)   bulbous bow
    bow2 block 2  (61 x 80)   forebody, stem to midship
    stn2 block 1  (41 x 31)   transom overhang
    stn2 block 2  (61 x 80)   afterbody, midship to stern profile

Seams, all EXACT (maximum point-to-point discrepancy 0.0):

    forebody i=imax  ==  afterbody i=1              (midship)
    bulb     i=imax  ==  forebody i=1, k=1..41      (bulb neck)
    transom  i=1     ==  afterbody i=imax, k=21..61 (overhang neck)

Every remaining block boundary is either on the centreplane (Y = 0), where
mirroring closes it, or one of exactly two open boundaries:

    the design top waterline, a FLAT plane at Z = 0.018261 * Lpp
    the transom end,         a FLAT plane at X = 0.526090 * Lpp

so the surface is closed by exactly two caps. Both are ruled between the port
curve and its mirror, which is watertight by construction rather than by
tolerance.

Coordinates in the source file are nondimensionalised by Lpp, origin at midship
on the centreplane at the calm free surface, X downstream, Y to starboard,
Z up. Model scale is therefore a single multiply.
"""

from __future__ import annotations

import math
import re
from collections import defaultdict
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Sequence, Tuple

# --------------------------------------------------------------------------- #
#  Published particulars - the independent check on the generated surface
# --------------------------------------------------------------------------- #

#: Scale ratio of the towing-tank model, stated by the workshop.
KCS_SCALE_RATIO = 31.5994

#: Model-scale length between perpendiculars (m), stated by the workshop.
KCS_MODEL_LPP = 7.2786

#: Full-scale particulars, stated in the offsets-table header (metres).
KCS_FULL_SCALE = {
    "lpp": 230.0,
    "beam": 32.2,
    "draft": 10.8,
    "cb": 0.6505,
}

#: Model-scale particulars derived from the full-scale figures by the stated
#: scale ratio. These are what the generated surface is checked against.
KCS_MODEL_PARTICULARS = {
    "lpp": KCS_MODEL_LPP,
    "beam": KCS_FULL_SCALE["beam"] / KCS_SCALE_RATIO,
    "draft": KCS_FULL_SCALE["draft"] / KCS_SCALE_RATIO,
    "cb": KCS_FULL_SCALE["cb"],
}

#: The referent's bare-hull wetted surface at the design waterline (m^2),
#: STATED by the workshop. The generated surface's wetted area below Z = 0 is
#: checked against this - it is the single most informative check available,
#: because the gate normalises by exactly this quantity.
KCS_WETTED_SURFACE_DWL = 9.4379

#: Nondimensional Z of the grid's top row. ABOVE the design waterline (Z = 0),
#: which is why the raw surface area is larger than the wetted area.
KCS_TOP_WATERLINE_Z = 0.018261

#: Nondimensional X of the flat transom end.
KCS_TRANSOM_X = 0.526090


Vec3 = Tuple[float, float, float]


def workshop_grid_dir() -> Path:
    """Directory holding the committed workshop surface grid (repo checkout)."""
    here = Path(__file__).resolve()
    for parent in here.parents:
        cand = (parent / "docs" / "api" / "cfd" / "cases" / "kcs_resistance" /
                "geometry")
        if (cand / "kcs_bow2.dat").is_file():
            return cand
    raise FileNotFoundError(
        "KCS workshop surface grid not found - requires a repo checkout with "
        "docs/api/cfd/cases/kcs_resistance/geometry/"
    )


def build_kcs_model_surface() -> "KcsSurface":
    """The KCS hull at model scale, from the committed workshop grid."""
    grid = workshop_grid_dir()
    return build_kcs_surface(
        grid / "kcs_bow2.dat", grid / "kcs_stn2.dat", scale=KCS_MODEL_LPP
    )


# --------------------------------------------------------------------------- #
#  Reading the workshop grid
# --------------------------------------------------------------------------- #

_ZONE_RE = re.compile(r"i\s*=\s*(\d+)\s+j\s*=\s*(\d+)", re.IGNORECASE)


def read_tecplot_zones(path: Path | str) -> List[List[List[Vec3]]]:
    """Read a Tecplot POINT-format file into a list of structured zones.

    Each zone is returned as ``zone[k][i] -> (x, y, z)``, matching the layout
    the workshop's own Fortran read loop implies: ``i`` varies fastest.

    The header lines are matched rather than assumed at fixed offsets, because
    the four files do not agree on leading whitespace.
    """
    lines = Path(path).read_text().splitlines()
    zones: List[List[List[Vec3]]] = []
    n = 0
    while n < len(lines):
        match = _ZONE_RE.search(lines[n])
        if not match:
            n += 1
            continue
        ni, nk = int(match.group(1)), int(match.group(2))
        values: List[float] = []
        n += 1
        while len(values) < ni * nk * 3 and n < len(lines):
            values.extend(float(tok) for tok in lines[n].split())
            n += 1
        if len(values) != ni * nk * 3:
            raise ValueError(
                f"zone {len(zones)} in {Path(path).name} declares {ni}x{nk} "
                f"points ({ni * nk * 3} values) but only {len(values)} were read"
            )
        zone = [
            [
                (
                    values[3 * (k * ni + i)],
                    values[3 * (k * ni + i) + 1],
                    values[3 * (k * ni + i) + 2],
                )
                for i in range(ni)
            ]
            for k in range(nk)
        ]
        zones.append(zone)
    if not zones:
        raise ValueError(f"no Tecplot zones found in {path}")
    return zones


# --------------------------------------------------------------------------- #
#  Tessellation
# --------------------------------------------------------------------------- #

def _triangulate_structured(
    zone: Sequence[Sequence[Vec3]],
) -> List[Tuple[Vec3, Vec3, Vec3]]:
    """Split a structured (k, i) patch into triangles, two per quad.

    Degenerate triangles are dropped. They are not a defect in the data: the
    bulb nose and the keel lines collapse onto the centreplane, so quads there
    genuinely have zero area, and emitting them would produce exactly the
    "illegal triangle" verdict this generator exists to avoid.
    """
    tris: List[Tuple[Vec3, Vec3, Vec3]] = []
    nk, ni = len(zone), len(zone[0])
    for k in range(nk - 1):
        for i in range(ni - 1):
            p00, p01 = zone[k][i], zone[k][i + 1]
            p10, p11 = zone[k + 1][i], zone[k + 1][i + 1]
            for tri in ((p00, p01, p11), (p00, p11, p10)):
                if _triangle_area(*tri) > 0.0:
                    tris.append(tri)
    return tris


def _triangle_area(a: Vec3, b: Vec3, c: Vec3) -> float:
    ux, uy, uz = b[0] - a[0], b[1] - a[1], b[2] - a[2]
    vx, vy, vz = c[0] - a[0], c[1] - a[1], c[2] - a[2]
    cx = uy * vz - uz * vy
    cy = uz * vx - ux * vz
    cz = ux * vy - uy * vx
    return 0.5 * math.sqrt(cx * cx + cy * cy + cz * cz)


def _ruled_cap(
    curve: Sequence[Vec3],
    outward: Vec3 = (0.0, 0.0, 1.0),
) -> List[Tuple[Vec3, Vec3, Vec3]]:
    """Close a planar region ruled between ``curve`` and its mirror in Y.

    Both caps in this hull have the same shape: a flat region whose boundary is
    a curve on the starboard side, the mirror of that curve to port, and
    nothing else. For every station along the curve the chord from +Y to -Y
    lies inside the region, so the region is exactly the union of the quads
    between consecutive chords.

    This is watertight *by construction*: adjacent quads share both of their
    chord vertices exactly, so no merge tolerance is involved anywhere.

    ``outward`` is the direction the cap's normals must point. Closure alone is
    not enough for a mesher: ``snappyHexMesh`` decides inside from outside by
    the surface normal, so a cap that closes the hull but faces inward makes
    the solid ambiguous. The winding is therefore checked against ``outward``
    and reversed as a whole if it disagrees, rather than being assumed from the
    order the source file happens to store the curve in.
    """
    tris: List[Tuple[Vec3, Vec3, Vec3]] = []
    for j in range(len(curve) - 1):
        a, b = curve[j], curve[j + 1]
        a_m, b_m = _mirror(a), _mirror(b)
        for tri in ((a, b, b_m), (a, b_m, a_m)):
            if _triangle_area(*tri) > 0.0:
                tris.append(tri)
    # Orient EVERY triangle individually, not the patch as a whole.
    #
    # A majority vote over the patch is not enough, and the reason is specific:
    # the strip's winding is only uniform while the curve's half-breadth grows
    # monotonically. The deck does not - it widens from the stem to its maximum
    # beam and then NARROWS again over the transom overhang - and the winding
    # flips exactly where the sign of that change does. A patch-level vote
    # leaves the minority facing inward, which is what "More than one normal
    # orientation" reports.
    #
    # Both caps are planar, so the outward direction is the same for every
    # triangle in them and a per-triangle test is exact rather than heuristic.
    oriented: List[Tuple[Vec3, Vec3, Vec3]] = []
    for a, b, c in tris:
        if _dot(_normal(a, b, c), outward) < 0.0:
            oriented.append((c, b, a))
        else:
            oriented.append((a, b, c))
    return oriented


def _dot(a: Vec3, b: Vec3) -> float:
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def orient_consistently(
    tris: Sequence[Tuple[Vec3, Vec3, Vec3]], *, quantise: int = 9
) -> List[Tuple[Vec3, Vec3, Vec3]]:
    """Give every triangle of a closed surface the same outward orientation.

    Per-patch orientation is not sufficient and assuming it is was a real
    defect here. The four source blocks do not share an index convention, so
    the winding that ``_triangulate_structured`` derives from ``(k, i)`` order
    points outward for some blocks and inward for others. ``surfaceCheck``
    reports that as "More than one normal orientation", and a mesher cannot
    decide inside from outside on such a surface.

    The fix does not depend on any block's convention. On a closed manifold,
    two triangles sharing an edge are consistently oriented exactly when they
    traverse that shared edge in OPPOSITE directions, so orientation propagates
    across the whole surface from a single seed. That fixes consistency; the
    enclosed volume then fixes the global sense, since outward normals give a
    positive volume.
    """
    if not tris:
        return []

    verts: Dict[Tuple[float, ...], int] = {}

    def vid(p: Vec3) -> int:
        key = tuple(round(c, quantise) + 0.0 for c in p)
        if key not in verts:
            verts[key] = len(verts)
        return verts[key]

    faces = [tuple(vid(p) for p in tri) for tri in tris]
    edge_map: Dict[Tuple[int, int], List[int]] = defaultdict(list)
    for n, f in enumerate(faces):
        for a, b in ((f[0], f[1]), (f[1], f[2]), (f[2], f[0])):
            edge_map[(a, b) if a < b else (b, a)].append(n)

    flipped = [False] * len(faces)
    seen = [False] * len(faces)
    for seed in range(len(faces)):
        if seen[seed]:
            continue
        seen[seed] = True
        stack = [seed]
        while stack:
            n = stack.pop()
            f = faces[n]
            if flipped[n]:
                f = (f[2], f[1], f[0])
            directed = {(f[0], f[1]), (f[1], f[2]), (f[2], f[0])}
            for a, b in list(directed):
                key = (a, b) if a < b else (b, a)
                for m in edge_map.get(key, ()):
                    if m == n or seen[m]:
                        continue
                    g = faces[m]
                    # Same direction on the shared edge => opposite orientation.
                    if (a, b) in {(g[0], g[1]), (g[1], g[2]), (g[2], g[0])}:
                        flipped[m] = True
                    seen[m] = True
                    stack.append(m)

    oriented = [
        (tri[2], tri[1], tri[0]) if flipped[n] else tri
        for n, tri in enumerate(tris)
    ]
    if sum(_flux_z(tri, 0.0) for tri in oriented) < 0.0:
        oriented = [(c, b, a) for a, b, c in oriented]
    return oriented


def _mirror(p: Vec3) -> Vec3:
    """Reflect in the centreplane, normalising -0.0 to 0.0.

    The normalisation matters: a vertex written as ``-0.0`` and one written as
    ``0.0`` are equal numerically but are distinct strings in an ASCII STL, and
    a mesher that welds by string would tear the hull down the centreline.
    """
    y = -p[1]
    return (p[0], 0.0 if y == 0.0 else y, p[2])


def _mirror_triangles(
    tris: Iterable[Tuple[Vec3, Vec3, Vec3]],
) -> List[Tuple[Vec3, Vec3, Vec3]]:
    """Mirror in Y, reversing winding so outward normals stay outward."""
    return [(_mirror(c), _mirror(b), _mirror(a)) for a, b, c in tris]


# --------------------------------------------------------------------------- #
#  Manifold verification - the check that is cheap and would have caught the
#  Wigley asset's 392 open edges
# --------------------------------------------------------------------------- #

@dataclass
class SurfaceCheck:
    """Result of the in-process closure check on a triangle soup."""

    triangle_count: int
    vertex_count: int
    open_edge_count: int
    nonmanifold_edge_count: int
    degenerate_count: int
    bounds: Dict[str, Tuple[float, float]] = field(default_factory=dict)

    @property
    def closed(self) -> bool:
        """Closed iff every edge is shared by exactly two triangles.

        Asserted on the COUNT of single-face edges, not parsed from a verdict
        string. A surface with hundreds of open edges can still be described as
        "not closed" by a one-line summary, and reading that summary loosely is
        how the shipped Wigley asset was scoped for a repair that could not
        have worked.
        """
        return self.open_edge_count == 0 and self.nonmanifold_edge_count == 0


def check_surface(
    tris: Sequence[Tuple[Vec3, Vec3, Vec3]], *, quantise: int = 9
) -> SurfaceCheck:
    """Count open and non-manifold edges in a triangle soup.

    Vertices are quantised to ``quantise`` decimals before hashing. This is not
    a merge tolerance standing in for real conformity: the source blocks join
    at exactly equal coordinates, so quantisation only guards against the
    round-trip through decimal text, and a genuinely mismatched seam still
    shows up as an open edge.
    """
    edges: Dict[Tuple[int, int], int] = defaultdict(int)
    verts: Dict[Tuple[float, ...], int] = {}
    degenerate = 0

    def vid(p: Vec3) -> int:
        key = tuple(round(c, quantise) + 0.0 for c in p)
        if key not in verts:
            verts[key] = len(verts)
        return verts[key]

    for tri in tris:
        if _triangle_area(*tri) <= 0.0:
            degenerate += 1
            continue
        ids = [vid(p) for p in tri]
        for a, b in ((ids[0], ids[1]), (ids[1], ids[2]), (ids[2], ids[0])):
            edges[(a, b) if a < b else (b, a)] += 1

    xs = [p[0] for t in tris for p in t]
    ys = [p[1] for t in tris for p in t]
    zs = [p[2] for t in tris for p in t]
    return SurfaceCheck(
        triangle_count=len(tris),
        vertex_count=len(verts),
        open_edge_count=sum(1 for c in edges.values() if c == 1),
        nonmanifold_edge_count=sum(1 for c in edges.values() if c > 2),
        degenerate_count=degenerate,
        bounds={
            "x": (min(xs), max(xs)),
            "y": (min(ys), max(ys)),
            "z": (min(zs), max(zs)),
        },
    )


# --------------------------------------------------------------------------- #
#  Hydrostatics on the generated surface - the independent particulars check
# --------------------------------------------------------------------------- #

def wetted_surface_area(
    tris: Sequence[Tuple[Vec3, Vec3, Vec3]], waterline_z: float = 0.0
) -> float:
    """Area of the surface strictly below ``waterline_z``.

    Triangles straddling the waterline are clipped, so the result is the true
    wetted area rather than a whole-triangle approximation. This matters: the
    gate divides by this quantity, so a 1% error here is a 1% error in Ct.
    """
    total = 0.0
    for tri in tris:
        total += _clipped_area_below(tri, waterline_z)
    return total


def _clipped_area_below(tri: Tuple[Vec3, Vec3, Vec3], z: float) -> float:
    below = [p for p in tri if p[2] <= z]
    if len(below) == 3:
        return _triangle_area(*tri)
    if not below:
        return 0.0
    # Clip the triangle against the plane and re-triangulate the polygon.
    poly: List[Vec3] = []
    for i in range(3):
        a, b = tri[i], tri[(i + 1) % 3]
        a_in, b_in = a[2] <= z, b[2] <= z
        if a_in:
            poly.append(a)
        if a_in != b_in:
            t = (z - a[2]) / (b[2] - a[2])
            poly.append(
                (a[0] + t * (b[0] - a[0]), a[1] + t * (b[1] - a[1]), z)
            )
    return sum(
        _triangle_area(poly[0], poly[i], poly[i + 1])
        for i in range(1, len(poly) - 1)
    )


def enclosed_volume(
    tris: Sequence[Tuple[Vec3, Vec3, Vec3]], waterline_z: float = 0.0
) -> float:
    """Volume enclosed below ``waterline_z`` by the divergence theorem.

    Uses the flux of the field (0, 0, z - waterline_z) through the clipped
    surface, so the flat waterline plane itself contributes nothing and does
    not need to be tessellated. The sign is taken as absolute because the
    generator's winding is verified separately by the closure check.
    """
    total = 0.0
    for tri in tris:
        for sub in _clip_polygon_below(tri, waterline_z):
            total += _flux_z(sub, waterline_z)
    return abs(total)


def _clip_polygon_below(
    tri: Tuple[Vec3, Vec3, Vec3], z: float
) -> List[Tuple[Vec3, Vec3, Vec3]]:
    below = [p for p in tri if p[2] <= z]
    if len(below) == 3:
        return [tri]
    if not below:
        return []
    poly: List[Vec3] = []
    for i in range(3):
        a, b = tri[i], tri[(i + 1) % 3]
        a_in, b_in = a[2] <= z, b[2] <= z
        if a_in:
            poly.append(a)
        if a_in != b_in:
            t = (z - a[2]) / (b[2] - a[2])
            poly.append((a[0] + t * (b[0] - a[0]), a[1] + t * (b[1] - a[1]), z))
    return [
        (poly[0], poly[i], poly[i + 1]) for i in range(1, len(poly) - 1)
    ]


def _flux_z(tri: Tuple[Vec3, Vec3, Vec3], z0: float) -> float:
    a, b, c = tri
    ux, uy, uz = b[0] - a[0], b[1] - a[1], b[2] - a[2]
    vx, vy, vz = c[0] - a[0], c[1] - a[1], c[2] - a[2]
    nz = ux * vy - uy * vx
    z_mean = ((a[2] - z0) + (b[2] - z0) + (c[2] - z0)) / 3.0
    return 0.5 * nz * z_mean


# --------------------------------------------------------------------------- #
#  The generator
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class KcsSurface:
    """A generated KCS hull surface plus everything needed to trust it."""

    triangles: List[Tuple[Vec3, Vec3, Vec3]]
    check: SurfaceCheck
    scale: float
    wetted_surface: float
    displaced_volume: float

    @property
    def particulars(self) -> Dict[str, float]:
        bx, by, bz = self.check.bounds["x"], self.check.bounds["y"], self.check.bounds["z"]
        return {
            "length_overall": bx[1] - bx[0],
            "beam": by[1] - by[0],
            "draft": -bz[0],
            "wetted_surface": self.wetted_surface,
            "displaced_volume": self.displaced_volume,
        }


def build_kcs_surface(
    bow_path: Path | str,
    stern_path: Path | str,
    *,
    scale: float = KCS_MODEL_LPP,
) -> KcsSurface:
    """Tessellate the workshop grid into a closed, full-body hull surface.

    The FULL body is generated, not the starboard half, even though the case is
    solved on a half domain. Two reasons, and the second is the load-bearing
    one:

    * the acceptance criterion requires the source STL to pass ``surfaceCheck``
      with zero edges connected to fewer than two faces, and a half surface is
      open along its whole centreplane by definition; and
    * a closed surface is checkable *before* meshing. An open one can only be
      judged after the mesher has already made its decisions, which is the
      wrong order when the next step costs a day of compute.
    """
    bow_zones = read_tecplot_zones(bow_path)
    stern_zones = read_tecplot_zones(stern_path)
    if len(bow_zones) != 2 or len(stern_zones) != 2:
        raise ValueError(
            "expected the two-block bow and stern grids (kcs_bow2.dat, "
            f"kcs_stn2.dat); got {len(bow_zones)} and {len(stern_zones)} zones"
        )
    bulb, fore = bow_zones
    transom, aft = stern_zones

    half: List[Tuple[Vec3, Vec3, Vec3]] = []
    for zone in (bulb, fore, transom, aft):
        half.extend(_triangulate_structured(zone))

    # --- the two caps -----------------------------------------------------
    # Top lid: the design-top waterline is flat, and its outline runs bow to
    # stern along the top row of the forebody, the afterbody and the transom
    # overhang in that order. The seams are exact, so the shared points are
    # dropped rather than merged.
    top_curve: List[Vec3] = list(fore[-1]) + list(aft[-1])[1:] + list(transom[-1])[1:]
    _assert_planar(top_curve, axis=2, name="top waterline")

    # Transom cap: the flat end face at constant X, ruled from the centreplane
    # up to the waterline.
    transom_curve: List[Vec3] = [row[-1] for row in transom]
    _assert_planar(transom_curve, axis=0, name="transom end")

    # The lid faces up (+Z, it is the top of the body); the transom faces aft
    # (+X, it is the downstream end).
    caps = (
        _ruled_cap(top_curve, outward=(0.0, 0.0, 1.0))
        + _ruled_cap(transom_curve, outward=(1.0, 0.0, 0.0))
    )

    # Drop triangles lying ENTIRELY in the centreplane before mirroring.
    #
    # The structured grid collapses onto Y = 0 at the bulb nose and at the
    # stern profile, so a quad there has three of its four corners on the
    # centreplane. Splitting such a quad yields one real hull triangle and one
    # sliver lying flat in the mirror plane. The sliver has non-zero area, so a
    # degeneracy filter does not catch it, but it is not hull surface: it is an
    # artefact of the parameterisation. Mirroring maps it onto ITSELF, which is
    # what produced the two non-manifold edges (four faces on one edge) seen
    # before this filter existed.
    #
    # A real hull is never tangent to its own centreplane over a finite area -
    # the centreplane intersection is a curve - so no legitimate surface
    # triangle is lost here.
    half = [tri for tri in half if not all(p[1] == 0.0 for p in tri)]

    full = half + _mirror_triangles(half) + caps
    full = [tri for tri in full if _triangle_area(*tri) > 0.0]
    full = [tuple(tuple(c * scale for c in p) for p in tri) for tri in full]

    full = orient_consistently(full)

    check = check_surface(full)
    return KcsSurface(
        triangles=full,
        check=check,
        scale=scale,
        wetted_surface=wetted_surface_area(full, 0.0),
        displaced_volume=enclosed_volume(full, 0.0),
    )


def _assert_planar(
    curve: Sequence[Vec3], *, axis: int, name: str, tol: float = 1e-9
) -> None:
    """A cap is only watertight by construction if its boundary really is flat.

    Checked rather than assumed, because the whole closure argument rests on
    it: if the top row were not exactly planar, the ruled cap would cut through
    the hull instead of closing it, and the error would surface as a meshing
    failure hours later rather than here.
    """
    values = [p[axis] for p in curve]
    spread = max(values) - min(values)
    if spread > tol:
        raise ValueError(
            f"{name} is not planar in axis {axis}: spread {spread:.3e} > {tol:.0e}. "
            f"The ruled cap would not close the surface."
        )


def write_stl(
    tris: Sequence[Tuple[Vec3, Vec3, Vec3]],
    path: Path | str,
    *,
    name: str = "hull",
) -> Path:
    """Write an ASCII STL with enough precision to survive the round trip.

    Coordinates are written at 12 significant digits. That is more than the
    source data carries, deliberately: the seams close at exactly equal
    coordinates, and truncating on the way out would manufacture gaps that the
    input does not have.
    """
    out = Path(path)
    lines = [f"solid {name}"]
    for a, b, c in tris:
        nx, ny, nz = _normal(a, b, c)
        lines.append(f"  facet normal {nx:.12e} {ny:.12e} {nz:.12e}")
        lines.append("    outer loop")
        for p in (a, b, c):
            lines.append(f"      vertex {p[0]:.12e} {p[1]:.12e} {p[2]:.12e}")
        lines.append("    endloop")
        lines.append("  endfacet")
    lines.append(f"endsolid {name}")
    out.write_text("\n".join(lines) + "\n")
    return out


def _normal(a: Vec3, b: Vec3, c: Vec3) -> Vec3:
    ux, uy, uz = b[0] - a[0], b[1] - a[1], b[2] - a[2]
    vx, vy, vz = c[0] - a[0], c[1] - a[1], c[2] - a[2]
    nx, ny, nz = uy * vz - uz * vy, uz * vx - ux * vz, ux * vy - uy * vx
    mag = math.sqrt(nx * nx + ny * ny + nz * nz)
    if mag == 0.0:
        return (0.0, 0.0, 0.0)
    return (nx / mag, ny / mag, nz / mag)
