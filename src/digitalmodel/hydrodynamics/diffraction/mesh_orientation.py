"""ABOUTME: Detect and repair inconsistent or inward panel normals in surface-piercing diffraction meshes.

A wetted-surface panel mesh that closes at the free surface satisfies an
invariant that does not depend on what the body looks like. Applying the
divergence theorem to the fields (x,0,0), (0,y,0) and (0,0,z) each returns the
displaced volume, and the waterplane lid contributes nothing to any of them
because it lies in z = 0 and its normal has no horizontal component. So for a
correctly oriented mesh

    integral(x n_x dA) = integral(y n_y dA) = integral(z n_z dA) = V > 0

Three answers that disagree prove the normals are not consistently outward.
Three that agree on a negative value prove they are consistent but inward.

This is what distinguishes the two faults found in the committed unit-box
fixtures (issue #898), which differ in *which* panels are wrong:

    examples/.../unit_box.gdf        axis volumes (-1, -1, +1)
    L01/.../unit_box_clean.gdf       axis volumes (+1, +1, -1)

Locating the offending panels uses a second, independent argument: on a closed
oriented surface every interior edge is traversed once in each direction by the
two panels that share it. Panels traversing a shared edge the same way disagree
with each other. Flood-filling that relation makes the mesh mutually
consistent; the sign of the resulting volume then fixes the global direction.

Note that the mean-normal test in geometry_quality.check_normals cannot see
either fault: for a closed body the mean normal is zero, so comparing each
panel against it is vacuous.
"""

from __future__ import annotations

from collections import defaultdict, deque
from dataclasses import dataclass
from typing import Iterable, Sequence

import numpy as np

__all__ = [
    "OrientationReport",
    "UnreliableOrientation",
    "orientation_report",
    "orient_outward",
    "panel_vector_areas",
    "repair_gdf_text",
]

_AREA_FLOOR = 1e-14


@dataclass(frozen=True)
class OrientationReport:
    """What the panel windings imply about the body they enclose."""

    axis_volumes: tuple[float, float, float]
    volume: float
    consistent: bool
    outward: bool
    inverted_panels: tuple[int, ...]
    max_axis_discrepancy: float
    waterplane_area: float
    components: int
    boundary_edges: int
    non_manifold_edges: int
    above_waterline_vertices: int
    waterline_lid_panels: int = 0
    symmetry_plane: str | None = None
    #: Boundary edges lying neither at the waterline nor on a declared symmetry
    #: plane. Any is a hole in the wetted surface.
    submerged_boundary_edges: int = 0

    @property
    def sector_fraction(self) -> int:
        """How many copies of the stored mesh make the whole body.

        A GDF may store a half or quarter model and declare the symmetry
        planes that reproduce the rest. Volume and waterplane area then belong
        to the stored sector, not to the vessel.
        """
        if not self.symmetry_plane:
            return 1
        s = self.symmetry_plane.lower()
        return 4 if len(s) >= 2 and "x" in s and "y" in s else 2

    @property
    def ok(self) -> bool:
        """Safe to use as is.

        A single connected component is required. With more than one, the
        edge-adjacency argument fixes orientation only within each component
        and the enclosed volume gives one global sign, so a component that is
        inverted relative to the others cannot be identified.

        Panels lying wholly in the free surface are rejected because they are
        not wetted-hull panels: an interior lid contributes waterline edges
        that cancel the hull's own, leaving a waterplane area of zero while
        every other indicator still looks correct.

        A boundary edge away from the waterline and away from any declared
        symmetry plane is a hole in the wetted surface. The axis volumes do not
        always show it: a face removed from a plane through the origin
        contributes nothing to any of the three integrals, so an open mesh can
        report the closed volume, agree across all three axes and read as
        outward.
        """
        return (
            self.consistent
            and self.outward
            and not self.inverted_panels
            and self.components == 1
            and self.non_manifold_edges == 0
            and self.above_waterline_vertices == 0
            and self.waterline_lid_panels == 0
            and self.submerged_boundary_edges == 0
        )

    def describe(self) -> str:
        if self.waterline_lid_panels:
            return (
                f"{self.waterline_lid_panels} panel(s) lie in the free "
                f"surface; waterplane area is not meaningful for this mesh"
            )
        if not self.consistent:
            return (
                f"normals not consistently oriented: axis volumes "
                f"{self.axis_volumes}, {len(self.inverted_panels)} panel(s) "
                f"disagree"
            )
        if not self.outward:
            return f"normals point inward: V = {self.volume:.9g} m^3"
        scope = (f" for the stored sector, {self.sector_fraction} of which "
                 f"make the body (symmetry {self.symmetry_plane})"
                 if self.sector_fraction > 1 else "")
        return (
            f"outward, V = {self.volume:.9g} m^3, "
            f"Awp = {self.waterplane_area:.9g} m^2{scope}"
        )


def _panel_triangles(
    vertices: np.ndarray, panel: Sequence[int]
) -> list[tuple[np.ndarray, np.ndarray]]:
    """Fan-triangulate a panel, returning (vector area, centroid) per triangle."""
    idx = list(panel)
    pts = vertices[idx]
    v0 = pts[0]
    out = []
    for k in range(1, len(idx) - 1):
        vec = np.cross(pts[k] - v0, pts[k + 1] - v0) / 2.0
        out.append((vec, (v0 + pts[k] + pts[k + 1]) / 3.0))
    return out


def panel_vector_areas(
    vertices: np.ndarray, panels: Sequence[Sequence[int]]
) -> tuple[np.ndarray, np.ndarray]:
    """Vector area and area-weighted centroid of every panel.

    These are reported for inspection. The volume integrals do not use them,
    because collapsing a panel to a single centroid and a single vector area
    is exact only when the panel is planar: for a warped quadrilateral the two
    triangle normals differ and the product of the mean centroid with the
    summed vector area is not the integral. Hull meshes are routinely warped,
    so the integrals are accumulated per triangle in ``_axis_volumes``.
    """
    verts = np.asarray(vertices, dtype=float)
    vec_areas = np.zeros((len(panels), 3))
    centroids = np.zeros((len(panels), 3))
    for i, panel in enumerate(panels):
        tris = _panel_triangles(verts, panel)
        total = np.zeros(3)
        weighted = np.zeros(3)
        weight = 0.0
        for vec, cen in tris:
            total += vec
            area = float(np.linalg.norm(vec))
            weighted += area * cen
            weight += area
        vec_areas[i] = total
        centroids[i] = (weighted / weight if weight > _AREA_FLOOR
                        else verts[list(panel)].mean(axis=0))
    return vec_areas, centroids


def _triangle_terms(
    vertices: np.ndarray, panels: Sequence[Sequence[int]]
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Per-triangle vector areas and centroids, with their owning panel index."""
    verts = np.asarray(vertices, dtype=float)
    vecs, cens, owner = [], [], []
    for i, panel in enumerate(panels):
        for vec, cen in _panel_triangles(verts, panel):
            vecs.append(vec)
            cens.append(cen)
            owner.append(i)
    return (np.asarray(vecs).reshape(-1, 3),
            np.asarray(cens).reshape(-1, 3),
            np.asarray(owner, dtype=int))


def _directed_edges(panel: Sequence[int]) -> list[tuple[int, int]]:
    idx = list(panel)
    edges = []
    for k in range(len(idx)):
        a, b = idx[k], idx[(k + 1) % len(idx)]
        if a != b:  # skip the collapsed edge of a triangle stored as a quad
            edges.append((a, b))
    return edges


def _consistency_flips(
    panels: Sequence[Sequence[int]],
) -> tuple[np.ndarray, int, int, int, list[tuple[int, int]]]:
    """Flip flags making every panel agree with its neighbours.

    Returns the flags, the number of connected components, the count of
    boundary edges (shared by one panel, expected along the waterline), the
    count of non-manifold edges (shared by more than two), and the boundary
    edges themselves.

    The edges are returned, not only counted, because where they lie is what
    separates a wetted surface that closes at the free surface from one with a
    hole in it. A count cannot make that distinction: a closed hull already has
    a boundary all along its waterline.
    """
    shared: dict[tuple[int, int], list[tuple[int, int]]] = defaultdict(list)
    for p, panel in enumerate(panels):
        for a, b in _directed_edges(panel):
            key = (a, b) if a < b else (b, a)
            shared[key].append((p, 1 if a < b else -1))

    adjacency: dict[int, list[tuple[int, int]]] = defaultdict(list)
    boundary_edges: list[tuple[int, int]] = []
    non_manifold = 0
    for key, users in shared.items():
        if len(users) == 1:
            boundary_edges.append(key)
            continue
        if len(users) > 2:
            non_manifold += 1
            continue
        (p, dp), (q, dq) = users
        # Opposite traversal means the two panels already agree.
        adjacency[p].append((q, dp != dq))
        adjacency[q].append((p, dp != dq))

    n = len(panels)
    flip = np.zeros(n, dtype=bool)
    seen = np.zeros(n, dtype=bool)
    components = 0
    for start in range(n):
        if seen[start]:
            continue
        components += 1
        seen[start] = True
        queue = deque([start])
        while queue:
            p = queue.popleft()
            for q, agrees in adjacency.get(p, ()):
                if seen[q]:
                    continue
                seen[q] = True
                flip[q] = flip[p] if agrees else not flip[p]
                queue.append(q)
    return flip, components, len(boundary_edges), non_manifold, boundary_edges


def _submerged_boundary_edges(
    vertices: np.ndarray,
    boundary_edges: Sequence[tuple[int, int]],
    symmetry_plane: str | None,
    z_tol: float,
) -> int:
    """Boundary edges that lie neither at the waterline nor on a symmetry cut.

    A wetted-surface mesh is legitimately open in two places: along the
    waterline, where it stops at z = 0, and along a declared symmetry plane,
    where the rest of the body is reproduced by reflection rather than stored.
    An edge anywhere else is a hole, and a hole breaks the divergence-theorem
    identity the whole module rests on.

    The hole is not always visible in the axis volumes. A face removed from a
    plane through the origin contributes nothing to any of the three integrals,
    so they stay equal to each other and to the closed value. Symmetry-reduced
    meshes are cut on exactly those planes, which is why this is checked
    directly rather than inferred from a volume discrepancy.
    """
    if not len(boundary_edges):
        return 0
    verts = np.asarray(vertices, dtype=float)
    planes = []
    if symmetry_plane:
        s = symmetry_plane.lower()
        # A GDF's ISX mirrors about x = 0, ISY about y = 0.
        if "x" in s:
            planes.append(0)
        if "y" in s:
            planes.append(1)

    count = 0
    for a, b in boundary_edges:
        pa, pb = verts[a], verts[b]
        if abs(pa[2]) <= z_tol and abs(pb[2]) <= z_tol:
            continue                      # at the waterline
        if any(abs(pa[i]) <= z_tol and abs(pb[i]) <= z_tol for i in planes):
            continue                      # on a declared symmetry cut
        count += 1
    return count


def _axis_volumes(
    tri_vecs: np.ndarray,
    tri_cens: np.ndarray,
    tri_owner: np.ndarray,
    signs: np.ndarray,
) -> tuple[float, float, float]:
    """Displaced volume along each axis, accumulated triangle by triangle.

    Each of the three is the divergence-theorem integral of one of (x,0,0),
    (0,y,0) and (0,0,z). For a correctly oriented surface that closes at the
    free surface they agree, since the waterplane lid lies in z = 0 and has no
    horizontal normal component.
    """
    signed = tri_vecs * signs[tri_owner][:, None]
    return tuple(float(np.sum(tri_cens[:, k] * signed[:, k])) for k in range(3))


def _waterplane_area(
    vertices: np.ndarray,
    panels: Sequence[Sequence[int]],
    signs: np.ndarray,
    z_tol: float,
) -> float:
    """Signed waterplane area from the panel edges lying in z = 0.

    Traversed in the sense an outward wetted surface implies, the waterline
    boundary runs clockwise seen from above, so the shoelace sum comes out
    negative and is negated here to report a positive area.
    """
    verts = np.asarray(vertices, dtype=float)
    total = 0.0
    for p, panel in enumerate(panels):
        idx = list(panel)
        if signs[p] < 0:
            idx = idx[::-1]
        for a, b in _directed_edges(idx):
            if abs(verts[a][2]) <= z_tol and abs(verts[b][2]) <= z_tol:
                x1, y1 = verts[a][0], verts[a][1]
                x2, y2 = verts[b][0], verts[b][1]
                total += (x1 * y2 - x2 * y1) / 2.0
    return -total


def orientation_report(
    mesh, rtol: float = 1e-6, z_tol: float = 1e-7
) -> OrientationReport:
    """Assess whether a panel mesh is consistently outward-oriented."""
    vertices = np.asarray(mesh.vertices, dtype=float)
    panels = [list(p) for p in np.asarray(mesh.panels)]

    tri_vecs, tri_cens, tri_owner = _triangle_terms(vertices, panels)
    flip, components, boundary, non_manifold, boundary_keys = _consistency_flips(panels)

    signs = np.where(flip, -1.0, 1.0)
    axis_v = _axis_volumes(tri_vecs, tri_cens, tri_owner, signs)
    volume = float(np.mean(axis_v))
    if volume < 0.0:
        # The adjacency argument fixes relative orientation only; the sign of
        # the enclosed volume fixes the absolute direction. With more than one
        # connected component this single global sign cannot resolve their
        # relative orientation, which is why `components` is reported and why
        # `ok` requires a single component.
        flip = ~flip
        signs = -signs
        axis_v = _axis_volumes(tri_vecs, tri_cens, tri_owner, signs)
        volume = float(np.mean(axis_v))

    as_stored = _axis_volumes(tri_vecs, tri_cens, tri_owner,
                              np.ones(len(panels)))
    spread = max(as_stored) - min(as_stored)
    scale = max(abs(volume), max(abs(v) for v in as_stored), 1e-30)
    consistent = spread <= rtol * scale
    inverted = tuple(int(i) for i in np.flatnonzero(flip))
    outward = consistent and volume > 0.0 and not inverted

    lids = sum(1 for p in panels
               if np.all(np.abs(vertices[list(p)][:, 2]) <= z_tol))

    return OrientationReport(
        axis_volumes=as_stored,
        volume=float(np.mean(as_stored)) if consistent else volume,
        consistent=consistent,
        outward=outward,
        inverted_panels=inverted,
        max_axis_discrepancy=float(spread),
        waterplane_area=_waterplane_area(vertices, panels, signs, z_tol),
        components=components,
        boundary_edges=boundary,
        non_manifold_edges=non_manifold,
        above_waterline_vertices=int(np.sum(vertices[:, 2] > z_tol)),
        waterline_lid_panels=lids,
        symmetry_plane=getattr(mesh, "symmetry_plane", None),
        submerged_boundary_edges=_submerged_boundary_edges(
            vertices, boundary_keys,
            getattr(mesh, "symmetry_plane", None), z_tol),
    )


class UnreliableOrientation(ValueError):
    """The mesh does not meet the conditions the orientation argument needs."""


def _refuse_if_unreliable(report: OrientationReport) -> None:
    """Refuse to repair a mesh whose orientation cannot be determined.

    Repair rests on two arguments, and each has a precondition. Edge traversal
    fixes orientation only within a connected component and only on a manifold
    surface. The enclosed volume then supplies one global sign, which is enough
    for a single component and not enough for several. And the axis-volume
    identity assumes the body closes in the plane z = 0, so geometry above the
    waterline invalidates it. Silently rewinding panels under any of these is
    worse than declining.
    """
    problems = []
    if report.components != 1:
        problems.append(
            f"{report.components} disconnected components; their relative "
            f"orientation cannot be resolved from one enclosed volume")
    if report.non_manifold_edges:
        problems.append(
            f"{report.non_manifold_edges} edges shared by more than two panels")
    if report.above_waterline_vertices:
        problems.append(
            f"{report.above_waterline_vertices} vertices above z = 0; the "
            f"axis-volume identity assumes the body closes at the free surface")
    if report.waterline_lid_panels:
        problems.append(
            f"{report.waterline_lid_panels} panel(s) lie wholly in the free "
            f"surface; this routine handles wetted-hull panels only, and a "
            f"lid cancels the hull's own waterline contributions")
    if report.submerged_boundary_edges:
        declared = report.symmetry_plane or "none"
        problems.append(
            f"{report.submerged_boundary_edges} boundary edge(s) lie away from "
            f"the waterline and away from any declared symmetry plane "
            f"(declared: {declared}); the wetted surface is open, and the "
            f"axis-volume identity does not hold through a hole. A hole in a "
            f"plane through the origin does not disturb the axis volumes at "
            f"all, so this cannot be inferred from them")
    if problems:
        raise UnreliableOrientation("; ".join(problems))


def orient_outward(mesh, strict: bool = True) -> tuple[object, tuple[int, ...]]:
    """Return a copy of the mesh with every panel wound outward.

    Vertex positions are untouched; only the order in which a panel lists them
    changes. The indices of the panels that were rewound are returned so the
    caller can record exactly what the repair did.

    Raises ``UnreliableOrientation`` when the mesh does not meet the conditions
    the argument needs. Pass ``strict=False`` only when the caller has its own
    reason to trust the result.
    """
    report = orientation_report(mesh)
    if strict:
        _refuse_if_unreliable(report)
    panels = np.asarray(mesh.panels).copy()
    for i in report.inverted_panels:
        panels[i] = panels[i][::-1]

    vertices = np.asarray(mesh.vertices, dtype=float)
    vec_areas, centroids = panel_vector_areas(vertices, panels)
    areas = np.linalg.norm(vec_areas, axis=1)
    with np.errstate(invalid="ignore", divide="ignore"):
        normals = np.where(
            areas[:, None] > _AREA_FLOOR, vec_areas / areas[:, None], 0.0
        )

    from dataclasses import replace

    fixed = replace(
        mesh,
        panels=panels,
        normals=normals,
        panel_areas=areas,
        panel_centers=centroids,
    )
    return fixed, report.inverted_panels


def repair_gdf_text(text: str, strict: bool = True) -> tuple[str, tuple[int, ...]]:
    """Rewind inward-facing panels in WAMIT GDF text, preserving formatting.

    A GDF file lists four vertex lines per panel, so rewinding a panel means
    reversing its four lines. Nothing else is touched: the header, the
    ULEN/GRAV and symmetry records, the panel count, the number formatting and
    the line endings all survive byte-for-byte, which keeps the diff readable
    and auditable. Re-serialising the mesh instead would rewrite every
    coordinate.
    """
    eol = "\r\n" if "\r\n" in text else "\n"
    trailing = text.endswith(eol)
    lines = text.split(eol)
    if trailing and lines and lines[-1] == "":
        lines = lines[:-1]

    if len(lines) < 4:
        raise ValueError("not a GDF file: fewer than four header lines")
    npan = int(float(lines[3].split()[0]))
    head, body = lines[:4], lines[4:]

    # Line 3 is the ISX/ISY symmetry record. A declared plane makes the cut
    # along it a legitimate boundary rather than a hole, so it has to reach the
    # refusal check; without it a half model would be rejected as open.
    symmetry = None
    try:
        isx, isy = (int(float(v)) for v in lines[2].split()[:2])
        symmetry = ("xy" if isx and isy else
                    "x" if isx else
                    "y" if isy else None)
    except (ValueError, IndexError):
        symmetry = None

    rows = [ln for ln in body if len(ln.split()) >= 3]
    if len(rows) != npan * 4:
        raise ValueError(
            f"declared NPAN={npan} implies {npan * 4} vertex lines, found {len(rows)}"
        )

    coords = np.asarray(
        [[float(v) for v in ln.split()[:3]] for ln in rows], dtype=float
    ).reshape(npan, 4, 3)

    # Panels share vertices only by coincidence of coordinates, so they have to
    # be welded before adjacency means anything. The weld is by exact bytes,
    # matching what GDFHandler does when it reads the same file: a rounding
    # bin would merge distinct coordinates that fall inside it while leaving
    # arbitrarily close ones on either side of a boundary apart, and would give
    # this routine a different topology from the one the reader recovers.
    flat = coords.reshape(-1, 3)
    _, first, inverse = np.unique(
        flat, axis=0, return_index=True, return_inverse=True
    )
    order_of_appearance = np.argsort(first)
    rank = np.empty_like(order_of_appearance)
    rank[order_of_appearance] = np.arange(len(first))
    vertices = flat[first[order_of_appearance]]
    panels = rank[inverse].reshape(-1, 4)

    # Near-coincident but unequal vertices would leave the surface open where
    # it should be closed, so the caller is told rather than left guessing.
    if len(vertices) > 1:
        extent = float(np.max(vertices.max(axis=0) - vertices.min(axis=0)))
        if extent > 0:
            from scipy.spatial import cKDTree  # local: optional at import time

            pairs = cKDTree(vertices).query_pairs(1e-9 * extent)
            if pairs:
                raise UnreliableOrientation(
                    f"{len(pairs)} vertex pair(s) are distinct but closer than "
                    f"1e-9 of the mesh extent; welding is ambiguous"
                )

    tri_vecs, tri_cens, tri_owner = _triangle_terms(vertices, panels)
    flip, components, boundary, non_manifold, boundary_keys = _consistency_flips(panels)
    if strict:
        _refuse_if_unreliable(OrientationReport(
            axis_volumes=(0.0, 0.0, 0.0), volume=0.0, consistent=True,
            outward=True, inverted_panels=(), max_axis_discrepancy=0.0,
            waterplane_area=0.0, components=components,
            boundary_edges=boundary, non_manifold_edges=non_manifold,
            above_waterline_vertices=int(np.sum(vertices[:, 2] > 1e-7)),
            waterline_lid_panels=sum(
                1 for p in panels
                if np.all(np.abs(vertices[list(p)][:, 2]) <= 1e-7)),
            symmetry_plane=symmetry,
            submerged_boundary_edges=_submerged_boundary_edges(
                vertices, boundary_keys, symmetry, 1e-7)))
    signs = np.where(flip, -1.0, 1.0)
    if float(np.mean(_axis_volumes(tri_vecs, tri_cens, tri_owner, signs))) < 0.0:
        flip = ~flip

    flipped = tuple(int(i) for i in np.flatnonzero(flip))
    out_rows: list[str] = []
    for p in range(npan):
        quad = rows[p * 4:(p + 1) * 4]
        out_rows.extend(quad[::-1] if flip[p] else quad)

    rebuilt = list(head)
    it = iter(out_rows)
    for ln in body:
        rebuilt.append(next(it) if len(ln.split()) >= 3 else ln)
    out = eol.join(rebuilt) + (eol if trailing else "")

    if strict and flipped:
        # Read the emitted text back and confirm it is what was intended,
        # rather than trusting that the rewrite did what the analysis asked.
        check = _report_from_coords(
            np.asarray([[float(v) for v in r.split()[:3]]
                        for r in out_rows], dtype=float).reshape(npan, 4, 3))
        if not check.outward or check.inverted_panels:
            raise UnreliableOrientation(
                f"repaired text does not read back as outward: axis volumes "
                f"{check.axis_volumes}, inverted {check.inverted_panels}"
            )

    return out, flipped


def _report_from_coords(coords: np.ndarray) -> OrientationReport:
    """Orientation report for panels given as an (n, 4, 3) coordinate array."""

    class _Bare:
        pass

    flat = coords.reshape(-1, 3)
    _, first, inverse = np.unique(
        flat, axis=0, return_index=True, return_inverse=True
    )
    order_of_appearance = np.argsort(first)
    rank = np.empty_like(order_of_appearance)
    rank[order_of_appearance] = np.arange(len(first))
    bare = _Bare()
    bare.vertices = flat[first[order_of_appearance]]
    bare.panels = rank[inverse].reshape(-1, 4)
    bare.symmetry_plane = None
    return orientation_report(bare)


def format_report(name: str, report: OrientationReport) -> str:
    """One-line human-readable verdict, for CLI and quality-gate output."""
    verdict = "PASS" if report.ok else "FAIL"
    return f"[{verdict}] {name}: {report.describe()}"
