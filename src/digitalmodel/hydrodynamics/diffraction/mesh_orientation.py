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

    @property
    def ok(self) -> bool:
        return self.consistent and self.outward and not self.inverted_panels

    def describe(self) -> str:
        if self.ok:
            return (
                f"outward, V = {self.volume:.9g} m^3, "
                f"Awp = {self.waterplane_area:.9g} m^2"
            )
        if not self.consistent:
            return (
                f"normals not consistently oriented: axis volumes "
                f"{self.axis_volumes}, {len(self.inverted_panels)} panel(s) "
                f"disagree"
            )
        return f"normals point inward: V = {self.volume:.9g} m^3"


def panel_vector_areas(
    vertices: np.ndarray, panels: Sequence[Sequence[int]]
) -> tuple[np.ndarray, np.ndarray]:
    """Vector area and centroid of every panel, from its stored winding.

    Quadrilaterals are split into two triangles and their vector areas summed,
    which is the quantity the divergence-theorem integrals require and which
    stays correct when a panel is not quite planar.
    """
    verts = np.asarray(vertices, dtype=float)
    vec_areas = np.zeros((len(panels), 3))
    centroids = np.zeros((len(panels), 3))
    for i, panel in enumerate(panels):
        idx = list(panel)
        pts = verts[idx]
        v0 = pts[0]
        n1 = np.cross(pts[1] - v0, pts[2] - v0) / 2.0
        if len(idx) >= 4:
            n2 = np.cross(pts[2] - v0, pts[3] - v0) / 2.0
        else:
            n2 = np.zeros(3)
        vec_areas[i] = n1 + n2
        a1, a2 = np.linalg.norm(n1), np.linalg.norm(n2)
        if a1 + a2 > _AREA_FLOOR:
            c1 = (v0 + pts[1] + pts[2]) / 3.0
            c2 = (v0 + pts[2] + pts[3]) / 3.0 if len(idx) >= 4 else np.zeros(3)
            centroids[i] = (a1 * c1 + a2 * c2) / (a1 + a2)
        else:
            centroids[i] = pts.mean(axis=0)
    return vec_areas, centroids


def _directed_edges(panel: Sequence[int]) -> list[tuple[int, int]]:
    idx = list(panel)
    edges = []
    for k in range(len(idx)):
        a, b = idx[k], idx[(k + 1) % len(idx)]
        if a != b:  # skip the collapsed edge of a triangle stored as a quad
            edges.append((a, b))
    return edges


def _consistency_flips(panels: Sequence[Sequence[int]]) -> tuple[np.ndarray, int, int, int]:
    """Flip flags making every panel agree with its neighbours.

    Returns the flags, the number of connected components, the count of
    boundary edges (shared by one panel, expected along the waterline) and the
    count of non-manifold edges (shared by more than two).
    """
    shared: dict[tuple[int, int], list[tuple[int, int]]] = defaultdict(list)
    for p, panel in enumerate(panels):
        for a, b in _directed_edges(panel):
            key = (a, b) if a < b else (b, a)
            shared[key].append((p, 1 if a < b else -1))

    adjacency: dict[int, list[tuple[int, int]]] = defaultdict(list)
    boundary = non_manifold = 0
    for key, users in shared.items():
        if len(users) == 1:
            boundary += 1
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
    return flip, components, boundary, non_manifold


def _axis_volumes(
    vec_areas: np.ndarray, centroids: np.ndarray, signs: np.ndarray
) -> tuple[float, float, float]:
    signed = vec_areas * signs[:, None]
    return tuple(float(np.sum(centroids[:, k] * signed[:, k])) for k in range(3))


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

    vec_areas, centroids = panel_vector_areas(vertices, panels)
    flip, components, boundary, non_manifold = _consistency_flips(panels)

    signs = np.where(flip, -1.0, 1.0)
    axis_v = _axis_volumes(vec_areas, centroids, signs)
    volume = float(np.mean(axis_v))
    if volume < 0.0:
        # The adjacency argument fixes relative orientation only; the sign of
        # the enclosed volume fixes the absolute direction.
        flip = ~flip
        signs = -signs
        axis_v = _axis_volumes(vec_areas, centroids, signs)
        volume = float(np.mean(axis_v))

    as_stored = _axis_volumes(vec_areas, centroids, np.ones(len(panels)))
    spread = max(as_stored) - min(as_stored)
    scale = max(abs(volume), max(abs(v) for v in as_stored), 1e-30)
    consistent = spread <= rtol * scale
    inverted = tuple(int(i) for i in np.flatnonzero(flip))
    outward = consistent and volume > 0.0 and not inverted

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
    )


def orient_outward(mesh) -> tuple[object, tuple[int, ...]]:
    """Return a copy of the mesh with every panel wound outward.

    Vertex positions are untouched; only the order in which a panel lists them
    changes. The indices of the panels that were rewound are returned so the
    caller can record exactly what the repair did.
    """
    report = orientation_report(mesh)
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


def repair_gdf_text(text: str) -> tuple[str, tuple[int, ...]]:
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

    rows = [ln for ln in body if len(ln.split()) >= 3]
    if len(rows) != npan * 4:
        raise ValueError(
            f"declared NPAN={npan} implies {npan * 4} vertex lines, found {len(rows)}"
        )

    coords = np.asarray(
        [[float(v) for v in ln.split()[:3]] for ln in rows], dtype=float
    ).reshape(npan, 4, 3)

    # Reuse the same analysis the report uses, on a flat index space where each
    # panel owns its own four vertices. Coincident vertices are merged first so
    # that panels sharing an edge are recognised as neighbours.
    flat = coords.reshape(-1, 3)
    _, first, inverse = np.unique(
        np.round(flat, 9), axis=0, return_index=True, return_inverse=True
    )
    vertices = flat[np.sort(first)]
    remap = {tuple(np.round(v, 9)): i for i, v in enumerate(vertices)}
    panels = np.asarray(
        [[remap[tuple(np.round(v, 9))] for v in quad] for quad in coords]
    )

    vec_areas, centroids = panel_vector_areas(vertices, panels)
    flip, _, _, _ = _consistency_flips(panels)
    signs = np.where(flip, -1.0, 1.0)
    if float(np.mean(_axis_volumes(vec_areas, centroids, signs))) < 0.0:
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

    return eol.join(rebuilt) + (eol if trailing else ""), flipped


def format_report(name: str, report: OrientationReport) -> str:
    """One-line human-readable verdict, for CLI and quality-gate output."""
    verdict = "PASS" if report.ok else "FAIL"
    return f"[{verdict}] {name}: {report.describe()}"
