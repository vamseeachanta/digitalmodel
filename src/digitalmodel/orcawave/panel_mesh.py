"""
ABOUTME: Panel mesh utilities for OrcaWave diffraction analysis.
Read/write WAMIT GDF panel format, compute mesh quality metrics
(aspect ratio, skewness, normal consistency), waterplane area,
displaced volume, and automatic refinement suggestions.
"""

from typing import Literal, List, Tuple

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------


class PanelQuad(BaseModel):
    """A single quadrilateral panel defined by 4 vertices."""

    vertices: List[List[float]] = Field(
        ...,
        description="4 vertices, each [x, y, z]",
        min_length=4,
        max_length=4,
    )


class MeshQualityMetrics(BaseModel):
    """Quality metrics for one panel."""

    panel_index: int
    area: float = Field(..., description="Panel area in m^2")
    aspect_ratio: float = Field(..., description="Max edge / min edge")
    skewness: float = Field(
        ...,
        description="Deviation from ideal shape, 0=perfect, 1=degenerate",
    )
    normal: List[float] = Field(..., description="Unit outward normal [nx, ny, nz]")
    normal_consistent: bool = Field(
        default=True,
        description="True if normal points outward (positive z for waterplane panels)",
    )


class MeshSummary(BaseModel):
    """Summary statistics for an entire panel mesh."""

    n_panels: int
    total_area: float
    waterplane_area: float
    displaced_volume: float
    mean_aspect_ratio: float
    max_aspect_ratio: float
    mean_skewness: float
    max_skewness: float
    n_inconsistent_normals: int
    refinement_suggestions: List[str] = Field(default_factory=list)


class GDFMesh(BaseModel):
    """A complete WAMIT GDF mesh."""

    header: str = Field(default="OrcaWave GDF mesh")
    ulen: float = Field(default=1.0, description="Characteristic length scale")
    gravity: float = Field(default=9.81)
    isx: Literal[0, 1] = Field(default=0, description="X-symmetry flag (0 or 1)")
    isy: Literal[0, 1] = Field(default=0, description="Y-symmetry flag (0 or 1)")
    panels: List[PanelQuad] = Field(default_factory=list)


# ---------------------------------------------------------------------------
# GDF I/O
# ---------------------------------------------------------------------------


def read_gdf(text: str) -> GDFMesh:
    """Parse a WAMIT GDF panel-model file.

    Standard GDF layout::

        header line
        ULEN  GRAVITY
        ISX  ISY
        NPAN
        x1 y1 z1  x2 y2 z2  x3 y3 z3  x4 y4 z4
        ...

    Args:
        text: GDF file content.

    Returns:
        Parsed GDFMesh.
    """
    lines = [line.strip() for line in text.strip().splitlines() if line.strip()]
    if len(lines) < 4:
        raise ValueError("GDF requires four header records")
    header = lines[0]

    parts1 = lines[1].split()
    ulen = float(parts1[0])
    gravity = float(parts1[1]) if len(parts1) > 1 else 9.81

    parts2 = lines[2].split()
    isx = int(parts2[0])
    isy = int(parts2[1]) if len(parts2) > 1 else 0

    npan = int(lines[3])

    if npan < 0:
        raise ValueError("NPAN must be nonnegative")
    # WAMIT 7.5 manual section 6.1: free-format vertex coordinates.
    coords = [
        float(x.replace("D", "E").replace("d", "e"))
        for line in lines[4:]
        for x in line.split()
    ]
    if len(coords) != 12 * npan or not np.all(np.isfinite(coords)):
        raise ValueError("GDF body must contain exactly 12 * NPAN finite coordinates")
    panels = [
        PanelQuad(vertices=np.array(coords[i : i + 12]).reshape(4, 3).tolist())
        for i in range(0, len(coords), 12)
    ]

    return GDFMesh(
        header=header,
        ulen=ulen,
        gravity=gravity,
        isx=isx,
        isy=isy,
        panels=panels,
    )


def write_gdf(mesh: GDFMesh) -> str:
    """Serialize a GDFMesh to WAMIT GDF format string.

    Args:
        mesh: The panel mesh to write.

    Returns:
        GDF file content.
    """
    lines = [mesh.header]
    lines.append(f" {mesh.ulen:.6f}  {mesh.gravity:.4f}")
    lines.append(f" {mesh.isx}  {mesh.isy}")
    lines.append(f" {len(mesh.panels)}")

    for panel in mesh.panels:
        coords = []
        for v in panel.vertices:
            coords.extend(v)
        lines.append("  ".join(f"{c:12.6f}" for c in coords))

    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# Geometry helpers
# ---------------------------------------------------------------------------


def _triangle_area_and_normal(
    p0: np.ndarray,
    p1: np.ndarray,
    p2: np.ndarray,
) -> Tuple[float, np.ndarray]:
    """Area and unit normal for a triangle."""
    e1 = p1 - p0
    e2 = p2 - p0
    cross = np.cross(e1, e2)
    area = 0.5 * np.linalg.norm(cross)
    normal = cross / max(np.linalg.norm(cross), 1e-30)
    return float(area), normal


def compute_panel_quality(panel: PanelQuad, panel_index: int = 0) -> MeshQualityMetrics:
    """Compute quality metrics for a single quadrilateral panel.

    Args:
        panel: The quadrilateral panel.
        panel_index: Index for identification.

    Returns:
        MeshQualityMetrics for the panel.
    """
    v = [np.array(pt) for pt in panel.vertices]

    # Area: sum of two triangles
    a1, n1 = _triangle_area_and_normal(v[0], v[1], v[2])
    a2, n2 = _triangle_area_and_normal(v[0], v[2], v[3])
    area = a1 + a2
    normal = n1 * a1 + n2 * a2
    norm_len = np.linalg.norm(normal)
    normal = normal / max(norm_len, 1e-30)

    # Edge lengths
    edges = [
        np.linalg.norm(v[1] - v[0]),
        np.linalg.norm(v[2] - v[1]),
        np.linalg.norm(v[3] - v[2]),
        np.linalg.norm(v[0] - v[3]),
    ]
    min_edge = min(edges) if min(edges) > 0 else 1e-30
    max_edge = max(edges)
    aspect_ratio = max_edge / min_edge

    # Skewness: deviation of diagonals from equal length
    d1 = np.linalg.norm(v[2] - v[0])
    d2 = np.linalg.norm(v[3] - v[1])
    max_diag = max(d1, d2, 1e-30)
    skewness = abs(d1 - d2) / max_diag

    # Normal consistency: for submerged panels, normal should point outward
    # Use heuristic: centroid below waterline should have outward normals
    centroid_z = np.mean([v_i[2] for v_i in v])
    # For waterplane panels (z ~ 0), normal should be positive z
    normal_consistent = True
    if abs(centroid_z) < 0.01:
        normal_consistent = normal[2] >= 0

    return MeshQualityMetrics(
        panel_index=panel_index,
        area=area,
        aspect_ratio=aspect_ratio,
        skewness=skewness,
        normal=normal.tolist(),
        normal_consistent=normal_consistent,
    )


def _is_closed_submerged(mesh: GDFMesh) -> bool:
    """Establish zero waterplane only below the C4-WATERLINE 1 mm band.

    Exact shared vertices and opposite directed edges are required. Unmatched
    edges on declared symmetry planes close under reflection. Other unmatched
    edges remain unqualified; no coordinate snapping hides gaps in a mesh.
    """
    if not mesh.panels or any(
        v[2] >= -0.001 for panel in mesh.panels for v in panel.vertices
    ):
        return False
    edges = {}
    for panel in mesh.panels:
        if compute_panel_quality(panel).area <= 0:
            return False
        vertices = [tuple(v) for v in panel.vertices]
        for a, b in zip(vertices, vertices[1:] + vertices[:1]):
            if a != b:  # Triangles may repeat their final vertex in GDF.
                edges[(a, b)] = edges.get((a, b), 0) + 1
    return bool(edges) and all(
        count == 1 and (
            edges.get((b, a), 0) == 1
            or (edges.get((b, a), 0) == 0 and (
                (mesh.isx == 1 and a[0] == b[0] == 0)
                or (mesh.isy == 1 and a[1] == b[1] == 0)
            ))
        )
        for (a, b), count in edges.items()
    )


def compute_mesh_summary(mesh: GDFMesh) -> MeshSummary:
    """Compute summary quality metrics for an entire mesh.

    Args:
        mesh: The GDF mesh.

    Returns:
        MeshSummary with overall statistics and refinement suggestions.
    """
    metrics = [compute_panel_quality(p, i) for i, p in enumerate(mesh.panels)]

    if not metrics:
        return MeshSummary(
            n_panels=0,
            total_area=0.0,
            waterplane_area=0.0,
            displaced_volume=0.0,
            mean_aspect_ratio=0.0,
            max_aspect_ratio=0.0,
            mean_skewness=0.0,
            max_skewness=0.0,
            n_inconsistent_normals=0,
            refinement_suggestions=["Nonpositive signed volume: empty mesh"],
        )

    areas = np.array([m.area for m in metrics])
    aspect_ratios = np.array([m.aspect_ratio for m in metrics])
    skewnesses = np.array([m.skewness for m in metrics])

    # Divergence theorem with F=(0,0,z): div(F)=1, V=integral(z*nz dS).
    # Integrate triangles exactly; retain the sign to expose reversed normals.
    vol = 0.0
    waterline_integral = 0.0
    for panel in mesh.panels:
        verts = np.asarray(panel.vertices, dtype=float)
        for ids in ((0, 1, 2), (0, 2, 3)):
            tri = verts[list(ids)]
            area_vector = 0.5 * np.cross(tri[1] - tri[0], tri[2] - tri[0])
            vol += tri[:, 2].mean() * area_vector[2]
        # Green's area theorem: the hull top boundary runs opposite the lid.
        # Exclude lids to avoid double counting. Axis closure edges contribute 0.
        if not np.all(np.isclose(verts[:, 2], 0.0, atol=1e-9, rtol=0)):
            for v0, v1 in zip(verts, np.roll(verts, -1, axis=0)):
                if np.all(np.isclose([v0[2], v1[2]], 0.0, atol=1e-9, rtol=0)):
                    waterline_integral -= 0.5 * (v0[0] * v1[1] - v1[0] * v0[1])
    wp_area = abs(waterline_integral)
    displaced_volume = float(vol)

    # Account for symmetry
    sym_factor = 1
    if mesh.isx:
        sym_factor *= 2
    if mesh.isy:
        sym_factor *= 2
    wp_area *= sym_factor
    displaced_volume *= sym_factor

    n_inconsistent = sum(1 for m in metrics if not m.normal_consistent)

    # Refinement suggestions
    suggestions: List[str] = []
    if wp_area <= 0 and not _is_closed_submerged(mesh):
        suggestions.append(
            "No waterline boundary at z=0: waterplane area is not established"
        )
    if displaced_volume <= 0:
        suggestions.append(
            "Nonpositive signed volume: check outward normals and hull closure"
        )
    if float(aspect_ratios.max()) > 5.0:
        bad = [m.panel_index for m in metrics if m.aspect_ratio > 5.0]
        suggestions.append(
            f"High aspect ratio (>5.0) on {len(bad)} panels: consider splitting"
        )
    if float(skewnesses.max()) > 0.5:
        bad = [m.panel_index for m in metrics if m.skewness > 0.5]
        suggestions.append(
            f"High skewness (>0.5) on {len(bad)} panels: consider reshaping"
        )
    if n_inconsistent > 0:
        suggestions.append(
            f"{n_inconsistent} panels have inconsistent normals: check orientation"
        )
    if len(mesh.panels) < 100:
        suggestions.append(
            "Mesh has fewer than 100 panels: may be too coarse for accurate results"
        )
    if len(mesh.panels) > 5000:
        suggestions.append(
            "Mesh has more than 5000 panels: consider coarsening for efficiency"
        )

    return MeshSummary(
        n_panels=len(mesh.panels),
        total_area=float(areas.sum()) * sym_factor,
        waterplane_area=wp_area,
        displaced_volume=displaced_volume,
        mean_aspect_ratio=float(aspect_ratios.mean()),
        max_aspect_ratio=float(aspect_ratios.max()),
        mean_skewness=float(skewnesses.mean()),
        max_skewness=float(skewnesses.max()),
        n_inconsistent_normals=n_inconsistent,
        refinement_suggestions=suggestions,
    )


def compute_waterplane_area(mesh: GDFMesh) -> float:
    """Calculate waterplane area from oriented waterline edges.

    Args:
        mesh: Panel mesh.

    Returns:
        Waterplane area in m^2 (accounting for symmetry). A closed surface
        wholly below z=-1 mm has zero area (including declared symmetry
        closure); an unavailable waterline raises.
    """
    summary = compute_mesh_summary(mesh)
    if summary.waterplane_area <= 0 and not _is_closed_submerged(mesh):
        raise ValueError("Positive waterplane area requires a waterline boundary at z=0")
    return summary.waterplane_area


def compute_displaced_volume(mesh: GDFMesh) -> float:
    """Calculate displaced volume via the divergence theorem.

    Args:
        mesh: Panel mesh.

    Returns:
        Displaced volume in m^3 (accounting for symmetry).
    """
    summary = compute_mesh_summary(mesh)
    if summary.displaced_volume <= 0:
        raise ValueError("Positive signed volume requires outward normals and hull closure")
    return summary.displaced_volume
