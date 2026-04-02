"""
ABOUTME: Panel mesh utilities for OrcaWave diffraction analysis.
Read/write WAMIT GDF panel format, compute mesh quality metrics
(aspect ratio, skewness, normal consistency), waterplane area,
displaced volume, and automatic refinement suggestions.
"""

import io
from typing import Dict, List, Optional, Tuple

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
    isx: int = Field(default=1, description="X-symmetry flag (0 or 1)")
    isy: int = Field(default=1, description="Y-symmetry flag (0 or 1)")
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
    lines = [l.strip() for l in text.strip().splitlines() if l.strip()]
    header = lines[0]

    parts1 = lines[1].split()
    ulen = float(parts1[0])
    gravity = float(parts1[1]) if len(parts1) > 1 else 9.81

    parts2 = lines[2].split()
    isx = int(parts2[0])
    isy = int(parts2[1]) if len(parts2) > 1 else 0

    npan = int(lines[3])

    panels: List[PanelQuad] = []
    for i in range(4, 4 + npan):
        if i >= len(lines):
            break
        coords = [float(x) for x in lines[i].split()]
        # 12 values: 4 vertices x 3 coords
        if len(coords) >= 12:
            verts = [
                coords[0:3],
                coords[3:6],
                coords[6:9],
                coords[9:12],
            ]
            panels.append(PanelQuad(vertices=verts))

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
    normal = (n1 * a1 + n2 * a2)
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


def compute_mesh_summary(mesh: GDFMesh) -> MeshSummary:
    """Compute summary quality metrics for an entire mesh.

    Args:
        mesh: The GDF mesh.

    Returns:
        MeshSummary with overall statistics and refinement suggestions.
    """
    metrics = [
        compute_panel_quality(p, i) for i, p in enumerate(mesh.panels)
    ]

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
        )

    areas = np.array([m.area for m in metrics])
    aspect_ratios = np.array([m.aspect_ratio for m in metrics])
    skewnesses = np.array([m.skewness for m in metrics])

    # Waterplane area: panels near z=0
    wp_area = 0.0
    for i, panel in enumerate(mesh.panels):
        verts = np.array(panel.vertices)
        if np.all(np.abs(verts[:, 2]) < 0.05):
            wp_area += metrics[i].area

    # Displaced volume via divergence theorem:
    # V = (1/3) * sum_panels( panel_area * centroid_z * nz )
    # For submerged panels (z < 0)
    vol = 0.0
    for i, panel in enumerate(mesh.panels):
        verts = np.array(panel.vertices)
        centroid = verts.mean(axis=0)
        nz = metrics[i].normal[2]
        vol += metrics[i].area * centroid[2] * nz
    displaced_volume = abs(vol / 3.0)

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
    """Calculate the waterplane area from panels at z ~ 0.

    Args:
        mesh: Panel mesh.

    Returns:
        Waterplane area in m^2 (accounting for symmetry).
    """
    summary = compute_mesh_summary(mesh)
    return summary.waterplane_area


def compute_displaced_volume(mesh: GDFMesh) -> float:
    """Calculate displaced volume via the divergence theorem.

    Args:
        mesh: Panel mesh.

    Returns:
        Displaced volume in m^3 (accounting for symmetry).
    """
    summary = compute_mesh_summary(mesh)
    return summary.displaced_volume
