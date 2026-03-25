"""
Mesh Data Models

Data models for panel mesh geometry and quality metrics.
"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any

import numpy as np
from numpy.typing import NDArray


class MeshFormat(Enum):
    """Supported mesh file formats."""

    GDF = "gdf"  # WAMIT geometry definition format
    DAT = "dat"  # AQWA/NEMOH panel mesh format
    STL = "stl"  # Stereolithography format
    MSH = "msh"  # Gmsh mesh format
    PNL = "pnl"  # Generic panel format
    UNKNOWN = "unknown"


@dataclass
class PanelMesh:
    """Panel mesh geometry representation.

    Attributes:
        vertices: Array of vertex coordinates [n_vertices x 3].
        panels: Array of panel vertex indices [n_panels x 4] (quads) or [n_panels x 3] (tris).
        normals: Panel normal vectors [n_panels x 3] (optional, computed if not provided).
        panel_areas: Panel areas [n_panels] (optional, computed if not provided).
        panel_centers: Panel centroids [n_panels x 3] (optional).
        name: Mesh name or identifier.
        format_origin: Original file format.
        symmetry_plane: Symmetry plane ('x', 'y', 'z', 'xy', 'xz', 'yz', or None).
        reference_point: Reference point for the mesh [x, y, z].
        metadata: Additional mesh information.
    """

    vertices: NDArray[np.float64]
    panels: NDArray[np.int32]
    normals: NDArray[np.float64] | None = None
    panel_areas: NDArray[np.float64] | None = None
    panel_centers: NDArray[np.float64] | None = None
    name: str = ""
    format_origin: MeshFormat = MeshFormat.UNKNOWN
    symmetry_plane: str | None = None
    reference_point: list[float] = field(default_factory=lambda: [0.0, 0.0, 0.0])
    metadata: dict[str, Any] = field(default_factory=dict)

    def __post_init__(self):
        """Compute derived properties if not provided."""
        if self.normals is None:
            self._compute_normals()
        if self.panel_areas is None:
            self._compute_areas()
        if self.panel_centers is None:
            self._compute_centers()

    @property
    def n_vertices(self) -> int:
        """Number of vertices in the mesh."""
        return len(self.vertices)

    @property
    def n_panels(self) -> int:
        """Number of panels in the mesh."""
        return len(self.panels)

    @property
    def is_quad_mesh(self) -> bool:
        """Check if mesh is composed of quadrilateral panels."""
        return self.panels.shape[1] == 4

    @property
    def is_tri_mesh(self) -> bool:
        """Check if mesh is composed of triangular panels."""
        return self.panels.shape[1] == 3

    @property
    def total_area(self) -> float:
        """Total surface area of the mesh."""
        if self.panel_areas is not None:
            return float(np.sum(self.panel_areas))
        return 0.0

    @property
    def bounding_box(self) -> tuple[NDArray[np.float64], NDArray[np.float64]]:
        """Get bounding box of the mesh.

        Returns:
            Tuple of (min_coords, max_coords) arrays.
        """
        return (
            np.min(self.vertices, axis=0),
            np.max(self.vertices, axis=0),
        )

    def _compute_normals(self) -> None:
        """Compute panel normal vectors."""
        normals = []
        for panel in self.panels:
            # Get valid vertex indices (skip -1 for triangles in quad format)
            valid_indices = [idx for idx in panel if idx >= 0]
            if len(valid_indices) < 3:
                normals.append(np.array([0.0, 0.0, 1.0]))
                continue

            v0 = self.vertices[valid_indices[0]]
            v1 = self.vertices[valid_indices[1]]
            v2 = self.vertices[valid_indices[2]]

            # Cross product of two edges
            edge1 = v1 - v0
            edge2 = v2 - v0
            normal = np.cross(edge1, edge2)

            # Normalize
            norm = np.linalg.norm(normal)
            if norm > 1e-10:
                normal = normal / norm
            else:
                normal = np.array([0.0, 0.0, 1.0])

            normals.append(normal)

        self.normals = np.array(normals, dtype=np.float64)

    def _compute_areas(self) -> None:
        """Compute panel areas."""
        areas = []
        for panel in self.panels:
            # Get valid vertex indices (skip -1 for triangles in quad format)
            valid_indices = [idx for idx in panel if idx >= 0]
            if len(valid_indices) < 3:
                areas.append(0.0)
                continue

            v0 = self.vertices[valid_indices[0]]
            v1 = self.vertices[valid_indices[1]]
            v2 = self.vertices[valid_indices[2]]

            # Area of triangle formed by first three vertices
            edge1 = v1 - v0
            edge2 = v2 - v0
            area = 0.5 * np.linalg.norm(np.cross(edge1, edge2))

            # If quad (4 valid vertices), add second triangle
            if len(valid_indices) == 4:
                v3 = self.vertices[valid_indices[3]]
                edge3 = v3 - v0
                area += 0.5 * np.linalg.norm(np.cross(edge2, edge3))

            areas.append(area)

        self.panel_areas = np.array(areas, dtype=np.float64)

    def _compute_centers(self) -> None:
        """Compute panel centroids."""
        centers = []
        for panel in self.panels:
            # Get valid vertex indices (skip -1 for triangles in quad format)
            valid_indices = [idx for idx in panel if idx >= 0]
            if len(valid_indices) < 3:
                centers.append(np.array([0.0, 0.0, 0.0]))
                continue
            center = np.mean(self.vertices[valid_indices], axis=0)
            centers.append(center)

        self.panel_centers = np.array(centers, dtype=np.float64)

    def flip_normals(self) -> None:
        """Flip all panel normal vectors."""
        if self.normals is not None:
            self.normals = -self.normals

    def translate(self, offset: list[float] | NDArray[np.float64]) -> None:
        """Translate mesh by given offset.

        Args:
            offset: Translation vector [dx, dy, dz].
        """
        offset_arr = np.array(offset, dtype=np.float64)
        self.vertices = self.vertices + offset_arr
        if self.panel_centers is not None:
            self.panel_centers = self.panel_centers + offset_arr

    def scale(self, factor: float) -> None:
        """Scale mesh by given factor.

        Args:
            factor: Scale factor.
        """
        self.vertices = self.vertices * factor
        if self.panel_centers is not None:
            self.panel_centers = self.panel_centers * factor
        if self.panel_areas is not None:
            self.panel_areas = self.panel_areas * (factor ** 2)


@dataclass
class MeshQualityReport:
    """Report on mesh quality metrics.

    Attributes:
        is_valid: Overall mesh validity.
        n_panels: Number of panels.
        n_vertices: Number of vertices.
        total_area: Total surface area.
        min_panel_area: Minimum panel area.
        max_panel_area: Maximum panel area.
        mean_panel_area: Mean panel area.
        aspect_ratio_max: Maximum panel aspect ratio.
        aspect_ratio_mean: Mean panel aspect ratio.
        skewness_max: Maximum panel skewness.
        n_degenerate_panels: Count of degenerate (zero-area) panels.
        n_duplicate_vertices: Count of duplicate vertex pairs.
        has_degenerate_panels: Whether mesh has degenerate (zero-area) panels.
        has_duplicate_vertices: Whether mesh has duplicate vertices.
        has_consistent_normals: Whether normals point consistently outward.
        has_inconsistent_normals: Whether normals point inconsistently (inverse of has_consistent_normals).
        quality_score: Overall quality score (0-100).
        errors: List of critical errors.
        warnings: List of warnings.
    """

    is_valid: bool = True
    n_panels: int = 0
    n_vertices: int = 0
    total_area: float = 0.0
    min_panel_area: float = 0.0
    max_panel_area: float = 0.0
    mean_panel_area: float = 0.0
    aspect_ratio_max: float = 0.0
    aspect_ratio_mean: float = 0.0
    skewness_max: float = 0.0
    n_degenerate_panels: int = 0
    n_duplicate_vertices: int = 0
    has_degenerate_panels: bool = False
    has_duplicate_vertices: bool = False
    has_consistent_normals: bool = True
    has_inconsistent_normals: bool = False
    quality_score: float = 100.0
    errors: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)

    def add_error(self, message: str) -> None:
        """Add error and mark report as invalid."""
        self.errors.append(message)
        self.is_valid = False

    def add_warning(self, message: str) -> None:
        """Add warning message."""
        self.warnings.append(message)

    def to_dict(self) -> dict[str, Any]:
        """Convert to dictionary."""
        return {
            "is_valid": self.is_valid,
            "n_panels": self.n_panels,
            "n_vertices": self.n_vertices,
            "total_area": self.total_area,
            "min_panel_area": self.min_panel_area,
            "max_panel_area": self.max_panel_area,
            "mean_panel_area": self.mean_panel_area,
            "aspect_ratio_max": self.aspect_ratio_max,
            "aspect_ratio_mean": self.aspect_ratio_mean,
            "skewness_max": self.skewness_max,
            "n_degenerate_panels": self.n_degenerate_panels,
            "n_duplicate_vertices": self.n_duplicate_vertices,
            "has_degenerate_panels": self.has_degenerate_panels,
            "has_duplicate_vertices": self.has_duplicate_vertices,
            "has_consistent_normals": self.has_consistent_normals,
            "has_inconsistent_normals": self.has_inconsistent_normals,
            "quality_score": self.quality_score,
            "errors": self.errors,
            "warnings": self.warnings,
        }
