"""Base mesh handler with common operations.

Provides the abstract base class for mesh format handlers and common
mesh validation and quality assessment functionality.
"""

from abc import abstractmethod
from pathlib import Path
from typing import List

import numpy as np
from loguru import logger

from ..core.interfaces import MeshHandlerInterface
from ..core.exceptions import MeshError
from ..models import PanelMesh, MeshQualityReport


class BaseMeshHandler(MeshHandlerInterface):
    """Base class for mesh format handlers.

    Provides common mesh validation and quality assessment functionality.
    Subclasses must implement read, write, format_name, and file_extension.
    """

    @property
    @abstractmethod
    def format_name(self) -> str:
        """Name of the mesh format this handler supports."""

    @property
    def file_extension(self) -> str:
        """Primary file extension for this format."""
        extensions = self.supported_extensions
        return extensions[0] if extensions else ""

    @property
    @abstractmethod
    def supported_extensions(self) -> List[str]:
        """List of file extensions this handler supports."""

    @abstractmethod
    def read(self, file_path: Path) -> PanelMesh:
        """Read mesh data from file."""

    @abstractmethod
    def write(self, mesh: PanelMesh, file_path: Path) -> Path:
        """Write mesh data to file."""

    def validate_mesh(self, mesh: PanelMesh) -> MeshQualityReport:
        """Validate mesh quality and return report.

        Args:
            mesh: PanelMesh to validate.

        Returns:
            MeshQualityReport with quality metrics and warnings.
        """
        warnings = []

        # Check for degenerate panels
        n_degenerate = self._count_degenerate_panels(mesh)
        if n_degenerate > 0:
            warnings.append(f"{n_degenerate} degenerate panels found")

        # Check for duplicate vertices
        n_duplicates = self._count_duplicate_vertices(mesh)
        if n_duplicates > 0:
            warnings.append(f"{n_duplicates} duplicate vertices found")

        # Calculate aspect ratios
        aspect_ratio_max = self._calculate_max_aspect_ratio(mesh)
        if aspect_ratio_max > 10:
            warnings.append(f"High aspect ratio panels: {aspect_ratio_max:.1f}")

        # Check normal consistency
        has_consistent_normals = self._check_normal_consistency(mesh)
        if not has_consistent_normals:
            warnings.append("Inconsistent panel normals detected")

        # Calculate quality score (0-100)
        quality_score = self._calculate_quality_score(
            n_degenerate, n_duplicates, aspect_ratio_max, has_consistent_normals, mesh.n_panels
        )

        return MeshQualityReport(
            n_panels=mesh.n_panels,
            n_vertices=mesh.n_vertices,
            total_area=mesh.total_area,
            min_panel_area=float(np.min(mesh.panel_areas)) if mesh.n_panels > 0 else 0.0,
            max_panel_area=float(np.max(mesh.panel_areas)) if mesh.n_panels > 0 else 0.0,
            mean_panel_area=float(np.mean(mesh.panel_areas)) if mesh.n_panels > 0 else 0.0,
            aspect_ratio_max=aspect_ratio_max,
            n_degenerate_panels=n_degenerate,
            n_duplicate_vertices=n_duplicates,
            has_consistent_normals=has_consistent_normals,
            quality_score=quality_score,
            warnings=warnings,
        )

    def _count_degenerate_panels(self, mesh: PanelMesh) -> int:
        """Count panels with zero or near-zero area.

        Args:
            mesh: PanelMesh to analyze.

        Returns:
            Number of degenerate panels.
        """
        threshold = 1e-10
        return int(np.sum(mesh.panel_areas < threshold))

    def _count_duplicate_vertices(self, mesh: PanelMesh, tolerance: float = 1e-6) -> int:
        """Count duplicate vertices within tolerance.

        Args:
            mesh: PanelMesh to analyze.
            tolerance: Distance threshold for considering vertices duplicate.

        Returns:
            Number of duplicate vertex pairs.
        """
        try:
            from scipy.spatial import KDTree
            tree = KDTree(mesh.vertices)
            pairs = tree.query_pairs(tolerance)
            return len(pairs)
        except ImportError:
            # Fallback if scipy not available - O(n^2) brute force
            logger.warning("scipy not available, using slow duplicate detection")
            n_duplicates = 0
            for i in range(len(mesh.vertices)):
                for j in range(i + 1, len(mesh.vertices)):
                    if np.linalg.norm(mesh.vertices[i] - mesh.vertices[j]) < tolerance:
                        n_duplicates += 1
            return n_duplicates

    def _calculate_max_aspect_ratio(self, mesh: PanelMesh) -> float:
        """Calculate maximum panel aspect ratio.

        Aspect ratio is defined as the ratio of longest to shortest edge.

        Args:
            mesh: PanelMesh to analyze.

        Returns:
            Maximum aspect ratio across all panels.
        """
        max_ratio = 1.0

        for panel in mesh.panels:
            v_indices = [idx for idx in panel if idx >= 0]
            if len(v_indices) < 3:
                continue

            pts = mesh.vertices[v_indices]

            # Calculate edge lengths
            edges = []
            for j in range(len(pts)):
                edge = np.linalg.norm(pts[(j + 1) % len(pts)] - pts[j])
                edges.append(edge)

            min_edge = min(edges)
            if min_edge > 1e-10:
                ratio = max(edges) / min_edge
                max_ratio = max(max_ratio, ratio)

        return max_ratio

    def _check_normal_consistency(self, mesh: PanelMesh) -> bool:
        """Check if all normals point consistently outward.

        Uses a simplified heuristic: assumes the mesh center is inside,
        so normals should point away from the center.

        Args:
            mesh: PanelMesh to analyze.

        Returns:
            True if normals are consistent, False otherwise.
        """
        if mesh.n_panels == 0:
            return True

        # Calculate center of mesh
        center = np.mean(mesh.vertices, axis=0)

        inconsistent_count = 0
        for i, panel_center in enumerate(mesh.panel_centers):
            # Vector from mesh center to panel center
            to_panel = panel_center - center

            # Check if normal points outward (dot product > 0)
            dot = np.dot(mesh.normals[i], to_panel)
            # Allow some inconsistency for complex geometries
            if dot < -0.1:
                inconsistent_count += 1

        # Allow up to 10% inconsistent normals for complex meshes
        return inconsistent_count < max(1, mesh.n_panels * 0.1)

    def _calculate_quality_score(
        self,
        n_degenerate: int,
        n_duplicates: int,
        aspect_ratio: float,
        consistent_normals: bool,
        n_panels: int
    ) -> float:
        """Calculate overall mesh quality score (0-100).

        Args:
            n_degenerate: Number of degenerate panels.
            n_duplicates: Number of duplicate vertex pairs.
            aspect_ratio: Maximum panel aspect ratio.
            consistent_normals: Whether normals are consistent.
            n_panels: Total number of panels.

        Returns:
            Quality score from 0 (worst) to 100 (best).
        """
        score = 100.0

        # Penalize degenerate panels
        if n_panels > 0:
            score -= min(30, (n_degenerate / n_panels) * 100)

        # Penalize duplicates
        score -= min(20, n_duplicates * 2)

        # Penalize high aspect ratio
        if aspect_ratio > 5:
            score -= min(20, (aspect_ratio - 5) * 2)

        # Penalize inconsistent normals
        if not consistent_normals:
            score -= 20

        return max(0, score)


def convert_mesh(
    input_file: Path,
    output_file: Path,
    input_format: str | None = None,
    output_format: str | None = None
) -> Path:
    """Convert mesh between formats.

    Args:
        input_file: Path to input mesh file.
        output_file: Path for output mesh file.
        input_format: Override input format detection (gdf, dat, stl).
        output_format: Override output format detection (gdf, dat, stl).

    Returns:
        Path to the written output file.

    Raises:
        MeshError: If format is unsupported or file operations fail.
    """
    from .gdf_handler import GDFHandler
    from .dat_handler import DATHandler
    from .stl_handler import STLHandler

    input_file = Path(input_file)
    output_file = Path(output_file)

    # Detect input format
    if input_format is None:
        input_format = input_file.suffix.lower().lstrip('.')

    # Detect output format
    if output_format is None:
        output_format = output_file.suffix.lower().lstrip('.')

    # Get handlers
    handlers = {
        'gdf': GDFHandler,
        'dat': DATHandler,
        'stl': STLHandler,
    }

    if input_format not in handlers:
        raise MeshError(f"Unsupported input format: {input_format}")
    if output_format not in handlers:
        raise MeshError(f"Unsupported output format: {output_format}")

    # Read and write
    input_handler = handlers[input_format]()
    output_handler = handlers[output_format]()

    logger.info(f"Converting {input_file} ({input_format}) -> {output_file} ({output_format})")

    mesh = input_handler.read(input_file)
    return output_handler.write(mesh, output_file)
