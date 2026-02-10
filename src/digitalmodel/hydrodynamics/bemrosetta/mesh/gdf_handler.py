"""WAMIT GDF mesh format handler.

GDF (Geometry Definition Format) is used by WAMIT for defining panel meshes.
File structure:
- Line 1: Header comment (optional, starting with #)
- Line 2: ULEN GRAV (unit length and gravity)
- Line 3: ISX ISY (symmetry flags: 0=none, 1=xz symmetry)
- Line 4: NPAN (number of panels)
- Following lines: 4 vertices per panel (X Y Z for each)
"""

from pathlib import Path
from typing import List

import numpy as np
from loguru import logger

from .mesh_handler import BaseMeshHandler
from ..core.exceptions import MeshError
from ..models import PanelMesh, MeshFormat


class GDFHandler(BaseMeshHandler):
    """Handler for WAMIT GDF mesh format."""

    @property
    def format_name(self) -> str:
        """Return format name."""
        return "GDF"

    @property
    def supported_extensions(self) -> List[str]:
        """Return supported file extensions."""
        return [".gdf", ".GDF"]

    def read(self, file_path: Path) -> PanelMesh:
        """Read GDF mesh file.

        Args:
            file_path: Path to GDF file.

        Returns:
            PanelMesh containing the mesh data.

        Raises:
            MeshError: If file not found or parsing fails.
        """
        file_path = Path(file_path)
        if not file_path.exists():
            raise MeshError(f"File not found: {file_path}", mesh_file=str(file_path))

        logger.info(f"Reading GDF file: {file_path}")

        with open(file_path, 'r') as f:
            lines = f.readlines()

        return self._parse_gdf(lines, file_path)

    def _parse_gdf(self, lines: List[str], file_path: Path) -> PanelMesh:
        """Parse GDF file content.

        Args:
            lines: Lines from GDF file.
            file_path: Path for error reporting.

        Returns:
            PanelMesh with parsed data.

        Raises:
            MeshError: If parsing fails.
        """
        line_idx = 0

        # Line 1 is always a title/header line in GDF format.
        # Skip it plus any additional comment lines starting with #.
        if line_idx < len(lines):
            line_idx += 1  # skip title line
        while line_idx < len(lines) and lines[line_idx].strip().startswith('#'):
            line_idx += 1

        if line_idx >= len(lines):
            raise MeshError(
                "Invalid GDF file: no data after header",
                mesh_file=str(file_path)
            )

        # Read ULEN, GRAV (line 2)
        try:
            parts = lines[line_idx].split()
            # ulen = float(parts[0]) if len(parts) > 0 else 1.0  # Not used currently
            # grav = float(parts[1]) if len(parts) > 1 else 9.81  # Not used currently
            line_idx += 1
        except (IndexError, ValueError) as e:
            raise MeshError(
                f"Invalid ULEN/GRAV line: {lines[line_idx]}",
                mesh_file=str(file_path)
            ) from e

        # Read symmetry flags (line 3)
        try:
            parts = lines[line_idx].split()
            isx = int(parts[0]) if len(parts) > 0 else 0
            isy = int(parts[1]) if len(parts) > 1 else 0
            line_idx += 1
        except (IndexError, ValueError) as e:
            raise MeshError(
                f"Invalid symmetry flags line: {lines[line_idx]}",
                mesh_file=str(file_path)
            ) from e

        # Determine symmetry plane
        symmetry_plane = None
        if isx == 1 and isy == 0:
            symmetry_plane = 'xz'
        elif isx == 0 and isy == 1:
            symmetry_plane = 'yz'
        elif isx == 1 and isy == 1:
            symmetry_plane = 'xy'

        # Read number of panels (line 4)
        try:
            npan = int(lines[line_idx].split()[0])
            line_idx += 1
        except (IndexError, ValueError) as e:
            raise MeshError(
                f"Invalid panel count line: {lines[line_idx]}",
                mesh_file=str(file_path)
            ) from e

        # Read panel vertices
        vertices = []
        panels = []
        vertex_map = {}

        for panel_idx in range(npan):
            panel_vertices = []
            for _ in range(4):
                if line_idx >= len(lines):
                    break
                line = lines[line_idx].strip()
                if not line:
                    line_idx += 1
                    continue
                parts = line.split()
                if len(parts) >= 3:
                    try:
                        vertex = (float(parts[0]), float(parts[1]), float(parts[2]))

                        # Check for existing vertex (deduplication)
                        if vertex not in vertex_map:
                            vertex_map[vertex] = len(vertices)
                            vertices.append(vertex)

                        panel_vertices.append(vertex_map[vertex])
                    except ValueError:
                        pass  # Skip malformed lines
                line_idx += 1

            if len(panel_vertices) == 4:
                panels.append(panel_vertices)

        if not vertices or not panels:
            raise MeshError(
                "No valid panels found in GDF file",
                mesh_file=str(file_path)
            )

        logger.info(f"Parsed GDF: {len(vertices)} vertices, {len(panels)} panels")

        return PanelMesh(
            vertices=np.array(vertices, dtype=np.float64),
            panels=np.array(panels, dtype=np.int32),
            symmetry_plane=symmetry_plane,
            format_origin=MeshFormat.GDF,
            name=file_path.stem,
        )

    def write(self, mesh: PanelMesh, file_path: Path) -> Path:
        """Write mesh to GDF format.

        Args:
            mesh: PanelMesh to write.
            file_path: Output file path.

        Returns:
            Path to written file.
        """
        file_path = Path(file_path)

        logger.info(f"Writing GDF file: {file_path}")

        # Determine symmetry flags from mesh
        isx, isy = 0, 0
        if mesh.symmetry_plane:
            if 'x' in mesh.symmetry_plane.lower():
                isx = 1
            if 'y' in mesh.symmetry_plane.lower():
                isy = 1

        with open(file_path, 'w') as f:
            # Header comment
            f.write("# GDF file generated by BEMRosetta module\n")

            # ULEN (unit length) and GRAV (gravity)
            f.write("1.0  9.81\n")

            # Symmetry flags
            f.write(f"{isx}  {isy}\n")

            # Number of panels
            f.write(f"{mesh.n_panels}\n")

            # Panel vertices (4 per panel)
            for panel in mesh.panels:
                valid_indices = [idx for idx in panel if idx >= 0]
                for i in range(4):
                    if i < len(valid_indices):
                        v = mesh.vertices[valid_indices[i]]
                    else:
                        # Repeat last vertex for triangles
                        v = mesh.vertices[valid_indices[-1]]
                    f.write(f"{v[0]:.6f}  {v[1]:.6f}  {v[2]:.6f}\n")

        return file_path
