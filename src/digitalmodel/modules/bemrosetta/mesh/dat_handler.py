"""AQWA/NEMOH DAT mesh format handler.

DAT format is used by AQWA and NEMOH for defining panel meshes.
NEMOH format structure:
- Line 1: NVERT NPAN (number of vertices and panels)
- Lines 2 to NVERT+1: X Y Z (vertex coordinates)
- Lines NVERT+2 to end: V1 V2 V3 V4 (panel vertex indices, 1-based)
- Final line: 0 0 0 0 (terminator)

AQWA format may have slight variations but follows similar structure.
"""

from pathlib import Path
from typing import List

import numpy as np
from loguru import logger

from .mesh_handler import BaseMeshHandler
from ..core.exceptions import MeshError
from ..models import PanelMesh, MeshFormat


class DATHandler(BaseMeshHandler):
    """Handler for AQWA/NEMOH DAT mesh format."""

    @property
    def format_name(self) -> str:
        """Return format name."""
        return "DAT"

    @property
    def supported_extensions(self) -> List[str]:
        """Return supported file extensions."""
        return [".dat", ".DAT"]

    def read(self, file_path: Path) -> PanelMesh:
        """Read DAT mesh file.

        Args:
            file_path: Path to DAT file.

        Returns:
            PanelMesh containing the mesh data.

        Raises:
            MeshError: If file not found or parsing fails.
        """
        file_path = Path(file_path)
        if not file_path.exists():
            raise MeshError(f"File not found: {file_path}", mesh_file=str(file_path))

        logger.info(f"Reading DAT file: {file_path}")

        with open(file_path, 'r') as f:
            lines = f.readlines()

        return self._parse_dat(lines, file_path)

    def _parse_dat(self, lines: List[str], file_path: Path) -> PanelMesh:
        """Parse DAT file content.

        Args:
            lines: Lines from DAT file.
            file_path: Path for error reporting.

        Returns:
            PanelMesh with parsed data.

        Raises:
            MeshError: If parsing fails.
        """
        # Skip empty lines and comments at start
        line_idx = 0
        while line_idx < len(lines):
            line = lines[line_idx].strip()
            if line and not line.startswith('#') and not line.startswith('!'):
                break
            line_idx += 1

        if line_idx >= len(lines):
            raise MeshError(
                "Invalid DAT file: no data found",
                mesh_file=str(file_path)
            )

        # Read header: NVERT NPAN
        try:
            parts = lines[line_idx].split()
            nvert = int(parts[0])
            npan = int(parts[1]) if len(parts) > 1 else 0
            line_idx += 1
        except (IndexError, ValueError) as e:
            raise MeshError(
                f"Invalid DAT header line: {lines[line_idx]}",
                mesh_file=str(file_path)
            ) from e

        # Read vertices
        vertices = []
        for i in range(nvert):
            if line_idx >= len(lines):
                raise MeshError(
                    f"Unexpected end of file while reading vertices (expected {nvert})",
                    mesh_file=str(file_path)
                )

            line = lines[line_idx].strip()
            if not line:
                line_idx += 1
                continue

            try:
                parts = line.split()
                x, y, z = float(parts[0]), float(parts[1]), float(parts[2])
                vertices.append([x, y, z])
            except (IndexError, ValueError) as e:
                raise MeshError(
                    f"Invalid vertex line {line_idx + 1}: {line}",
                    mesh_file=str(file_path)
                ) from e
            line_idx += 1

        # Read panels (1-based indices in file, convert to 0-based)
        panels = []
        while line_idx < len(lines):
            line = lines[line_idx].strip()
            if not line:
                line_idx += 1
                continue

            try:
                parts = line.split()
                if len(parts) < 3:
                    line_idx += 1
                    continue

                # Check for terminator line (0 0 0 0)
                indices = [int(p) for p in parts[:4] if p.lstrip('-').isdigit()]
                if len(indices) >= 3 and all(i == 0 for i in indices):
                    break

                # Convert 1-based to 0-based indices
                panel_indices = []
                for idx in indices[:4]:
                    if idx > 0:
                        panel_indices.append(idx - 1)  # Convert to 0-based
                    else:
                        panel_indices.append(-1)  # Invalid index marker

                # Pad to 4 vertices if needed
                while len(panel_indices) < 4:
                    panel_indices.append(-1)

                panels.append(panel_indices)
            except ValueError:
                pass  # Skip malformed lines

            line_idx += 1

        if not vertices:
            raise MeshError(
                "No vertices found in DAT file",
                mesh_file=str(file_path)
            )

        if not panels:
            raise MeshError(
                "No panels found in DAT file",
                mesh_file=str(file_path)
            )

        logger.info(f"Parsed DAT: {len(vertices)} vertices, {len(panels)} panels")

        return PanelMesh(
            vertices=np.array(vertices, dtype=np.float64),
            panels=np.array(panels, dtype=np.int32),
            format_origin=MeshFormat.DAT,
            name=file_path.stem,
        )

    def write(self, mesh: PanelMesh, file_path: Path) -> Path:
        """Write mesh to DAT format (NEMOH style).

        Args:
            mesh: PanelMesh to write.
            file_path: Output file path.

        Returns:
            Path to written file.
        """
        file_path = Path(file_path)

        logger.info(f"Writing DAT file: {file_path}")

        with open(file_path, 'w') as f:
            # Header: NVERT NPAN
            f.write(f"    {mesh.n_vertices}     {mesh.n_panels}\n")

            # Vertices
            for vertex in mesh.vertices:
                f.write(f"    {vertex[0]:.6f}     {vertex[1]:.6f}     {vertex[2]:.6f}\n")

            # Panels (1-based indices)
            for panel in mesh.panels:
                # Convert 0-based to 1-based, handle -1 as 0
                indices = []
                for idx in panel:
                    if idx >= 0:
                        indices.append(idx + 1)  # Convert to 1-based
                    else:
                        indices.append(0)  # Invalid marker

                # Pad to 4 indices
                while len(indices) < 4:
                    indices.append(0)

                f.write(f"    {indices[0]}    {indices[1]}    {indices[2]}    {indices[3]}\n")

            # Terminator
            f.write("    0    0    0    0\n")

        return file_path
