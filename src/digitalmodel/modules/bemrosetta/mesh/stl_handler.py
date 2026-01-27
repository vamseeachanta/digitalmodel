"""STL mesh format handler.

STL (Stereolithography) format supports both ASCII and binary variants.
Used for visualization and 3D printing. Contains only triangular facets.

ASCII format:
    solid name
      facet normal nx ny nz
        outer loop
          vertex x y z
          vertex x y z
          vertex x y z
        endloop
      endfacet
    endsolid name

Binary format:
    80 bytes header
    4 bytes: number of triangles (uint32)
    For each triangle:
        12 bytes: normal vector (3 x float32)
        36 bytes: 3 vertices (9 x float32)
        2 bytes: attribute byte count (uint16)
"""

from pathlib import Path
from typing import List
import struct

import numpy as np
from loguru import logger

from .mesh_handler import BaseMeshHandler
from ..core.exceptions import MeshError
from ..models import PanelMesh, MeshFormat


class STLHandler(BaseMeshHandler):
    """Handler for STL mesh format (ASCII and binary)."""

    @property
    def format_name(self) -> str:
        """Return format name."""
        return "STL"

    @property
    def supported_extensions(self) -> List[str]:
        """Return supported file extensions."""
        return [".stl", ".STL"]

    def read(self, file_path: Path) -> PanelMesh:
        """Read STL mesh file (auto-detect ASCII vs binary).

        Args:
            file_path: Path to STL file.

        Returns:
            PanelMesh containing the mesh data.

        Raises:
            MeshError: If file not found or parsing fails.
        """
        file_path = Path(file_path)
        if not file_path.exists():
            raise MeshError(f"File not found: {file_path}", mesh_file=str(file_path))

        # Detect ASCII vs binary
        is_ascii = self._is_ascii_stl(file_path)

        if is_ascii:
            logger.info(f"Reading ASCII STL file: {file_path}")
            return self._read_ascii_stl(file_path)
        else:
            logger.info(f"Reading binary STL file: {file_path}")
            return self._read_binary_stl(file_path)

    def _is_ascii_stl(self, file_path: Path) -> bool:
        """Detect if STL file is ASCII or binary.

        Args:
            file_path: Path to STL file.

        Returns:
            True if ASCII, False if binary.
        """
        with open(file_path, 'rb') as f:
            # Read first 80 bytes (header)
            header = f.read(80)

            # ASCII files typically start with 'solid'
            if header.startswith(b'solid'):
                # But binary files can also start with 'solid' in the header
                # Check for actual ASCII content by looking for 'facet' keyword
                f.seek(0)
                try:
                    content = f.read(1000).decode('ascii')
                    if 'facet' in content.lower() and 'vertex' in content.lower():
                        return True
                except UnicodeDecodeError:
                    return False
            return False

    def _read_ascii_stl(self, file_path: Path) -> PanelMesh:
        """Read ASCII STL file.

        Args:
            file_path: Path to STL file.

        Returns:
            PanelMesh with triangular panels.
        """
        with open(file_path, 'r') as f:
            lines = f.readlines()

        vertices = []
        panels = []
        vertex_map = {}
        current_panel = []

        for line in lines:
            line = line.strip().lower()

            if line.startswith('vertex'):
                parts = line.split()
                if len(parts) >= 4:
                    try:
                        vertex = (float(parts[1]), float(parts[2]), float(parts[3]))

                        # Deduplicate vertices
                        if vertex not in vertex_map:
                            vertex_map[vertex] = len(vertices)
                            vertices.append(vertex)

                        current_panel.append(vertex_map[vertex])
                    except ValueError:
                        pass  # Skip malformed lines

            elif line.startswith('endfacet'):
                if len(current_panel) == 3:
                    # STL is triangular, pad to quad format with -1
                    panels.append(current_panel + [-1])
                current_panel = []

        if not vertices or not panels:
            raise MeshError(
                "No valid triangles found in ASCII STL file",
                mesh_file=str(file_path)
            )

        logger.info(f"Parsed ASCII STL: {len(vertices)} vertices, {len(panels)} triangles")

        return PanelMesh(
            vertices=np.array(vertices, dtype=np.float64),
            panels=np.array(panels, dtype=np.int32),
            format_origin=MeshFormat.STL,
            name=file_path.stem,
        )

    def _read_binary_stl(self, file_path: Path) -> PanelMesh:
        """Read binary STL file.

        Args:
            file_path: Path to STL file.

        Returns:
            PanelMesh with triangular panels.
        """
        with open(file_path, 'rb') as f:
            # Skip header (80 bytes)
            f.read(80)

            # Read number of triangles
            n_triangles = struct.unpack('I', f.read(4))[0]

            vertices = []
            panels = []
            vertex_map = {}

            for _ in range(n_triangles):
                # Normal vector (skip, we'll compute our own)
                f.read(12)

                # 3 vertices
                current_panel = []
                for _ in range(3):
                    data = f.read(12)
                    if len(data) < 12:
                        break
                    x, y, z = struct.unpack('fff', data)
                    vertex = (x, y, z)

                    # Deduplicate vertices
                    if vertex not in vertex_map:
                        vertex_map[vertex] = len(vertices)
                        vertices.append(vertex)

                    current_panel.append(vertex_map[vertex])

                if len(current_panel) == 3:
                    # Pad to quad format with -1
                    panels.append(current_panel + [-1])

                # Attribute byte count (skip)
                f.read(2)

        if not vertices or not panels:
            raise MeshError(
                "No valid triangles found in binary STL file",
                mesh_file=str(file_path)
            )

        logger.info(f"Parsed binary STL: {len(vertices)} vertices, {len(panels)} triangles")

        return PanelMesh(
            vertices=np.array(vertices, dtype=np.float64),
            panels=np.array(panels, dtype=np.int32),
            format_origin=MeshFormat.STL,
            name=file_path.stem,
        )

    def write(self, mesh: PanelMesh, file_path: Path, binary: bool = False) -> Path:
        """Write mesh to STL format.

        Args:
            mesh: PanelMesh to write.
            file_path: Output file path.
            binary: If True, write binary STL. Otherwise write ASCII.

        Returns:
            Path to written file.
        """
        file_path = Path(file_path)

        if binary:
            return self._write_binary_stl(mesh, file_path)
        else:
            return self._write_ascii_stl(mesh, file_path)

    def _write_ascii_stl(self, mesh: PanelMesh, file_path: Path) -> Path:
        """Write ASCII STL file.

        Args:
            mesh: PanelMesh to write.
            file_path: Output file path.

        Returns:
            Path to written file.
        """
        logger.info(f"Writing ASCII STL file: {file_path}")

        with open(file_path, 'w') as f:
            f.write("solid mesh\n")

            for i, panel in enumerate(mesh.panels):
                v_indices = [idx for idx in panel if idx >= 0]
                if len(v_indices) < 3:
                    continue

                normal = mesh.normals[i]

                # Write triangles (split quads into triangles)
                for j in range(len(v_indices) - 2):
                    f.write(f"  facet normal {normal[0]:.6e} {normal[1]:.6e} {normal[2]:.6e}\n")
                    f.write("    outer loop\n")

                    # Triangle fan from first vertex
                    for k in [0, j + 1, j + 2]:
                        v = mesh.vertices[v_indices[k]]
                        f.write(f"      vertex {v[0]:.6e} {v[1]:.6e} {v[2]:.6e}\n")

                    f.write("    endloop\n")
                    f.write("  endfacet\n")

            f.write("endsolid mesh\n")

        return file_path

    def _write_binary_stl(self, mesh: PanelMesh, file_path: Path) -> Path:
        """Write binary STL file.

        Args:
            mesh: PanelMesh to write.
            file_path: Output file path.

        Returns:
            Path to written file.
        """
        logger.info(f"Writing binary STL file: {file_path}")

        # Count triangles (quads become 2 triangles)
        n_triangles = 0
        for panel in mesh.panels:
            v_indices = [idx for idx in panel if idx >= 0]
            n_triangles += max(0, len(v_indices) - 2)

        with open(file_path, 'wb') as f:
            # Header (80 bytes)
            header = b'BEMRosetta mesh'.ljust(80, b'\0')
            f.write(header)

            # Triangle count
            f.write(struct.pack('I', n_triangles))

            # Write triangles
            for i, panel in enumerate(mesh.panels):
                v_indices = [idx for idx in panel if idx >= 0]
                if len(v_indices) < 3:
                    continue

                normal = mesh.normals[i]

                # Triangle fan
                for j in range(len(v_indices) - 2):
                    # Normal
                    f.write(struct.pack('fff', float(normal[0]), float(normal[1]), float(normal[2])))

                    # Vertices
                    for k in [0, j + 1, j + 2]:
                        v = mesh.vertices[v_indices[k]]
                        f.write(struct.pack('fff', float(v[0]), float(v[1]), float(v[2])))

                    # Attribute (2 bytes)
                    f.write(struct.pack('H', 0))

        return file_path
