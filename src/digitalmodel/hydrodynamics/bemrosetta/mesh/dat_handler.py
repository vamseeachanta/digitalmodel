"""AQWA/NEMOH DAT mesh format handler.

Supports two DAT sub-formats:

1. **NEMOH format** (simple mesh-only file):
   - Line 1: NVERT NPAN (number of vertices and panels)
   - Lines 2 to NVERT+1: X Y Z (vertex coordinates)
   - Lines NVERT+2 to end: V1 V2 V3 V4 (panel vertex indices, 1-based)
   - Final line: 0 0 0 0 (terminator)

2. **AQWA deck format** (full analysis file with embedded mesh):
   - Starts with ``*********1*********2...`` column header
   - DECK 1 contains COOR/NOD5 vertex blocks and QPPL/TPPL element lines
   - Vertex format: ``STRC_ID SEQ_NUM X Y Z``
   - Element format: ``STRC_ID QPPL PROP(BODY)(N1)(N2)(N3)(N4) ...``

Auto-detection: if the first non-blank line starts with ``*``, AQWA deck
format is used; otherwise NEMOH format is assumed.
"""

from pathlib import Path
import re
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

        with open(file_path, 'r', errors='replace') as f:
            lines = f.readlines()

        # Auto-detect format: AQWA deck files start with *********
        if self._is_aqwa_deck(lines):
            logger.info("Detected AQWA deck format")
            return self._parse_aqwa_deck(lines, file_path)

        return self._parse_dat(lines, file_path)

    @staticmethod
    def _is_aqwa_deck(lines: List[str]) -> bool:
        """Check if lines are from an AQWA deck-format analysis file."""
        for line in lines[:10]:
            stripped = line.strip()
            if stripped and stripped.startswith("*********"):
                return True
        return False

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

    # Regex for floating-point numbers, including scientific notation.
    # Handles FORTRAN fixed-format where numbers touch: 149.10987-18.016252
    _FLOAT_RE = re.compile(
        r"[+-]?"
        r"(?:\d+\.?\d*(?:[eE][+-]?\d+)?|\.\d+(?:[eE][+-]?\d+)?)"
    )

    # Regex for QPPL/TPPL element lines (with optional DIFF/NOND modifier).
    # Captures the parenthesised node-index groups.
    _ELEM_RE = re.compile(
        r"^\s*\d+\s*[QT]PPL"          # STRC + QPPL/TPPL
        r"(?:\s+\w+)*"                 # optional modifiers (DIFF, NOND, …)
        r"\s+\d+\(\s*\d+\)"           # PROP(BODY)
        r"((?:\(\s*\d+\))+)"          # node groups: (N1)(N2)…
    )

    def _parse_aqwa_deck(
        self, lines: List[str], file_path: Path
    ) -> PanelMesh:
        """Parse mesh from an AQWA deck-format analysis file.

        Extracts vertices from DECK 1 COOR/NOD5 blocks and panels from
        QPPL (quad) and TPPL (tri) element lines.

        Vertex lines (NOD5 format, FORTRAN fixed-width)::

            STRC_ID  SEQ_NUM       X            Y            Z

        Coordinates may touch when FORTRAN fields abut (e.g.
        ``149.10987-18.016252``).  A float-finding regex is used instead
        of naive whitespace splitting.

        Element lines::

            STRC_ID QPPL [DIFF]  PROP(BODY)(N1)(N2)(N3)(N4) ...
            STRC_ID TPPL [DIFF]  PROP(BODY)(N1)(N2)(N3) ...
        """
        vertices: List[List[float]] = []
        panels: List[List[int]] = []
        node_id_map: dict[int, int] = {}  # AQWA seq -> 0-based index

        in_coor = False

        for line in lines:
            trimmed = line.strip()

            # Skip blank, comment, and column-header lines
            if not trimmed or trimmed.startswith("*"):
                continue

            # Detect COOR section start
            if trimmed == "COOR":
                in_coor = True
                continue

            # Skip structural keywords inside COOR
            if trimmed.startswith("STRC") or trimmed == "NOD5":
                continue

            # COOR section ends at END or a DECK header
            if in_coor and (trimmed == "END" or "DECK" in trimmed):
                in_coor = False
                continue

            # Parse vertex lines inside COOR section
            if in_coor:
                self._parse_vertex_line(trimmed, vertices, node_id_map)
                continue

            # Parse QPPL/TPPL element lines (DECK 2 and beyond)
            m = self._ELEM_RE.match(trimmed)
            if m:
                node_groups = re.findall(r"\(\s*(\d+)\)", m.group(1))
                panel = []
                for n in node_groups:
                    panel.append(node_id_map.get(int(n), -1))
                while len(panel) < 4:
                    panel.append(-1)
                panels.append(panel[:4])

        if not vertices:
            raise MeshError(
                "No vertices found in AQWA deck file",
                mesh_file=str(file_path),
            )

        if not panels:
            raise MeshError(
                "No QPPL/TPPL elements found in AQWA deck file",
                mesh_file=str(file_path),
            )

        logger.info(
            f"Parsed AQWA deck: {len(vertices)} vertices, {len(panels)} panels"
        )

        return PanelMesh(
            vertices=np.array(vertices, dtype=np.float64),
            panels=np.array(panels, dtype=np.int32),
            format_origin=MeshFormat.DAT,
            name=file_path.stem,
        )

    def _parse_vertex_line(
        self,
        trimmed: str,
        vertices: List[List[float]],
        node_id_map: dict,
    ) -> None:
        """Parse a single AQWA NOD5 vertex line.

        Handles FORTRAN fixed-format where coordinates may touch
        (e.g. ``149.10987-18.016252``).
        """
        parts = trimmed.split()
        if len(parts) < 3:
            return

        try:
            seq_num = int(parts[1])
        except (ValueError, IndexError):
            return

        # Join everything after STRC_ID and SEQ_NUM as coordinate string
        # (handles both space-separated and touching numbers)
        coord_str = trimmed
        # Strip the first two integer fields
        for _ in range(2):
            coord_str = coord_str.lstrip()
            idx = 0
            while idx < len(coord_str) and (
                coord_str[idx].isdigit() or coord_str[idx] in "+-"
            ):
                idx += 1
            coord_str = coord_str[idx:]

        nums = self._FLOAT_RE.findall(coord_str)
        if len(nums) >= 3:
            try:
                x, y, z = float(nums[0]), float(nums[1]), float(nums[2])
                node_id_map[seq_num] = len(vertices)
                vertices.append([x, y, z])
            except ValueError:
                pass

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
