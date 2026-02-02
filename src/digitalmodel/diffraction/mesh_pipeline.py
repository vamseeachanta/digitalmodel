"""Mesh conversion pipeline for the diffraction spec converter (WRK-060).

Thin integration layer wrapping existing BEMRosetta mesh handlers to provide
a unified API for the diffraction module. All heavy lifting (parsing, writing,
validation) is delegated to the BEMRosetta mesh infrastructure.

Solver format conventions:
    - AQWA requires DAT format (NEMOH-style panel mesh)
    - OrcaWave requires GDF format (WAMIT geometry definition)

Example usage::

    from digitalmodel.diffraction.mesh_pipeline import MeshPipeline

    pipeline = MeshPipeline()
    mesh = pipeline.load(Path("vessel.gdf"))
    report = pipeline.validate(mesh)
    dat_path = pipeline.prepare_for_solver(Path("vessel.gdf"), "aqwa", out_dir)
"""

from __future__ import annotations

from pathlib import Path

from loguru import logger

from digitalmodel.bemrosetta.mesh import (
    DATHandler,
    GDFHandler,
    STLHandler,
)
from digitalmodel.bemrosetta.models import (
    MeshFormat,
    MeshQualityReport,
    PanelMesh,
)
from digitalmodel.diffraction.input_schemas import MeshFormatType

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

# Use string-based lookups to avoid enum identity issues when the project
# is importable via both ``src.digitalmodel`` and ``digitalmodel`` paths.

_EXTENSION_TO_FORMAT_STR: dict[str, str] = {
    ".gdf": "gdf",
    ".dat": "dat",
    ".stl": "stl",
}

_FORMAT_TYPE_VALUE_TO_FORMAT_STR: dict[str, str] = {
    "gdf": "gdf",
    "dat": "dat",
    "stl": "stl",
}

_SOLVER_TARGET_FORMAT_STR: dict[str, str] = {
    "aqwa": "dat",
    "orcawave": "gdf",
}

_SOLVER_EXTENSION: dict[str, str] = {
    "aqwa": ".dat",
    "orcawave": ".gdf",
}


# ---------------------------------------------------------------------------
# Handler registry
# ---------------------------------------------------------------------------

_HANDLER_REGISTRY: dict[str, type] = {
    "gdf": GDFHandler,
    "dat": DATHandler,
    "stl": STLHandler,
}


def _handler_for_format_str(fmt_value: str):
    """Return the appropriate handler instance for a format value string."""
    handler_cls = _HANDLER_REGISTRY.get(fmt_value.lower())
    if handler_cls is None:
        raise ValueError(f"No handler available for format: {fmt_value}")
    return handler_cls()


def _mesh_format_from_str(value: str) -> MeshFormat:
    """Resolve a MeshFormat enum member from its string value."""
    for member in MeshFormat:
        if member.value == value:
            return member
    raise ValueError(f"Unknown MeshFormat value: {value}")


# ---------------------------------------------------------------------------
# MeshPipeline
# ---------------------------------------------------------------------------


class MeshPipeline:
    """Mesh conversion pipeline for the diffraction spec converter.

    Wraps BEMRosetta mesh handlers to provide loading, conversion,
    solver preparation, and validation through a single interface.
    """

    def load(
        self,
        mesh_path: Path,
        format: MeshFormatType = MeshFormatType.AUTO,
    ) -> PanelMesh:
        """Load mesh from file, auto-detecting format if needed.

        Args:
            mesh_path: Path to the mesh file.
            format: Mesh format type. AUTO detects from extension.

        Returns:
            PanelMesh with the loaded mesh data.

        Raises:
            ValueError: If format cannot be determined.
            MeshError: If file cannot be read.
        """
        mesh_path = Path(mesh_path)

        if format.value == "auto":
            fmt_str = self._detect_format_str(mesh_path)
        else:
            fmt_str = self._map_format_type_str(format)

        handler = _handler_for_format_str(fmt_str)
        logger.info(f"Loading mesh: {mesh_path} (format={fmt_str})")
        return handler.read(mesh_path)

    def convert(
        self,
        mesh: PanelMesh,
        target_format: MeshFormat,
        output_path: Path,
    ) -> Path:
        """Convert mesh to target format and write to output_path.

        Args:
            mesh: Source PanelMesh.
            target_format: Target mesh format.
            output_path: Path for the output file.

        Returns:
            Path to the written file.
        """
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)

        fmt_str = target_format.value
        handler = _handler_for_format_str(fmt_str)
        logger.info(f"Converting mesh to {fmt_str}: {output_path}")
        return handler.write(mesh, output_path)

    def convert_by_name(
        self,
        mesh: PanelMesh,
        target_format_name: str,
        output_path: Path,
    ) -> Path:
        """Convert mesh to target format using a format name string.

        Convenience method that avoids MeshFormat enum identity issues
        when the module is imported via different paths.

        Args:
            mesh: Source PanelMesh.
            target_format_name: Format name string (``"gdf"``, ``"dat"``,
                ``"stl"``).
            output_path: Path for the output file.

        Returns:
            Path to the written file.
        """
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)

        handler = _handler_for_format_str(target_format_name)
        logger.info(
            f"Converting mesh to {target_format_name}: {output_path}"
        )
        return handler.write(mesh, output_path)

    def prepare_for_solver(
        self,
        mesh_path: Path,
        solver: str,
        output_dir: Path,
    ) -> Path:
        """Prepare mesh for a specific solver.

        Loads the mesh, converts to the solver's required format, and
        writes to the output directory.

        Solver conventions:
            - ``aqwa``     -> DAT format (.dat)
            - ``orcawave`` -> GDF format (.gdf)

        Args:
            mesh_path: Path to the source mesh file.
            solver: Solver name (``"aqwa"`` or ``"orcawave"``).
            output_dir: Directory for the output file.

        Returns:
            Path to the solver-ready mesh file.

        Raises:
            ValueError: If the solver name is unknown.
        """
        solver_lower = solver.lower()
        if solver_lower not in _SOLVER_TARGET_FORMAT_STR:
            raise ValueError(
                f"Unknown solver: {solver!r}. "
                f"Supported: {list(_SOLVER_TARGET_FORMAT_STR.keys())}"
            )

        mesh_path = Path(mesh_path)
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        target_fmt_str = _SOLVER_TARGET_FORMAT_STR[solver_lower]
        target_ext = _SOLVER_EXTENSION[solver_lower]

        # Load the mesh
        mesh = self.load(mesh_path)

        # Write in target format
        output_path = output_dir / f"{mesh_path.stem}{target_ext}"
        logger.info(
            f"Preparing mesh for {solver}: "
            f"{mesh_path} -> {output_path} ({target_fmt_str})"
        )
        handler = _handler_for_format_str(target_fmt_str)
        return handler.write(mesh, output_path)

    def validate(self, mesh: PanelMesh) -> MeshQualityReport:
        """Run quality checks on the mesh.

        Delegates to BaseMeshHandler.validate_mesh, using the handler
        matching the mesh's original format (or GDF as default).

        Args:
            mesh: PanelMesh to validate.

        Returns:
            MeshQualityReport with quality metrics.
        """
        # Use the handler matching the mesh origin, defaulting to GDF
        fmt_str = mesh.format_origin.value
        if fmt_str == "unknown":
            fmt_str = "gdf"

        handler = _handler_for_format_str(fmt_str)
        return handler.validate_mesh(mesh)

    def detect_format(self, path: Path) -> MeshFormat:
        """Auto-detect mesh format from file extension.

        Args:
            path: Path to a mesh file.

        Returns:
            Detected MeshFormat.

        Raises:
            ValueError: If the extension is not recognized.
        """
        fmt_str = self._detect_format_str(path)
        return _mesh_format_from_str(fmt_str)

    def map_format_type(self, format_type: MeshFormatType) -> MeshFormat:
        """Map a spec MeshFormatType enum to a BEMRosetta MeshFormat enum.

        Args:
            format_type: MeshFormatType from input_schemas.

        Returns:
            Corresponding MeshFormat.

        Raises:
            ValueError: If format_type is AUTO (requires a path) or
                unsupported.
        """
        fmt_str = self._map_format_type_str(format_type)
        return _mesh_format_from_str(fmt_str)

    # -----------------------------------------------------------------
    # Private helpers (string-based to avoid enum identity issues)
    # -----------------------------------------------------------------

    @staticmethod
    def _detect_format_str(path: Path) -> str:
        """Detect format string from file extension."""
        ext = path.suffix.lower()
        fmt_str = _EXTENSION_TO_FORMAT_STR.get(ext)
        if fmt_str is None:
            raise ValueError(
                f"Unsupported mesh file extension: {ext!r}. "
                f"Supported: {list(_EXTENSION_TO_FORMAT_STR.keys())}"
            )
        return fmt_str

    @staticmethod
    def _map_format_type_str(format_type: MeshFormatType) -> str:
        """Map MeshFormatType to a format value string."""
        ft_value = format_type.value
        if ft_value == "auto":
            raise ValueError(
                "AUTO format requires a file path for detection. "
                "Use detect_format() or pass a concrete format type."
            )
        fmt_str = _FORMAT_TYPE_VALUE_TO_FORMAT_STR.get(ft_value)
        if fmt_str is None:
            raise ValueError(
                f"No BEMRosetta MeshFormat mapping for: {ft_value}"
            )
        return fmt_str
