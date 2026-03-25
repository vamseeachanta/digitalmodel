"""
ABOUTME: Hull panel geometry generator from waterline, section, and profile
line definitions.  Converts sparse naval architecture line data into
panelised meshes suitable for diffraction analysis solvers.

Phases:
  1. line_parser    — parse CSV/JSON/YAML station offsets into HullLineDefinition
  2. hull_surface   — BSpline-interpolate from sparse stations to full surface grid
  3. panelizer      — convert surface grid to quad/tri PanelMesh with quality checks
  4. exporter       — write GDF (WAMIT) and OrcaWave formats, SVG cross-sections
"""

from __future__ import annotations

from .exporter import export_gdf, export_orcawave, export_sections_svg
from .hull_surface import HullSurface, HullSurfaceConfig, HullSurfaceInterpolator
from .line_parser import HullLineDefinition, LineParser, StationOffset
from .panelizer import (
    MeshQuality,
    Panelizer,
    PanelizerConfig,
    compute_mesh_quality,
)


class HullPanelGenerator:
    """Facade that drives all four phases end-to-end.

    Usage::

        from digitalmodel.hydrodynamics.hull_library.line_generator import (
            HullPanelGenerator,
        )
        gen = HullPanelGenerator()
        mesh = gen.generate(hull_dict)
        gen.generate_and_export(hull_dict, "hull.gdf", fmt="gdf")
    """

    def __init__(
        self,
        surface_config: HullSurfaceConfig | None = None,
        panel_config: PanelizerConfig | None = None,
    ) -> None:
        self._surface_cfg = surface_config or HullSurfaceConfig()
        self._panel_cfg = panel_config or PanelizerConfig()

    def generate(self, definition: dict) -> "PanelMesh":  # noqa: F821
        """Generate a PanelMesh from a hull definition dict.

        Args:
            definition: Hull line definition as a plain dict (see
                ``HullLineDefinition`` schema).

        Returns:
            PanelMesh with quad panels, normals, areas, and centers computed.
        """
        parser = LineParser()
        defn = parser.parse_json_dict(definition)
        surface = HullSurfaceInterpolator(self._surface_cfg).interpolate(defn)
        return Panelizer(self._panel_cfg).panelise(surface, defn)

    def generate_and_export(
        self,
        definition: dict,
        output_path: "str | Path",  # noqa: F821
        fmt: str = "gdf",
    ) -> "Path":  # noqa: F821
        """Generate mesh and export to file.

        Args:
            definition: Hull line definition dict.
            output_path: Destination file path.
            fmt: Export format — ``"gdf"`` (WAMIT/BEMRosetta) or
                 ``"orcawave"``.

        Returns:
            Path to the written file.

        Raises:
            ValueError: If *fmt* is not recognised.
        """
        from pathlib import Path

        mesh = self.generate(definition)
        out = Path(output_path)
        fmt_lower = fmt.lower()
        if fmt_lower == "gdf":
            return export_gdf(mesh, out)
        if fmt_lower in {"orcawave", "dat"}:
            return export_orcawave(mesh, out)
        raise ValueError(
            f"Unsupported export format: '{fmt}'. "
            "Choose 'gdf' or 'orcawave'."
        )


__all__ = [
    # Data models
    "StationOffset",
    "HullLineDefinition",
    # Parser
    "LineParser",
    # Surface
    "HullSurfaceConfig",
    "HullSurface",
    "HullSurfaceInterpolator",
    # Panelizer
    "PanelizerConfig",
    "MeshQuality",
    "Panelizer",
    "compute_mesh_quality",
    # Exporter
    "export_gdf",
    "export_orcawave",
    "export_sections_svg",
    # Facade
    "HullPanelGenerator",
]
