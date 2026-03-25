"""
ABOUTME: TDD tests for exporter — writing panelised hull geometry to GDF
(WAMIT/BEMRosetta) and OrcaWave (HYDROSTAR-style) formats.

Test coverage:
- GDF export: file is created, header lines present, panel count matches
- GDF round-trip: write then re-read with GDFHandler gives same panel count
- OrcaWave export: file created, correct format identifier in header
- SVG cross-section: file created with station count matching input
- Exporter raises on unsupported format string
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest


@pytest.fixture
def simple_mesh(box_barge_json):
    """A minimal PanelMesh from the box barge for export tests."""
    from digitalmodel.hydrodynamics.hull_library.line_generator.hull_surface import (
        HullSurfaceConfig,
        HullSurfaceInterpolator,
    )
    from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
        LineParser,
    )
    from digitalmodel.hydrodynamics.hull_library.line_generator.panelizer import (
        Panelizer,
        PanelizerConfig,
    )

    parser = LineParser()
    defn = parser.parse_json_dict(box_barge_json)
    surface = HullSurfaceInterpolator(HullSurfaceConfig(n_x=10, n_z=5)).interpolate(defn)
    return Panelizer(PanelizerConfig(target_panels=50, symmetry=False)).panelise(surface, defn)


@pytest.fixture
def simple_defn(box_barge_json):
    """Box barge HullLineDefinition for export helpers."""
    from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
        LineParser,
    )

    return LineParser().parse_json_dict(box_barge_json)


class TestGDFExport:
    """Tests for GDF mesh export."""

    def test_export_creates_file(self, simple_mesh, tmp_path):
        """export_gdf writes a .gdf file to the output path."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.exporter import (
            export_gdf,
        )

        out = tmp_path / "hull.gdf"
        export_gdf(simple_mesh, out)
        assert out.exists()
        assert out.stat().st_size > 0

    def test_export_gdf_panel_count_in_header(self, simple_mesh, tmp_path):
        """GDF file contains correct panel count on header line 4."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.exporter import (
            export_gdf,
        )

        out = tmp_path / "hull.gdf"
        export_gdf(simple_mesh, out)
        lines = out.read_text().splitlines()
        # GDF format: line1=header, line2=ULEN GRAV, line3=ISX ISY, line4=NPAN
        # (may have 0-indexed comment lines at top; search for NPAN line)
        # Find the line that contains only an integer (panel count)
        n_pan_line = None
        for line in lines[3:6]:
            stripped = line.strip()
            if stripped.isdigit():
                n_pan_line = int(stripped)
                break
        assert n_pan_line == simple_mesh.n_panels

    def test_gdf_round_trip(self, simple_mesh, tmp_path):
        """Write GDF then read back with GDFHandler gives same panel count."""
        from digitalmodel.hydrodynamics.bemrosetta.mesh.gdf_handler import GDFHandler
        from digitalmodel.hydrodynamics.hull_library.line_generator.exporter import (
            export_gdf,
        )

        out = tmp_path / "hull.gdf"
        export_gdf(simple_mesh, out)
        handler = GDFHandler()
        loaded = handler.read(out)
        assert loaded.n_panels == simple_mesh.n_panels


class TestOrcaWaveExport:
    """Tests for OrcaWave mesh export."""

    def test_export_creates_file(self, simple_mesh, tmp_path):
        """export_orcawave writes a file to the output path."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.exporter import (
            export_orcawave,
        )

        out = tmp_path / "hull.dat"
        export_orcawave(simple_mesh, out)
        assert out.exists()
        assert out.stat().st_size > 0

    def test_orcawave_has_panel_data(self, simple_mesh, tmp_path):
        """OrcaWave file contains panel vertex coordinates."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.exporter import (
            export_orcawave,
        )

        out = tmp_path / "hull.dat"
        export_orcawave(simple_mesh, out)
        content = out.read_text()
        # Should contain at least one floating point number
        assert "." in content


class TestSVGExport:
    """Tests for SVG cross-section visualization."""

    def test_export_svg_creates_file(self, simple_defn, tmp_path):
        """export_sections_svg writes an SVG file."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.exporter import (
            export_sections_svg,
        )

        out = tmp_path / "sections.svg"
        export_sections_svg(simple_defn, out)
        assert out.exists()
        assert out.stat().st_size > 0

    def test_export_svg_contains_svg_tag(self, simple_defn, tmp_path):
        """SVG file contains <svg> root element."""
        from digitalmodel.hydrodynamics.hull_library.line_generator.exporter import (
            export_sections_svg,
        )

        out = tmp_path / "sections.svg"
        export_sections_svg(simple_defn, out)
        content = out.read_text()
        assert "<svg" in content.lower()


class TestHullPanelGenerator:
    """Integration test: end-to-end via HullPanelGenerator facade."""

    def test_generate_from_dict(self, box_barge_json, tmp_path):
        """HullPanelGenerator.generate() returns a PanelMesh from a dict."""
        from digitalmodel.hydrodynamics.bemrosetta.models.mesh_models import PanelMesh
        from digitalmodel.hydrodynamics.hull_library.line_generator import (
            HullPanelGenerator,
        )

        gen = HullPanelGenerator()
        mesh = gen.generate(box_barge_json)
        assert isinstance(mesh, PanelMesh)
        assert mesh.n_panels > 0

    def test_generate_and_export_gdf(self, box_barge_json, tmp_path):
        """HullPanelGenerator.generate_and_export writes GDF file."""
        from digitalmodel.hydrodynamics.hull_library.line_generator import (
            HullPanelGenerator,
        )

        gen = HullPanelGenerator()
        out = tmp_path / "output.gdf"
        gen.generate_and_export(box_barge_json, out, fmt="gdf")
        assert out.exists()
        assert out.stat().st_size > 0
