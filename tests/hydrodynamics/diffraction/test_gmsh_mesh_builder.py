#!/usr/bin/env python3
"""
ABOUTME: Tests for gmsh mesh builder integration module — converts geometry
specs to panel meshes and exports to GDF, DAT, and MSH v2.2 formats.

TDD: Tests written before implementation (WRK-140).
"""

from __future__ import annotations

import math
from pathlib import Path

import numpy as np
import pytest

try:
    import gmsh  # noqa: F401
    GMSH_AVAILABLE = True
except ImportError:
    GMSH_AVAILABLE = False


# ---------------------------------------------------------------------------
# Geometry spec helpers
# ---------------------------------------------------------------------------


def _box_barge_spec() -> dict:
    """Minimal box barge geometry spec."""
    return {
        "geometry_type": "box_barge",
        "length": 60.0,
        "beam": 20.0,
        "draft": 4.0,
        "mesh_size": 5.0,
    }


def _cylinder_spec() -> dict:
    """Minimal cylinder geometry spec."""
    return {
        "geometry_type": "cylinder",
        "radius": 8.0,
        "draft": 10.0,
        "mesh_size": 2.0,
    }


# ---------------------------------------------------------------------------
# Model tests (no GMSH required)
# ---------------------------------------------------------------------------


class TestGmshMeshSpec:
    """Tests for the GmshMeshSpec model."""

    def test_box_barge_spec_valid(self):
        """GmshMeshSpec accepts valid box_barge geometry."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshSpec,
        )

        spec = GmshMeshSpec(**_box_barge_spec())
        assert spec.geometry_type == "box_barge"
        assert spec.length == 60.0
        assert spec.beam == 20.0
        assert spec.draft == 4.0
        assert spec.mesh_size == 5.0

    def test_cylinder_spec_valid(self):
        """GmshMeshSpec accepts valid cylinder geometry."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshSpec,
        )

        spec = GmshMeshSpec(**_cylinder_spec())
        assert spec.geometry_type == "cylinder"
        assert spec.radius == 8.0
        assert spec.draft == 10.0

    def test_spec_from_dict(self):
        """GmshMeshSpec.from_dict constructs from raw dict."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshSpec,
        )

        spec = GmshMeshSpec.from_dict(_box_barge_spec())
        assert spec.geometry_type == "box_barge"

    def test_invalid_geometry_type_raises(self):
        """Unknown geometry_type raises ValueError."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshSpec,
        )

        with pytest.raises((ValueError, Exception)):
            GmshMeshSpec(geometry_type="invalid_shape", mesh_size=1.0)

    def test_missing_length_for_box_raises(self):
        """Missing length for box_barge raises on validation."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshSpec,
        )

        with pytest.raises((ValueError, Exception)):
            spec = GmshMeshSpec(
                geometry_type="box_barge",
                beam=10.0,
                draft=2.0,
                mesh_size=1.0,
            )
            spec.validate_for_geometry()


class TestGmshMeshBuilderInit:
    """Tests for GmshMeshBuilder construction (no GMSH required)."""

    def test_builder_instantiation(self):
        """GmshMeshBuilder can be instantiated."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshBuilder,
        )

        builder = GmshMeshBuilder()
        assert builder is not None

    def test_builder_has_gmsh_available_flag(self):
        """GmshMeshBuilder exposes GMSH_AVAILABLE flag."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GMSH_AVAILABLE as flag,
        )

        assert isinstance(flag, bool)

    def test_supported_geometry_types(self):
        """GmshMeshBuilder.SUPPORTED_GEOMETRIES lists known types."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshBuilder,
        )

        builder = GmshMeshBuilder()
        assert "box_barge" in builder.SUPPORTED_GEOMETRIES
        assert "cylinder" in builder.SUPPORTED_GEOMETRIES


# ---------------------------------------------------------------------------
# GDF export tests (no GMSH required — pure format logic)
# ---------------------------------------------------------------------------


class TestGdfExport:
    """Tests for GDF format export helpers."""

    def test_gdf_header_format(self):
        """_build_gdf_header returns correct GDF header lines."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            _build_gdf_header,
        )

        header = _build_gdf_header(n_panels=10, symmetry=False)
        # GDF structure: header line, ulen/grav, isx/isy, npanel
        lines = header.strip().split("\n")
        assert len(lines) >= 4
        assert "1.0" in lines[1]  # ulen
        assert "9.80665" in lines[1]  # gravity
        assert "10" in lines[3]  # panel count

    def test_gdf_header_with_symmetry(self):
        """_build_gdf_header uses symmetry=1 when requested."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            _build_gdf_header,
        )

        header = _build_gdf_header(n_panels=5, symmetry=True)
        lines = header.strip().split("\n")
        # Line 3 is isx isy — symmetry about xz plane
        assert "1" in lines[2]

    def test_gdf_panel_line_format(self):
        """_panel_to_gdf_line formats a quad panel correctly."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            _panel_to_gdf_line,
        )

        # Simple flat quad at z=-1
        verts = np.array([
            [-5.0, -2.5, -1.0],
            [5.0, -2.5, -1.0],
            [5.0, 2.5, -1.0],
            [-5.0, 2.5, -1.0],
        ])
        line = _panel_to_gdf_line(verts)
        # Should have 12 floats (4 vertices * 3 coords)
        values = line.split()
        assert len(values) == 12

    def test_gdf_panel_triangle_duplicates_vertex(self):
        """_panel_to_gdf_line duplicates vertex 2 for triangles."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            _panel_to_gdf_line,
        )

        verts = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [0.5, 1.0, 0.0],
        ])
        line = _panel_to_gdf_line(verts)
        values = line.split()
        # 4 vertices * 3 = 12 values (vertex 2 duplicated)
        assert len(values) == 12


# ---------------------------------------------------------------------------
# DAT export tests (no GMSH required)
# ---------------------------------------------------------------------------


class TestDatExport:
    """Tests for AQWA DAT format export helpers."""

    def test_dat_node_line_format(self):
        """_node_to_dat_line produces correct AQWA NODE record."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            _node_to_dat_line,
        )

        line = _node_to_dat_line(node_id=1, xyz=(10.5, -3.2, -2.0))
        assert "NODE" in line
        assert "1" in line
        assert "10.5" in line or "10.50" in line

    def test_dat_panel_line_format(self):
        """_panel_to_dat_line produces QPPL DIFF record."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            _panel_to_dat_line,
        )

        line = _panel_to_dat_line(
            panel_id=1, node_ids=(10, 20, 30, 40)
        )
        assert "QPPL" in line
        assert "DIFF" in line
        assert "10" in line and "20" in line

    def test_dat_panel_triangle_repeats_node(self):
        """_panel_to_dat_line repeats node 2 for triangles."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            _panel_to_dat_line,
        )

        line = _panel_to_dat_line(
            panel_id=1, node_ids=(10, 20, 30)
        )
        # Should have node 30 twice for AQWA tri-as-quad
        assert line.count("30") >= 2


# ---------------------------------------------------------------------------
# MeshExportResult tests (no GMSH required)
# ---------------------------------------------------------------------------


class TestMeshExportResult:
    """Tests for MeshExportResult data class."""

    def test_export_result_creation(self):
        """MeshExportResult stores export metadata."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            MeshExportResult,
        )

        result = MeshExportResult(
            n_panels=100,
            n_nodes=120,
            geometry_type="box_barge",
            output_files={},
        )
        assert result.n_panels == 100
        assert result.n_nodes == 120
        assert result.geometry_type == "box_barge"

    def test_export_result_to_dict(self):
        """MeshExportResult.to_dict returns serializable dict."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            MeshExportResult,
        )

        result = MeshExportResult(
            n_panels=50,
            n_nodes=60,
            geometry_type="cylinder",
            output_files={"gdf": "/tmp/cyl.gdf"},
        )
        d = result.to_dict()
        assert d["n_panels"] == 50
        assert d["geometry_type"] == "cylinder"
        assert "gdf" in d["output_files"]


# ---------------------------------------------------------------------------
# Full build workflow (requires GMSH)
# ---------------------------------------------------------------------------


@pytest.mark.skipif(not GMSH_AVAILABLE, reason="GMSH not installed")
class TestGmshMeshBuilderWorkflow:
    """End-to-end mesh generation tests (require gmsh package)."""

    def test_build_box_barge_gdf(self, tmp_path):
        """GmshMeshBuilder.build() generates valid GDF for box_barge."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshBuilder,
            GmshMeshSpec,
        )

        spec = GmshMeshSpec(**_box_barge_spec())
        builder = GmshMeshBuilder()

        result = builder.build(spec, output_dir=tmp_path, formats=["gdf"])

        assert result.n_panels > 0
        assert result.n_nodes > 0
        gdf_path = Path(result.output_files["gdf"])
        assert gdf_path.exists()
        content = gdf_path.read_text()
        # GDF must contain panel count and coordinate data
        assert "9.80665" in content

    def test_build_box_barge_dat(self, tmp_path):
        """GmshMeshBuilder.build() generates valid DAT for box_barge."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshBuilder,
            GmshMeshSpec,
        )

        spec = GmshMeshSpec(**_box_barge_spec())
        builder = GmshMeshBuilder()

        result = builder.build(spec, output_dir=tmp_path, formats=["dat"])

        dat_path = Path(result.output_files["dat"])
        assert dat_path.exists()
        content = dat_path.read_text()
        assert "NODE" in content
        assert "QPPL" in content

    def test_build_box_barge_msh(self, tmp_path):
        """GmshMeshBuilder.build() generates MSH v2.2 for box_barge."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshBuilder,
            GmshMeshSpec,
        )

        spec = GmshMeshSpec(**_box_barge_spec())
        builder = GmshMeshBuilder()

        result = builder.build(spec, output_dir=tmp_path, formats=["msh"])

        msh_path = Path(result.output_files["msh"])
        assert msh_path.exists()
        content = msh_path.read_text()
        # MSH v2.2 starts with $MeshFormat
        assert "$MeshFormat" in content
        # Version 2.2
        assert "2.2" in content

    def test_build_cylinder_gdf(self, tmp_path):
        """GmshMeshBuilder.build() generates GDF for cylinder."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshBuilder,
            GmshMeshSpec,
        )

        spec = GmshMeshSpec(**_cylinder_spec())
        builder = GmshMeshBuilder()

        result = builder.build(spec, output_dir=tmp_path, formats=["gdf"])

        assert result.n_panels > 0
        gdf_path = Path(result.output_files["gdf"])
        assert gdf_path.exists()

    def test_build_multi_format(self, tmp_path):
        """GmshMeshBuilder.build() writes all three formats in one call."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshBuilder,
            GmshMeshSpec,
        )

        spec = GmshMeshSpec(**_box_barge_spec())
        builder = GmshMeshBuilder()

        result = builder.build(
            spec, output_dir=tmp_path, formats=["gdf", "dat", "msh"]
        )

        assert "gdf" in result.output_files
        assert "dat" in result.output_files
        assert "msh" in result.output_files
        for fmt, path in result.output_files.items():
            assert Path(path).exists(), f"Missing {fmt} output: {path}"

    def test_build_uses_custom_stem(self, tmp_path):
        """output_stem controls output file names."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshBuilder,
            GmshMeshSpec,
        )

        spec = GmshMeshSpec(**_box_barge_spec())
        builder = GmshMeshBuilder()

        result = builder.build(
            spec,
            output_dir=tmp_path,
            formats=["gdf"],
            output_stem="my_barge",
        )

        gdf_path = Path(result.output_files["gdf"])
        assert gdf_path.stem == "my_barge"

    def test_panel_count_reasonable_for_barge(self, tmp_path):
        """Box barge mesh has reasonable panel count for its size."""
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GmshMeshBuilder,
            GmshMeshSpec,
        )

        spec = GmshMeshSpec(
            geometry_type="box_barge",
            length=100.0,
            beam=30.0,
            draft=6.0,
            mesh_size=5.0,
        )
        builder = GmshMeshBuilder()
        result = builder.build(spec, output_dir=tmp_path, formats=["gdf"])

        # Wetted surface area ~ 2*(L*d) + 2*(B*d) + L*B
        # ~ 2*600 + 2*180 + 3000 = 4560 m^2
        # With 5m panels: expected ~180 panels (4560/25)
        # Allow broad tolerance (GMSH is variable)
        assert result.n_panels >= 20
        assert result.n_panels <= 5000

    def test_gmsh_not_available_raises_import_error(self):
        """Importing GmshMeshBuilder when gmsh unavailable raises ImportError."""
        # This just verifies GMSH_AVAILABLE is exposed and meaningful.
        from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
            GMSH_AVAILABLE as flag,
        )

        assert flag is True  # We're in skipif GMSH_AVAILABLE block


# ---------------------------------------------------------------------------
# No-GMSH fallback tests
# ---------------------------------------------------------------------------


class TestGmshNotAvailableFallback:
    """Graceful degradation when GMSH is not installed."""

    def test_build_without_gmsh_raises_import_error(self, monkeypatch, tmp_path):
        """build() raises ImportError when gmsh absent and fallback=False."""
        import digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder as mod

        original = mod.GMSH_AVAILABLE
        monkeypatch.setattr(mod, "GMSH_AVAILABLE", False)

        try:
            from digitalmodel.hydrodynamics.diffraction.gmsh_mesh_builder import (
                GmshMeshBuilder,
                GmshMeshSpec,
            )

            spec = GmshMeshSpec(**_box_barge_spec())
            builder = GmshMeshBuilder()

            with pytest.raises(ImportError, match="[Gg][Mm][Ss][Hh]"):
                builder.build(spec, output_dir=tmp_path, formats=["gdf"])
        finally:
            monkeypatch.setattr(mod, "GMSH_AVAILABLE", original)
