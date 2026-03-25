#!/usr/bin/env python3
"""
ABOUTME: Tests for STEP import and CalculiX INP export functionality in the
GMSH meshing module.
"""

import pytest
import numpy as np
import tempfile
from pathlib import Path

from digitalmodel.solvers.gmsh_meshing.models import INPExportConfig

# Skip GMSH tests if not installed
try:
    from digitalmodel.solvers.gmsh_meshing.mesh_generator import (
        GMSHMeshGenerator,
        GMSH_AVAILABLE,
    )
except ImportError:
    GMSH_AVAILABLE = False

requires_gmsh = pytest.mark.skipif(
    not GMSH_AVAILABLE,
    reason="gmsh not available",
)


# ============================================================================
# INPExportConfig Model Tests (no gmsh needed)
# ============================================================================


class TestINPExportConfig:
    """Test INPExportConfig dataclass."""

    def test_inp_export_config_defaults(self):
        """Verify INPExportConfig has sensible defaults."""
        config = INPExportConfig()
        assert config.include_sets is True
        assert config.node_offset == 1
        assert "Tetrahedron 4" in config.element_type_map
        assert config.element_type_map["Tetrahedron 4"] == "C3D4"
        assert config.element_type_map["Hexahedron 8"] == "C3D8"
        assert config.element_type_map["Tetrahedron 10"] == "C3D10"

    def test_inp_export_config_custom(self):
        """Verify INPExportConfig accepts custom values."""
        custom_map = {"Tetrahedron 4": "C3D4"}
        config = INPExportConfig(
            element_type_map=custom_map,
            include_sets=False,
            node_offset=0,
        )
        assert config.include_sets is False
        assert config.node_offset == 0
        assert len(config.element_type_map) == 1


# ============================================================================
# INP Export Tests (write-side, uses gmsh only for mesh generation)
# ============================================================================


@requires_gmsh
class TestINPExport:
    """Test INP file export from generated meshes."""

    def test_export_mesh_to_inp_format(self, tmp_path):
        """Generate a box mesh, export to INP, verify file structure."""
        with GMSHMeshGenerator() as gen:
            gen.generate_simple_box_mesh(
                dimensions=(2.0, 2.0, 2.0), element_size=1.0
            )
            inp_file = tmp_path / "box.inp"
            result = gen.export_mesh_inp(str(inp_file))

        assert result.exists()
        content = inp_file.read_text()
        assert "*HEADING" in content
        assert "*NODE" in content
        assert "*ELEMENT" in content
        assert "*NSET" in content

    def test_inp_file_has_correct_node_count(self, tmp_path):
        """Verify node count in INP matches mesh."""
        with GMSHMeshGenerator() as gen:
            mesh = gen.generate_simple_box_mesh(
                dimensions=(1.0, 1.0, 1.0), element_size=0.5
            )
            inp_file = tmp_path / "box_nodes.inp"
            gen.export_mesh_inp(str(inp_file))

        n_nodes = len(mesh["nodes"])
        content = inp_file.read_text()

        # Count lines between *NODE and next *ELEMENT
        lines = content.splitlines()
        in_node_block = False
        node_count = 0
        for line in lines:
            if line.startswith("*NODE"):
                in_node_block = True
                continue
            if in_node_block and line.startswith("*"):
                break
            if in_node_block and line.strip():
                node_count += 1

        assert node_count == n_nodes

    def test_inp_file_has_correct_element_type(self, tmp_path):
        """Verify C3D4 for tetrahedra in INP output."""
        with GMSHMeshGenerator() as gen:
            gen.generate_simple_box_mesh(
                dimensions=(1.0, 1.0, 1.0), element_size=1.0
            )
            inp_file = tmp_path / "box_eltype.inp"
            gen.export_mesh_inp(str(inp_file))

        content = inp_file.read_text()
        assert "TYPE=C3D4" in content

    def test_inp_export_with_element_sets(self, tmp_path):
        """Verify element sets written correctly."""
        with GMSHMeshGenerator() as gen:
            gen.generate_simple_box_mesh(
                dimensions=(1.0, 1.0, 1.0), element_size=1.0
            )
            inp_file = tmp_path / "box_elsets.inp"
            gen.export_mesh_inp(str(inp_file))

        content = inp_file.read_text()
        # Element set name should appear in ELEMENT header
        assert "ELSET=EC3D4" in content
        # Node set for all nodes
        assert "NSET=NALL" in content

    def test_inp_export_not_initialized_raises(self):
        """Export without mesh raises RuntimeError."""
        gen = GMSHMeshGenerator()
        with pytest.raises(RuntimeError, match="not initialized"):
            gen.export_mesh_inp("dummy.inp")


# ============================================================================
# STEP Import Tests (require gmsh with OCC kernel)
# ============================================================================


@requires_gmsh
class TestSTEPImport:
    """Test STEP geometry import and meshing."""

    def test_generate_mesh_from_step_file(self, tmp_path):
        """Create a STEP file via OCC, then import and mesh it."""
        with GMSHMeshGenerator() as gen:
            step_file = tmp_path / "box.step"
            gen.create_step_geometry(
                "box", str(step_file), dimensions=(3.0, 2.0, 1.0)
            )
            assert step_file.exists()

            mesh = gen.generate_mesh_from_step(
                str(step_file), element_size=0.8
            )

        assert mesh["nodes"].shape[1] == 3
        assert len(mesh["nodes"]) > 0
        assert mesh["geometry"] == "step"
        assert mesh["dimension"] == 3
        assert "Tetrahedron" in " ".join(mesh["elements"].keys())

    def test_step_file_not_found_raises(self):
        """Non-existent STEP file raises FileNotFoundError."""
        with GMSHMeshGenerator() as gen:
            with pytest.raises(FileNotFoundError):
                gen.generate_mesh_from_step("/nonexistent/file.step")

    def test_create_step_geometry_box(self, tmp_path):
        """Create box STEP geometry."""
        with GMSHMeshGenerator() as gen:
            path = gen.create_step_geometry(
                "box", str(tmp_path / "b.step"), dimensions=(1, 1, 1)
            )
        assert path.exists()
        assert path.stat().st_size > 0

    def test_create_step_geometry_cylinder(self, tmp_path):
        """Create cylinder STEP geometry."""
        with GMSHMeshGenerator() as gen:
            path = gen.create_step_geometry(
                "cylinder",
                str(tmp_path / "c.step"),
                radius=0.5,
                height=2.0,
            )
        assert path.exists()

    def test_create_step_geometry_sphere(self, tmp_path):
        """Create sphere STEP geometry."""
        with GMSHMeshGenerator() as gen:
            path = gen.create_step_geometry(
                "sphere", str(tmp_path / "s.step"), radius=1.0
            )
        assert path.exists()

    def test_create_step_geometry_unknown_raises(self):
        """Unknown geometry type raises ValueError."""
        with GMSHMeshGenerator() as gen:
            with pytest.raises(ValueError, match="Unknown geometry"):
                gen.create_step_geometry("torus", "/tmp/t.step")


# ============================================================================
# STEP → INP Round Trip
# ============================================================================


@requires_gmsh
class TestSTEPToINPRoundTrip:
    """Test full STEP import → mesh → INP export pipeline."""

    def test_step_to_inp_round_trip(self, tmp_path):
        """STEP import, mesh, INP export — verify file readable."""
        with GMSHMeshGenerator() as gen:
            # Create STEP
            step_file = tmp_path / "rt_box.step"
            gen.create_step_geometry(
                "box", str(step_file), dimensions=(2.0, 1.0, 1.0)
            )

            # Import and mesh
            mesh = gen.generate_mesh_from_step(
                str(step_file), element_size=0.5
            )

            # Export to INP
            inp_file = tmp_path / "rt_box.inp"
            gen.export_mesh_inp(str(inp_file), title="Round-trip test")

        # Verify INP file
        content = inp_file.read_text()
        lines = content.splitlines()
        assert lines[0] == "*HEADING"
        assert lines[1] == "Round-trip test"
        assert any("*NODE" in l for l in lines)
        assert any("*ELEMENT" in l for l in lines)

        # Verify node/element counts are nonzero
        node_lines = 0
        in_nodes = False
        for line in lines:
            if line.startswith("*NODE"):
                in_nodes = True
                continue
            if in_nodes and line.startswith("*"):
                break
            if in_nodes and line.strip():
                node_lines += 1
        assert node_lines == len(mesh["nodes"])
