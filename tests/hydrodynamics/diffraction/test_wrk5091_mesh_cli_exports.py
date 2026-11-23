#!/usr/bin/env python3
"""
ABOUTME: TDD tests for WRK-5091 — mesh-build CLI subcommand + GmshMeshBuilder
exports in diffraction __init__.py.

Tests written BEFORE implementation (TDD red phase).
"""

from __future__ import annotations

from pathlib import Path

import pytest

try:
    import gmsh  # noqa: F401
    GMSH_AVAILABLE = True
except (ImportError, OSError):
    GMSH_AVAILABLE = False


# ---------------------------------------------------------------------------
# __init__.py export tests (no GMSH required)
# ---------------------------------------------------------------------------


class TestDiffractionExports:
    """GmshMeshBuilder and friends are importable from the diffraction package."""

    def test_gmsh_mesh_builder_importable(self):
        """GmshMeshBuilder is importable from diffraction __init__.py."""
        from digitalmodel.hydrodynamics.diffraction import GmshMeshBuilder
        assert GmshMeshBuilder is not None

    def test_gmsh_mesh_spec_importable(self):
        """GmshMeshSpec is importable from diffraction __init__.py."""
        from digitalmodel.hydrodynamics.diffraction import GmshMeshSpec
        assert GmshMeshSpec is not None

    def test_mesh_export_result_importable(self):
        """MeshExportResult is importable from diffraction __init__.py."""
        from digitalmodel.hydrodynamics.diffraction import MeshExportResult
        assert MeshExportResult is not None

    def test_gmsh_available_flag_importable(self):
        """GMSH_AVAILABLE flag is importable from diffraction __init__.py."""
        from digitalmodel.hydrodynamics.diffraction import GMSH_AVAILABLE as flag
        assert isinstance(flag, bool)

    def test_exports_in_all(self):
        """GmshMeshBuilder exports are listed in __all__."""
        import digitalmodel.hydrodynamics.diffraction as mod
        for name in ("GmshMeshBuilder", "GmshMeshSpec", "MeshExportResult", "GMSH_AVAILABLE"):
            assert name in mod.__all__, f"{name} missing from __all__"


# ---------------------------------------------------------------------------
# CLI tests (require click + gmsh for full flow)
# ---------------------------------------------------------------------------


class TestMeshBuildCliBasic:
    """Tests for mesh-build CLI subcommand that don't need gmsh."""

    def test_mesh_build_command_exists(self):
        """mesh-build is a registered CLI command."""
        from digitalmodel.hydrodynamics.diffraction.cli import cli
        cmd_names = [cmd for cmd in cli.commands]
        assert "mesh-build" in cmd_names

    def test_mesh_build_help(self):
        """mesh-build --help exits cleanly."""
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        runner = CliRunner()
        result = runner.invoke(cli, ["mesh-build", "--help"])
        assert result.exit_code == 0
        assert "mesh-build" in result.output.lower() or "spec" in result.output.lower()

    def test_mesh_build_missing_spec_file(self):
        """mesh-build with nonexistent spec file fails gracefully."""
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        runner = CliRunner()
        result = runner.invoke(cli, ["mesh-build", "/nonexistent/spec.yaml"])
        assert result.exit_code != 0


@pytest.mark.skipif(not GMSH_AVAILABLE, reason="GMSH not installed")
class TestMeshBuildCliWorkflow:
    """End-to-end CLI tests (require gmsh)."""

    def _write_spec_yaml(self, tmp_path: Path) -> Path:
        """Write a valid box barge spec YAML file."""
        spec_file = tmp_path / "spec.yaml"
        spec_file.write_text(
            "geometry_type: box_barge\n"
            "length: 60.0\n"
            "beam: 20.0\n"
            "draft: 4.0\n"
            "mesh_size: 5.0\n"
        )
        return spec_file

    def test_mesh_build_happy_path(self, tmp_path):
        """mesh-build generates mesh files from spec YAML."""
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        spec_file = self._write_spec_yaml(tmp_path)
        output_dir = tmp_path / "output"

        runner = CliRunner()
        result = runner.invoke(cli, [
            "mesh-build", str(spec_file),
            "-o", str(output_dir),
            "-f", "gdf",
        ])

        assert result.exit_code == 0, f"CLI failed: {result.output}"
        assert (output_dir / "box_barge.gdf").exists()

    def test_mesh_build_multiple_formats(self, tmp_path):
        """mesh-build with multiple -f flags generates all formats."""
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        spec_file = self._write_spec_yaml(tmp_path)
        output_dir = tmp_path / "output"

        runner = CliRunner()
        result = runner.invoke(cli, [
            "mesh-build", str(spec_file),
            "-o", str(output_dir),
            "-f", "gdf", "-f", "dat", "-f", "msh",
        ])

        assert result.exit_code == 0, f"CLI failed: {result.output}"
        assert (output_dir / "box_barge.gdf").exists()
        assert (output_dir / "box_barge.dat").exists()
        assert (output_dir / "box_barge.msh").exists()

    def test_mesh_build_custom_stem(self, tmp_path):
        """--stem controls output file name."""
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        spec_file = self._write_spec_yaml(tmp_path)
        output_dir = tmp_path / "output"

        runner = CliRunner()
        result = runner.invoke(cli, [
            "mesh-build", str(spec_file),
            "-o", str(output_dir),
            "-f", "gdf",
            "--stem", "my_vessel",
        ])

        assert result.exit_code == 0, f"CLI failed: {result.output}"
        assert (output_dir / "my_vessel.gdf").exists()

    def test_mesh_build_dry_run(self, tmp_path):
        """--dry-run validates spec without generating mesh files."""
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        spec_file = self._write_spec_yaml(tmp_path)
        output_dir = tmp_path / "output"

        runner = CliRunner()
        result = runner.invoke(cli, [
            "mesh-build", str(spec_file),
            "-o", str(output_dir),
            "-f", "gdf",
            "--dry-run",
        ])

        assert result.exit_code == 0, f"CLI failed: {result.output}"
        # Dry run should NOT create mesh files
        assert not (output_dir / "box_barge.gdf").exists()
        assert "dry" in result.output.lower() or "valid" in result.output.lower()

    def test_mesh_build_invalid_spec_yaml(self, tmp_path):
        """mesh-build with invalid geometry type fails gracefully."""
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        spec_file = tmp_path / "bad_spec.yaml"
        spec_file.write_text("geometry_type: invalid_shape\nmesh_size: 5.0\n")
        output_dir = tmp_path / "output"

        runner = CliRunner()
        result = runner.invoke(cli, [
            "mesh-build", str(spec_file),
            "-o", str(output_dir),
            "-f", "gdf",
        ])

        assert result.exit_code != 0

    def test_mesh_build_outputs_summary(self, tmp_path):
        """mesh-build prints panel/node count summary."""
        from click.testing import CliRunner
        from digitalmodel.hydrodynamics.diffraction.cli import cli

        spec_file = self._write_spec_yaml(tmp_path)
        output_dir = tmp_path / "output"

        runner = CliRunner()
        result = runner.invoke(cli, [
            "mesh-build", str(spec_file),
            "-o", str(output_dir),
            "-f", "gdf",
        ])

        assert result.exit_code == 0
        # Should print panel/node counts
        assert "panel" in result.output.lower() or "node" in result.output.lower()
