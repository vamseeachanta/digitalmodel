"""
Tests for BEMRosetta CLI module.

Tests cover all CLI commands:
- convert: AQWA to OrcaFlex conversion
- info: Display file information
- validate: Coefficient validation
- convert-mesh: Mesh format conversion
- validate-mesh: Mesh quality validation
- status: Module status display

Following TDD - these tests define the expected behavior.
"""

import pytest
import numpy as np
from pathlib import Path
from click.testing import CliRunner

from digitalmodel.hydrodynamics.bemrosetta.cli import cli, main


# ============================================================================
# Fixtures
# ============================================================================


@pytest.fixture
def runner():
    """Create Click CLI runner."""
    return CliRunner()


@pytest.fixture
def temp_lis_file(tmp_path):
    """Create a minimal AQWA .LIS file for testing."""
    content = """
AQWA 19.2

STRUCTURE NAME: TestVessel

WATER DEPTH = 100.00

ADDED MASS-VARIATION WITH WAVE PERIOD/FREQUENCY

  PERIOD   FREQ     M11        M22        M33        M44        M55        M66        M13        M15        M24        M26        M35        M46
  ------   ----     ---        ---        ---        ---        ---        ---        ---        ---        ---        ---        ---        ---
   22.00   0.286   2.33E+06   2.93E+07   1.80E+08   1.00E+09   2.00E+09   5.00E+08   1.00E+05   2.00E+06   3.00E+05   4.00E+06   5.00E+05   6.00E+06
   19.00   0.331   2.40E+06   3.00E+07   1.85E+08   1.05E+09   2.05E+09   5.10E+08   1.10E+05   2.10E+06   3.10E+05   4.10E+06   5.10E+05   6.10E+06
   16.00   0.393   2.50E+06   3.10E+07   1.90E+08   1.10E+09   2.10E+09   5.20E+08   1.20E+05   2.20E+06   3.20E+05   4.20E+06   5.20E+05   6.20E+06

DAMPING-VARIATION WITH WAVE PERIOD/FREQUENCY

  PERIOD   FREQ     C11        C22        C33        C44        C55        C66        C13        C15        C24        C26        C35        C46
  ------   ----     ---        ---        ---        ---        ---        ---        ---        ---        ---        ---        ---        ---
   22.00   0.286   1.00E+05   2.00E+05   3.00E+05   4.00E+05   5.00E+05   6.00E+05   1.00E+04   2.00E+04   3.00E+04   4.00E+04   5.00E+04   6.00E+04
   19.00   0.331   1.10E+05   2.10E+05   3.10E+05   4.10E+05   5.10E+05   6.10E+05   1.10E+04   2.10E+04   3.10E+04   4.10E+04   5.10E+04   6.10E+04
   16.00   0.393   1.20E+05   2.20E+05   3.20E+05   4.20E+05   5.20E+05   6.20E+05   1.20E+04   2.20E+04   3.20E+04   4.20E+04   5.20E+04   6.20E+04

R.A.O.S-VARIATION WITH WAVE PERIOD/FREQUENCY

  PERIOD   FREQ   DIRECTION    X-AMP   X-PHA    Y-AMP   Y-PHA    Z-AMP   Z-PHA   RX-AMP  RX-PHA   RY-AMP  RY-PHA   RZ-AMP  RZ-PHA
  ------   ----   ---------    -----   -----    -----   -----    -----   -----   ------  ------   ------  ------   ------  ------
   22.00   0.286   -180.00    0.8926  -90.20   0.0100   0.00    0.9500  -85.00   0.0500  -45.00   1.0000  -88.00   0.0050  -10.00
   19.00   0.331              0.8500  -89.00   0.0120   1.00    0.9200  -84.00   0.0550  -44.00   1.0500  -87.00   0.0060  -11.00
   16.00   0.393              0.8000  -88.00   0.0140   2.00    0.9000  -83.00   0.0600  -43.00   1.1000  -86.00   0.0070  -12.00
   22.00   0.286   -135.00    0.7500  -70.00   0.3000  -20.00   0.9300  -80.00   0.1500  -50.00   0.9000  -75.00   0.1000  -30.00
   19.00   0.331              0.7200  -69.00   0.3100  -19.00   0.9100  -79.00   0.1550  -49.00   0.9200  -74.00   0.1050  -29.00
   16.00   0.393              0.6900  -68.00   0.3200  -18.00   0.8900  -78.00   0.1600  -48.00   0.9400  -73.00   0.1100  -28.00
"""
    lis_file = tmp_path / "test_vessel.LIS"
    lis_file.write_text(content)
    return lis_file


@pytest.fixture
def temp_qtf_file(tmp_path):
    """Create a minimal QTF file for testing."""
    content = """QTF DIFFERENCE FREQUENCY
STRUCTURE: TestVessel
NFREQ1: 3
NFREQ2: 3
NHEADING: 2
FREQUENCIES1: 0.286 0.331 0.393
FREQUENCIES2: 0.286 0.331 0.393
HEADINGS: 0.0 180.0
DOF: 1
HEADING: 0.0
REAL:
  1.00E+03  1.10E+03  1.20E+03
  1.10E+03  1.50E+03  1.60E+03
  1.20E+03  1.60E+03  2.00E+03
IMAG:
  0.00E+00  1.00E+02  2.00E+02
  -1.00E+02  0.00E+00  1.50E+02
  -2.00E+02  -1.50E+02  0.00E+00
DOF: 1
HEADING: 180.0
REAL:
  9.00E+02  1.00E+03  1.10E+03
  1.00E+03  1.40E+03  1.50E+03
  1.10E+03  1.50E+03  1.90E+03
IMAG:
  0.00E+00  9.00E+01  1.80E+02
  -9.00E+01  0.00E+00  1.35E+02
  -1.80E+02  -1.35E+02  0.00E+00
"""
    qtf_file = tmp_path / "test_vessel.qtf"
    qtf_file.write_text(content)
    return qtf_file


@pytest.fixture
def temp_gdf_file(tmp_path):
    """Create a minimal GDF mesh file for testing."""
    content = """# Test mesh file
1.0  9.81
0  0
4
0.0  0.0  0.0
1.0  0.0  0.0
1.0  1.0  0.0
0.0  1.0  0.0
0.0  0.0  -1.0
1.0  0.0  -1.0
1.0  1.0  -1.0
0.0  1.0  -1.0
0.0  0.0  0.0
1.0  0.0  0.0
1.0  0.0  -1.0
0.0  0.0  -1.0
1.0  0.0  0.0
1.0  1.0  0.0
1.0  1.0  -1.0
1.0  0.0  -1.0
1.0  1.0  0.0
0.0  1.0  0.0
0.0  1.0  -1.0
1.0  1.0  -1.0
"""
    gdf_file = tmp_path / "test_mesh.gdf"
    gdf_file.write_text(content)
    return gdf_file


# ============================================================================
# CLI Group Tests
# ============================================================================


class TestCLIGroup:
    """Tests for CLI group and basic functionality."""

    def test_cli_help(self, runner):
        """CLI shows help message."""
        result = runner.invoke(cli, ["--help"])
        assert result.exit_code == 0
        assert "BEMRosetta" in result.output
        assert "Hydrodynamic" in result.output

    def test_cli_version(self, runner):
        """CLI shows version."""
        result = runner.invoke(cli, ["--version"])
        assert result.exit_code == 0
        assert "1.0.0" in result.output

    def test_main_entry_point(self):
        """Main function is callable."""
        # main() calls cli() which expects arguments
        # Just check it's importable and callable
        assert callable(main)


# ============================================================================
# Convert Command Tests
# ============================================================================


class TestConvertCommand:
    """Tests for the convert command."""

    def test_convert_help(self, runner):
        """Convert command shows help."""
        result = runner.invoke(cli, ["convert", "--help"])
        assert result.exit_code == 0
        assert "Convert AQWA output" in result.output

    def test_convert_basic(self, runner, temp_lis_file, tmp_path):
        """Convert command processes AQWA file."""
        output_dir = tmp_path / "output"
        result = runner.invoke(
            cli,
            ["convert", str(temp_lis_file), "-o", str(output_dir)],
        )
        assert result.exit_code == 0
        assert "Success" in result.output
        assert output_dir.exists()

    def test_convert_creates_csv_files(self, runner, temp_lis_file, tmp_path):
        """Convert command creates expected CSV files."""
        output_dir = tmp_path / "output"
        result = runner.invoke(
            cli,
            ["convert", str(temp_lis_file), "-o", str(output_dir)],
        )
        assert result.exit_code == 0

        # Check for expected output files
        assert (output_dir / "test_vessel_added_mass.csv").exists()
        assert (output_dir / "test_vessel_damping.csv").exists()
        assert (output_dir / "test_vessel_raos.csv").exists()

    def test_convert_with_qtf(self, runner, temp_lis_file, temp_qtf_file, tmp_path):
        """Convert command handles QTF file."""
        output_dir = tmp_path / "output"
        result = runner.invoke(
            cli,
            [
                "convert",
                str(temp_lis_file),
                "--qtf",
                str(temp_qtf_file),
                "-o",
                str(output_dir),
            ],
        )
        assert result.exit_code == 0
        assert "QTF" in result.output

    def test_convert_with_vessel_name(self, runner, temp_lis_file, tmp_path):
        """Convert command uses custom vessel name."""
        output_dir = tmp_path / "output"
        result = runner.invoke(
            cli,
            [
                "convert",
                str(temp_lis_file),
                "-o",
                str(output_dir),
                "-n",
                "CustomVessel",
            ],
        )
        assert result.exit_code == 0
        assert (output_dir / "CustomVessel_added_mass.csv").exists()

    def test_convert_no_validate(self, runner, temp_lis_file, tmp_path):
        """Convert command can skip validation."""
        output_dir = tmp_path / "output"
        result = runner.invoke(
            cli,
            ["convert", str(temp_lis_file), "-o", str(output_dir), "--no-validate"],
        )
        assert result.exit_code == 0

    def test_convert_missing_file(self, runner, tmp_path):
        """Convert command fails for missing file."""
        result = runner.invoke(cli, ["convert", str(tmp_path / "nonexistent.LIS")])
        assert result.exit_code != 0

    def test_convert_default_output_dir(self, runner, temp_lis_file):
        """Convert command uses default output directory."""
        result = runner.invoke(cli, ["convert", str(temp_lis_file)])
        assert result.exit_code == 0
        # Default output dir should be created next to input
        expected_dir = temp_lis_file.parent / "orcaflex_output"
        assert expected_dir.exists()


# ============================================================================
# Info Command Tests
# ============================================================================


class TestInfoCommand:
    """Tests for the info command."""

    def test_info_help(self, runner):
        """Info command shows help."""
        result = runner.invoke(cli, ["info", "--help"])
        assert result.exit_code == 0
        assert "Display information" in result.output

    def test_info_displays_metadata(self, runner, temp_lis_file):
        """Info command displays file metadata."""
        result = runner.invoke(cli, ["info", str(temp_lis_file)])
        assert result.exit_code == 0
        assert "test_vessel.LIS" in result.output
        assert "Water Depth" in result.output

    def test_info_displays_frequency_info(self, runner, temp_lis_file):
        """Info command displays frequency information."""
        result = runner.invoke(cli, ["info", str(temp_lis_file)])
        assert result.exit_code == 0
        assert "Frequency Range" in result.output

    def test_info_displays_heading_info(self, runner, temp_lis_file):
        """Info command displays heading information."""
        result = runner.invoke(cli, ["info", str(temp_lis_file)])
        assert result.exit_code == 0
        assert "Heading Range" in result.output

    def test_info_displays_data_summary(self, runner, temp_lis_file):
        """Info command displays data availability summary."""
        result = runner.invoke(cli, ["info", str(temp_lis_file)])
        assert result.exit_code == 0
        assert "Data Available" in result.output
        assert "RAOs" in result.output
        assert "Added Mass" in result.output
        assert "Damping" in result.output

    def test_info_missing_file(self, runner, tmp_path):
        """Info command fails for missing file."""
        result = runner.invoke(cli, ["info", str(tmp_path / "nonexistent.LIS")])
        assert result.exit_code != 0


# ============================================================================
# Validate Command Tests
# ============================================================================


class TestValidateCommand:
    """Tests for the validate command."""

    def test_validate_help(self, runner):
        """Validate command shows help."""
        result = runner.invoke(cli, ["validate", "--help"])
        assert result.exit_code == 0
        assert "Validate hydrodynamic" in result.output

    def test_validate_basic(self, runner, temp_lis_file):
        """Validate command processes file."""
        result = runner.invoke(cli, ["validate", str(temp_lis_file)])
        assert result.exit_code == 0
        assert "Validating" in result.output

    def test_validate_passes_for_good_data(self, runner, temp_lis_file):
        """Validate command passes for valid data."""
        result = runner.invoke(cli, ["validate", str(temp_lis_file)])
        assert result.exit_code == 0
        assert "passed" in result.output.lower()

    def test_validate_strict_mode(self, runner, temp_lis_file):
        """Validate command supports strict mode."""
        result = runner.invoke(cli, ["validate", str(temp_lis_file), "--strict"])
        assert result.exit_code == 0

    def test_validate_causality_check(self, runner, temp_lis_file):
        """Validate command supports causality check."""
        result = runner.invoke(cli, ["validate", str(temp_lis_file), "--causality"])
        assert result.exit_code == 0
        assert "Kramers-Kronig" in result.output

    def test_validate_missing_file(self, runner, tmp_path):
        """Validate command fails for missing file."""
        result = runner.invoke(cli, ["validate", str(tmp_path / "nonexistent.LIS")])
        assert result.exit_code != 0


# ============================================================================
# Convert-Mesh Command Tests
# ============================================================================


class TestConvertMeshCommand:
    """Tests for the convert-mesh command."""

    def test_convert_mesh_help(self, runner):
        """Convert-mesh command shows help."""
        result = runner.invoke(cli, ["convert-mesh", "--help"])
        assert result.exit_code == 0
        assert "Convert mesh" in result.output

    def test_convert_mesh_gdf_to_stl(self, runner, temp_gdf_file, tmp_path):
        """Convert-mesh converts GDF to STL."""
        output_file = tmp_path / "output.stl"
        result = runner.invoke(
            cli,
            ["convert-mesh", str(temp_gdf_file), "-o", str(output_file)],
        )
        assert result.exit_code == 0
        assert output_file.exists()

    def test_convert_mesh_gdf_to_dat(self, runner, temp_gdf_file, tmp_path):
        """Convert-mesh converts GDF to DAT."""
        output_file = tmp_path / "output.dat"
        result = runner.invoke(
            cli,
            ["convert-mesh", str(temp_gdf_file), "-o", str(output_file)],
        )
        assert result.exit_code == 0
        assert output_file.exists()

    def test_convert_mesh_with_format_override(self, runner, temp_gdf_file, tmp_path):
        """Convert-mesh supports explicit format specification."""
        output_file = tmp_path / "output.mesh"
        result = runner.invoke(
            cli,
            ["convert-mesh", str(temp_gdf_file), "-o", str(output_file), "-f", "stl"],
        )
        assert result.exit_code == 0

    def test_convert_mesh_missing_file(self, runner, tmp_path):
        """Convert-mesh fails for missing input file."""
        result = runner.invoke(
            cli,
            [
                "convert-mesh",
                str(tmp_path / "nonexistent.gdf"),
                "-o",
                str(tmp_path / "output.stl"),
            ],
        )
        assert result.exit_code != 0

    def test_convert_mesh_requires_output(self, runner, temp_gdf_file):
        """Convert-mesh requires output path."""
        result = runner.invoke(cli, ["convert-mesh", str(temp_gdf_file)])
        assert result.exit_code != 0


# ============================================================================
# Validate-Mesh Command Tests
# ============================================================================


class TestValidateMeshCommand:
    """Tests for the validate-mesh command."""

    def test_validate_mesh_help(self, runner):
        """Validate-mesh command shows help."""
        result = runner.invoke(cli, ["validate-mesh", "--help"])
        assert result.exit_code == 0
        assert "Validate mesh" in result.output

    def test_validate_mesh_basic(self, runner, temp_gdf_file):
        """Validate-mesh displays quality report."""
        result = runner.invoke(cli, ["validate-mesh", str(temp_gdf_file)])
        assert result.exit_code == 0
        assert "Quality Report" in result.output

    def test_validate_mesh_shows_geometry(self, runner, temp_gdf_file):
        """Validate-mesh displays geometry information."""
        result = runner.invoke(cli, ["validate-mesh", str(temp_gdf_file)])
        assert result.exit_code == 0
        assert "Vertices" in result.output
        assert "Panels" in result.output

    def test_validate_mesh_shows_statistics(self, runner, temp_gdf_file):
        """Validate-mesh displays panel statistics."""
        result = runner.invoke(cli, ["validate-mesh", str(temp_gdf_file)])
        assert result.exit_code == 0
        assert "Min Area" in result.output
        assert "Max Area" in result.output

    def test_validate_mesh_shows_quality_score(self, runner, temp_gdf_file):
        """Validate-mesh displays quality score."""
        result = runner.invoke(cli, ["validate-mesh", str(temp_gdf_file)])
        assert result.exit_code == 0
        assert "Quality Score" in result.output

    def test_validate_mesh_with_check_normals(self, runner, temp_gdf_file):
        """Validate-mesh supports normal checking flag."""
        result = runner.invoke(
            cli, ["validate-mesh", str(temp_gdf_file), "--check-normals"]
        )
        assert result.exit_code == 0

    def test_validate_mesh_missing_file(self, runner, tmp_path):
        """Validate-mesh fails for missing file."""
        result = runner.invoke(
            cli, ["validate-mesh", str(tmp_path / "nonexistent.gdf")]
        )
        assert result.exit_code != 0

    def test_validate_mesh_unsupported_format(self, runner, tmp_path):
        """Validate-mesh fails for unsupported format."""
        bad_file = tmp_path / "test.xyz"
        bad_file.write_text("some content")
        result = runner.invoke(cli, ["validate-mesh", str(bad_file)])
        assert result.exit_code != 0


# ============================================================================
# Status Command Tests
# ============================================================================


class TestStatusCommand:
    """Tests for the status command."""

    def test_status_help(self, runner):
        """Status command shows help."""
        result = runner.invoke(cli, ["status", "--help"])
        assert result.exit_code == 0

    def test_status_shows_executable_status(self, runner):
        """Status command shows executable availability."""
        result = runner.invoke(cli, ["status"])
        assert result.exit_code == 0
        assert "BEMRosetta Executable" in result.output

    def test_status_shows_input_formats(self, runner):
        """Status command shows supported input formats."""
        result = runner.invoke(cli, ["status"])
        assert result.exit_code == 0
        assert "Supported Input Formats" in result.output
        assert "AQWA" in result.output

    def test_status_shows_output_formats(self, runner):
        """Status command shows supported output formats."""
        result = runner.invoke(cli, ["status"])
        assert result.exit_code == 0
        assert "Supported Output Formats" in result.output
        assert "OrcaFlex" in result.output

    def test_status_shows_mesh_formats(self, runner):
        """Status command shows supported mesh formats."""
        result = runner.invoke(cli, ["status"])
        assert result.exit_code == 0
        assert "Supported Mesh Formats" in result.output
        assert "GDF" in result.output
        assert "STL" in result.output

    def test_status_shows_validation_features(self, runner):
        """Status command shows validation features."""
        result = runner.invoke(cli, ["status"])
        assert result.exit_code == 0
        assert "Validation Features" in result.output
        assert "Kramers-Kronig" in result.output


# ============================================================================
# Integration Tests
# ============================================================================


class TestCLIIntegration:
    """Integration tests for CLI workflows."""

    def test_full_conversion_workflow(self, runner, temp_lis_file, tmp_path):
        """Test complete conversion workflow."""
        output_dir = tmp_path / "conversion_output"

        # Run conversion
        result = runner.invoke(
            cli,
            ["convert", str(temp_lis_file), "-o", str(output_dir)],
        )
        assert result.exit_code == 0

        # Verify outputs exist
        assert (output_dir / "test_vessel_added_mass.csv").exists()
        assert (output_dir / "test_vessel_damping.csv").exists()
        assert (output_dir / "test_vessel_raos.csv").exists()

        # Verify CSV content
        import csv

        with open(output_dir / "test_vessel_added_mass.csv") as f:
            reader = csv.reader(f)
            header = next(reader)
            assert "Frequency_rad/s" in header
            data_rows = list(reader)
            assert len(data_rows) >= 3

    def test_info_then_validate_workflow(self, runner, temp_lis_file):
        """Test info followed by validation workflow."""
        # Get info
        info_result = runner.invoke(cli, ["info", str(temp_lis_file)])
        assert info_result.exit_code == 0

        # Validate
        validate_result = runner.invoke(cli, ["validate", str(temp_lis_file)])
        assert validate_result.exit_code == 0

    def test_mesh_conversion_then_validation(self, runner, temp_gdf_file, tmp_path):
        """Test mesh conversion followed by validation."""
        output_stl = tmp_path / "converted.stl"

        # Convert
        convert_result = runner.invoke(
            cli,
            ["convert-mesh", str(temp_gdf_file), "-o", str(output_stl)],
        )
        assert convert_result.exit_code == 0

        # Validate converted mesh
        validate_result = runner.invoke(cli, ["validate-mesh", str(output_stl)])
        assert validate_result.exit_code == 0


# ============================================================================
# Error Handling Tests
# ============================================================================


class TestCLIErrorHandling:
    """Tests for CLI error handling."""

    def test_convert_handles_parser_error(self, runner, tmp_path):
        """Convert command handles parser errors gracefully."""
        # Create invalid file
        invalid_file = tmp_path / "invalid.LIS"
        invalid_file.write_text("Not valid AQWA content")

        result = runner.invoke(cli, ["convert", str(invalid_file)])
        assert result.exit_code != 0
        assert "error" in result.output.lower()

    def test_validate_handles_empty_file(self, runner, tmp_path):
        """Validate command handles empty file."""
        empty_file = tmp_path / "empty.LIS"
        empty_file.write_text("")

        result = runner.invoke(cli, ["validate", str(empty_file)])
        assert result.exit_code != 0

    def test_convert_mesh_handles_invalid_format(self, runner, tmp_path):
        """Convert-mesh handles invalid output format."""
        gdf_content = """# Test
1.0  9.81
0  0
1
0.0  0.0  0.0
1.0  0.0  0.0
1.0  1.0  0.0
0.0  1.0  0.0
"""
        gdf_file = tmp_path / "test.gdf"
        gdf_file.write_text(gdf_content)

        result = runner.invoke(
            cli,
            ["convert-mesh", str(gdf_file), "-o", str(tmp_path / "out.xyz"), "-f", "invalid"],
        )
        assert result.exit_code != 0
