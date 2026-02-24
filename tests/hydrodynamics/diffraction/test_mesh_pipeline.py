"""Tests for diffraction mesh pipeline (WRK-060).

Tests cover the MeshPipeline integration layer that wraps BEMRosetta
mesh handlers for use in the diffraction spec converter. All tests use
real mesh fixtures from tests/modules/bemrosetta/fixtures/.

Note: Assertions use value-based comparisons (not ``isinstance`` or enum
identity) because the src-layout can produce two distinct module
namespaces (``src.digitalmodel`` vs. ``digitalmodel``).
"""

from pathlib import Path

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.input_schemas import MeshFormatType
from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

BEMROSETTA_FIXTURES = (
    Path(__file__).parent.parent / "bemrosetta" / "fixtures"
)


@pytest.fixture
def pipeline():
    """Create a MeshPipeline instance."""
    return MeshPipeline()


@pytest.fixture
def gdf_fixture() -> Path:
    """Path to sample_box.gdf fixture."""
    return BEMROSETTA_FIXTURES / "sample_box.gdf"


@pytest.fixture
def dat_fixture() -> Path:
    """Path to sample_box.dat fixture."""
    return BEMROSETTA_FIXTURES / "sample_box.dat"


# ---------------------------------------------------------------------------
# Test: Loading
# ---------------------------------------------------------------------------


class TestMeshPipelineLoad:
    """Tests for loading mesh files."""

    def test_load_gdf_mesh(self, pipeline, gdf_fixture):
        """Test MeshPipeline can load a GDF mesh file."""
        mesh = pipeline.load(gdf_fixture)

        assert hasattr(mesh, "n_vertices")
        assert hasattr(mesh, "n_panels")
        assert mesh.n_vertices > 0
        assert mesh.n_panels > 0
        assert mesh.format_origin.value == "gdf"

    def test_load_dat_mesh(self, pipeline, dat_fixture):
        """Test MeshPipeline can load a DAT mesh file."""
        mesh = pipeline.load(dat_fixture)

        assert hasattr(mesh, "n_vertices")
        assert hasattr(mesh, "n_panels")
        assert mesh.n_vertices > 0
        assert mesh.n_panels > 0
        assert mesh.format_origin.value == "dat"

    def test_load_with_explicit_format(self, pipeline, gdf_fixture):
        """Test loading with an explicit format override."""
        mesh = pipeline.load(gdf_fixture, format=MeshFormatType.GDF)

        assert mesh.n_panels > 0

    def test_load_auto_detects_format(self, pipeline, gdf_fixture):
        """Test that AUTO format detects from file extension."""
        mesh = pipeline.load(gdf_fixture, format=MeshFormatType.AUTO)

        assert mesh.n_panels > 0


# ---------------------------------------------------------------------------
# Test: Format detection
# ---------------------------------------------------------------------------


class TestMeshPipelineFormatDetection:
    """Tests for auto-detection of mesh format."""

    def test_detect_gdf_format(self, pipeline):
        """Test auto-detection of GDF format from .gdf extension."""
        detected = pipeline.detect_format(Path("some_mesh.gdf"))
        assert detected.value == "gdf"

    def test_detect_dat_format(self, pipeline):
        """Test auto-detection of DAT format from .dat extension."""
        detected = pipeline.detect_format(Path("some_mesh.dat"))
        assert detected.value == "dat"

    def test_detect_stl_format(self, pipeline):
        """Test auto-detection of STL format from .stl extension."""
        detected = pipeline.detect_format(Path("some_mesh.stl"))
        assert detected.value == "stl"

    def test_detect_unknown_format_raises(self, pipeline):
        """Test that unknown extension raises ValueError."""
        with pytest.raises(ValueError, match="Unsupported"):
            pipeline.detect_format(Path("some_mesh.xyz"))

    def test_detect_case_insensitive(self, pipeline):
        """Test that detection is case-insensitive."""
        detected = pipeline.detect_format(Path("some_mesh.GDF"))
        assert detected.value == "gdf"


# ---------------------------------------------------------------------------
# Test: Round-trip conversions
# ---------------------------------------------------------------------------


class TestMeshPipelineRoundTrip:
    """Tests for round-trip mesh conversions."""

    def test_roundtrip_gdf(self, pipeline, gdf_fixture, tmp_path):
        """Test GDF -> PanelMesh -> GDF preserves vertex/panel count."""
        original = pipeline.load(gdf_fixture)
        output_path = tmp_path / "roundtrip.gdf"

        result = pipeline.convert_by_name(original, "gdf", output_path)
        reloaded = pipeline.load(result)

        assert reloaded.n_panels == original.n_panels
        assert reloaded.n_vertices == original.n_vertices

    def test_roundtrip_dat(self, pipeline, dat_fixture, tmp_path):
        """Test DAT -> PanelMesh -> DAT preserves vertex/panel count."""
        original = pipeline.load(dat_fixture)
        output_path = tmp_path / "roundtrip.dat"

        result = pipeline.convert_by_name(original, "dat", output_path)
        reloaded = pipeline.load(result)

        assert reloaded.n_panels == original.n_panels
        assert reloaded.n_vertices == original.n_vertices


# ---------------------------------------------------------------------------
# Test: Cross-format conversion
# ---------------------------------------------------------------------------


class TestMeshPipelineCrossFormat:
    """Tests for cross-format mesh conversions."""

    def test_gdf_to_dat_preserves_panel_count(
        self, pipeline, gdf_fixture, tmp_path
    ):
        """Test GDF -> DAT conversion preserves panel count."""
        gdf_mesh = pipeline.load(gdf_fixture)
        dat_path = tmp_path / "converted.dat"

        pipeline.convert_by_name(gdf_mesh, "dat", dat_path)
        dat_mesh = pipeline.load(dat_path)

        assert dat_mesh.n_panels == gdf_mesh.n_panels

    def test_dat_to_gdf_preserves_panel_count(
        self, pipeline, dat_fixture, tmp_path
    ):
        """Test DAT -> GDF conversion preserves panel count."""
        dat_mesh = pipeline.load(dat_fixture)
        gdf_path = tmp_path / "converted.gdf"

        pipeline.convert_by_name(dat_mesh, "gdf", gdf_path)
        gdf_mesh = pipeline.load(gdf_path)

        assert gdf_mesh.n_panels == dat_mesh.n_panels


# ---------------------------------------------------------------------------
# Test: Solver preparation
# ---------------------------------------------------------------------------


class TestMeshPipelineSolverPreparation:
    """Tests for solver-specific mesh preparation."""

    def test_prepare_for_aqwa_returns_dat(
        self, pipeline, gdf_fixture, tmp_path
    ):
        """Test prepare_for_solver('aqwa') returns DAT format path."""
        result_path = pipeline.prepare_for_solver(
            gdf_fixture, "aqwa", tmp_path
        )

        assert result_path.exists()
        assert result_path.suffix == ".dat"

    def test_prepare_for_orcawave_returns_gdf(
        self, pipeline, dat_fixture, tmp_path
    ):
        """Test prepare_for_solver('orcawave') returns GDF format path."""
        result_path = pipeline.prepare_for_solver(
            dat_fixture, "orcawave", tmp_path
        )

        assert result_path.exists()
        assert result_path.suffix == ".gdf"

    def test_prepare_for_aqwa_with_dat_input_no_conversion(
        self, pipeline, dat_fixture, tmp_path
    ):
        """Test that AQWA with DAT input still produces a valid file."""
        result_path = pipeline.prepare_for_solver(
            dat_fixture, "aqwa", tmp_path
        )

        assert result_path.exists()
        assert result_path.suffix == ".dat"

        # Verify the output mesh is valid
        mesh = pipeline.load(result_path)
        assert mesh.n_panels > 0

    def test_prepare_for_orcawave_with_gdf_input_no_conversion(
        self, pipeline, gdf_fixture, tmp_path
    ):
        """Test that OrcaWave with GDF input still produces a valid file."""
        result_path = pipeline.prepare_for_solver(
            gdf_fixture, "orcawave", tmp_path
        )

        assert result_path.exists()
        assert result_path.suffix == ".gdf"

        mesh = pipeline.load(result_path)
        assert mesh.n_panels > 0

    def test_prepare_for_unknown_solver_raises(
        self, pipeline, gdf_fixture, tmp_path
    ):
        """Test that unknown solver raises ValueError."""
        with pytest.raises(ValueError, match="Unknown solver"):
            pipeline.prepare_for_solver(gdf_fixture, "unknown_solver", tmp_path)


# ---------------------------------------------------------------------------
# Test: Mesh quality validation
# ---------------------------------------------------------------------------


class TestMeshPipelineValidation:
    """Tests for mesh quality validation."""

    def test_validate_returns_quality_report(self, pipeline, gdf_fixture):
        """Test that validate returns a MeshQualityReport."""
        mesh = pipeline.load(gdf_fixture)
        report = pipeline.validate(mesh)

        assert hasattr(report, "n_panels")
        assert hasattr(report, "quality_score")
        assert hasattr(report, "is_valid")

    def test_validate_report_has_panel_count(self, pipeline, gdf_fixture):
        """Test that quality report includes panel count."""
        mesh = pipeline.load(gdf_fixture)
        report = pipeline.validate(mesh)

        assert report.n_panels == mesh.n_panels
        assert report.n_vertices == mesh.n_vertices

    def test_validate_report_has_quality_score(self, pipeline, gdf_fixture):
        """Test that quality report includes a quality score."""
        mesh = pipeline.load(gdf_fixture)
        report = pipeline.validate(mesh)

        assert 0 <= report.quality_score <= 100

    def test_validate_good_mesh_has_high_score(self, pipeline, gdf_fixture):
        """Test that the sample box mesh has a reasonable quality score."""
        mesh = pipeline.load(gdf_fixture)
        report = pipeline.validate(mesh)

        # The sample box should be reasonable quality
        assert report.quality_score > 50


# ---------------------------------------------------------------------------
# Test: MeshFormatType to MeshFormat mapping
# ---------------------------------------------------------------------------


class TestFormatMapping:
    """Tests for mapping spec MeshFormatType to BEMRosetta MeshFormat."""

    def test_map_gdf(self, pipeline):
        """Test mapping MeshFormatType.GDF to MeshFormat.GDF."""
        result = pipeline.map_format_type(MeshFormatType.GDF)
        assert result.value == "gdf"

    def test_map_dat(self, pipeline):
        """Test mapping MeshFormatType.DAT to MeshFormat.DAT."""
        result = pipeline.map_format_type(MeshFormatType.DAT)
        assert result.value == "dat"

    def test_map_stl(self, pipeline):
        """Test mapping MeshFormatType.STL to MeshFormat.STL."""
        result = pipeline.map_format_type(MeshFormatType.STL)
        assert result.value == "stl"

    def test_map_auto_raises(self, pipeline):
        """Test that mapping AUTO raises ValueError (needs a path)."""
        with pytest.raises(ValueError, match="AUTO"):
            pipeline.map_format_type(MeshFormatType.AUTO)
