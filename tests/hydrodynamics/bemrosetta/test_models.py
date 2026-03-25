"""
Tests for BEMRosetta data models.

Following TDD approach - tests validate existing implementation.
"""

import pytest
import numpy as np
from numpy.testing import assert_array_almost_equal


class TestQTFTypeEnum:
    """Tests for QTFType enum."""

    def test_qtf_type_values(self):
        """Test QTF type enum values."""
        from digitalmodel.hydrodynamics.bemrosetta.models import QTFType

        assert QTFType.SUM_FREQUENCY.value == "sum"
        assert QTFType.DIFFERENCE_FREQUENCY.value == "difference"

    def test_qtf_type_is_enum(self):
        """Test QTFType is proper enum."""
        from digitalmodel.hydrodynamics.bemrosetta.models import QTFType

        assert hasattr(QTFType, "SUM_FREQUENCY")
        assert hasattr(QTFType, "DIFFERENCE_FREQUENCY")

    def test_qtf_type_mean_drift(self):
        """Test MEAN_DRIFT type exists."""
        from digitalmodel.hydrodynamics.bemrosetta.models import QTFType

        assert QTFType.MEAN_DRIFT.value == "mean_drift"


class TestMeshFormatEnum:
    """Tests for MeshFormat enum."""

    def test_mesh_format_values(self):
        """Test supported mesh format values."""
        from digitalmodel.hydrodynamics.bemrosetta.models import MeshFormat

        assert MeshFormat.GDF.value == "gdf"
        assert MeshFormat.DAT.value == "dat"
        assert MeshFormat.STL.value == "stl"

    def test_mesh_format_additional_types(self):
        """Test additional mesh format types."""
        from digitalmodel.hydrodynamics.bemrosetta.models import MeshFormat

        assert MeshFormat.MSH.value == "msh"
        assert MeshFormat.PNL.value == "pnl"
        assert MeshFormat.UNKNOWN.value == "unknown"


class TestBEMSolverMetadata:
    """Tests for BEMSolverMetadata dataclass."""

    def test_creation_default(self):
        """Test creation with default values."""
        from digitalmodel.hydrodynamics.bemrosetta.models import BEMSolverMetadata
        from digitalmodel.hydrodynamics.bemrosetta.models.solver_metadata import BEMSolverType

        metadata = BEMSolverMetadata()

        assert metadata.solver_type == BEMSolverType.UNKNOWN
        assert metadata.solver_version == ""
        assert metadata.water_depth == float("inf")
        assert metadata.body_count == 1
        assert metadata.frequency_count == 0
        assert metadata.heading_count == 0

    def test_creation_with_solver_type(self):
        """Test creation with specific solver type."""
        from digitalmodel.hydrodynamics.bemrosetta.models import BEMSolverMetadata
        from digitalmodel.hydrodynamics.bemrosetta.models.solver_metadata import BEMSolverType

        metadata = BEMSolverMetadata(
            solver_type=BEMSolverType.WAMIT,
            solver_version="7.4",
            water_depth=150.0,
            project_name="Test Project",
            body_count=2,
            frequency_count=50,
            heading_count=12,
        )

        assert metadata.solver_type == BEMSolverType.WAMIT
        assert metadata.solver_version == "7.4"
        assert metadata.water_depth == 150.0
        assert metadata.project_name == "Test Project"
        assert metadata.body_count == 2
        assert metadata.frequency_count == 50
        assert metadata.heading_count == 12

    def test_is_deep_water_property(self):
        """Test deep water detection."""
        from digitalmodel.hydrodynamics.bemrosetta.models import BEMSolverMetadata

        # Infinite depth
        metadata_deep = BEMSolverMetadata(water_depth=float("inf"))
        assert metadata_deep.is_deep_water is True

        # Very deep (>500m)
        metadata_very_deep = BEMSolverMetadata(water_depth=600.0)
        assert metadata_very_deep.is_deep_water is True

        # Shallow
        metadata_shallow = BEMSolverMetadata(water_depth=100.0)
        assert metadata_shallow.is_deep_water is False

    def test_has_qtf_property(self):
        """Test QTF availability detection by solver type."""
        from digitalmodel.hydrodynamics.bemrosetta.models import BEMSolverMetadata
        from digitalmodel.hydrodynamics.bemrosetta.models.solver_metadata import BEMSolverType

        # Solvers that support QTF
        for solver in [BEMSolverType.AQWA, BEMSolverType.WAMIT, BEMSolverType.ORCAWAVE]:
            metadata = BEMSolverMetadata(solver_type=solver)
            assert metadata.has_qtf is True

        # Solvers that may not support QTF
        metadata_nemoh = BEMSolverMetadata(solver_type=BEMSolverType.NEMOH)
        assert metadata_nemoh.has_qtf is False

    def test_to_dict_infinite_depth(self):
        """Test to_dict handles infinite depth correctly."""
        from digitalmodel.hydrodynamics.bemrosetta.models import BEMSolverMetadata

        metadata = BEMSolverMetadata(water_depth=float("inf"))
        result = metadata.to_dict()

        assert result["water_depth"] == "infinite"

    def test_to_dict_finite_depth(self):
        """Test to_dict with finite depth."""
        from digitalmodel.hydrodynamics.bemrosetta.models import BEMSolverMetadata

        metadata = BEMSolverMetadata(water_depth=200.0)
        result = metadata.to_dict()

        assert result["water_depth"] == 200.0

    def test_default_units(self):
        """Test default units are set."""
        from digitalmodel.hydrodynamics.bemrosetta.models import BEMSolverMetadata

        metadata = BEMSolverMetadata()

        assert metadata.units["length"] == "m"
        assert metadata.units["mass"] == "kg"
        assert metadata.units["frequency"] == "rad/s"

    def test_from_dict(self):
        """Test creating metadata from dictionary."""
        from digitalmodel.hydrodynamics.bemrosetta.models import BEMSolverMetadata
        from digitalmodel.hydrodynamics.bemrosetta.models.solver_metadata import BEMSolverType

        data = {
            "solver_type": "WAMIT",
            "solver_version": "7.4",
            "water_depth": 150.0,
            "project_name": "Test",
            "frequency_count": 50,
        }

        metadata = BEMSolverMetadata.from_dict(data)

        assert metadata.solver_type == BEMSolverType.WAMIT
        assert metadata.solver_version == "7.4"
        assert metadata.water_depth == 150.0


class TestQTFData:
    """Tests for QTFData dataclass."""

    def test_creation(self):
        """Test QTF data creation."""
        from digitalmodel.hydrodynamics.bemrosetta.models import QTFData, QTFType

        qtf = QTFData(
            qtf_type=QTFType.DIFFERENCE_FREQUENCY,
            body_name="Test Vessel",
        )

        assert qtf.qtf_type == QTFType.DIFFERENCE_FREQUENCY
        assert qtf.body_name == "Test Vessel"
        assert qtf.n_frequencies == 0
        assert qtf.n_headings == 0

    def test_default_dof_names(self):
        """Test default DOF names."""
        from digitalmodel.hydrodynamics.bemrosetta.models import QTFData, QTFType

        qtf = QTFData(qtf_type=QTFType.SUM_FREQUENCY)

        expected_dofs = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
        assert qtf.dof_names == expected_dofs

    def test_with_frequencies_and_headings(self):
        """Test QTF data with frequency and heading arrays."""
        from digitalmodel.hydrodynamics.bemrosetta.models import QTFData, QTFType

        frequencies = np.linspace(0.1, 2.0, 10)
        headings = np.array([0.0, 45.0, 90.0, 135.0, 180.0])

        qtf = QTFData(
            qtf_type=QTFType.DIFFERENCE_FREQUENCY,
            frequencies=frequencies,
            headings=headings,
        )

        assert qtf.n_frequencies == 10
        assert qtf.n_headings == 5

    def test_to_dict(self):
        """Test QTF data serialization."""
        from digitalmodel.hydrodynamics.bemrosetta.models import QTFData, QTFType

        frequencies = np.linspace(0.1, 2.0, 5)
        headings = np.array([0.0, 90.0, 180.0])

        qtf = QTFData(
            qtf_type=QTFType.SUM_FREQUENCY,
            body_name="Test",
            frequencies=frequencies,
            headings=headings,
        )

        result = qtf.to_dict()

        assert result["qtf_type"] == "sum"
        assert result["body_name"] == "Test"
        assert isinstance(result["frequencies"], list)
        assert isinstance(result["headings"], list)
        assert len(result["dof_names"]) == 6

    def test_from_dict(self):
        """Test QTF data deserialization."""
        from digitalmodel.hydrodynamics.bemrosetta.models import QTFData, QTFType

        data = {
            "qtf_type": "difference",
            "body_name": "Vessel",
            "frequencies": [0.1, 0.5, 1.0],
            "headings": [0.0, 90.0],
            "dof_names": ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"],
            "components": [],
        }

        qtf = QTFData.from_dict(data)

        assert qtf.qtf_type == QTFType.DIFFERENCE_FREQUENCY
        assert qtf.body_name == "Vessel"
        assert qtf.n_frequencies == 3
        assert qtf.n_headings == 2


class TestQTFComponent:
    """Tests for QTFComponent dataclass."""

    def test_creation(self):
        """Test QTF component creation."""
        from digitalmodel.hydrodynamics.bemrosetta.models.qtf_data import QTFComponent, QTFType

        frequencies = np.array([0.1, 0.5, 1.0])
        amplitude = np.random.rand(3, 3)
        phase = np.random.rand(3, 3)

        component = QTFComponent(
            dof=0,
            heading=0.0,
            qtf_type=QTFType.DIFFERENCE_FREQUENCY,
            frequencies_1=frequencies,
            frequencies_2=frequencies,
            amplitude=amplitude,
            phase=phase,
        )

        assert component.dof == 0
        assert component.heading == 0.0
        assert component.qtf_type == QTFType.DIFFERENCE_FREQUENCY

    def test_complex_qtf_property(self):
        """Test complex QTF calculation."""
        from digitalmodel.hydrodynamics.bemrosetta.models.qtf_data import QTFComponent, QTFType

        frequencies = np.array([0.1, 0.5])
        amplitude = np.array([[1.0, 2.0], [3.0, 4.0]])
        phase = np.array([[0.0, np.pi/4], [np.pi/2, np.pi]])

        component = QTFComponent(
            dof=0,
            heading=0.0,
            qtf_type=QTFType.DIFFERENCE_FREQUENCY,
            frequencies_1=frequencies,
            frequencies_2=frequencies,
            amplitude=amplitude,
            phase=phase,
        )

        complex_qtf = component.complex_qtf
        expected = amplitude * np.exp(1j * phase)

        assert_array_almost_equal(complex_qtf, expected)

    def test_mean_drift_extraction(self):
        """Test mean drift force extraction from diagonal."""
        from digitalmodel.hydrodynamics.bemrosetta.models.qtf_data import QTFComponent, QTFType

        frequencies = np.array([0.1, 0.5, 1.0])
        amplitude = np.array([
            [1.0, 0.5, 0.3],
            [0.5, 2.0, 0.4],
            [0.3, 0.4, 3.0],
        ])
        phase = np.zeros((3, 3))

        component = QTFComponent(
            dof=0,
            heading=0.0,
            qtf_type=QTFType.DIFFERENCE_FREQUENCY,
            frequencies_1=frequencies,
            frequencies_2=frequencies,
            amplitude=amplitude,
            phase=phase,
        )

        mean_drift = component.get_mean_drift()
        expected = np.array([1.0, 2.0, 3.0])

        assert_array_almost_equal(mean_drift, expected)

    def test_dimension_validation(self):
        """Test that dimension mismatch raises error."""
        from digitalmodel.hydrodynamics.bemrosetta.models.qtf_data import QTFComponent, QTFType

        frequencies = np.array([0.1, 0.5, 1.0])
        # Wrong dimensions
        amplitude = np.array([[1.0, 2.0], [3.0, 4.0]])  # 2x2 instead of 3x3
        phase = np.zeros((3, 3))

        with pytest.raises(ValueError, match="Amplitude shape"):
            QTFComponent(
                dof=0,
                heading=0.0,
                qtf_type=QTFType.DIFFERENCE_FREQUENCY,
                frequencies_1=frequencies,
                frequencies_2=frequencies,
                amplitude=amplitude,
                phase=phase,
            )


class TestPanelMesh:
    """Tests for PanelMesh dataclass."""

    def test_creation_quad_panels(self, sample_panel_mesh):
        """Test mesh creation with quad panels."""
        from digitalmodel.hydrodynamics.bemrosetta.models import PanelMesh

        mesh = PanelMesh(
            vertices=sample_panel_mesh["vertices"].astype(np.float64),
            panels=sample_panel_mesh["panels"].astype(np.int32),
        )

        assert mesh.n_vertices == 8
        assert mesh.n_panels == 5
        assert mesh.is_quad_mesh is True

    def test_creation_triangle_panels(self):
        """Test mesh creation with triangle panels."""
        from digitalmodel.hydrodynamics.bemrosetta.models import PanelMesh

        vertices = np.array([
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [0.5, 1.0, 0.0],
        ], dtype=np.float64)

        panels = np.array([
            [0, 1, 2],
        ], dtype=np.int32)

        mesh = PanelMesh(
            vertices=vertices,
            panels=panels,
        )

        assert mesh.n_vertices == 3
        assert mesh.n_panels == 1
        assert mesh.is_tri_mesh is True

    def test_panel_properties_computed(self, sample_panel_mesh):
        """Test that panel properties are automatically computed."""
        from digitalmodel.hydrodynamics.bemrosetta.models import PanelMesh

        mesh = PanelMesh(
            vertices=sample_panel_mesh["vertices"].astype(np.float64),
            panels=sample_panel_mesh["panels"].astype(np.int32),
        )

        assert mesh.normals is not None
        assert mesh.panel_areas is not None
        assert mesh.panel_centers is not None
        assert mesh.normals.shape == (5, 3)
        assert mesh.panel_areas.shape == (5,)
        assert mesh.panel_centers.shape == (5, 3)

    def test_total_area(self, sample_panel_mesh):
        """Test total area calculation."""
        from digitalmodel.hydrodynamics.bemrosetta.models import PanelMesh

        mesh = PanelMesh(
            vertices=sample_panel_mesh["vertices"].astype(np.float64),
            panels=sample_panel_mesh["panels"].astype(np.int32),
        )

        # Simple box: 5 faces of 1x1 = 5.0 area
        assert mesh.total_area == pytest.approx(5.0, rel=0.01)

    def test_bounding_box(self, sample_panel_mesh):
        """Test bounding box calculation."""
        from digitalmodel.hydrodynamics.bemrosetta.models import PanelMesh

        mesh = PanelMesh(
            vertices=sample_panel_mesh["vertices"].astype(np.float64),
            panels=sample_panel_mesh["panels"].astype(np.int32),
        )

        bbox_min, bbox_max = mesh.bounding_box

        assert bbox_min[0] == pytest.approx(0.0)
        assert bbox_max[0] == pytest.approx(1.0)
        assert bbox_min[1] == pytest.approx(0.0)
        assert bbox_max[1] == pytest.approx(1.0)
        assert bbox_min[2] == pytest.approx(-1.0)
        assert bbox_max[2] == pytest.approx(0.0)

    def test_symmetry_settings(self, sample_panel_mesh):
        """Test mesh with symmetry enabled."""
        from digitalmodel.hydrodynamics.bemrosetta.models import PanelMesh

        mesh = PanelMesh(
            vertices=sample_panel_mesh["vertices"].astype(np.float64),
            panels=sample_panel_mesh["panels"].astype(np.int32),
            symmetry_plane="xz",
        )

        assert mesh.symmetry_plane == "xz"

    def test_precomputed_properties(self, sample_panel_mesh):
        """Test mesh with precomputed normals/areas/centers."""
        from digitalmodel.hydrodynamics.bemrosetta.models import PanelMesh

        precomputed_normals = np.zeros((5, 3), dtype=np.float64)
        precomputed_normals[:, 2] = 1.0  # All pointing up

        precomputed_areas = np.ones(5, dtype=np.float64) * 1.0
        precomputed_centers = np.zeros((5, 3), dtype=np.float64)

        mesh = PanelMesh(
            vertices=sample_panel_mesh["vertices"].astype(np.float64),
            panels=sample_panel_mesh["panels"].astype(np.int32),
            normals=precomputed_normals,
            panel_areas=precomputed_areas,
            panel_centers=precomputed_centers,
        )

        # Should use precomputed values
        assert_array_almost_equal(mesh.normals, precomputed_normals)
        assert_array_almost_equal(mesh.panel_areas, precomputed_areas)

    def test_translate(self, sample_panel_mesh):
        """Test mesh translation."""
        from digitalmodel.hydrodynamics.bemrosetta.models import PanelMesh

        mesh = PanelMesh(
            vertices=sample_panel_mesh["vertices"].astype(np.float64),
            panels=sample_panel_mesh["panels"].astype(np.int32),
        )

        original_min = np.min(mesh.vertices, axis=0).copy()
        mesh.translate([10.0, 20.0, 30.0])

        expected_min = original_min + np.array([10.0, 20.0, 30.0])
        assert_array_almost_equal(np.min(mesh.vertices, axis=0), expected_min)

    def test_scale(self, sample_panel_mesh):
        """Test mesh scaling."""
        from digitalmodel.hydrodynamics.bemrosetta.models import PanelMesh

        mesh = PanelMesh(
            vertices=sample_panel_mesh["vertices"].astype(np.float64),
            panels=sample_panel_mesh["panels"].astype(np.int32),
        )

        original_area = mesh.total_area
        mesh.scale(2.0)

        # Area scales by factor squared
        assert mesh.total_area == pytest.approx(original_area * 4.0, rel=0.01)


class TestMeshQualityReport:
    """Tests for MeshQualityReport dataclass."""

    def test_creation(self):
        """Test report creation."""
        from digitalmodel.hydrodynamics.bemrosetta.models import MeshQualityReport

        report = MeshQualityReport(
            n_panels=100,
            n_vertices=80,
            total_area=500.0,
            min_panel_area=0.5,
            max_panel_area=10.0,
            mean_panel_area=5.0,
            aspect_ratio_max=3.5,
        )

        assert report.n_panels == 100
        assert report.n_vertices == 80
        assert report.is_valid is True
        assert len(report.warnings) == 0

    def test_add_error_marks_invalid(self):
        """Test that adding error marks report as invalid."""
        from digitalmodel.hydrodynamics.bemrosetta.models import MeshQualityReport

        report = MeshQualityReport(n_panels=100, n_vertices=80)
        assert report.is_valid is True

        report.add_error("Degenerate panel found")

        assert report.is_valid is False
        assert "Degenerate panel found" in report.errors

    def test_add_warning(self):
        """Test adding warnings."""
        from digitalmodel.hydrodynamics.bemrosetta.models import MeshQualityReport

        report = MeshQualityReport(n_panels=100, n_vertices=80)
        report.add_warning("High aspect ratio")

        assert report.is_valid is True  # Warnings don't invalidate
        assert "High aspect ratio" in report.warnings

    def test_to_dict(self):
        """Test report serialization."""
        from digitalmodel.hydrodynamics.bemrosetta.models import MeshQualityReport

        report = MeshQualityReport(
            n_panels=50,
            n_vertices=40,
            total_area=250.0,
            min_panel_area=1.0,
            max_panel_area=8.0,
            mean_panel_area=5.0,
            aspect_ratio_max=2.0,
        )

        result = report.to_dict()

        assert result["n_panels"] == 50
        assert result["n_vertices"] == 40
        assert result["is_valid"] is True
        assert result["warnings"] == []


class TestConversionResult:
    """Tests for ConversionResult dataclass."""

    def test_successful_conversion(self):
        """Test successful conversion result."""
        from digitalmodel.hydrodynamics.bemrosetta.models import ConversionResult
        from digitalmodel.hydrodynamics.bemrosetta.models.conversion_result import ConversionStatus

        result = ConversionResult(
            status=ConversionStatus.SUCCESS,
            source_format="gdf",
            target_format="dat",
        )

        assert result.is_success is True
        assert result.has_errors is False

    def test_failed_conversion(self):
        """Test failed conversion result."""
        from digitalmodel.hydrodynamics.bemrosetta.models import ConversionResult
        from digitalmodel.hydrodynamics.bemrosetta.models.conversion_result import ConversionStatus

        result = ConversionResult(
            status=ConversionStatus.FAILED,
            source_format="gdf",
            target_format="dat",
            errors=["Invalid header format", "Missing panel data"],
        )

        assert result.is_success is False
        assert result.has_errors is True
        assert len(result.errors) == 2

    def test_partial_conversion(self):
        """Test partial conversion with warnings."""
        from digitalmodel.hydrodynamics.bemrosetta.models import ConversionResult
        from digitalmodel.hydrodynamics.bemrosetta.models.conversion_result import ConversionStatus

        result = ConversionResult(
            status=ConversionStatus.PARTIAL,
            source_format="stl",
            target_format="gdf",
            warnings=["Precision loss in coordinates"],
        )

        assert result.is_success is False
        assert result.has_warnings is True
        assert result.status == ConversionStatus.PARTIAL

    def test_add_warning_changes_status(self):
        """Test that adding warning changes status from SUCCESS to PARTIAL."""
        from digitalmodel.hydrodynamics.bemrosetta.models import ConversionResult
        from digitalmodel.hydrodynamics.bemrosetta.models.conversion_result import ConversionStatus

        result = ConversionResult(
            status=ConversionStatus.SUCCESS,
            source_format="gdf",
            target_format="dat",
        )

        result.add_warning("Minor precision loss")

        assert result.status == ConversionStatus.PARTIAL
        assert result.has_warnings is True

    def test_add_error_changes_status(self):
        """Test that adding error changes status to FAILED."""
        from digitalmodel.hydrodynamics.bemrosetta.models import ConversionResult
        from digitalmodel.hydrodynamics.bemrosetta.models.conversion_result import ConversionStatus

        result = ConversionResult(
            status=ConversionStatus.SUCCESS,
            source_format="gdf",
            target_format="dat",
        )

        result.add_error("Critical failure")

        assert result.status == ConversionStatus.FAILED
        assert result.has_errors is True

    def test_to_dict(self):
        """Test conversion result serialization."""
        from pathlib import Path
        from digitalmodel.hydrodynamics.bemrosetta.models import ConversionResult
        from digitalmodel.hydrodynamics.bemrosetta.models.conversion_result import ConversionStatus

        result = ConversionResult(
            status=ConversionStatus.SUCCESS,
            source_format="stl",
            target_format="gdf",
            metadata={"source": "BEMRosetta"},
        )
        result.add_output_file(Path("test.gdf"), is_primary=True)

        data = result.to_dict()

        assert data["status"] == "success"
        assert data["source_format"] == "stl"
        assert data["target_format"] == "gdf"
        assert data["metadata"]["source"] == "BEMRosetta"
        assert data["primary_output"] is not None

    def test_summary(self):
        """Test human-readable summary generation."""
        from digitalmodel.hydrodynamics.bemrosetta.models import ConversionResult
        from digitalmodel.hydrodynamics.bemrosetta.models.conversion_result import ConversionStatus

        result = ConversionResult(
            status=ConversionStatus.SUCCESS,
            source_format="gdf",
            target_format="dat",
            duration_seconds=1.5,
        )

        summary = result.summary()

        assert "gdf -> dat" in summary
        assert "SUCCESS" in summary
        assert "1.50s" in summary


class TestIntegrationWithDiffractionModule:
    """Tests verifying compatibility with existing diffraction module."""

    def test_metadata_compatible_with_rao_set(self):
        """Verify metadata can integrate with diffraction RAOSet."""
        from digitalmodel.hydrodynamics.bemrosetta.models import BEMSolverMetadata
        from digitalmodel.hydrodynamics.bemrosetta.models.solver_metadata import BEMSolverType

        metadata = BEMSolverMetadata(
            solver_type=BEMSolverType.WAMIT,
            water_depth=150.0,
            project_name="Test Vessel",
            frequency_count=50,
            heading_count=12,
        )

        # Verify fields align with RAOSet expectations
        assert metadata.water_depth == 150.0
        assert metadata.project_name == "Test Vessel"
        assert metadata.frequency_count > 0
        assert metadata.heading_count > 0

    def test_qtf_dof_names_standard_convention(self):
        """Verify QTF DOF names follow standard convention."""
        from digitalmodel.hydrodynamics.bemrosetta.models import QTFData, QTFType

        qtf = QTFData(qtf_type=QTFType.DIFFERENCE_FREQUENCY)

        # These names should match standard DOF convention
        expected_dofs = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]
        assert qtf.dof_names == expected_dofs
        assert qtf.n_dof == 6
