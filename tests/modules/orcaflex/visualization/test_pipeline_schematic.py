"""
Tests for Pipeline Schematic Visualization Module.

Tests the PipelineSchematicGenerator class for creating plan and elevation
view schematics with boundary condition markups.
"""

import pytest
import numpy as np
from pathlib import Path
import tempfile
import shutil

from digitalmodel.modules.orcaflex.visualization.pipeline_schematic import (
    PipelineSchematicGenerator,
    BoundaryConditionType,
    BoundaryCondition,
    PipelineData,
    BC_COLORS,
)


# Test fixtures
@pytest.fixture
def sample_yaml_path():
    """Path to the sample pipeline installation model."""
    return Path(
        "docs/modules/orcaflex/pipeline/installation/floating/"
        "24in_pipeline/monolithic/basefile/vessel_end_winch.yml"
    )


@pytest.fixture
def temp_output_dir():
    """Create a temporary output directory for tests."""
    temp_dir = tempfile.mkdtemp()
    yield Path(temp_dir)
    shutil.rmtree(temp_dir, ignore_errors=True)


class TestBoundaryConditionType:
    """Tests for BoundaryConditionType enum."""

    def test_all_types_defined(self):
        """Verify all expected BC types are defined."""
        expected_types = [
            "FIXED_SEABED",
            "FREE_6DOF",
            "WINCH_TENSION",
            "FIXED_SUPPORT",
            "VERTICAL_RESTRAINT",
            "DISTRIBUTED_BUOYANCY",
        ]
        actual_types = [t.name for t in BoundaryConditionType]
        assert set(expected_types) == set(actual_types)

    def test_bc_type_values(self):
        """Verify BC type values are lowercase with underscores."""
        for bc_type in BoundaryConditionType:
            assert bc_type.value == bc_type.name.lower()


class TestBoundaryCondition:
    """Tests for BoundaryCondition dataclass."""

    def test_create_boundary_condition(self):
        """Test creating a boundary condition."""
        bc = BoundaryCondition(
            name="End A",
            bc_type=BoundaryConditionType.FIXED_SEABED,
            position=(-101, 0, 4.505),
            properties={"constraint": "All 6 DOF fixed"},
        )

        assert bc.name == "End A"
        assert bc.bc_type == BoundaryConditionType.FIXED_SEABED
        assert bc.position == (-101, 0, 4.505)
        assert bc.x == -101
        assert bc.y == 0
        assert bc.z == 4.505
        assert bc.properties["constraint"] == "All 6 DOF fixed"

    def test_position_properties(self):
        """Test position x, y, z properties."""
        bc = BoundaryCondition(
            name="Test",
            bc_type=BoundaryConditionType.FREE_6DOF,
            position=(100.5, -20.3, -5.7),
        )

        assert bc.x == pytest.approx(100.5)
        assert bc.y == pytest.approx(-20.3)
        assert bc.z == pytest.approx(-5.7)

    def test_default_properties(self):
        """Test default empty properties dict."""
        bc = BoundaryCondition(
            name="Test",
            bc_type=BoundaryConditionType.FIXED_SUPPORT,
            position=(0, 0, 0),
        )

        assert bc.properties == {}


class TestBCColors:
    """Tests for boundary condition color scheme."""

    def test_all_types_have_colors(self):
        """Verify all BC types have assigned colors."""
        for bc_type in BoundaryConditionType:
            assert bc_type in BC_COLORS
            assert BC_COLORS[bc_type].startswith("#")

    def test_color_format(self):
        """Verify colors are valid hex format."""
        import re

        hex_pattern = r"^#[0-9A-Fa-f]{6}$"
        for color in BC_COLORS.values():
            assert re.match(hex_pattern, color), f"Invalid color format: {color}"


class TestPipelineData:
    """Tests for PipelineData dataclass."""

    def test_create_pipeline_data(self):
        """Test creating pipeline data."""
        path_x = np.array([0, 100, 200])
        path_y = np.array([0, 0, 0])
        path_z = np.array([0, -5, -8])
        bcs = [
            BoundaryCondition(
                name="End A",
                bc_type=BoundaryConditionType.FIXED_SEABED,
                position=(0, 0, 0),
            )
        ]

        data = PipelineData(
            path_x=path_x,
            path_y=path_y,
            path_z=path_z,
            boundary_conditions=bcs,
            model_name="test_model",
            water_depth=8.0,
            pipeline_length=200.0,
        )

        assert len(data.path_x) == 3
        assert len(data.boundary_conditions) == 1
        assert data.model_name == "test_model"
        assert data.water_depth == 8.0


class TestPipelineSchematicGenerator:
    """Tests for PipelineSchematicGenerator class."""

    def test_init_with_valid_path(self, sample_yaml_path):
        """Test initialization with valid YAML path."""
        if not sample_yaml_path.exists():
            pytest.skip("Sample YAML file not found")

        generator = PipelineSchematicGenerator(str(sample_yaml_path))

        assert generator.yaml_file == sample_yaml_path
        assert generator.output_dir == sample_yaml_path.parent

    def test_init_with_custom_output_dir(self, sample_yaml_path, temp_output_dir):
        """Test initialization with custom output directory."""
        if not sample_yaml_path.exists():
            pytest.skip("Sample YAML file not found")

        generator = PipelineSchematicGenerator(
            str(sample_yaml_path), str(temp_output_dir)
        )

        assert generator.output_dir == temp_output_dir

    @pytest.mark.skipif(
        not Path(
            "docs/modules/orcaflex/pipeline/installation/floating/"
            "24in_pipeline/monolithic/basefile/vessel_end_winch.yml"
        ).exists(),
        reason="Sample YAML not available",
    )
    def test_parse_model(self, sample_yaml_path):
        """Test parsing the model file."""
        generator = PipelineSchematicGenerator(str(sample_yaml_path))
        data = generator.parse_model()

        assert isinstance(data, PipelineData)
        assert len(data.path_x) > 0
        assert len(data.path_y) == len(data.path_x)
        assert len(data.path_z) == len(data.path_x)
        assert len(data.boundary_conditions) > 0
        assert data.model_name == "vessel_end_winch"

    @pytest.mark.skipif(
        not Path(
            "docs/modules/orcaflex/pipeline/installation/floating/"
            "24in_pipeline/monolithic/basefile/vessel_end_winch.yml"
        ).exists(),
        reason="Sample YAML not available",
    )
    def test_parse_model_extracts_end_a(self, sample_yaml_path):
        """Test that End A boundary condition is extracted."""
        generator = PipelineSchematicGenerator(str(sample_yaml_path))
        data = generator.parse_model()

        end_a_bcs = [
            bc
            for bc in data.boundary_conditions
            if bc.bc_type == BoundaryConditionType.FIXED_SEABED
        ]
        assert len(end_a_bcs) >= 1

        # End A should be at approximately X=-101
        end_a = end_a_bcs[0]
        assert end_a.x == pytest.approx(-101, abs=1)

    @pytest.mark.skipif(
        not Path(
            "docs/modules/orcaflex/pipeline/installation/floating/"
            "24in_pipeline/monolithic/basefile/vessel_end_winch.yml"
        ).exists(),
        reason="Sample YAML not available",
    )
    def test_create_plan_view(self, sample_yaml_path):
        """Test creating plan view figure."""
        generator = PipelineSchematicGenerator(str(sample_yaml_path))
        fig = generator.create_plan_view()

        assert fig is not None
        assert len(fig.data) > 0  # Should have traces

        # Check layout
        assert "Plan View" in fig.layout.title.text
        assert fig.layout.xaxis.title.text == "Global X (m)"
        assert fig.layout.yaxis.title.text == "Global Y (m)"

    @pytest.mark.skipif(
        not Path(
            "docs/modules/orcaflex/pipeline/installation/floating/"
            "24in_pipeline/monolithic/basefile/vessel_end_winch.yml"
        ).exists(),
        reason="Sample YAML not available",
    )
    def test_create_elevation_view(self, sample_yaml_path):
        """Test creating elevation view figure."""
        generator = PipelineSchematicGenerator(str(sample_yaml_path))
        fig = generator.create_elevation_view()

        assert fig is not None
        assert len(fig.data) > 0

        # Check layout
        assert "Elevation View" in fig.layout.title.text
        assert fig.layout.xaxis.title.text == "Global X (m)"
        assert fig.layout.yaxis.title.text == "Global Z (m)"

    @pytest.mark.skipif(
        not Path(
            "docs/modules/orcaflex/pipeline/installation/floating/"
            "24in_pipeline/monolithic/basefile/vessel_end_winch.yml"
        ).exists(),
        reason="Sample YAML not available",
    )
    def test_generate_html_report(self, sample_yaml_path, temp_output_dir):
        """Test generating HTML report."""
        generator = PipelineSchematicGenerator(
            str(sample_yaml_path), str(temp_output_dir)
        )
        report_path = generator.generate_html_report()

        assert report_path.exists()
        assert report_path.suffix == ".html"

        # Verify content
        content = report_path.read_text(encoding="utf-8")
        assert "Pipeline Installation Schematic" in content
        assert "Plan View" in content
        assert "Elevation View" in content
        assert "plotly" in content.lower()

    @pytest.mark.skipif(
        not Path(
            "docs/modules/orcaflex/pipeline/installation/floating/"
            "24in_pipeline/monolithic/basefile/vessel_end_winch.yml"
        ).exists(),
        reason="Sample YAML not available",
    )
    def test_save_individual_views(self, sample_yaml_path, temp_output_dir):
        """Test saving individual view files."""
        generator = PipelineSchematicGenerator(
            str(sample_yaml_path), str(temp_output_dir)
        )
        plan_path, elevation_path = generator.save_individual_views()

        assert plan_path.exists()
        assert elevation_path.exists()
        assert "plan_view" in plan_path.name
        assert "elevation_view" in elevation_path.name

    def test_get_marker_symbol(self, sample_yaml_path):
        """Test marker symbol mapping."""
        if not sample_yaml_path.exists():
            pytest.skip("Sample YAML file not found")

        generator = PipelineSchematicGenerator(str(sample_yaml_path))

        assert generator._get_marker_symbol(BoundaryConditionType.FIXED_SEABED) == "triangle-up"
        assert generator._get_marker_symbol(BoundaryConditionType.FREE_6DOF) == "circle"
        assert generator._get_marker_symbol(BoundaryConditionType.WINCH_TENSION) == "diamond"
        assert generator._get_marker_symbol(BoundaryConditionType.FIXED_SUPPORT) == "square"

    def test_get_marker_size(self, sample_yaml_path):
        """Test marker size mapping."""
        if not sample_yaml_path.exists():
            pytest.skip("Sample YAML file not found")

        generator = PipelineSchematicGenerator(str(sample_yaml_path))

        assert generator._get_marker_size(BoundaryConditionType.FIXED_SEABED) == 20
        assert generator._get_marker_size(BoundaryConditionType.FREE_6DOF) == 18
        assert generator._get_marker_size(BoundaryConditionType.FIXED_SUPPORT) == 14

    def test_create_boundary_condition_legend(self, sample_yaml_path):
        """Test creating the BC legend figure."""
        if not sample_yaml_path.exists():
            pytest.skip("Sample YAML file not found")

        generator = PipelineSchematicGenerator(str(sample_yaml_path))
        legend_fig = generator.create_boundary_condition_legend()

        assert legend_fig is not None
        assert legend_fig.layout.title.text == "Boundary Condition Legend"


class TestPipelineLengthCalculation:
    """Tests for pipeline length calculation."""

    def test_simple_straight_line(self, sample_yaml_path):
        """Test length calculation for straight line."""
        if not sample_yaml_path.exists():
            pytest.skip("Sample YAML file not found")

        generator = PipelineSchematicGenerator(str(sample_yaml_path))

        x = np.array([0, 100, 200])
        y = np.array([0, 0, 0])
        z = np.array([0, 0, 0])

        length = generator._calculate_pipeline_length(x, y, z)
        assert length == pytest.approx(200.0)

    def test_diagonal_line(self, sample_yaml_path):
        """Test length calculation for diagonal line."""
        if not sample_yaml_path.exists():
            pytest.skip("Sample YAML file not found")

        generator = PipelineSchematicGenerator(str(sample_yaml_path))

        x = np.array([0, 3])
        y = np.array([0, 4])
        z = np.array([0, 0])

        length = generator._calculate_pipeline_length(x, y, z)
        assert length == pytest.approx(5.0)  # 3-4-5 triangle


class TestIntegration:
    """Integration tests with actual model file."""

    @pytest.mark.skipif(
        not Path(
            "docs/modules/orcaflex/pipeline/installation/floating/"
            "24in_pipeline/monolithic/basefile/vessel_end_winch.yml"
        ).exists(),
        reason="Sample YAML not available",
    )
    def test_full_workflow(self, sample_yaml_path, temp_output_dir):
        """Test complete workflow from parsing to report generation."""
        generator = PipelineSchematicGenerator(
            str(sample_yaml_path), str(temp_output_dir)
        )

        # Parse model
        data = generator.parse_model()
        assert data is not None

        # Verify boundary conditions
        bc_types = [bc.bc_type for bc in data.boundary_conditions]
        assert BoundaryConditionType.FIXED_SEABED in bc_types

        # Generate report
        report_path = generator.generate_html_report()
        assert report_path.exists()

        # Verify report content
        content = report_path.read_text(encoding="utf-8")
        assert "vessel_end_winch" in content
        assert "Pipeline Length" in content

    @pytest.mark.skipif(
        not Path(
            "docs/modules/orcaflex/pipeline/installation/floating/"
            "24in_pipeline/monolithic/basefile/vessel_end_winch.yml"
        ).exists(),
        reason="Sample YAML not available",
    )
    def test_model_coordinates_accuracy(self, sample_yaml_path):
        """Verify extracted coordinates match expected values."""
        generator = PipelineSchematicGenerator(str(sample_yaml_path))
        data = generator.parse_model()

        # End A should be at X=-101 (from plan)
        assert data.path_x[0] == pytest.approx(-101, abs=1)

        # End B should be at X~4795 (from plan)
        assert data.path_x[-1] > 4700

        # Pipeline should be roughly 4900m long
        assert data.pipeline_length > 4800
        assert data.pipeline_length < 5100
