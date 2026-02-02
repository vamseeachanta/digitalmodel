"""
Unit tests for OrcaFlex Library Generator.

Tests the CSV-to-YAML conversion functions and library generation capabilities.
"""

import pytest
import yaml
import tempfile
from pathlib import Path

from digitalmodel.orcaflex.library_generator import (
    csv_row_to_line_type,
    csv_row_to_buoy_type,
    csv_row_to_vessel_type,
    LibraryGenerator,
    OrcaFlexLibraryDumper,
)


class TestCsvRowToLineType:
    """Tests for csv_row_to_line_type function."""

    def test_standard_columns(self):
        """Test with standard column naming convention."""
        row = {
            "OD_m": "0.2731",
            "ID_m": "0.2461",
            "MassPerUnitLength_kg_m": "80.5",
            "EA_kN": "785000",
        }
        result = csv_row_to_line_type(row)

        assert result["OD"] == 0.2731
        assert result["ID"] == 0.2461
        assert result["MassPerUnitLength"] == 80.5
        assert result["EA"] == 785000
        assert result["CompressionIsLimited"] is True

    def test_risers_csv_columns(self):
        """Test with risers.csv column naming convention (OD, ID, Mass)."""
        row = {
            "RiserID": "SCR_10inch_X65",
            "OD": "0.2731",
            "ID": "0.2461",
            "Mass": "80.5",
            "EA": "785000000",
            "EI": "1850000",
            "PoissonRatio": "0.3",
        }
        result = csv_row_to_line_type(row)

        assert result["OD"] == 0.2731
        assert result["ID"] == 0.2461
        assert result["MassPerUnitLength"] == 80.5
        assert result["EA"] == 785000000
        assert result["EI"] == [1850000.0, None]
        assert result["PoissonRatio"] == 0.3

    def test_alternative_columns(self):
        """Test with alternative column naming (Diameter_m, Mass_kg_per_m)."""
        row = {
            "Diameter_m": "0.15",
            "Mass_kg_per_m": "25.4",
            "EA": "285000000",
        }
        result = csv_row_to_line_type(row)

        assert result["OD"] == 0.15
        assert result["MassPerUnitLength"] == 25.4

    def test_optional_drag_coefficients(self):
        """Test optional Cd and Ca properties."""
        row = {
            "OD": "0.1",
            "Mass": "50",
            "EA": "100000",
            "Cd": "1.2",
            "Ca": "1.0",
        }
        result = csv_row_to_line_type(row)

        assert "Cd" in result
        assert result["Cd"] == [1.2, None, 0.4]
        assert "Ca" in result
        assert result["Ca"] == [1.0, None, 0.07]

    def test_seabed_friction(self):
        """Test seabed friction coefficient."""
        row = {
            "OD": "0.1",
            "Mass": "50",
            "EA": "100000",
            "SeabedFriction": "0.6",
        }
        result = csv_row_to_line_type(row)

        assert result["SeabedLateralFrictionCoefficient"] == 0.6

    def test_default_values(self):
        """Test default values when columns are missing."""
        row = {}
        result = csv_row_to_line_type(row)

        assert result["OD"] == 0.1  # Default
        assert result["ID"] == 0  # Default
        assert result["MassPerUnitLength"] == 0.1  # Default
        assert result["EA"] == 100000  # Default
        assert result["PoissonRatio"] == 0.5  # Default
        assert result["GJ"] == 0


class TestCsvRowToBuoyType:
    """Tests for csv_row_to_buoy_type function."""

    def test_basic_buoy(self):
        """Test basic buoy conversion."""
        row = {
            "BuoyID": "CALM_12m",
            "Diameter_m": "12.0",
            "Height_m": "4.5",
            "Mass_te": "95",
        }
        result = csv_row_to_buoy_type(row)

        assert result["BuoyType"] == "Spar buoy"
        assert result["Connection"] == "Free"
        assert result["Mass"] == 95000  # Converted from tonnes
        assert "MomentsOfInertia" in result
        assert len(result["MomentsOfInertia"]) == 3

    def test_buoy_with_drag_coefficients(self):
        """Test buoy with drag and added mass coefficients."""
        row = {
            "Diameter_m": "10.0",
            "Height_m": "3.5",
            "Mass_kg": "75000",
            "Cd_Normal": "0.8",
            "Cd_Axial": "1.13",
            "Ca_Normal": "1.0",
            "Ca_Axial": "0.92",
        }
        result = csv_row_to_buoy_type(row)

        assert result["Mass"] == 75000
        assert "Cylinders" in result
        cylinder = result["Cylinders"][0]
        assert cylinder["CylinderOuterDiameter"] == 10.0
        assert cylinder["CylinderLength"] == 3.5
        assert cylinder["DragForceCoefficient"] == [0.8, 1.13]
        assert cylinder["AddedMassForceCoefficient"] == [1.0, 0.92]

    def test_buoy_stack_base_centre(self):
        """Test buoy stack base centre calculation."""
        row = {
            "Diameter_m": "12.0",
            "Height_m": "6.0",
            "Mass_te": "100",
        }
        result = csv_row_to_buoy_type(row)

        # Stack base should be at -height/2
        assert result["StackBaseCentre"] == [0, 0, -3.0]


class TestCsvRowToVesselType:
    """Tests for csv_row_to_vessel_type function."""

    def test_basic_vessel_type(self):
        """Test basic vessel type conversion."""
        row = {
            "VesselID": "FPSO_01",
            "Length_m": "300",
        }
        result = csv_row_to_vessel_type(row)

        assert result["Length"] == 300
        assert result["RAOResponseUnits"] == "degrees"
        assert result["Symmetry"] == "xz plane"


class TestLibraryGenerator:
    """Tests for LibraryGenerator class."""

    def test_generate_component(self, tmp_path):
        """Test generating a single component file."""
        generator = LibraryGenerator(str(tmp_path))

        props = {
            "OD": 0.2731,
            "ID": 0.2461,
            "MassPerUnitLength": 80.5,
            "EA": 785000000,
        }

        file_path = generator.generate_component(
            name="SCR_10inch_X65",
            props=props,
            category="line_types",
            description="Standard 10-inch steel catenary riser"
        )

        assert file_path.exists()
        assert file_path.name == "scr_10inch_x65.yml"

        # Verify content
        content = file_path.read_text()
        assert "SCR_10inch_X65" in content
        assert "line_types" in content
        assert "No section header" in content

        # Verify YAML is valid
        data = yaml.safe_load(content)
        assert data["OD"] == 0.2731

    def test_generate_from_csv(self, tmp_path):
        """Test generating components from CSV file."""
        # Create test CSV
        csv_content = """RiserID,RiserName,OD,ID,Mass,EA,Description
SCR_10inch,10-inch SCR,0.2731,0.2461,80.5,785000000,Test riser
SCR_12inch,12-inch SCR,0.3239,0.2909,115.8,1210000000,Large riser
"""
        csv_file = tmp_path / "test_risers.csv"
        csv_file.write_text(csv_content)

        generator = LibraryGenerator(str(tmp_path / "library"))
        generated = generator.generate_from_csv(
            str(csv_file),
            "line_types"
        )

        assert len(generated) == 2
        assert (tmp_path / "library" / "line_types" / "scr_10inch.yml").exists()
        assert (tmp_path / "library" / "line_types" / "scr_12inch.yml").exists()

    def test_name_column_detection_riser_id(self, tmp_path):
        """Test that RiserID is used for naming over generic ID."""
        csv_content = """RiserID,RiserName,OD,ID,Mass,EA
MY_RISER,My Riser,0.2731,0.2461,80.5,785000000
"""
        csv_file = tmp_path / "test.csv"
        csv_file.write_text(csv_content)

        generator = LibraryGenerator(str(tmp_path / "library"))
        generated = generator.generate_from_csv(str(csv_file), "line_types")

        # Should use RiserID (MY_RISER), not ID (0.2461)
        assert generated[0].name == "my_riser.yml"

    def test_name_column_detection_pipeline_id(self, tmp_path):
        """Test that PipelineID is used for naming."""
        csv_content = """PipelineID,OD,ID,Mass,EA
PIPE_16inch,0.4064,0.3734,145.2,1850000000
"""
        csv_file = tmp_path / "test.csv"
        csv_file.write_text(csv_content)

        generator = LibraryGenerator(str(tmp_path / "library"))
        generated = generator.generate_from_csv(str(csv_file), "line_types")

        assert generated[0].name == "pipe_16inch.yml"

    def test_explicit_name_column(self, tmp_path):
        """Test explicit name column specification."""
        csv_content = """CustomName,OD,ID,Mass,EA
MyCustomRiser,0.2731,0.2461,80.5,785000000
"""
        csv_file = tmp_path / "test.csv"
        csv_file.write_text(csv_content)

        generator = LibraryGenerator(str(tmp_path / "library"))
        generated = generator.generate_from_csv(
            str(csv_file),
            "line_types",
            name_column="CustomName"
        )

        assert generated[0].name == "mycustomriser.yml"

    def test_generate_index(self, tmp_path):
        """Test index generation."""
        # Create some component files
        generator = LibraryGenerator(str(tmp_path))

        generator.generate_component(
            "Component1", {"prop1": 1}, "line_types"
        )
        generator.generate_component(
            "Component2", {"prop2": 2}, "buoy_types"
        )

        index = generator.generate_index()

        assert "line_types" in index
        assert "buoy_types" in index
        assert len(index["line_types"]) == 1
        assert len(index["buoy_types"]) == 1

    def test_write_index(self, tmp_path):
        """Test writing index file."""
        generator = LibraryGenerator(str(tmp_path))

        generator.generate_component(
            "TestComponent", {"prop": 1}, "line_types"
        )

        index_path = generator.write_index()

        assert index_path.exists()
        assert index_path.name == "index.yml"

        content = index_path.read_text()
        assert "OrcaFlex Library Index" in content
        assert "line_types" in content


class TestYAMLOutput:
    """Tests for YAML output formatting."""

    def test_none_represented_as_tilde(self, tmp_path):
        """Test that None is represented as ~ in YAML."""
        generator = LibraryGenerator(str(tmp_path))

        props = {
            "EI": [0, None],
            "Cd": [1.0, None, 0.4],
        }

        file_path = generator.generate_component(
            "test_line", props, "line_types"
        )

        content = file_path.read_text()
        assert "~" in content  # None should be ~

    def test_bool_represented_as_yes_no(self, tmp_path):
        """Test that booleans are Yes/No in OrcaFlex style."""
        generator = LibraryGenerator(str(tmp_path))

        props = {
            "CompressionIsLimited": True,
            "SomeOtherFlag": False,
        }

        file_path = generator.generate_component(
            "test_line", props, "line_types"
        )

        content = file_path.read_text()
        assert "Yes" in content
        assert "No" in content


class TestFileNotFound:
    """Tests for error handling."""

    def test_csv_not_found(self, tmp_path):
        """Test error when CSV file doesn't exist."""
        generator = LibraryGenerator(str(tmp_path))

        with pytest.raises(FileNotFoundError):
            generator.generate_from_csv(
                "nonexistent.csv",
                "line_types"
            )

    def test_unknown_category(self, tmp_path):
        """Test error for unknown category."""
        csv_file = tmp_path / "test.csv"
        csv_file.write_text("Name,Value\nTest,1")

        generator = LibraryGenerator(str(tmp_path))

        with pytest.raises(ValueError, match="Unknown category"):
            generator.generate_from_csv(
                str(csv_file),
                "unknown_category"
            )
