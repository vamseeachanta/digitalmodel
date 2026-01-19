"""
TDD Tests for OrcaFlex Object-Level Include Library System

Tests the hybrid approach:
1. Object-level IncludeFile within sections
2. BaseFile + variations pattern
3. Library component loading
"""

import pytest
import yaml
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock
import tempfile
import shutil


# Test fixtures
@pytest.fixture
def temp_library(tmp_path):
    """Create a temporary library structure for testing."""
    library = tmp_path / "library"
    library.mkdir()

    # Create subdirectories
    (library / "line_types").mkdir()
    (library / "buoy_types").mkdir()
    (library / "vessel_types").mkdir()

    # Create sample line type (NO section header)
    chain_file = library / "line_types" / "chain_84mm_r4.yml"
    chain_file.write_text("""# Chain 84mm R4 - Library Component
# NO section header - properties only
OD: 0.084
ID: 0
MassPerUnitLength: 0.141
EA: 612000
Cd: [1.0, ~, 0.4]
Ca: [1, ~, 0.07]
SeabedLateralFrictionCoefficient: 0.5
""")

    # Create sample buoy type (NO section header)
    buoy_file = library / "buoy_types" / "calm_12m.yml"
    buoy_file.write_text("""# CALM Buoy 12m - Library Component
BuoyType: Spar buoy
Connection: Free
Mass: 95000
MomentsOfInertia: [3500000, 3500000, 5400000]
CentreOfMass: [0, 0, 0]
StackBaseCentre: [0, 0, -2.25]
""")

    return library


@pytest.fixture
def temp_project(tmp_path, temp_library):
    """Create a temporary project using the library."""
    project = tmp_path / "project"
    project.mkdir()

    # Relative path to library
    lib_rel = Path("..") / "library"

    return project, lib_rel


class TestLibraryComponentFormat:
    """Test that library components have correct format (no section headers)."""

    def test_line_type_has_no_section_header(self, temp_library):
        """Line type file should NOT have LineTypes: header."""
        chain_file = temp_library / "line_types" / "chain_84mm_r4.yml"
        content = chain_file.read_text()

        # Should NOT contain section header
        assert "LineTypes:" not in content

        # Should contain properties
        data = yaml.safe_load(content)
        assert "OD" in data
        assert "EA" in data
        assert data["OD"] == 0.084

    def test_buoy_type_has_no_section_header(self, temp_library):
        """Buoy type file should NOT have 6DBuoys: header."""
        buoy_file = temp_library / "buoy_types" / "calm_12m.yml"
        content = buoy_file.read_text()

        # Should NOT contain section header
        assert "6DBuoys:" not in content
        assert "Buoys:" not in content

        # Should contain properties
        data = yaml.safe_load(content)
        assert "Mass" in data
        assert data["Mass"] == 95000


class TestModelWithObjectIncludes:
    """Test model files that use object-level IncludeFile."""

    def test_model_structure_with_includes(self, temp_project, temp_library):
        """Model should have sections with IncludeFile at object level."""
        project, lib_rel = temp_project

        model_content = f"""
%YAML 1.1
---
General:
  UnitsSystem: SI
  StageDuration: [10, 100]

LineTypes:
  - Name: Chain_84mm_R4
    IncludeFile: {lib_rel}/line_types/chain_84mm_r4.yml

6DBuoys:
  - Name: CALM Buoy
    IncludeFile: {lib_rel}/buoy_types/calm_12m.yml
    InitialPosition: [0, 0, -0.8]
"""

        model_file = project / "model.yml"
        model_file.write_text(model_content)

        # Parse and verify structure
        data = yaml.safe_load(model_content)

        assert "General" in data
        assert "LineTypes" in data
        assert "6DBuoys" in data

        # Check IncludeFile references
        assert data["LineTypes"][0]["Name"] == "Chain_84mm_R4"
        assert "IncludeFile" in data["LineTypes"][0]

        # Check override capability
        assert data["6DBuoys"][0]["InitialPosition"] == [0, 0, -0.8]

    def test_include_path_resolution(self, temp_project, temp_library):
        """IncludeFile paths should resolve correctly."""
        project, lib_rel = temp_project

        model_file = project / "model.yml"
        include_path = f"{lib_rel}/line_types/chain_84mm_r4.yml"

        # Resolve the path
        resolved = (model_file.parent / include_path).resolve()
        expected = (temp_library / "line_types" / "chain_84mm_r4.yml").resolve()

        assert resolved == expected
        assert resolved.exists()


class TestBaseFilePattern:
    """Test BaseFile + IncludeFile variation pattern."""

    def test_basefile_with_variation(self, tmp_path):
        """BaseFile should load base, IncludeFile applies changes."""
        base_dir = tmp_path / "base"
        base_dir.mkdir()

        # Create base model
        base_content = """
%YAML 1.1
---
General:
  UnitsSystem: SI

Environment:
  WaterDepth: 100
  WindSpeed: 12
"""
        base_file = base_dir / "base_model.yml"
        base_file.write_text(base_content)

        # Create variation (only changes)
        variation_content = """
# Only the differences from base
Environment:
  WaterDepth: 150
  WindSpeed: 25
"""
        variation_file = base_dir / "variation_100yr.yml"
        variation_file.write_text(variation_content)

        # Create master file
        master_content = f"""
%YAML 1.1
---
BaseFile: base_model.yml
IncludeFile: variation_100yr.yml
"""
        master_file = base_dir / "master.yml"
        master_file.write_text(master_content)

        # Verify structure
        master_data = yaml.safe_load(master_content)
        assert "BaseFile" in master_data
        assert "IncludeFile" in master_data

        variation_data = yaml.safe_load(variation_content)
        assert variation_data["Environment"]["WaterDepth"] == 150


class TestCSVToLibraryConversion:
    """Test generating library components from CSV equipment catalogs."""

    def test_line_type_from_csv_row(self):
        """Should generate line type YAML from CSV data."""
        csv_row = {
            "LineTypeID": "Chain_84mm_R4",
            "OD_m": 0.084,
            "ID_m": 0,
            "MassPerUnitLength_kg_m": 0.141,
            "EA_kN": 612000,
            "Description": "84mm R4 Studless Chain"
        }

        # Expected output (properties only, no section header)
        expected_props = {
            "OD": 0.084,
            "ID": 0,
            "MassPerUnitLength": 0.141,
            "EA": 612000,
        }

        # Conversion function (to be implemented)
        from digitalmodel.modules.orcaflex.library_generator import csv_row_to_line_type
        result = csv_row_to_line_type(csv_row)

        for key, value in expected_props.items():
            assert key in result
            assert result[key] == value

    def test_buoy_type_from_csv_row(self):
        """Should generate buoy type YAML from CSV data."""
        csv_row = {
            "BuoyID": "CALM_12m",
            "Type": "CALM",
            "Diameter_m": 12.0,
            "Height_m": 4.5,
            "Mass_kg": 95000,
            "Description": "Standard CALM buoy"
        }

        expected_props = {
            "Mass": 95000,
            "BuoyType": "Spar buoy",
        }

        from digitalmodel.modules.orcaflex.library_generator import csv_row_to_buoy_type
        result = csv_row_to_buoy_type(csv_row)

        assert result["Mass"] == 95000


class TestOrcaFlexIntegration:
    """Integration tests with actual OrcaFlex API (requires license)."""

    @pytest.fixture
    def orcaflex_available(self):
        """Check if OrcFxAPI is available."""
        try:
            import OrcFxAPI
            return True
        except ImportError:
            return False

    @pytest.mark.skipif(
        not pytest.importorskip("OrcFxAPI", reason="OrcFxAPI not available"),
        reason="OrcFxAPI not available"
    )
    def test_load_model_with_object_includes(self, temp_project, temp_library):
        """OrcaFlex should load model with object-level IncludeFile."""
        import OrcFxAPI

        project, lib_rel = temp_project

        # Create a minimal but complete model
        model_content = f"""
%YAML 1.1
---
General:
  UnitsSystem: SI
  StageDuration: [10]

Environment:
  WaterDepth: 100
  SeabedModel: Elastic

LineTypes:
  - Name: TestChain
    IncludeFile: {lib_rel}/line_types/chain_84mm_r4.yml
"""

        model_file = project / "model.yml"
        model_file.write_text(model_content)

        # Try to load with OrcaFlex
        model = OrcFxAPI.Model()
        try:
            model.LoadData(str(model_file))

            # Verify line type was loaded
            line_types = [obj for obj in model.objects if obj.typeName == "Line type"]
            assert len(line_types) >= 1

        except OrcFxAPI.DLLError as e:
            pytest.fail(f"OrcaFlex failed to load model: {e}")

    @pytest.mark.skipif(
        not pytest.importorskip("OrcFxAPI", reason="OrcFxAPI not available"),
        reason="OrcFxAPI not available"
    )
    def test_basefile_variation_loading(self, tmp_path):
        """OrcaFlex should load BaseFile and apply IncludeFile variations."""
        import OrcFxAPI

        # Create base model
        base_content = """
%YAML 1.1
---
General:
  UnitsSystem: SI
  StageDuration: [10]

Environment:
  WaterDepth: 100
"""
        base_file = tmp_path / "base.yml"
        base_file.write_text(base_content)

        # Create variation
        variation_content = """
Environment:
  WaterDepth: 200
"""
        variation_file = tmp_path / "variation.yml"
        variation_file.write_text(variation_content)

        # Create master
        master_content = """
%YAML 1.1
---
BaseFile: base.yml
IncludeFile: variation.yml
"""
        master_file = tmp_path / "master.yml"
        master_file.write_text(master_content)

        # Load and verify
        model = OrcFxAPI.Model()
        try:
            model.LoadData(str(master_file))

            # Check water depth was overridden
            assert model.environment.WaterDepth == 200

        except OrcFxAPI.DLLError as e:
            pytest.fail(f"OrcaFlex failed to load model: {e}")


class TestLibraryStructure:
    """Test the library directory structure and organization."""

    def test_library_directories_exist(self, temp_library):
        """Library should have standard subdirectories."""
        expected_dirs = ["line_types", "buoy_types", "vessel_types"]

        for dir_name in expected_dirs:
            assert (temp_library / dir_name).is_dir()

    def test_component_naming_convention(self, temp_library):
        """Component files should follow naming convention."""
        # Pattern: lowercase_with_underscores.yml
        chain_file = temp_library / "line_types" / "chain_84mm_r4.yml"

        assert chain_file.name.islower() or "_" in chain_file.name
        assert chain_file.suffix == ".yml"

    def test_library_index_generation(self, temp_library):
        """Should be able to generate index of all library components."""
        index = {}

        for category_dir in temp_library.iterdir():
            if category_dir.is_dir():
                category = category_dir.name
                index[category] = []

                for component_file in category_dir.glob("*.yml"):
                    data = yaml.safe_load(component_file.read_text())
                    index[category].append({
                        "file": component_file.name,
                        "properties": list(data.keys()) if data else []
                    })

        assert "line_types" in index
        assert len(index["line_types"]) > 0
        assert index["line_types"][0]["file"] == "chain_84mm_r4.yml"
