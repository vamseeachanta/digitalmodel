"""Tests for riser model generation via ModularModelGenerator.

These tests verify that:
1. Riser specs are correctly validated by Pydantic schema
2. ModularModelGenerator produces correct modular output for risers
3. Pipeline-specific builders are skipped for riser models
4. Generated YAML loads in OrcFxAPI (if available)
"""

import pytest
import yaml
from pathlib import Path

from digitalmodel.solvers.orcaflex.modular_generator.schema import (
    ProjectInputSpec,
    Riser,
    RiserVessel,
    RiserLineType,
    RiserLine,
    RiserSection,
    EndConnection,
    ConnectionType,
    RiserConfiguration,
)
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator


# Test data paths
RISER_LIBRARY = Path("docs/modules/orcaflex/library/tier2_fast")
A01_CATENARY_SPEC = RISER_LIBRARY / "a01_catenary_riser" / "spec.yml"


class TestRiserSchemaValidation:
    """Tests for riser Pydantic schema validation."""

    def test_minimal_riser_line_type(self):
        """Test minimal valid RiserLineType."""
        lt = RiserLineType(
            name="test_pipe",
            outer_diameter=0.3,
            inner_diameter=0.2,
            mass_per_length=0.1,
            bending_stiffness=100,
            axial_stiffness=500000,
        )
        assert lt.name == "test_pipe"
        assert lt.outer_diameter == 0.3
        assert lt.torsional_stiffness == 10  # default

    def test_line_type_validates_diameters(self):
        """Test that ID must be less than OD."""
        with pytest.raises(ValueError, match="inner_diameter.*must be less than.*outer_diameter"):
            RiserLineType(
                name="bad_pipe",
                outer_diameter=0.2,
                inner_diameter=0.3,  # ID > OD
                mass_per_length=0.1,
                bending_stiffness=100,
                axial_stiffness=500000,
            )

    def test_end_connection_vessel_requires_name(self):
        """Test that vessel connections require a name."""
        with pytest.raises(ValueError, match="requires 'name' field"):
            EndConnection(
                type=ConnectionType.VESSEL,
                position=[0, 0, 0],
                # Missing name
            )

    def test_end_connection_anchor_no_name(self):
        """Test that anchor connections don't require name."""
        conn = EndConnection(
            type=ConnectionType.ANCHOR,
            position=[10, 0, -100],
        )
        assert conn.type == ConnectionType.ANCHOR
        assert conn.name is None

    def test_riser_section(self):
        """Test RiserSection creation."""
        section = RiserSection(
            line_type="test_pipe",
            length=50,
            segment_length=2,
        )
        assert section.length == 50
        assert section.segment_length == 2

    def test_riser_line_total_length(self):
        """Test RiserLine.get_total_length()."""
        line = RiserLine(
            name="Test Riser",
            end_a=EndConnection(
                type=ConnectionType.VESSEL,
                name="FPSO",
                position=[0, 0, -10],
            ),
            end_b=EndConnection(
                type=ConnectionType.ANCHOR,
                position=[100, 0, -200],
            ),
            sections=[
                RiserSection(line_type="pipe", length=100, segment_length=5),
                RiserSection(line_type="pipe", length=50, segment_length=2),
            ],
        )
        assert line.get_total_length() == 150

    def test_riser_validates_line_type_references(self):
        """Test that Riser validates line type references."""
        with pytest.raises(ValueError, match="references unknown line_type"):
            Riser(
                vessel=RiserVessel(name="FPSO"),
                line_types=[
                    RiserLineType(
                        name="pipe_a",
                        outer_diameter=0.3,
                        inner_diameter=0.2,
                        mass_per_length=0.1,
                        bending_stiffness=100,
                        axial_stiffness=500000,
                    )
                ],
                lines=[
                    RiserLine(
                        name="Test",
                        end_a=EndConnection(
                            type=ConnectionType.VESSEL,
                            name="FPSO",
                            position=[0, 0, 0],
                        ),
                        end_b=EndConnection(
                            type=ConnectionType.ANCHOR,
                            position=[0, 0, -100],
                        ),
                        sections=[
                            RiserSection(
                                line_type="pipe_b",  # Unknown
                                length=100,
                                segment_length=5,
                            )
                        ],
                    )
                ],
            )


class TestRiserSpecLoading:
    """Tests for loading riser spec files."""

    @pytest.mark.skipif(
        not A01_CATENARY_SPEC.exists(),
        reason="A01 catenary riser spec not found",
    )
    def test_load_a01_catenary_spec(self):
        """Test loading A01 catenary riser spec."""
        with open(A01_CATENARY_SPEC) as f:
            data = yaml.safe_load(f)

        spec = ProjectInputSpec(**data)

        assert spec.is_riser()
        assert not spec.is_pipeline()
        assert spec.riser is not None
        assert spec.riser.vessel.name == "FPSO"
        assert len(spec.riser.line_types) >= 1
        assert len(spec.riser.lines) >= 1

    @pytest.mark.skipif(
        not A01_CATENARY_SPEC.exists(),
        reason="A01 catenary riser spec not found",
    )
    def test_spec_model_type_methods(self):
        """Test model type detection methods."""
        with open(A01_CATENARY_SPEC) as f:
            data = yaml.safe_load(f)

        spec = ProjectInputSpec(**data)

        assert spec.is_riser() is True
        assert spec.is_pipeline() is False
        assert spec.get_riser_line_names() == ["Catenary Hose"]


class TestRiserModularGeneration:
    """Tests for riser modular model generation."""

    @pytest.fixture
    def catenary_spec(self):
        """Load and validate A01 catenary riser spec."""
        if not A01_CATENARY_SPEC.exists():
            pytest.skip("A01 catenary riser spec not found")

        with open(A01_CATENARY_SPEC) as f:
            data = yaml.safe_load(f)
        return ProjectInputSpec(**data)

    def test_generate_riser_modular_output(self, catenary_spec, tmp_path):
        """Test that ModularModelGenerator produces correct riser output."""
        generator = ModularModelGenerator.from_spec(catenary_spec)
        generator.generate(tmp_path)

        # Verify expected files exist
        master = tmp_path / "master.yml"
        assert master.exists()

        includes = tmp_path / "includes"
        assert includes.exists()

        # Riser-specific files
        assert (includes / "05_riser_line_types.yml").exists()
        assert (includes / "06_riser_vessels.yml").exists()
        assert (includes / "07_riser_lines.yml").exists()

        # Pipeline-specific files should NOT exist
        assert not (includes / "02_var_data.yml").exists()
        assert not (includes / "05_line_types.yml").exists()
        assert not (includes / "06_vessels.yml").exists()
        assert not (includes / "07_lines.yml").exists()
        assert not (includes / "08_buoys.yml").exists()

    def test_generated_line_types_content(self, catenary_spec, tmp_path):
        """Test that generated line types have correct content."""
        generator = ModularModelGenerator.from_spec(catenary_spec)
        generator.generate(tmp_path)

        lt_file = tmp_path / "includes" / "05_riser_line_types.yml"
        with open(lt_file) as f:
            data = yaml.safe_load(f)

        assert "LineTypes" in data
        line_types = data["LineTypes"]
        assert len(line_types) == 1

        lt = line_types[0]
        assert lt["Name"] == "10in_flexible"
        assert lt["Category"] == "General"
        assert lt["OD"] == pytest.approx(0.3556)
        assert lt["EA"] == pytest.approx(711200)

    def test_generated_vessel_content(self, catenary_spec, tmp_path):
        """Test that generated vessel has correct content."""
        generator = ModularModelGenerator.from_spec(catenary_spec)
        generator.generate(tmp_path)

        v_file = tmp_path / "includes" / "06_riser_vessels.yml"
        with open(v_file) as f:
            data = yaml.safe_load(f)

        assert "Vessels" in data
        vessels = data["Vessels"]
        assert len(vessels) == 1

        v = vessels[0]
        assert v["Name"] == "FPSO"
        assert v["VesselType"] == "Vessel Type1"
        assert v["Length"] == 103

    def test_generated_lines_content(self, catenary_spec, tmp_path):
        """Test that generated lines have correct content."""
        generator = ModularModelGenerator.from_spec(catenary_spec)
        generator.generate(tmp_path)

        l_file = tmp_path / "includes" / "07_riser_lines.yml"
        with open(l_file) as f:
            data = yaml.safe_load(f)

        assert "Lines" in data
        lines = data["Lines"]
        assert len(lines) == 1

        line = lines[0]
        assert line["Name"] == "Catenary Hose"
        assert line["StaticsStep1"] == "Catenary"
        assert line["LayAzimuth"] == 180


# OrcFxAPI integration test (skipped if not available)
try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
except ImportError:
    ORCAFLEX_AVAILABLE = False


@pytest.mark.skipif(not ORCAFLEX_AVAILABLE, reason="OrcFxAPI not available")
class TestRiserOrcaFlexLoad:
    """Tests for loading generated riser models in OrcFxAPI."""

    @pytest.fixture
    def catenary_spec(self):
        """Load and validate A01 catenary riser spec."""
        if not A01_CATENARY_SPEC.exists():
            pytest.skip("A01 catenary riser spec not found")

        with open(A01_CATENARY_SPEC) as f:
            data = yaml.safe_load(f)
        return ProjectInputSpec(**data)

    def test_generated_riser_loads_in_orcaflex(self, catenary_spec, tmp_path):
        """Test that generated riser YAML loads in OrcFxAPI."""
        generator = ModularModelGenerator.from_spec(catenary_spec)
        generator.generate(tmp_path)

        master = tmp_path / "master.yml"

        model = OrcFxAPI.Model()
        model.LoadData(str(master))

        # Verify key objects exist by accessing them directly
        vessel = model["FPSO"]
        assert vessel.Name == "FPSO"
        assert vessel.typeName == "Vessel"

        line = model["Catenary Hose"]
        assert line.Name == "Catenary Hose"
        assert line.typeName == "Line"
