# ABOUTME: Unit tests for pipe cross-section calculator module.
# ABOUTME: Tests geometry, weight, and buoyancy calculations against known values.

"""
Unit Tests for Pipe Cross-Section Calculator
============================================

Tests cover:
- PipeLayer geometry and weight calculations
- PipeCrossSection multi-layer assembly
- Buoyancy analysis
- Unit conversions
- Data export functionality
"""

import pytest
import math
from pathlib import Path


class TestPipeLayer:
    """Tests for PipeLayer dataclass."""

    def test_layer_creation(self):
        """Test creating a pipe layer with basic properties."""
        from digitalmodel.pipe_cross_section import PipeLayer

        layer = PipeLayer(
            name="Steel Pipe",
            inner_diameter_mm=581.0,
            outer_diameter_mm=609.6,
            density_kg_m3=7850,
        )
        assert layer.name == "Steel Pipe"
        assert layer.inner_diameter_mm == 581.0
        assert layer.outer_diameter_mm == 609.6
        assert layer.density_kg_m3 == 7850

    def test_layer_thickness(self):
        """Test wall thickness calculation."""
        from digitalmodel.pipe_cross_section import PipeLayer

        layer = PipeLayer(
            name="Steel",
            inner_diameter_mm=581.0,
            outer_diameter_mm=609.6,
            density_kg_m3=7850,
        )
        expected_thickness = (609.6 - 581.0) / 2
        assert abs(layer.thickness_mm - expected_thickness) < 0.01

    def test_layer_cross_sectional_area(self):
        """Test cross-sectional area calculation for steel pipe."""
        from digitalmodel.pipe_cross_section import PipeLayer

        layer = PipeLayer(
            name="Steel",
            inner_diameter_mm=581.0,
            outer_diameter_mm=609.6,
            density_kg_m3=7850,
        )
        # Area = π/4 * (OD² - ID²) in m²
        expected_area = (math.pi / 4) * (0.6096**2 - 0.581**2)
        assert abs(layer.cross_sectional_area_m2 - expected_area) < 0.0001

    def test_layer_weight_per_meter(self, expected_values):
        """Test weight per meter calculation for steel pipe."""
        from digitalmodel.pipe_cross_section import PipeLayer

        layer = PipeLayer(
            name="Steel",
            inner_diameter_mm=581.0,
            outer_diameter_mm=609.6,
            density_kg_m3=7850,
        )
        assert abs(layer.weight_per_meter_kg - expected_values["steel_weight_kg_m"]) < 1.0


class TestPipeCrossSection:
    """Tests for PipeCrossSection class with full pipe configuration."""

    def test_pipe_creation(self, pipe_config):
        """Test creating a pipe cross-section with all layers."""
        from digitalmodel.pipe_cross_section import PipeCrossSection

        pipe = PipeCrossSection(**pipe_config)
        assert len(pipe.layers) == 3
        assert pipe.layers[0].name == "Steel Pipe"
        assert pipe.layers[1].name == "3LPP Coating"
        assert pipe.layers[2].name == "Concrete Coating"

    def test_steel_dimensions(self, pipe_instance, expected_values):
        """Test steel pipe dimensions match expected values."""
        steel = pipe_instance.layers[0]
        assert abs(steel.inner_diameter_mm - expected_values["steel_id_mm"]) < 0.1
        assert abs(steel.outer_diameter_mm - expected_values["steel_od_mm"]) < 0.1
        assert abs(steel.thickness_mm - 14.29) < 0.01

    def test_lpp_dimensions(self, pipe_instance, expected_values):
        """Test 3LPP coating dimensions."""
        lpp = pipe_instance.layers[1]
        assert abs(lpp.inner_diameter_mm - expected_values["steel_od_mm"]) < 0.1
        assert abs(lpp.outer_diameter_mm - expected_values["lpp_od_mm"]) < 0.1

    def test_concrete_dimensions(self, pipe_instance, expected_values):
        """Test concrete coating dimensions."""
        concrete = pipe_instance.layers[2]
        assert abs(concrete.inner_diameter_mm - expected_values["lpp_od_mm"]) < 0.1
        assert abs(concrete.outer_diameter_mm - expected_values["concrete_od_mm"]) < 0.1

    def test_final_outer_diameter(self, pipe_instance, expected_values):
        """Test overall pipe outer diameter."""
        assert abs(pipe_instance.outer_diameter_mm - expected_values["concrete_od_mm"]) < 0.1
        assert abs(pipe_instance.outer_diameter_inch - expected_values["final_od_inch"]) < 0.01

    def test_steel_weight(self, pipe_instance, expected_values):
        """Test steel pipe weight per meter."""
        steel = pipe_instance.layers[0]
        assert abs(steel.weight_per_meter_kg - expected_values["steel_weight_kg_m"]) < 1.0

    def test_lpp_weight(self, pipe_instance, expected_values):
        """Test 3LPP coating weight per meter."""
        lpp = pipe_instance.layers[1]
        assert abs(lpp.weight_per_meter_kg - expected_values["lpp_weight_kg_m"]) < 0.5

    def test_concrete_weight(self, pipe_instance, expected_values):
        """Test concrete coating weight per meter."""
        concrete = pipe_instance.layers[2]
        assert abs(concrete.weight_per_meter_kg - expected_values["concrete_weight_kg_m"]) < 2.0

    def test_total_weight_in_air(self, pipe_instance, expected_values):
        """Test total pipe weight in air."""
        assert abs(pipe_instance.total_weight_in_air_kg_m - expected_values["total_weight_kg_m"]) < 2.0

    def test_displaced_volume(self, pipe_instance, expected_values):
        """Test displaced water volume per meter."""
        assert abs(pipe_instance.displaced_volume_m3_m - expected_values["displaced_volume_m3_m"]) < 0.01

    def test_buoyancy_force(self, pipe_instance, expected_values):
        """Test buoyancy force per meter."""
        assert abs(pipe_instance.buoyancy_force_kg_m - expected_values["buoyancy_force_kg_m"]) < 2.0

    def test_submerged_weight(self, pipe_instance, expected_values):
        """Test submerged weight per meter."""
        assert abs(pipe_instance.submerged_weight_kg_m - expected_values["submerged_weight_kg_m"]) < 2.0

    def test_pipe_sinks(self, pipe_instance):
        """Test that pipe has negative buoyancy (will sink)."""
        assert pipe_instance.submerged_weight_kg_m > 0
        assert not pipe_instance.is_buoyant


class TestPipeCreationMethods:
    """Tests for alternative pipe creation methods."""

    def test_from_inches(self):
        """Test creating pipe from imperial units."""
        from digitalmodel.pipe_cross_section import PipeCrossSection

        pipe = PipeCrossSection.from_inches(
            steel_od_inch=24,
            steel_wt_inch=0.5625,
            lpp_thickness_mm=3.5,
            concrete_thickness_inch=3.15,
        )
        assert abs(pipe.steel_od_mm - 609.6) < 0.1
        assert abs(pipe.steel_wt_mm - 14.2875) < 0.01

    def test_from_dict(self, pipe_config):
        """Test creating pipe from dictionary configuration."""
        from digitalmodel.pipe_cross_section import PipeCrossSection

        pipe = PipeCrossSection.from_dict({"pipe_cross_section": pipe_config})
        assert len(pipe.layers) == 3

    def test_from_config(self, pipe_config):
        """Test creating pipe from config object."""
        from digitalmodel.pipe_cross_section import (
            PipeCrossSection,
            PipeCrossSectionConfig,
        )

        config = PipeCrossSectionConfig(**pipe_config)
        pipe = PipeCrossSection.from_config(config)
        assert len(pipe.layers) == 3


class TestUnitConversions:
    """Tests for unit conversion utilities."""

    def test_inch_to_mm(self):
        """Test inch to mm conversion."""
        from digitalmodel.pipe_cross_section import inch_to_mm

        assert abs(inch_to_mm(24) - 609.6) < 0.1
        assert abs(inch_to_mm(0.5625) - 14.2875) < 0.01

    def test_mm_to_inch(self):
        """Test mm to inch conversion."""
        from digitalmodel.pipe_cross_section import mm_to_inch

        assert abs(mm_to_inch(609.6) - 24) < 0.01
        assert abs(mm_to_inch(776.6) - 30.57) < 0.01


class TestDataExport:
    """Tests for data export functionality."""

    def test_to_dict(self, pipe_instance):
        """Test conversion to dictionary."""
        data = pipe_instance.to_dict()
        assert "layers" in data
        assert "summary" in data
        assert "buoyancy" in data
        assert len(data["layers"]) == 3

    def test_to_csv_data(self, pipe_instance):
        """Test CSV data generation."""
        csv_data = pipe_instance.to_csv_data()
        assert len(csv_data) >= 3  # At least 3 layer rows + total
        assert csv_data[0]["Layer"] == "Steel Pipe"
        assert csv_data[-1]["Layer"] == "TOTAL"

    def test_get_summary(self, pipe_instance):
        """Test summary generation."""
        summary = pipe_instance.get_summary()
        assert "Outer_Diameter_mm" in summary
        assert "Submerged_Weight_kg_m" in summary
        assert "Is_Buoyant" in summary

    def test_buoyancy_result(self, pipe_instance):
        """Test buoyancy result generation."""
        result = pipe_instance.get_buoyancy_result()
        assert hasattr(result, "displaced_volume_m3_m")
        assert hasattr(result, "is_buoyant")
        assert result.is_buoyant == False


class TestEdgeCases:
    """Tests for edge cases and special configurations."""

    def test_bare_steel_pipe(self, pipe_config_no_coatings):
        """Test pipe with no coatings."""
        from digitalmodel.pipe_cross_section import PipeCrossSection

        pipe = PipeCrossSection(**pipe_config_no_coatings)
        assert len(pipe.layers) == 1
        assert pipe.layers[0].name == "Steel Pipe"
        assert pipe.outer_diameter_mm == 609.6

    def test_lpp_only_pipe(self, pipe_config_lpp_only):
        """Test pipe with 3LPP but no concrete."""
        from digitalmodel.pipe_cross_section import PipeCrossSection

        pipe = PipeCrossSection(**pipe_config_lpp_only)
        assert len(pipe.layers) == 2
        assert pipe.layers[1].name == "3LPP Coating"
        assert abs(pipe.outer_diameter_mm - 616.6) < 0.1


@pytest.fixture
def circular_section_example(capsys):
    """Execute the documented snippet so its inputs and API cannot drift."""
    document = (
        Path(__file__).resolve().parents[3]
        / "docs/examples/circular-section-weight-buoyancy.md"
    ).read_text(encoding="utf-8")
    snippet = document.split("```python\n")[1].split("```")[0]
    namespace = {"__name__": "__main__"}
    exec(compile(snippet, "circular-section-weight-buoyancy.md", "exec"), namespace)
    return namespace["pipe"]


class TestCircularSectionWorkedExample:
    """Independent expectations from the worked example's H1 hand derivation."""

    def test_layer_geometry_and_mass(self, circular_section_example):
        """Check millimetre inputs against H1 annular areas in square metres."""
        pipe = circular_section_example
        # S1 dimensions; H1: pi/4 * (OD_m**2 - ID_m**2).
        assert pipe.inner_diameter_mm == 180
        assert pipe.outer_diameter_mm == 250
        assert [layer.cross_sectional_area_m2 for layer in pipe.layers] == pytest.approx(
            [0.0019 * math.pi, 0.001025 * math.pi, 0.0046 * math.pi]
        )
        # H1 areas times S1 synthetic densities, independently reduced by hand.
        assert [layer.weight_per_meter_kg for layer in pipe.layers] == pytest.approx(
            [15.2 * math.pi, 1.025 * math.pi, 11.04 * math.pi]
        )

    def test_dry_mass_and_displacement(self, circular_section_example, capsys):
        """Check H1 dry mass and full exterior displacement, including the bore."""
        pipe = circular_section_example
        assert pipe.total_weight_in_air_kg_m == pytest.approx(27.265 * math.pi)
        assert pipe.displaced_volume_m3_m == pytest.approx(0.015625 * math.pi)
        assert pipe.buoyancy_force_kg_m == pytest.approx(15.625 * math.pi)
        # H1 arithmetic rounded only for display; U1/G1 establish the labels.
        assert capsys.readouterr().out == (
            "Dry mass: 85.7 kg/m\n"
            "Displaced volume: 0.049087 m^3/m\n"
            "Displaced water mass: 49.1 kg/m\n"
            "Submerged mass equivalent: 36.6 kg/m\n"
            "Submerged force: 0.359 kN/m\n"
        )

    def test_submerged_mass(self, circular_section_example):
        """H1: (27.265 - 15.625) pi = 11.64 pi kg/m, downward positive."""
        assert circular_section_example.submerged_weight_kg_m == pytest.approx(
            11.64 * math.pi
        )
        assert not circular_section_example.is_buoyant

    @pytest.mark.parametrize(
        ("attribute", "hand_mass_coefficient"),
        [
            ("total_weight_in_air_kN_m", 27.265),
            ("buoyancy_force_kN_m", 15.625),
            ("submerged_weight_kN_m", 11.64),
        ],
        ids=["dry", "buoyancy", "submerged"],
    )
    def test_force_units(
        self, circular_section_example, attribute, hand_mass_coefficient
    ):
        """H1 mass times G1 gravity, divided by U1 newtons per kilonewton."""
        expected_kN_m = hand_mass_coefficient * math.pi * 9.81 / 1000
        assert getattr(circular_section_example, attribute) == pytest.approx(expected_kN_m)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
