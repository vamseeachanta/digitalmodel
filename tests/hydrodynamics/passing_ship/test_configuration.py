"""
Test suite for passing ship configuration system.

Tests YAML configuration parsing, Pydantic validation, unit conversions,
and configuration merging capabilities.
"""

import pytest
import yaml
import tempfile
import os
from pathlib import Path
from pydantic import ValidationError

from digitalmodel.hydrodynamics.passing_ship.configuration import (
    VesselConfig,
    EnvironmentalConfig,
    CalculationConfig,
    PassingShipConfig,
    YAMLConfigParser,
    UnitConverter,
    ConfigurationMerger,
)


class TestPydanticModels:
    """Test Pydantic configuration models."""
    
    def test_vessel_config_valid(self):
        """Test valid vessel configuration."""
        config = VesselConfig(
            length=200.0,
            beam=32.0,
            draft=12.0,
            block_coefficient=0.85,
            name="Test Vessel"
        )
        assert config.length == 200.0
        assert config.beam == 32.0
        assert config.draft == 12.0
        assert config.block_coefficient == 0.85
        assert config.name == "Test Vessel"
    
    def test_vessel_config_validation_ranges(self):
        """Test vessel configuration validation ranges."""
        # Block coefficient must be between 0 and 1
        with pytest.raises(ValidationError):
            VesselConfig(
                length=200.0,
                beam=32.0,
                draft=12.0,
                block_coefficient=1.5  # Invalid
            )
        
        # Negative dimensions not allowed
        with pytest.raises(ValidationError):
            VesselConfig(
                length=-200.0,  # Invalid
                beam=32.0,
                draft=12.0,
                block_coefficient=0.85
            )
    
    def test_vessel_config_computed_properties(self):
        """Test computed properties on vessel config."""
        config = VesselConfig(
            length=200.0,
            beam=32.0,
            draft=12.0,
            block_coefficient=0.85
        )
        # Cross-sectional area
        expected_area = 32.0 * 12.0 * 0.85
        assert config.midship_area == expected_area
        
        # Displacement (approximate)
        expected_displacement = 200.0 * 32.0 * 12.0 * 0.85 * 1.025
        assert abs(config.displacement - expected_displacement) < 1.0
    
    def test_environmental_config_valid(self):
        """Test valid environmental configuration."""
        config = EnvironmentalConfig(
            water_depth=50.0,
            water_density=1025.0,
            current_velocity=0.5
        )
        assert config.water_depth == 50.0
        assert config.water_density == 1025.0
        assert config.current_velocity == 0.5
    
    def test_environmental_config_infinite_depth(self):
        """Test infinite depth configuration."""
        config = EnvironmentalConfig(
            water_depth=None,  # Infinite depth
            water_density=1025.0
        )
        assert config.water_depth is None
        assert config.is_infinite_depth is True
    
    def test_calculation_config_valid(self):
        """Test valid calculation configuration."""
        config = CalculationConfig(
            stagger_distance=100.0,
            lateral_separation=50.0,
            passing_velocity=10.0,
            num_points=100,
            integration_tolerance=1e-4,
            max_iterations=1000
        )
        assert config.stagger_distance == 100.0
        assert config.lateral_separation == 50.0
        assert config.passing_velocity == 10.0
        assert config.num_points == 100
    
    def test_calculation_config_defaults(self):
        """Test calculation configuration defaults."""
        config = CalculationConfig(
            stagger_distance=100.0,
            lateral_separation=50.0,
            passing_velocity=10.0
        )
        # Check defaults
        assert config.num_points == 50  # Default
        assert config.integration_tolerance == 1e-6  # Default
        assert config.max_iterations == 1000  # Default
    
    def test_passing_ship_config_complete(self):
        """Test complete passing ship configuration."""
        config = PassingShipConfig(
            moored_vessel=VesselConfig(
                length=200.0,
                beam=32.0,
                draft=12.0,
                block_coefficient=0.85,
                name="Moored"
            ),
            passing_vessel=VesselConfig(
                length=180.0,
                beam=30.0,
                draft=10.0,
                block_coefficient=0.82,
                name="Passing"
            ),
            environment=EnvironmentalConfig(
                water_depth=50.0,
                water_density=1025.0
            ),
            calculation=CalculationConfig(
                stagger_distance=100.0,
                lateral_separation=50.0,
                passing_velocity=10.0
            ),
            output_units="SI",
            description="Test configuration"
        )
        assert config.moored_vessel.name == "Moored"
        assert config.passing_vessel.name == "Passing"
        assert config.output_units == "SI"


class TestYAMLParser:
    """Test YAML configuration parser."""
    
    def test_parse_simple_yaml(self):
        """Test parsing simple YAML configuration."""
        yaml_content = """
        moored_vessel:
          length: 200.0
          beam: 32.0
          draft: 12.0
          block_coefficient: 0.85
          name: "Test Vessel"
        
        passing_vessel:
          length: 180.0
          beam: 30.0
          draft: 10.0
          block_coefficient: 0.82
          name: "Passing Vessel"
        
        environment:
          water_depth: 50.0
          water_density: 1025.0
        
        calculation:
          stagger_distance: 100.0
          lateral_separation: 50.0
          passing_velocity: 10.0
        """
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write(yaml_content)
            temp_path = f.name
        
        try:
            parser = YAMLConfigParser()
            config = parser.parse_file(temp_path)
            
            assert isinstance(config, PassingShipConfig)
            assert config.moored_vessel.length == 200.0
            assert config.passing_vessel.name == "Passing Vessel"
        finally:
            os.unlink(temp_path)
    
    def test_parse_with_expressions(self):
        """Test parsing YAML with mathematical expressions."""
        yaml_content = """
        moored_vessel:
          length: 200.0
          beam: !eval "200.0 / 6.5"  # L/B ratio of 6.5
          draft: !eval "200.0 / 16.67"  # L/T ratio of 16.67
          block_coefficient: 0.85
        
        passing_vessel:
          length: !eval "200.0 * 0.9"  # 90% of moored vessel
          beam: 30.0
          draft: 10.0
          block_coefficient: 0.82
        
        environment:
          water_depth: !eval "12.0 * 4"  # 4x draft
          water_density: 1025.0
        
        calculation:
          stagger_distance: !eval "200.0 * 0.5"  # Half length
          lateral_separation: !eval "32.0 * 1.5"  # 1.5x beam
          passing_velocity: 10.0
        """
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write(yaml_content)
            temp_path = f.name
        
        try:
            parser = YAMLConfigParser()
            config = parser.parse_file(temp_path)
            
            # Check evaluated expressions
            assert abs(config.moored_vessel.beam - 200.0/6.5) < 0.01
            assert abs(config.moored_vessel.draft - 200.0/16.67) < 0.01
            assert config.passing_vessel.length == 180.0
            assert config.environment.water_depth == 48.0
            assert config.calculation.stagger_distance == 100.0
            assert config.calculation.lateral_separation == 48.0
        finally:
            os.unlink(temp_path)
    
    def test_parse_with_variables(self):
        """Test parsing YAML with variable references."""
        yaml_content = """
        variables:
          base_length: 200.0
          scale_factor: 0.9
        
        moored_vessel:
          length: !var base_length
          beam: 32.0
          draft: 12.0
          block_coefficient: 0.85
        
        passing_vessel:
          length: !eval "var:base_length * var:scale_factor"
          beam: 30.0
          draft: 10.0
          block_coefficient: 0.82
        
        environment:
          water_depth: 50.0
          water_density: 1025.0
        
        calculation:
          stagger_distance: !var base_length
          lateral_separation: 50.0
          passing_velocity: 10.0
        """
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write(yaml_content)
            temp_path = f.name
        
        try:
            parser = YAMLConfigParser()
            config = parser.parse_file(temp_path)
            
            assert config.moored_vessel.length == 200.0
            assert config.passing_vessel.length == 180.0
            assert config.calculation.stagger_distance == 200.0
        finally:
            os.unlink(temp_path)
    
    def test_parse_invalid_yaml(self):
        """Test parsing invalid YAML raises appropriate errors."""
        yaml_content = """
        moored_vessel:
          length: "not_a_number"  # Invalid
          beam: 32.0
          draft: 12.0
          block_coefficient: 0.85
        """
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write(yaml_content)
            temp_path = f.name
        
        try:
            parser = YAMLConfigParser()
            with pytest.raises(ValidationError):
                parser.parse_file(temp_path)
        finally:
            os.unlink(temp_path)


class TestUnitConverter:
    """Test unit conversion functionality."""
    
    def test_length_conversions(self):
        """Test length unit conversions."""
        converter = UnitConverter()
        
        # Meters to feet
        assert abs(converter.convert(100.0, "m", "ft") - 328.084) < 0.01
        
        # Feet to meters
        assert abs(converter.convert(328.084, "ft", "m") - 100.0) < 0.01
        
        # Meters to meters (no conversion)
        assert converter.convert(100.0, "m", "m") == 100.0
    
    def test_force_conversions(self):
        """Test force unit conversions."""
        converter = UnitConverter()
        
        # Newtons to kilonewtons
        assert converter.convert(1000.0, "N", "kN") == 1.0
        
        # Newtons to pounds force
        assert abs(converter.convert(1000.0, "N", "lbf") - 224.809) < 0.01
        
        # Meganewtons to newtons
        assert converter.convert(1.5, "MN", "N") == 1.5e6
    
    def test_moment_conversions(self):
        """Test moment unit conversions."""
        converter = UnitConverter()
        
        # Newton-meters to kilonewton-meters
        assert converter.convert(1000.0, "N.m", "kN.m") == 1.0
        
        # Newton-meters to meganewton-meters
        assert converter.convert(1e6, "N.m", "MN.m") == 1.0
    
    def test_velocity_conversions(self):
        """Test velocity unit conversions."""
        converter = UnitConverter()
        
        # Meters per second to knots
        assert abs(converter.convert(10.0, "m/s", "knots") - 19.438) < 0.01
        
        # Knots to meters per second
        assert abs(converter.convert(19.438, "knots", "m/s") - 10.0) < 0.01
    
    def test_density_conversions(self):
        """Test density unit conversions."""
        converter = UnitConverter()
        
        # kg/m³ to specific gravity
        assert abs(converter.convert(1025.0, "kg/m3", "sg") - 1.025) < 0.001
        
        # Specific gravity to kg/m³
        assert converter.convert(1.025, "sg", "kg/m3") == 1025.0
    
    def test_unit_detection(self):
        """Test automatic unit detection from values."""
        converter = UnitConverter()
        
        # Detect imperial from large force value with 'lbf' suffix
        units = converter.detect_units({"force": "1000 lbf"})
        assert units == "Imperial"
        
        # Detect SI from 'kN' suffix
        units = converter.detect_units({"force": "100 kN"})
        assert units == "SI"
    
    def test_convert_config_units(self):
        """Test converting entire configuration units."""
        converter = UnitConverter()
        
        config_si = {
            "length": 200.0,  # meters
            "beam": 32.0,     # meters
            "draft": 12.0,    # meters
            "velocity": 10.0, # m/s
        }
        
        # Convert to Imperial
        config_imp = converter.convert_config(config_si, "SI", "Imperial")
        
        assert abs(config_imp["length"] - 656.168) < 0.01
        assert abs(config_imp["beam"] - 104.987) < 0.01
        assert abs(config_imp["draft"] - 39.370) < 0.01
        assert abs(config_imp["velocity"] - 19.438) < 0.01


class TestConfigurationMerger:
    """Test configuration merging and override capabilities."""
    
    def test_merge_simple_configs(self):
        """Test merging two simple configurations."""
        merger = ConfigurationMerger()
        
        base_config = {
            "moored_vessel": {
                "length": 200.0,
                "beam": 32.0,
                "draft": 12.0,
                "block_coefficient": 0.85
            },
            "environment": {
                "water_depth": 50.0,
                "water_density": 1025.0
            }
        }
        
        override_config = {
            "moored_vessel": {
                "length": 210.0,  # Override
                "name": "Modified Vessel"  # New field
            },
            "calculation": {  # New section
                "stagger_distance": 100.0,
                "lateral_separation": 50.0,
                "passing_velocity": 10.0
            }
        }
        
        merged = merger.merge(base_config, override_config)
        
        # Check overridden value
        assert merged["moored_vessel"]["length"] == 210.0
        
        # Check preserved values
        assert merged["moored_vessel"]["beam"] == 32.0
        assert merged["moored_vessel"]["draft"] == 12.0
        
        # Check new field
        assert merged["moored_vessel"]["name"] == "Modified Vessel"
        
        # Check new section
        assert merged["calculation"]["stagger_distance"] == 100.0
        
        # Check untouched section
        assert merged["environment"]["water_depth"] == 50.0
    
    def test_merge_nested_configs(self):
        """Test merging deeply nested configurations."""
        merger = ConfigurationMerger()
        
        base_config = {
            "vessels": {
                "moored": {
                    "dimensions": {
                        "length": 200.0,
                        "beam": 32.0
                    }
                }
            }
        }
        
        override_config = {
            "vessels": {
                "moored": {
                    "dimensions": {
                        "draft": 12.0  # Add new field
                    },
                    "properties": {  # Add new section
                        "block_coefficient": 0.85
                    }
                },
                "passing": {  # Add new vessel
                    "dimensions": {
                        "length": 180.0
                    }
                }
            }
        }
        
        merged = merger.merge(base_config, override_config)
        
        # Check preserved nested values
        assert merged["vessels"]["moored"]["dimensions"]["length"] == 200.0
        assert merged["vessels"]["moored"]["dimensions"]["beam"] == 32.0
        
        # Check added nested values
        assert merged["vessels"]["moored"]["dimensions"]["draft"] == 12.0
        assert merged["vessels"]["moored"]["properties"]["block_coefficient"] == 0.85
        assert merged["vessels"]["passing"]["dimensions"]["length"] == 180.0
    
    def test_merge_with_arrays(self):
        """Test merging configurations with arrays."""
        merger = ConfigurationMerger()
        
        base_config = {
            "stagger_distances": [0, 50, 100, 150],
            "options": {
                "output_formats": ["csv", "json"]
            }
        }
        
        override_config = {
            "stagger_distances": [0, 25, 50, 75, 100],  # Replace array
            "options": {
                "output_formats": ["csv", "json", "excel"]  # Extend array
            }
        }
        
        merged = merger.merge(base_config, override_config)
        
        # Arrays should be replaced by default
        assert merged["stagger_distances"] == [0, 25, 50, 75, 100]
        assert merged["options"]["output_formats"] == ["csv", "json", "excel"]
    
    def test_merge_multiple_configs(self):
        """Test merging multiple configuration files."""
        merger = ConfigurationMerger()
        
        configs = [
            {"a": 1, "b": 2},
            {"b": 3, "c": 4},
            {"c": 5, "d": 6}
        ]
        
        merged = merger.merge_multiple(configs)
        
        assert merged["a"] == 1
        assert merged["b"] == 3  # Overridden
        assert merged["c"] == 5  # Overridden
        assert merged["d"] == 6


class TestConfigurationTemplates:
    """Test configuration template loading."""
    
    def test_load_basic_template(self):
        """Test loading basic vessel template."""
        template_dir = Path(__file__).parent.parent.parent.parent / "src" / "digitalmodel" / "hydrodynamics" / "passing_ship" / "templates"
        template_file = template_dir / "basic.yml"
        
        if template_file.exists():
            parser = YAMLConfigParser()
            config = parser.parse_file(str(template_file))
            
            assert isinstance(config, PassingShipConfig)
            assert config.moored_vessel.length > 0
            assert config.passing_vessel.length > 0
    
    def test_load_tanker_template(self):
        """Test loading tanker vessel template."""
        template_dir = Path(__file__).parent.parent.parent.parent / "src" / "digitalmodel" / "hydrodynamics" / "passing_ship" / "templates"
        template_file = template_dir / "tanker.yml"
        
        if template_file.exists():
            parser = YAMLConfigParser()
            config = parser.parse_file(str(template_file))
            
            assert isinstance(config, PassingShipConfig)
            # Tankers typically have high block coefficient
            assert config.moored_vessel.block_coefficient > 0.8
    
    def test_load_offshore_template(self):
        """Test loading offshore vessel template."""
        template_dir = Path(__file__).parent.parent.parent.parent / "src" / "digitalmodel" / "hydrodynamics" / "passing_ship" / "templates"
        template_file = template_dir / "offshore.yml"
        
        if template_file.exists():
            parser = YAMLConfigParser()
            config = parser.parse_file(str(template_file))
            
            assert isinstance(config, PassingShipConfig)
            # Offshore vessels often operate in deeper water
            if config.environment.water_depth:
                assert config.environment.water_depth > 30.0


class TestConfigurationIntegration:
    """Test configuration system integration."""
    
    def test_complete_workflow(self):
        """Test complete configuration workflow."""
        # Create a test configuration
        yaml_content = """
        moored_vessel:
          length: 200.0
          beam: 32.0
          draft: 12.0
          block_coefficient: 0.85
          name: "Moored FPSO"
        
        passing_vessel:
          length: !eval "200.0 * 0.9"
          beam: 30.0
          draft: 10.0
          block_coefficient: 0.82
          name: "Supply Vessel"
        
        environment:
          water_depth: 50.0
          water_density: 1025.0
          current_velocity: 0.5
        
        calculation:
          stagger_distance: 100.0
          lateral_separation: 50.0
          passing_velocity: 10.0
          num_points: 100
          integration_tolerance: 1e-4

        output_units: "SI"
        description: "FPSO and supply vessel passing scenario"
        """
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            f.write(yaml_content)
            temp_path = f.name
        
        try:
            # Parse configuration
            parser = YAMLConfigParser()
            config = parser.parse_file(temp_path)
            
            # Validate parsed configuration
            assert config.moored_vessel.name == "Moored FPSO"
            assert config.passing_vessel.length == 180.0
            assert config.calculation.num_points == 100
            
            # Test configuration can be used for calculations
            vessel_params = {
                'L': config.moored_vessel.length,
                'B': config.moored_vessel.beam,
                'T': config.moored_vessel.draft,
                'Cb': config.moored_vessel.block_coefficient
            }
            
            passing_params = {
                'U': config.calculation.passing_velocity,
                'y': config.calculation.lateral_separation,
                'x': config.calculation.stagger_distance
            }
            
            # Verify parameters are correctly extracted
            assert vessel_params['L'] == 200.0
            assert passing_params['U'] == 10.0
            
        finally:
            os.unlink(temp_path)
    
    def test_multi_scenario_batch(self):
        """Test batch configuration for multiple scenarios."""
        yaml_content = """
        base_vessel:
          length: 200.0
          beam: 32.0
          draft: 12.0
          block_coefficient: 0.85
        
        scenarios:
          - name: "Close passing"
            lateral_separation: 30.0
            passing_velocity: 8.0
          - name: "Fast passing"
            lateral_separation: 50.0
            passing_velocity: 15.0
          - name: "Slow wide passing"
            lateral_separation: 100.0
            passing_velocity: 5.0
        
        environment:
          water_depth: 50.0
          water_density: 1025.0
        """
        
        # This test verifies the configuration can handle batch scenarios
        data = yaml.safe_load(yaml_content)
        
        assert len(data['scenarios']) == 3
        assert data['scenarios'][0]['name'] == "Close passing"
        assert data['scenarios'][1]['passing_velocity'] == 15.0
        assert data['scenarios'][2]['lateral_separation'] == 100.0