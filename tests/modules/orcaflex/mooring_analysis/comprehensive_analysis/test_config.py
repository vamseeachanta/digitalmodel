"""Tests for configuration management in comprehensive mooring analysis."""

import pytest
import tempfile
from pathlib import Path
import yaml

from digitalmodel.modules.orcaflex.mooring_analysis.comprehensive_analysis.config import (
    AnalysisConfig,
    ConvergenceCriteria,
    StiffnessConfig,
    FenderConfig,
    GroupingConfig,
    ReportConfig,
    ProcessingConfig,
    IndustryStandards,
    create_default_config
)


class TestConvergenceCriteria:
    """Test suite for ConvergenceCriteria configuration."""
    
    def test_default_values(self):
        """Test default convergence criteria values."""
        criteria = ConvergenceCriteria()
        
        assert criteria.tolerance == 0.05
        assert criteria.max_iterations == 100
        assert criteria.min_tension_kN == 10.0
        assert criteria.warning_threshold == 0.10
    
    def test_custom_values(self):
        """Test custom convergence criteria values."""
        criteria = ConvergenceCriteria(
            tolerance=0.02,
            max_iterations=50,
            min_tension_kN=5.0,
            warning_threshold=0.08
        )
        
        assert criteria.tolerance == 0.02
        assert criteria.max_iterations == 50


class TestStiffnessConfig:
    """Test suite for StiffnessConfig configuration."""
    
    def test_default_values(self):
        """Test default stiffness configuration values."""
        config = StiffnessConfig()
        
        assert config.compute_natural_periods is True
        assert config.vessel_mass_tonnes is None
        assert config.include_rotational_dof is True
        assert config.eigenvalue_analysis is True
        assert config.anisotropy_threshold == 2.0
    
    def test_custom_values(self):
        """Test custom stiffness configuration values."""
        config = StiffnessConfig(
            compute_natural_periods=False,
            vessel_mass_tonnes=150000.0,
            include_rotational_dof=False,
            eigenvalue_analysis=False,
            anisotropy_threshold=3.0
        )
        
        assert config.compute_natural_periods is False
        assert config.vessel_mass_tonnes == 150000.0


class TestFenderConfig:
    """Test suite for FenderConfig configuration."""
    
    def test_default_values(self):
        """Test default fender configuration values."""
        config = FenderConfig()
        
        assert config.design_capacity_kN == {}
        assert config.warning_utilization == 0.8
        assert config.critical_utilization == 0.95
        assert config.contact_time_threshold == 0.1
    
    def test_custom_values(self):
        """Test custom fender configuration values."""
        config = FenderConfig(
            design_capacity_kN={'F1': 5000, 'F2': 5000},
            warning_utilization=0.75,
            critical_utilization=0.90
        )
        
        assert config.design_capacity_kN['F1'] == 5000
        assert config.warning_utilization == 0.75


class TestGroupingConfig:
    """Test suite for GroupingConfig configuration."""
    
    def test_default_values(self):
        """Test default grouping configuration values."""
        config = GroupingConfig()
        
        assert config.auto_group is True
        assert len(config.group_patterns) == 4
        assert config.min_group_size == 2
        assert 'pretension_convergence' in config.compare_metrics
    
    def test_custom_patterns(self):
        """Test custom grouping patterns."""
        config = GroupingConfig(
            group_patterns=[r'(?P<vessel>custom_pattern)'],
            min_group_size=3
        )
        
        assert len(config.group_patterns) == 1
        assert config.min_group_size == 3


class TestReportConfig:
    """Test suite for ReportConfig configuration."""
    
    def test_default_values(self):
        """Test default report configuration values."""
        config = ReportConfig()
        
        assert 'markdown' in config.formats
        assert 'html' in config.formats
        assert config.include_plots is True
        assert config.plot_dpi == 150
        assert config.embed_images is True
    
    def test_custom_formats(self):
        """Test custom report formats."""
        config = ReportConfig(
            formats=['markdown', 'html', 'pdf', 'excel'],
            plot_dpi=300,
            embed_images=False
        )
        
        assert 'pdf' in config.formats
        assert 'excel' in config.formats
        assert config.plot_dpi == 300


class TestProcessingConfig:
    """Test suite for ProcessingConfig configuration."""
    
    def test_default_values(self):
        """Test default processing configuration values."""
        config = ProcessingConfig()
        
        assert config.parallel is True
        assert config.max_workers is None
        assert config.chunk_size == 10
        assert config.progress_bar is True
        assert config.cache_results is True
        assert config.memory_limit_gb == 8.0
    
    def test_custom_values(self):
        """Test custom processing configuration values."""
        config = ProcessingConfig(
            parallel=False,
            max_workers=16,
            chunk_size=20,
            memory_limit_gb=16.0
        )
        
        assert config.parallel is False
        assert config.max_workers == 16


class TestIndustryStandards:
    """Test suite for IndustryStandards configuration."""
    
    def test_default_values(self):
        """Test default industry standards values."""
        standards = IndustryStandards()
        
        assert standards.standard == "DNV-OS-E301"
        assert standards.safety_factors['intact'] == 1.67
        assert standards.tension_limits['chain'] == 0.95
        assert standards.api_rp_2sk_compliance is True
    
    def test_custom_standards(self):
        """Test custom industry standards."""
        standards = IndustryStandards(
            standard="API-RP-2SK",
            safety_factors={'intact': 2.0, 'damaged': 1.5},
            tension_limits={'chain': 0.90, 'wire': 0.80}
        )
        
        assert standards.standard == "API-RP-2SK"
        assert standards.safety_factors['intact'] == 2.0


class TestAnalysisConfig:
    """Test suite for main AnalysisConfig."""
    
    def test_default_config(self):
        """Test default analysis configuration."""
        config = AnalysisConfig()
        
        assert config.input_directory == Path.cwd()
        assert config.output_directory == Path('output')
        assert config.file_pattern == "*.csv"
        assert config.recursive is True
        assert config.use_llm_context is True
        assert config.log_level == "INFO"
    
    def test_custom_config(self):
        """Test custom analysis configuration."""
        config = AnalysisConfig(
            input_directory=Path('/data/input'),
            output_directory=Path('/data/output'),
            file_pattern="*analysis*.csv",
            recursive=False,
            use_llm_context=False,
            log_level="DEBUG"
        )
        
        assert config.input_directory == Path('/data/input')
        assert config.file_pattern == "*analysis*.csv"
        assert config.use_llm_context is False
    
    def test_nested_configs(self):
        """Test nested configuration objects."""
        config = AnalysisConfig()
        
        assert isinstance(config.convergence, ConvergenceCriteria)
        assert isinstance(config.stiffness, StiffnessConfig)
        assert isinstance(config.fender, FenderConfig)
        assert isinstance(config.grouping, GroupingConfig)
        assert isinstance(config.report, ReportConfig)
        assert isinstance(config.processing, ProcessingConfig)
        assert isinstance(config.standards, IndustryStandards)
    
    def test_config_from_dict(self):
        """Test creating configuration from dictionary."""
        config_dict = {
            'input_directory': '/test/input',
            'output_directory': '/test/output',
            'convergence': {
                'tolerance': 0.03,
                'max_iterations': 75
            },
            'stiffness': {
                'vessel_mass_tonnes': 200000.0
            },
            'processing': {
                'max_workers': 8,
                'cache_dir': '/test/cache'
            }
        }
        
        config = AnalysisConfig.from_dict(config_dict)
        
        assert config.input_directory == Path('/test/input')
        assert config.convergence.tolerance == 0.03
        assert config.stiffness.vessel_mass_tonnes == 200000.0
        assert config.processing.max_workers == 8
        assert config.processing.cache_dir == Path('/test/cache')
    
    def test_config_to_yaml(self, temp_directory):
        """Test saving configuration to YAML file."""
        config = AnalysisConfig(
            input_directory=Path('/test/input'),
            output_directory=Path('/test/output')
        )
        config.convergence.tolerance = 0.04
        config.stiffness.vessel_mass_tonnes = 175000.0
        
        yaml_path = temp_directory / 'test_config.yml'
        config.to_yaml(yaml_path)
        
        assert yaml_path.exists()
        
        # Read back and verify
        with open(yaml_path, 'r') as f:
            loaded_dict = yaml.safe_load(f)
        
        # Handle Windows path separators
        assert loaded_dict['input_directory'].replace('\\', '/') == '/test/input'
        assert loaded_dict['convergence']['tolerance'] == 0.04
        assert loaded_dict['stiffness']['vessel_mass_tonnes'] == 175000.0
    
    def test_config_from_yaml(self, temp_directory):
        """Test loading configuration from YAML file."""
        # Create YAML file
        yaml_content = """
input_directory: /yaml/input
output_directory: /yaml/output
file_pattern: "*.csv"
recursive: false

convergence:
  tolerance: 0.025
  max_iterations: 80
  
stiffness:
  vessel_mass_tonnes: 180000.0
  compute_natural_periods: true
  
fender:
  design_capacity_kN:
    F1: 6000
    F2: 6000
  warning_utilization: 0.85
  
processing:
  max_workers: 12
  chunk_size: 15
  cache_dir: /yaml/cache
"""
        
        yaml_path = temp_directory / 'load_config.yml'
        with open(yaml_path, 'w') as f:
            f.write(yaml_content)
        
        config = AnalysisConfig.from_yaml(yaml_path)
        
        assert config.input_directory == Path('/yaml/input')
        assert config.convergence.tolerance == 0.025
        assert config.stiffness.vessel_mass_tonnes == 180000.0
        assert config.fender.design_capacity_kN['F1'] == 6000
        assert config.processing.max_workers == 12
        assert config.processing.cache_dir == Path('/yaml/cache')
    
    def test_config_validation(self):
        """Test configuration validation."""
        # Valid configuration
        config = AnalysisConfig()
        warnings = config.validate()
        # Default config might have warning about input directory not existing
        assert len(warnings) <= 1
        
        # Invalid configuration
        config.convergence.tolerance = -0.05  # Negative tolerance
        config.fender.warning_utilization = 1.0
        config.fender.critical_utilization = 0.9  # Warning > critical
        config.processing.memory_limit_gb = -1  # Negative memory
        
        warnings = config.validate()
        assert len(warnings) >= 3
        assert any('tolerance' in w for w in warnings)
        assert any('utilization' in w for w in warnings)
        assert any('memory' in w for w in warnings)


class TestCreateDefaultConfig:
    """Test default configuration creation."""
    
    def test_create_default(self):
        """Test creating default configuration."""
        config = create_default_config()
        
        assert isinstance(config, AnalysisConfig)
        assert config.input_directory == Path.cwd()
        assert config.convergence.tolerance == 0.05
    
    def test_create_and_save_default(self, temp_directory):
        """Test creating and saving default configuration."""
        yaml_path = temp_directory / 'default_config.yml'
        config = create_default_config(output_path=yaml_path)
        
        assert yaml_path.exists()
        
        # Verify saved content
        loaded_config = AnalysisConfig.from_yaml(yaml_path)
        assert loaded_config.convergence.tolerance == config.convergence.tolerance
        assert loaded_config.processing.parallel == config.processing.parallel