"""
Tests for OrcaFlex Configuration Management System

This module tests the unified configuration system including:
- Pydantic model validation
- YAML loading and saving
- Legacy configuration conversion
- Configuration merging and validation
"""

import pytest
import yaml
import tempfile
from pathlib import Path
from datetime import datetime
from typing import Dict, Any

from src.digitalmodel.modules.orcaflex.core.configuration import (
    OrcaFlexConfig,
    FileManagementConfig,
    AnalysisConfig,
    StaticAnalysisConfig,
    DynamicAnalysisConfig,
    IterationConfig,
    ParallelConfig,
    PostProcessConfig,
    ConfigurationManager,
    AnalysisType,
    SolverType,
    OutputFormat,
    LogLevel
)
from src.digitalmodel.modules.orcaflex.core.exceptions import (
    ConfigurationError,
    ValidationError
)


class TestFileManagementConfig:
    """Test FileManagementConfig model."""
    
    def test_default_values(self):
        """Test default configuration values."""
        config = FileManagementConfig()
        assert config.input_directory == Path(".")
        assert config.output_directory == Path("./results")
        assert config.input_files == {}
        assert config.output_formats == [OutputFormat.SIM]
        assert config.overwrite_existing is False
        assert config.create_subdirectories is True
    
    def test_custom_values(self):
        """Test custom configuration values."""
        config = FileManagementConfig(
            input_directory="/custom/input",
            output_directory="/custom/output",
            input_files={"dat": ["file1.dat", "file2.dat"]},
            output_formats=[OutputFormat.CSV, OutputFormat.EXCEL],
            overwrite_existing=True
        )
        assert config.input_directory == Path("/custom/input")
        assert config.output_directory == Path("/custom/output")
        assert config.input_files == {"dat": ["file1.dat", "file2.dat"]}
        assert OutputFormat.CSV in config.output_formats
        assert OutputFormat.EXCEL in config.output_formats
    
    def test_path_validation(self):
        """Test path validation."""
        config = FileManagementConfig(input_directory="./relative/path")
        assert isinstance(config.input_directory, Path)


class TestAnalysisConfig:
    """Test AnalysisConfig model."""
    
    def test_default_static_analysis(self):
        """Test default static analysis configuration."""
        config = AnalysisConfig()
        assert AnalysisType.STATIC in config.analysis_type
        assert config.static.enabled is True
        assert config.dynamic.enabled is False
    
    def test_dynamic_analysis_config(self):
        """Test dynamic analysis configuration."""
        config = AnalysisConfig(
            analysis_type=[AnalysisType.DYNAMIC],
            dynamic=DynamicAnalysisConfig(
                enabled=True,
                duration=7200.0,
                time_step=0.05
            )
        )
        assert AnalysisType.DYNAMIC in config.analysis_type
        assert config.dynamic.enabled is True
        assert config.dynamic.duration == 7200.0
        assert config.dynamic.time_step == 0.05
    
    def test_analysis_consistency_validation(self):
        """Test analysis consistency validation."""
        # This should raise a validation error
        with pytest.raises(ValueError):
            AnalysisConfig(
                analysis_type=[AnalysisType.STATIC],
                static=StaticAnalysisConfig(enabled=False)
            )
    
    def test_iteration_config(self):
        """Test iteration configuration."""
        config = AnalysisConfig(
            iteration=IterationConfig(
                enabled=True,
                target_parameter="tension",
                target_value=1000.0,
                tolerance=0.001
            )
        )
        assert config.iteration.enabled is True
        assert config.iteration.target_parameter == "tension"
        assert config.iteration.target_value == 1000.0


class TestParallelConfig:
    """Test ParallelConfig model."""
    
    def test_default_values(self):
        """Test default parallel configuration."""
        config = ParallelConfig()
        assert config.enabled is True
        assert config.num_threads == 30
        assert config.use_processes is True
        assert config.chunk_size == 10
        assert config.timeout == 3600.0
    
    def test_thread_limits(self):
        """Test thread count limits."""
        config = ParallelConfig(num_threads=64)
        assert config.num_threads == 64
        
        # Test upper limit
        with pytest.raises(ValueError):
            ParallelConfig(num_threads=200)
        
        # Test lower limit
        with pytest.raises(ValueError):
            ParallelConfig(num_threads=0)


class TestOrcaFlexConfig:
    """Test main OrcaFlexConfig model."""
    
    def test_default_configuration(self):
        """Test default configuration creation."""
        config = OrcaFlexConfig()
        assert config.name == "orcaflex_analysis"
        assert config.version == "2.0.0"
        assert config.log_level == LogLevel.INFO
        assert config.dry_run is False
        assert config.validate_only is False
    
    def test_complete_configuration(self):
        """Test complete configuration with all sections."""
        config = OrcaFlexConfig(
            name="test_analysis",
            description="Test analysis configuration",
            file_management=FileManagementConfig(
                input_directory="/data/input",
                output_directory="/data/output"
            ),
            analysis=AnalysisConfig(
                analysis_type=[AnalysisType.STATIC, AnalysisType.DYNAMIC]
            ),
            parallel=ParallelConfig(num_threads=16),
            post_process=PostProcessConfig(
                extract_summary=True,
                extract_time_series=True
            )
        )
        assert config.name == "test_analysis"
        assert config.file_management.input_directory == Path("/data/input")
        assert config.parallel.num_threads == 16
        assert config.post_process.extract_time_series is True
    
    def test_yaml_round_trip(self):
        """Test YAML serialization and deserialization."""
        original_config = OrcaFlexConfig(
            name="yaml_test",
            file_management=FileManagementConfig(
                input_files={"dat": ["test.dat"]}
            ),
            analysis=AnalysisConfig(
                static=StaticAnalysisConfig(tolerance=1e-8)
            )
        )
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            original_config.to_yaml(f.name)
            temp_file = f.name
        
        try:
            loaded_config = OrcaFlexConfig.from_yaml(temp_file)
            assert loaded_config.name == "yaml_test"
            assert loaded_config.analysis.static.tolerance == 1e-8
            assert loaded_config.file_management.input_files == {"dat": ["test.dat"]}
        finally:
            Path(temp_file).unlink()
    
    def test_configuration_merge(self):
        """Test configuration merging."""
        base_config = OrcaFlexConfig(
            name="base",
            parallel=ParallelConfig(num_threads=10)
        )
        
        override_config = OrcaFlexConfig(
            name="override",
            parallel=ParallelConfig(num_threads=20)
        )
        
        merged = base_config.merge(override_config)
        assert merged.name == "override"
        assert merged.parallel.num_threads == 20
    
    def test_compatibility_warnings(self):
        """Test configuration compatibility warnings."""
        config = OrcaFlexConfig(
            analysis=AnalysisConfig(
                static=StaticAnalysisConfig(use_calculated_positions=True),
                dynamic=DynamicAnalysisConfig(enabled=True)
            ),
            parallel=ParallelConfig(num_threads=60)
        )
        
        warnings = config.validate_compatibility()
        assert len(warnings) == 2
        assert any("calculated positions" in w for w in warnings)
        assert any("High thread count" in w for w in warnings)


class TestConfigurationManager:
    """Test ConfigurationManager functionality."""
    
    def test_default_configuration(self):
        """Test loading default configuration."""
        manager = ConfigurationManager()
        config = manager.load()
        assert isinstance(config, OrcaFlexConfig)
        assert config.name == "orcaflex_analysis"
    
    def test_load_from_dict(self):
        """Test loading configuration from dictionary."""
        manager = ConfigurationManager()
        config_dict = {
            "name": "dict_config",
            "parallel": {
                "num_threads": 12
            }
        }
        config = manager.load(config_dict=config_dict)
        assert config.name == "dict_config"
        assert config.parallel.num_threads == 12
    
    def test_legacy_format_detection(self):
        """Test legacy format detection."""
        manager = ConfigurationManager()
        
        # Legacy format with 'orcaflex' key
        legacy_config = {
            "orcaflex": {
                "analysis": {
                    "static": True
                }
            }
        }
        assert manager._is_legacy_format(legacy_config) is True
        
        # New format
        new_config = {
            "name": "new_format",
            "analysis": {}
        }
        assert manager._is_legacy_format(new_config) is False
    
    def test_legacy_conversion(self):
        """Test legacy configuration conversion."""
        manager = ConfigurationManager()
        
        legacy_config = {
            "meta": {
                "basename": "legacy_analysis",
                "version": "1.0.0",
                "description": "Legacy format test"
            },
            "file_management": {
                "input_directory": "/legacy/input",
                "output_directory": "/legacy/output",
                "input_files": {
                    "dat": ["legacy.dat"]
                },
                "overwrite": {
                    "output": True
                }
            },
            "orcaflex": {
                "analysis": {
                    "static": True,
                    "simulation": True,
                    "simulation_duration": 3600.0,
                    "convergence": {
                        "tolerance": 1e-7
                    }
                },
                "parallel": {
                    "enabled": True,
                    "num_threads": 24
                },
                "postprocess": {
                    "summary": {
                        "flag": True
                    },
                    "visualization": {
                        "flag": True
                    }
                }
            }
        }
        
        converted = manager._convert_legacy(legacy_config)
        
        assert converted["name"] == "legacy_analysis"
        assert converted["version"] == "1.0.0"
        assert converted["description"] == "Legacy format test"
        assert converted["file_management"]["input_directory"] == "/legacy/input"
        assert converted["file_management"]["overwrite_existing"] is True
        assert converted["analysis"]["static"]["enabled"] is True
        assert converted["analysis"]["static"]["tolerance"] == 1e-7
        assert converted["analysis"]["dynamic"]["enabled"] is True
        assert converted["analysis"]["dynamic"]["duration"] == 3600.0
        assert converted["parallel"]["num_threads"] == 24
        assert converted["post_process"]["extract_summary"] is True
        assert converted["post_process"]["visualization"] is True
    
    def test_load_legacy_configuration(self):
        """Test loading and converting legacy configuration."""
        manager = ConfigurationManager()
        
        legacy_dict = {
            "orcaflex": {
                "analysis": {
                    "static": True
                }
            },
            "meta": {
                "basename": "auto_converted"
            }
        }
        
        config = manager.load(config_dict=legacy_dict)
        assert config.name == "auto_converted"
        assert config.analysis.static.enabled is True
    
    def test_get_config_error(self):
        """Test error when getting config before loading."""
        manager = ConfigurationManager()
        manager._config = None  # Reset any loaded config
        
        with pytest.raises(ConfigurationError):
            manager.get_config()


class TestConfigurationValidation:
    """Test configuration validation edge cases."""
    
    def test_invalid_enum_value(self):
        """Test invalid enum values."""
        with pytest.raises(ValueError):
            OrcaFlexConfig(log_level="INVALID")
    
    def test_extra_fields_forbidden(self):
        """Test that extra fields are not allowed."""
        with pytest.raises(ValueError):
            OrcaFlexConfig(unknown_field="value")
    
    def test_numeric_constraints(self):
        """Test numeric field constraints."""
        # Test positive float constraint
        with pytest.raises(ValueError):
            DynamicAnalysisConfig(duration=-100.0)
        
        # Test tolerance constraint
        with pytest.raises(ValueError):
            StaticAnalysisConfig(tolerance=2.0)  # > 1
        
        # Test damping constraint
        with pytest.raises(ValueError):
            StaticAnalysisConfig(damping=1.5)  # > 1


class TestConfigurationExamples:
    """Test real-world configuration examples."""
    
    def test_mooring_analysis_config(self):
        """Test configuration for mooring analysis."""
        config = OrcaFlexConfig(
            name="mooring_analysis",
            description="Mooring tension iteration analysis",
            analysis=AnalysisConfig(
                analysis_type=[AnalysisType.STATIC],
                iteration=IterationConfig(
                    enabled=True,
                    target_parameter="MaxTension",
                    target_value=5000.0,
                    tolerance=0.01,
                    max_iterations=100
                )
            ),
            post_process=PostProcessConfig(
                extract_summary=True,
                statistics=["min", "max", "mean", "std", "p95"]
            )
        )
        
        assert config.analysis.iteration.enabled is True
        assert "p95" in config.post_process.statistics
    
    def test_fatigue_analysis_config(self):
        """Test configuration for fatigue analysis."""
        config = OrcaFlexConfig(
            name="fatigue_analysis",
            description="Fatigue damage calculation",
            analysis=AnalysisConfig(
                analysis_type=[AnalysisType.DYNAMIC, AnalysisType.FATIGUE],
                dynamic=DynamicAnalysisConfig(
                    enabled=True,
                    duration=10800.0,  # 3 hours
                    time_step=0.1,
                    log_interval=10.0
                )
            ),
            post_process=PostProcessConfig(
                extract_time_series=True,
                extract_range_graphs=True
            )
        )
        
        assert AnalysisType.FATIGUE in config.analysis.analysis_type
        assert config.post_process.extract_range_graphs is True
    
    def test_installation_analysis_config(self):
        """Test configuration for installation analysis."""
        config = OrcaFlexConfig(
            name="installation_analysis",
            description="Vessel installation sequence",
            analysis=AnalysisConfig(
                analysis_type=[AnalysisType.INSTALLATION],
                dynamic=DynamicAnalysisConfig(
                    enabled=True,
                    duration=7200.0,
                    solver_type=SolverType.EXPLICIT
                )
            ),
            file_management=FileManagementConfig(
                output_formats=[OutputFormat.SIM, OutputFormat.CSV],
                create_subdirectories=True
            )
        )
        
        assert AnalysisType.INSTALLATION in config.analysis.analysis_type
        assert config.analysis.dynamic.solver_type == SolverType.EXPLICIT


if __name__ == "__main__":
    pytest.main([__file__, "-v"])