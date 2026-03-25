"""
Unified Configuration Management System for OrcaFlex Module

This module provides a comprehensive configuration system using Pydantic for
validation, type safety, and schema enforcement. It supports YAML loading,
legacy configuration migration, and extensible configuration schemas.
"""

from typing import Any, Dict, List, Optional, Union, Literal
from pathlib import Path
from enum import Enum
import yaml
from datetime import datetime
from pydantic import BaseModel, Field, validator, model_validator
from pydantic.types import PositiveInt, PositiveFloat, confloat, conint

from .exceptions import ConfigurationError, ValidationError
from .logging_config import LoggerMixin, OrcaFlexLogger


class AnalysisType(str, Enum):
    """Types of OrcaFlex analyses."""
    STATIC = "static"
    DYNAMIC = "dynamic"
    MODAL = "modal"
    FATIGUE = "fatigue"
    INSTALLATION = "installation"
    CUSTOM = "custom"


class SolverType(str, Enum):
    """Types of solvers."""
    IMPLICIT = "implicit"
    EXPLICIT = "explicit"
    ADAPTIVE = "adaptive"


class OutputFormat(str, Enum):
    """Output file formats."""
    SIM = "sim"
    DAT = "dat"
    CSV = "csv"
    EXCEL = "excel"
    JSON = "json"
    HDF5 = "hdf5"


class LogLevel(str, Enum):
    """Logging levels."""
    DEBUG = "DEBUG"
    INFO = "INFO"
    WARNING = "WARNING"
    ERROR = "ERROR"
    CRITICAL = "CRITICAL"


# ============================================================================
# Base Configuration Models
# ============================================================================

class BaseConfig(BaseModel):
    """Base configuration class with common functionality."""
    
    class Config:
        """Pydantic configuration."""
        use_enum_values = True
        validate_assignment = True
        extra = "forbid"  # Prevent unknown fields
        json_encoders = {
            datetime: lambda v: v.isoformat(),
            Path: lambda v: str(v)
        }
    
    def merge(self, other: 'BaseConfig') -> 'BaseConfig':
        """Merge with another configuration."""
        if not isinstance(other, self.__class__):
            raise ConfigurationError(
                f"Cannot merge {self.__class__.__name__} with {type(other).__name__}"
            )
        
        merged_data = self.dict()
        other_data = other.dict(exclude_unset=True)
        
        def deep_merge(base: dict, update: dict) -> dict:
            for key, value in update.items():
                if key in base and isinstance(base[key], dict) and isinstance(value, dict):
                    base[key] = deep_merge(base[key], value)
                else:
                    base[key] = value
            return base
        
        merged_data = deep_merge(merged_data, other_data)
        return self.__class__(**merged_data)


# ============================================================================
# File Management Configuration
# ============================================================================

class FileManagementConfig(BaseConfig):
    """Configuration for file management."""
    
    input_directory: Path = Field(
        default=Path("."),
        description="Directory containing input files"
    )
    output_directory: Path = Field(
        default=Path("./results"),
        description="Directory for output files"
    )
    input_files: Dict[str, List[str]] = Field(
        default_factory=dict,
        description="Input files organized by type"
    )
    output_formats: List[OutputFormat] = Field(
        default=[OutputFormat.SIM],
        description="Desired output formats"
    )
    overwrite_existing: bool = Field(
        default=False,
        description="Overwrite existing output files"
    )
    create_subdirectories: bool = Field(
        default=True,
        description="Create subdirectories for different analysis types"
    )
    
    @validator('input_directory', 'output_directory')
    def validate_directory(cls, v):
        """Ensure directory paths are valid."""
        if not isinstance(v, Path):
            v = Path(v)
        return v
    
    @validator('input_files')
    def validate_input_files(cls, v):
        """Validate input files structure."""
        # Ensure proper structure for input files
        if v and not isinstance(v, dict):
            raise ValidationError("input_files must be a dictionary")
        return v


# ============================================================================
# Analysis Configuration
# ============================================================================

class StaticAnalysisConfig(BaseConfig):
    """Configuration for static analysis."""
    
    enabled: bool = Field(default=True, description="Enable static analysis")
    tolerance: float = Field(
        default=1e-6,
        gt=0,
        le=1,
        description="Convergence tolerance"
    )
    max_iterations: PositiveInt = Field(
        default=100,
        description="Maximum iterations"
    )
    damping: confloat(ge=0, le=1) = Field(
        default=0.8,
        description="Damping factor"
    )
    use_calculated_positions: bool = Field(
        default=False,
        description="Use calculated positions from previous analysis"
    )


class DynamicAnalysisConfig(BaseConfig):
    """Configuration for dynamic analysis."""
    
    enabled: bool = Field(default=False, description="Enable dynamic analysis")
    duration: PositiveFloat = Field(
        default=3600.0,
        description="Simulation duration in seconds"
    )
    time_step: PositiveFloat = Field(
        default=0.1,
        description="Time step in seconds"
    )
    ramp_time: float = Field(
        default=0.0,
        ge=0,
        description="Ramp time in seconds"
    )
    solver_type: SolverType = Field(
        default=SolverType.IMPLICIT,
        description="Solver type"
    )
    log_interval: PositiveFloat = Field(
        default=1.0,
        description="Result logging interval in seconds"
    )


class IterationConfig(BaseConfig):
    """Configuration for iterative analysis."""
    
    enabled: bool = Field(default=False, description="Enable iteration")
    target_parameter: Optional[str] = Field(
        default=None,
        description="Parameter to iterate"
    )
    target_value: Optional[float] = Field(
        default=None,
        description="Target value for iteration"
    )
    tolerance: float = Field(
        default=0.01,
        gt=0,
        description="Iteration tolerance"
    )
    max_iterations: PositiveInt = Field(
        default=50,
        description="Maximum iterations"
    )
    step_size: float = Field(
        default=0.1,
        gt=0,
        description="Initial step size"
    )


class AnalysisConfig(BaseConfig):
    """Main analysis configuration."""
    
    analysis_type: List[AnalysisType] = Field(
        default=[AnalysisType.STATIC],
        description="Types of analysis to perform"
    )
    static: StaticAnalysisConfig = Field(
        default_factory=StaticAnalysisConfig,
        description="Static analysis configuration"
    )
    dynamic: DynamicAnalysisConfig = Field(
        default_factory=DynamicAnalysisConfig,
        description="Dynamic analysis configuration"
    )
    iteration: IterationConfig = Field(
        default_factory=IterationConfig,
        description="Iteration configuration"
    )
    save_intermediate: bool = Field(
        default=False,
        description="Save intermediate results"
    )
    
    @model_validator(mode='after')
    def validate_analysis_consistency(self):
        """Ensure analysis configuration is consistent."""
        if AnalysisType.STATIC in self.analysis_type and not self.static.enabled:
            raise ValidationError("Static analysis in types but not enabled")
        
        if AnalysisType.DYNAMIC in self.analysis_type and not self.dynamic.enabled:
            raise ValidationError("Dynamic analysis in types but not enabled")
        
        return self


# ============================================================================
# Parallel Processing Configuration
# ============================================================================

class ParallelConfig(BaseConfig):
    """Configuration for parallel processing."""
    
    enabled: bool = Field(
        default=True,
        description="Enable parallel processing"
    )
    num_threads: conint(ge=1, le=128) = Field(
        default=30,
        description="Number of parallel threads"
    )
    use_processes: bool = Field(
        default=True,
        description="Use processes instead of threads"
    )
    chunk_size: PositiveInt = Field(
        default=10,
        description="Chunk size for batch processing"
    )
    timeout: Optional[PositiveFloat] = Field(
        default=3600.0,
        description="Timeout per task in seconds"
    )


# ============================================================================
# Post-Processing Configuration
# ============================================================================

class PostProcessConfig(BaseConfig):
    """Configuration for post-processing."""
    
    enabled: bool = Field(default=True, description="Enable post-processing")
    extract_summary: bool = Field(
        default=True,
        description="Extract summary statistics"
    )
    extract_time_series: bool = Field(
        default=False,
        description="Extract time series data"
    )
    extract_range_graphs: bool = Field(
        default=False,
        description="Extract range graphs"
    )
    visualization: bool = Field(
        default=False,
        description="Generate visualizations"
    )
    statistics: List[str] = Field(
        default=["min", "max", "mean", "std"],
        description="Statistics to calculate"
    )


# ============================================================================
# Main Configuration
# ============================================================================

class OrcaFlexConfig(BaseConfig):
    """Main OrcaFlex configuration model."""
    
    # Metadata
    name: str = Field(
        default="orcaflex_analysis",
        description="Analysis name"
    )
    version: str = Field(
        default="2.0.0",
        description="Configuration version"
    )
    description: Optional[str] = Field(
        default=None,
        description="Analysis description"
    )
    created_at: datetime = Field(
        default_factory=datetime.now,
        description="Configuration creation time"
    )
    
    # Component configurations
    file_management: FileManagementConfig = Field(
        default_factory=FileManagementConfig,
        description="File management configuration"
    )
    analysis: AnalysisConfig = Field(
        default_factory=AnalysisConfig,
        description="Analysis configuration"
    )
    parallel: ParallelConfig = Field(
        default_factory=ParallelConfig,
        description="Parallel processing configuration"
    )
    post_process: PostProcessConfig = Field(
        default_factory=PostProcessConfig,
        description="Post-processing configuration"
    )
    
    # System settings
    log_level: LogLevel = Field(
        default=LogLevel.INFO,
        description="Logging level"
    )
    dry_run: bool = Field(
        default=False,
        description="Perform dry run without actual analysis"
    )
    validate_only: bool = Field(
        default=False,
        description="Only validate configuration without running"
    )
    
    @classmethod
    def from_yaml(cls, file_path: Union[str, Path]) -> 'OrcaFlexConfig':
        """Load configuration from YAML file."""
        file_path = Path(file_path)
        
        if not file_path.exists():
            raise ConfigurationError(
                f"Configuration file not found: {file_path}",
                suggestions=["Check file path", "Ensure file exists"]
            )
        
        try:
            with open(file_path, 'r') as f:
                data = yaml.safe_load(f)
            
            return cls(**data)
            
        except yaml.YAMLError as e:
            raise ConfigurationError(
                f"Invalid YAML in configuration file: {e}",
                suggestions=["Check YAML syntax", "Validate indentation"]
            )
        except Exception as e:
            raise ConfigurationError(
                f"Error loading configuration: {e}",
                context={"file": str(file_path)}
            )
    
    def to_yaml(self, file_path: Union[str, Path]) -> None:
        """Save configuration to YAML file."""
        file_path = Path(file_path)
        
        try:
            with open(file_path, 'w') as f:
                yaml.dump(
                    self.dict(exclude_unset=True),
                    f,
                    default_flow_style=False,
                    sort_keys=False
                )
        except Exception as e:
            raise ConfigurationError(
                f"Error saving configuration: {e}",
                context={"file": str(file_path)}
            )
    
    def validate_compatibility(self) -> List[str]:
        """Validate configuration compatibility."""
        warnings = []
        
        # Check for incompatible settings
        if self.analysis.dynamic.enabled and self.analysis.static.use_calculated_positions:
            warnings.append(
                "Dynamic analysis with calculated positions may not converge"
            )
        
        if self.parallel.enabled and self.parallel.num_threads > 50:
            warnings.append(
                f"High thread count ({self.parallel.num_threads}) may cause resource issues"
            )
        
        return warnings


# ============================================================================
# Configuration Manager
# ============================================================================

class ConfigurationManager(LoggerMixin):
    """Manages configuration loading, validation, and migration."""
    
    def __init__(self):
        """Initialize configuration manager."""
        self._config: Optional[OrcaFlexConfig] = None
        self._legacy_converters = {}
        self._register_legacy_converters()
    
    def load(self, 
             file_path: Optional[Union[str, Path]] = None,
             config_dict: Optional[Dict[str, Any]] = None) -> OrcaFlexConfig:
        """
        Load configuration from file or dictionary.
        
        Args:
            file_path: Path to configuration file
            config_dict: Configuration dictionary
            
        Returns:
            Loaded configuration
        """
        if file_path:
            self._config = OrcaFlexConfig.from_yaml(file_path)
            self.log_info(f"Loaded configuration from {file_path}")
            
        elif config_dict:
            # Check if it's a legacy format
            if self._is_legacy_format(config_dict):
                config_dict = self._convert_legacy(config_dict)
                self.log_info("Converted legacy configuration format")
            
            self._config = OrcaFlexConfig(**config_dict)
            self.log_info("Loaded configuration from dictionary")
            
        else:
            self._config = OrcaFlexConfig()
            self.log_info("Created default configuration")
        
        # Validate
        warnings = self._config.validate_compatibility()
        for warning in warnings:
            self.log_warning(warning)
        
        return self._config
    
    def get_config(self) -> OrcaFlexConfig:
        """Get current configuration."""
        if self._config is None:
            raise ConfigurationError("No configuration loaded")
        return self._config
    
    def _is_legacy_format(self, config: Dict[str, Any]) -> bool:
        """Check if configuration is in legacy format."""
        # Legacy format indicators
        legacy_keys = ['orcaflex', 'default', 'Analysis']
        return any(key in config for key in legacy_keys)
    
    def _convert_legacy(self, legacy_config: Dict[str, Any]) -> Dict[str, Any]:
        """Convert legacy configuration to new format."""
        converted = {
            'name': legacy_config.get('meta', {}).get('basename', 'orcaflex_analysis'),
            'version': legacy_config.get('meta', {}).get('version', '1.0.0'),
            'description': legacy_config.get('meta', {}).get('description'),
        }
        
        # Convert file management
        if 'file_management' in legacy_config:
            fm = legacy_config['file_management']
            converted['file_management'] = {
                'input_directory': fm.get('input_directory', '.'),
                'output_directory': fm.get('output_directory', './results'),
                'input_files': fm.get('input_files', {}),
                'overwrite_existing': fm.get('overwrite', {}).get('output', False)
            }
        
        # Convert analysis settings
        if 'orcaflex' in legacy_config:
            ofx = legacy_config['orcaflex']
            analysis = ofx.get('analysis', {})
            
            converted['analysis'] = {
                'static': {
                    'enabled': analysis.get('static', False),
                    'tolerance': analysis.get('convergence', {}).get('tolerance', 1e-6)
                },
                'dynamic': {
                    'enabled': analysis.get('simulation', False) or analysis.get('dynamic', False),
                    'duration': analysis.get('simulation_duration', 3600.0)
                }
            }
            
            # Convert parallel settings
            if 'parallel' in ofx:
                converted['parallel'] = ofx['parallel']
        
        # Convert post-processing
        if 'postprocess' in legacy_config.get('orcaflex', {}):
            pp = legacy_config['orcaflex']['postprocess']
            converted['post_process'] = {
                'enabled': True,
                'extract_summary': pp.get('summary', {}).get('flag', False),
                'visualization': pp.get('visualization', {}).get('flag', False)
            }
        
        return converted
    
    def _register_legacy_converters(self):
        """Register converters for different legacy formats."""
        # Register version-specific converters
        self._legacy_converters['1.0'] = self._convert_v1_to_v2
        self._legacy_converters['default'] = self._convert_legacy
    
    def _convert_v1_to_v2(self, legacy_config: Dict[str, Any]) -> Dict[str, Any]:
        """Convert version 1.0 configuration to version 2.0."""
        # This is a specialized converter for v1.0 format
        return self._convert_legacy(legacy_config)


# Global configuration manager instance
_config_manager = ConfigurationManager()

def get_config_manager() -> ConfigurationManager:
    """Get the global configuration manager."""
    return _config_manager