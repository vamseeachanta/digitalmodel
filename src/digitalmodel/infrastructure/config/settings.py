"""
ABOUTME: Global configuration settings using Pydantic BaseSettings
ABOUTME: Supports environment variables, .env files, and runtime overrides
"""

from pathlib import Path
from typing import Optional, Literal
from pydantic_settings import BaseSettings, SettingsConfigDict
from pydantic import Field, field_validator, AliasChoices
import os


class GlobalSettings(BaseSettings):
    """
    Global digitalmodel configuration.

    All settings can be overridden via environment variables with DM_ prefix.
    Example: DM_DATA_DIR=/custom/path
    """

    # ============================================================================
    # Paths
    # ============================================================================

    data_dir: Path = Field(
        default=Path("./data"),
        env="DM_DATA_DIR",
        description="Root directory for data files"
    )

    output_dir: Path = Field(
        default=Path("./reports"),
        env="DM_OUTPUT_DIR",
        description="Root directory for generated reports"
    )

    log_dir: Path = Field(
        default=Path("./logs"),
        env="DM_LOG_DIR",
        description="Root directory for log files"
    )

    # ============================================================================
    # Engineering Parameters
    # ============================================================================

    safety_factor: float = Field(
        default=1.5,
        ge=1.0,
        le=5.0,
        env="DM_SAFETY_FACTOR",
        description="Global safety factor for engineering calculations"
    )

    material_database: Optional[Path] = Field(
        default=None,
        env="DM_MATERIAL_DATABASE",
        description="Path to custom material property database"
    )

    sn_curve_database: Optional[Path] = Field(
        default=None,
        env="DM_SN_CURVE_DATABASE",
        description="Path to custom S-N curve database"
    )

    # ============================================================================
    # Analysis Settings
    # ============================================================================

    default_analysis_mode: Literal["deterministic", "probabilistic"] = Field(
        default="deterministic",
        env="DM_ANALYSIS_MODE",
        description="Default analysis mode for calculations"
    )

    max_iterations: int = Field(
        default=1000,
        ge=10,
        le=100000,
        env="DM_MAX_ITERATIONS",
        description="Maximum iterations for iterative solvers"
    )

    convergence_tolerance: float = Field(
        default=1e-6,
        gt=0.0,
        env="DM_CONVERGENCE_TOLERANCE",
        description="Convergence tolerance for iterative solvers"
    )

    # ============================================================================
    # OrcaFlex Settings
    # ============================================================================

    orcaflex_workers: int = Field(
        default=30,
        ge=1,
        le=100,
        env="DM_ORCAFLEX_WORKERS",
        description="Number of parallel workers for OrcaFlex analysis"
    )

    orcaflex_license_timeout: int = Field(
        default=300,
        ge=10,
        le=3600,
        env="DM_ORCAFLEX_LICENSE_TIMEOUT",
        description="Timeout in seconds for OrcaFlex license acquisition"
    )

    # ============================================================================
    # Reporting Settings
    # ============================================================================

    report_format: Literal["html", "json", "csv", "all"] = Field(
        default="html",
        env="DM_REPORT_FORMAT",
        description="Default output format for reports"
    )

    interactive_plots: bool = Field(
        default=True,
        env="DM_INTERACTIVE_PLOTS",
        description="Use interactive plots (Plotly) instead of static images"
    )

    plot_theme: Literal["plotly", "plotly_white", "plotly_dark", "ggplot2", "seaborn"] = Field(
        default="plotly_white",
        env="DM_PLOT_THEME",
        description="Default theme for Plotly visualizations"
    )

    # ============================================================================
    # Performance Settings
    # ============================================================================

    enable_caching: bool = Field(
        default=True,
        env="DM_ENABLE_CACHING",
        description="Enable result caching for expensive calculations"
    )

    cache_dir: Path = Field(
        default=Path("./cache"),
        env="DM_CACHE_DIR",
        description="Directory for cached results"
    )

    max_memory_mb: int = Field(
        default=4096,
        ge=512,
        le=65536,
        env="DM_MAX_MEMORY_MB",
        description="Maximum memory usage in megabytes"
    )

    # ============================================================================
    # Logging Settings
    # ============================================================================

    log_level: Literal["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"] = Field(
        default="INFO",
        env="DM_LOG_LEVEL",
        description="Logging level"
    )

    log_format: Literal["standard", "detailed", "json"] = Field(
        default="standard",
        env="DM_LOG_FORMAT",
        description="Log message format"
    )

    # ============================================================================
    # Testing/Development Settings
    # ============================================================================

    environment: Literal["development", "testing", "production"] = Field(
        default="production",
        env="DM_ENVIRONMENT",
        description="Runtime environment"
    )

    debug_mode: bool = Field(
        default=False,
        env="DM_DEBUG",
        description="Enable debug mode with verbose output"
    )

    # ============================================================================
    # Validators
    # ============================================================================

    @field_validator('data_dir', 'output_dir', 'log_dir', 'cache_dir', mode='before')
    @classmethod
    def expand_path(cls, v):
        """Expand path to absolute and resolve"""
        if v is None:
            return v
        path = Path(v).expanduser().resolve()
        return path

    @field_validator('material_database', 'sn_curve_database', mode='before')
    @classmethod
    def validate_optional_path(cls, v):
        """Validate optional paths exist if specified"""
        if v is None:
            return v
        path = Path(v).expanduser().resolve()
        if not path.exists():
            raise ValueError(f"Database file not found: {path}")
        return path

    @field_validator('debug_mode', 'enable_caching', 'interactive_plots', mode='before')
    @classmethod
    def parse_boolean(cls, v):
        """Parse boolean from environment variable strings"""
        if isinstance(v, bool):
            return v
        if isinstance(v, str):
            return v.lower() in ('true', '1', 'yes', 'on')
        return bool(v)

    # ============================================================================
    # Configuration
    # ============================================================================

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        case_sensitive=False,
        env_prefix="DM_",
        json_schema_extra= {
            "example": {
                "data_dir": "./data",
                "output_dir": "./reports",
                "safety_factor": 1.5,
                "orcaflex_workers": 30,
                "report_format": "html",
                "log_level": "INFO",
                "environment": "production"
            }
        }
    )


# ============================================================================
# Global Settings Instance
# ============================================================================

_global_settings: Optional[GlobalSettings] = None


def get_settings() -> GlobalSettings:
    """
    Get global settings instance (singleton pattern).

    Returns:
        GlobalSettings: The global configuration instance

    Example:
        >>> settings = get_settings()
        >>> print(settings.safety_factor)
        1.5
    """
    global _global_settings

    if _global_settings is None:
        _global_settings = GlobalSettings()

        # Create directories if they don't exist
        for dir_path in [
            _global_settings.data_dir,
            _global_settings.output_dir,
            _global_settings.log_dir,
            _global_settings.cache_dir
        ]:
            dir_path.mkdir(parents=True, exist_ok=True)

    return _global_settings


def override_settings(**kwargs) -> GlobalSettings:
    """
    Override global settings at runtime.

    Args:
        **kwargs: Settings to override

    Returns:
        GlobalSettings: Updated global configuration

    Example:
        >>> settings = override_settings(safety_factor=2.0, debug_mode=True)
        >>> print(settings.safety_factor)
        2.0

    Warning:
        This creates a NEW settings instance. Use sparingly and only when
        environment variables or .env file configuration is not suitable.
    """
    global _global_settings

    # Create new settings with overrides
    _global_settings = GlobalSettings(**kwargs)

    # Create directories
    for dir_path in [
        _global_settings.data_dir,
        _global_settings.output_dir,
        _global_settings.log_dir,
        _global_settings.cache_dir
    ]:
        dir_path.mkdir(parents=True, exist_ok=True)

    return _global_settings


def reset_settings() -> None:
    """
    Reset settings to default (for testing purposes).

    Example:
        >>> reset_settings()
        >>> settings = get_settings()  # Fresh instance with defaults
    """
    global _global_settings
    _global_settings = None
