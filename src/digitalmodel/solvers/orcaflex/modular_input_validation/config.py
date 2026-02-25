"""
ABOUTME: Configuration management for modular input validation
ABOUTME: Defines validation settings, paths, and tolerances
"""

from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional, Dict, Any


@dataclass
class ValidationConfig:
    """
    Configuration for modular input file validation.

    Attributes:
        tolerance_percent: Percentage tolerance for physical parameter validation (default: 10%)
        enable_orcaflex: Enable OrcaFlex API validation (Level 2)
        calm_buoy_data_dir: Root directory for CALM buoy reference data
        reports_dir: Directory for validation reports
        results_dir: Directory for detailed validation results
        run_all_levels: Run all validation levels by default
        skip_levels: List of validation levels to skip
        generate_reports: Auto-generate reports after validation
        report_formats: List of report formats to generate
    """

    # Physical validation
    tolerance_percent: float = 10.0

    # Software integration
    enable_orcaflex: bool = True

    # Data directories
    calm_buoy_data_dir: Path = field(default_factory=lambda: Path("data"))
    reports_dir: Path = field(default_factory=lambda: Path("reports/validation/calm_buoy"))
    results_dir: Path = field(default_factory=lambda: Path("results/validation/calm_buoy"))

    # Validation control
    run_all_levels: bool = True
    skip_levels: list = field(default_factory=list)

    # Reporting
    generate_reports: bool = True
    report_formats: list = field(default_factory=lambda: ["console", "csv", "markdown", "html"])

    # Logging
    log_level: str = "INFO"
    enable_color: bool = True

    def __post_init__(self):
        """Validate and normalize configuration."""
        # Ensure Path objects
        self.calm_buoy_data_dir = Path(self.calm_buoy_data_dir)
        self.reports_dir = Path(self.reports_dir)
        self.results_dir = Path(self.results_dir)

        # Validate tolerance
        if not 0 < self.tolerance_percent <= 100:
            raise ValueError(f"tolerance_percent must be between 0 and 100, got {self.tolerance_percent}")

        # Validate skip_levels
        if self.skip_levels:
            valid_levels = {1, 2, 3}
            invalid = set(self.skip_levels) - valid_levels
            if invalid:
                raise ValueError(f"Invalid skip_levels: {invalid}. Must be 1, 2, or 3.")

        # Validate report formats
        valid_formats = {"console", "csv", "markdown", "html"}
        invalid_formats = set(self.report_formats) - valid_formats
        if invalid_formats:
            raise ValueError(f"Invalid report_formats: {invalid_formats}")

    def should_run_level(self, level: int) -> bool:
        """
        Check if a validation level should run.

        Args:
            level: Validation level (1, 2, or 3)

        Returns:
            True if level should run
        """
        return level not in self.skip_levels

    def get_reference_data_paths(self) -> Dict[str, Path]:
        """
        Get paths to all reference data directories.

        Returns:
            Dictionary of data category to path
        """
        base = self.calm_buoy_data_dir

        return {
            'generic_range': base / 'raw' / 'calm_buoy' / 'generic_range',
            'mature_design': base / 'processed' / 'calm_buoy' / 'mature_design',
            'project_specific': base / 'results' / 'calm_buoy' / 'project_specific'
        }

    def ensure_output_directories(self) -> None:
        """Create output directories if they don't exist."""
        self.reports_dir.mkdir(parents=True, exist_ok=True)
        # results_dir is gitignored, only create if generating results
        if self.generate_reports:
            self.results_dir.mkdir(parents=True, exist_ok=True)

    @classmethod
    def from_dict(cls, config_dict: Dict[str, Any]) -> 'ValidationConfig':
        """
        Create configuration from dictionary.

        Args:
            config_dict: Configuration dictionary

        Returns:
            ValidationConfig instance
        """
        return cls(**config_dict)

    def to_dict(self) -> Dict[str, Any]:
        """
        Convert configuration to dictionary.

        Returns:
            Configuration as dictionary
        """
        return {
            'tolerance_percent': self.tolerance_percent,
            'enable_orcaflex': self.enable_orcaflex,
            'calm_buoy_data_dir': str(self.calm_buoy_data_dir),
            'reports_dir': str(self.reports_dir),
            'results_dir': str(self.results_dir),
            'run_all_levels': self.run_all_levels,
            'skip_levels': self.skip_levels,
            'generate_reports': self.generate_reports,
            'report_formats': self.report_formats,
            'log_level': self.log_level,
            'enable_color': self.enable_color
        }
