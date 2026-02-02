"""
ABOUTME: Data models for AQWA validation results and configuration
ABOUTME: Defines AQWA-specific validation data structures
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import List, Dict, Optional, Any


class AQWAFileType(Enum):
    """AQWA file types."""
    YAML_CONFIG = "yaml_config"
    DAT_INPUT = "dat_input"
    LIS_OUTPUT = "lis_output"
    MES_MESH = "mes_mesh"
    HYD_COEFFICIENTS = "hyd_coefficients"


class AQWAAnalysisType(Enum):
    """AQWA analysis types."""
    FREQUENCY_DOMAIN = "frequency_domain"
    TIME_DOMAIN = "time_domain"
    VISCOUS_DAMPING = "viscous_damping"
    RAO_CALCULATION = "rao_calculation"


@dataclass
class HydrodynamicRanges:
    """Expected ranges for hydrodynamic coefficients."""
    added_mass_min: float = 0.0
    added_mass_max: float = 1e8
    damping_min: float = 0.0
    damping_max: float = 1e7
    rao_amplitude_min: float = 0.0
    rao_amplitude_max: float = 10.0  # m/m or deg/m
    frequency_min: float = 0.1  # rad/s
    frequency_max: float = 3.0  # rad/s


@dataclass
class AQWAValidationConfig:
    """Configuration for AQWA validation."""
    tolerance_percent: float = 10.0
    enable_aqwa_software_check: bool = True
    hydrodynamic_data_dir: Path = Path("data/marine_engineering/hydrodynamic")
    rao_data_dir: Path = Path("data/marine_engineering/raos")
    expected_analysis_type: Optional[AQWAAnalysisType] = None
    hydrodynamic_ranges: HydrodynamicRanges = field(default_factory=HydrodynamicRanges)


@dataclass
class AQWAValidationResult:
    """AQWA-specific validation result."""
    file_path: Path
    file_type: AQWAFileType
    analysis_type: Optional[AQWAAnalysisType] = None
    timestamp: datetime = field(default_factory=datetime.now)

    # AQWA-specific checks
    dat_file_valid: bool = False
    lis_file_available: bool = False
    hydrodynamic_coefficients_valid: bool = False
    raos_valid: bool = False

    # Software check
    aqwa_available: bool = False
    aqwa_version: Optional[str] = None

    # Issues
    issues: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)

    validation_time_seconds: float = 0.0
    overall_status: str = "UNKNOWN"

    def add_issue(self, message: str, is_critical: bool = True):
        """Add validation issue."""
        if is_critical:
            self.issues.append(message)
        else:
            self.warnings.append(message)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for export."""
        return {
            'file_path': str(self.file_path),
            'file_type': self.file_type.value,
            'analysis_type': self.analysis_type.value if self.analysis_type else None,
            'timestamp': self.timestamp.isoformat(),
            'dat_file_valid': self.dat_file_valid,
            'lis_file_available': self.lis_file_available,
            'hydrodynamic_coefficients_valid': self.hydrodynamic_coefficients_valid,
            'raos_valid': self.raos_valid,
            'aqwa_available': self.aqwa_available,
            'aqwa_version': self.aqwa_version,
            'issues': self.issues,
            'warnings': self.warnings,
            'validation_time_seconds': self.validation_time_seconds,
            'overall_status': self.overall_status
        }
