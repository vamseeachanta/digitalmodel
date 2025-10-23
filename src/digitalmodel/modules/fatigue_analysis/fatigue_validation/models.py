"""
ABOUTME: Data models for Fatigue validation results and configuration
ABOUTME: Defines fatigue-specific validation data structures including S-N curves
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import List, Dict, Optional, Any


class SNStandard(Enum):
    """S-N curve standards."""
    DNV = "dnv"
    API = "api"
    BS = "bs"
    ASME = "asme"
    AWS = "aws"
    CUSTOM = "custom"


class FatigueEnvironment(Enum):
    """Fatigue environment."""
    IN_AIR = "in_air"
    SEAWATER_CP = "seawater_cathodic_protection"
    SEAWATER_FC = "seawater_free_corrosion"


class JointType(Enum):
    """Welded joint types."""
    PLATED = "plated_welded_joint"
    TUBULAR = "tubular_welded_joint"
    BOLTED = "bolted_connection"


@dataclass
class SNRanges:
    """Expected ranges for S-N curve parameters."""
    log_a_min: float = 10.0
    log_a_max: float = 18.0
    m_slope_min: float = 2.5
    m_slope_max: float = 6.0
    endurance_cycles_min: float = 1e6
    endurance_cycles_max: float = 1e8
    cafl_stress_min: float = 10.0  # MPa
    cafl_stress_max: float = 200.0  # MPa
    scf_min: float = 1.0
    scf_max: float = 20.0
    dff_min: float = 1.0
    dff_max: float = 10.0


@dataclass
class FatigueValidationConfig:
    """Configuration for fatigue validation."""
    tolerance_percent: float = 10.0
    fatigue_curves_file: Path = Path("data/fatigue/fatigue_curves_structured.csv")
    fatigue_references_file: Path = Path("data/fatigue/fatigue_curves_references.csv")
    expected_standard: Optional[SNStandard] = None
    expected_environment: Optional[FatigueEnvironment] = None
    sn_ranges: SNRanges = field(default_factory=SNRanges)
    require_thickness_correction: bool = True


@dataclass
class SNValidationCheck:
    """Validation check for a single S-N curve."""
    curve_id: str
    standard: SNStandard
    environment: FatigueEnvironment
    joint_type: JointType

    # S-N parameters
    log_a: float
    m_slope: float
    cafl_stress: float
    endurance_limit_cycles: float

    # Validation results
    within_ranges: bool = True
    matches_reference: bool = True
    thickness_correction_valid: bool = True

    issues: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)


@dataclass
class FatigueValidationResult:
    """Fatigue-specific validation result."""
    file_path: Path
    timestamp: datetime = field(default_factory=datetime.now)

    # Fatigue-specific checks
    sn_curves_valid: bool = False
    scf_within_range: bool = False
    dff_within_range: bool = False
    thickness_correction_valid: bool = False

    # S-N curve checks
    sn_checks: List[SNValidationCheck] = field(default_factory=list)

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
            'timestamp': self.timestamp.isoformat(),
            'sn_curves_valid': self.sn_curves_valid,
            'scf_within_range': self.scf_within_range,
            'dff_within_range': self.dff_within_range,
            'thickness_correction_valid': self.thickness_correction_valid,
            'sn_checks_count': len(self.sn_checks),
            'issues': self.issues,
            'warnings': self.warnings,
            'validation_time_seconds': self.validation_time_seconds,
            'overall_status': self.overall_status
        }
