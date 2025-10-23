"""
ABOUTME: Data models for validation results and configuration
ABOUTME: Defines enums, dataclasses, and types for the validation system
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import List, Dict, Optional, Any


class ValidationLevel(Enum):
    """Validation levels in order of complexity."""
    LEVEL_1_YAML = "level_1_yaml_syntax"
    LEVEL_2_ORCAFLEX = "level_2_orcaflex_api"
    LEVEL_3_PHYSICAL = "level_3_physical_consistency"


class ValidationStatus(Enum):
    """Overall validation status."""
    PASS = "pass"
    WARN = "warn"
    FAIL = "fail"
    UNKNOWN = "unknown"
    SKIPPED = "skipped"


class Severity(Enum):
    """Issue severity levels."""
    INFO = "info"
    WARNING = "warning"
    CRITICAL = "critical"


class OrcaFlexAvailability(Enum):
    """OrcaFlex software availability status."""
    YES = "yes"
    NO = "no"
    UNKNOWN = "unknown"


@dataclass
class ValidationIssue:
    """Individual validation issue or warning."""
    level: ValidationLevel
    severity: Severity
    message: str
    file: Optional[str] = None
    parameter: Optional[str] = None
    expected_value: Optional[Any] = None
    actual_value: Optional[Any] = None
    difference_percent: Optional[float] = None
    suggestion: Optional[str] = None
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for JSON/CSV export."""
        return {
            'level': self.level.value,
            'severity': self.severity.value,
            'message': self.message,
            'file': self.file,
            'parameter': self.parameter,
            'expected_value': self.expected_value,
            'actual_value': self.actual_value,
            'difference_percent': self.difference_percent,
            'suggestion': self.suggestion,
            'timestamp': self.timestamp.isoformat()
        }


@dataclass
class Level1Result:
    """Level 1 YAML syntax validation results."""
    yaml_valid: bool
    file_exists: bool
    includes_resolved: bool
    missing_includes: List[str] = field(default_factory=list)
    syntax_errors: List[str] = field(default_factory=list)
    sections: List[str] = field(default_factory=list)
    total_modules: int = 0
    status: ValidationStatus = ValidationStatus.UNKNOWN


@dataclass
class Level2Result:
    """Level 2 OrcaFlex API validation results."""
    orcaflex_available: OrcaFlexAvailability
    orcaflex_version: Optional[str] = None
    orcaflex_loadable: bool = False
    static_converged: bool = False
    load_errors: List[str] = field(default_factory=list)
    load_warnings: List[str] = field(default_factory=list)
    static_errors: List[str] = field(default_factory=list)
    static_warnings: List[str] = field(default_factory=list)
    computation_time_seconds: Optional[float] = None
    status: ValidationStatus = ValidationStatus.UNKNOWN
    availability_comment: str = ""


@dataclass
class PhysicalConsistencyCheck:
    """Physical consistency validation for a single parameter."""
    parameter: str
    actual_value: Any
    expected_min: Optional[float] = None
    expected_max: Optional[float] = None
    project_specific_value: Optional[float] = None
    tolerance_percent: float = 10.0
    difference_percent: Optional[float] = None
    within_range: bool = True
    matches_project: bool = True
    severity: Severity = Severity.INFO
    notes: str = ""


@dataclass
class Level3Result:
    """Level 3 physical consistency validation results."""
    checks_performed: int = 0
    checks_passed: int = 0
    warnings: int = 0
    failures: int = 0
    consistency_checks: List[PhysicalConsistencyCheck] = field(default_factory=list)
    status: ValidationStatus = ValidationStatus.UNKNOWN


@dataclass
class ValidationResult:
    """Complete validation results for a single file."""
    file_path: Path
    timestamp: datetime = field(default_factory=datetime.now)

    # Level results
    level_1: Optional[Level1Result] = None
    level_2: Optional[Level2Result] = None
    level_3: Optional[Level3Result] = None

    # Overall status
    overall_status: ValidationStatus = ValidationStatus.UNKNOWN

    # All issues collected
    issues: List[ValidationIssue] = field(default_factory=list)

    # Performance metrics
    validation_time_seconds: float = 0.0

    def add_issue(self,
                  level: ValidationLevel,
                  severity: Severity,
                  message: str,
                  **kwargs) -> None:
        """Add a validation issue."""
        issue = ValidationIssue(
            level=level,
            severity=severity,
            message=message,
            file=str(self.file_path),
            **kwargs
        )
        self.issues.append(issue)

    def get_issues_by_severity(self, severity: Severity) -> List[ValidationIssue]:
        """Get all issues of a specific severity."""
        return [issue for issue in self.issues if issue.severity == severity]

    def get_issues_by_level(self, level: ValidationLevel) -> List[ValidationIssue]:
        """Get all issues from a specific validation level."""
        return [issue for issue in self.issues if issue.level == level]

    @property
    def has_critical_issues(self) -> bool:
        """Check if there are any critical issues."""
        return any(issue.severity == Severity.CRITICAL for issue in self.issues)

    @property
    def has_warnings(self) -> bool:
        """Check if there are any warnings."""
        return any(issue.severity == Severity.WARNING for issue in self.issues)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for export."""
        return {
            'file_path': str(self.file_path),
            'timestamp': self.timestamp.isoformat(),
            'overall_status': self.overall_status.value,
            'validation_time_seconds': self.validation_time_seconds,
            'level_1': {
                'yaml_valid': self.level_1.yaml_valid if self.level_1 else None,
                'includes_resolved': self.level_1.includes_resolved if self.level_1 else None,
                'total_modules': self.level_1.total_modules if self.level_1 else 0,
                'status': self.level_1.status.value if self.level_1 else 'unknown'
            },
            'level_2': {
                'orcaflex_available': self.level_2.orcaflex_available.value if self.level_2 else 'unknown',
                'orcaflex_version': self.level_2.orcaflex_version if self.level_2 else None,
                'orcaflex_loadable': self.level_2.orcaflex_loadable if self.level_2 else False,
                'static_converged': self.level_2.static_converged if self.level_2 else False,
                'status': self.level_2.status.value if self.level_2 else 'unknown'
            },
            'level_3': {
                'checks_performed': self.level_3.checks_performed if self.level_3 else 0,
                'checks_passed': self.level_3.checks_passed if self.level_3 else 0,
                'warnings': self.level_3.warnings if self.level_3 else 0,
                'status': self.level_3.status.value if self.level_3 else 'unknown'
            },
            'issues': [issue.to_dict() for issue in self.issues],
            'summary': {
                'total_issues': len(self.issues),
                'critical': len(self.get_issues_by_severity(Severity.CRITICAL)),
                'warnings': len(self.get_issues_by_severity(Severity.WARNING)),
                'info': len(self.get_issues_by_severity(Severity.INFO))
            }
        }
