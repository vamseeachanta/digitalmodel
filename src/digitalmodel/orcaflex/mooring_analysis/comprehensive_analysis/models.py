"""Data models for comprehensive mooring analysis."""

from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
import numpy as np
import pandas as pd


# =============================================================================
# Pretension Analysis Models
# =============================================================================

@dataclass
class PretensionLineData:
    """Data for a single mooring line pretension analysis."""
    object_name: str
    target_tension: float  # kN
    current_tension: float  # kN
    line_length: float  # m
    calculated_length: float  # m
    new_line_length: float  # m
    tension_diff_percent: float  # %
    converged: bool
    end_forces: Tuple[float, float, float]  # Gx, Gy, Gz in kN
    line_EA: Optional[float] = None  # kN
    iterations: Optional[int] = None


@dataclass
class PretensionData:
    """Complete pretension analysis data from CSV."""
    filename: Path
    timestamp: datetime
    lines: List[PretensionLineData]
    raw_dataframe: pd.DataFrame
    
    @property
    def num_lines(self) -> int:
        """Number of mooring lines."""
        return len(self.lines)
    
    @property
    def num_converged(self) -> int:
        """Number of converged lines."""
        return sum(1 for line in self.lines if line.converged)
    
    @property
    def convergence_rate(self) -> float:
        """Overall convergence rate as percentage."""
        if self.num_lines == 0:
            return 0.0
        return (self.num_converged / self.num_lines) * 100


@dataclass
class PretensionMetrics:
    """Calculated metrics for pretension analysis."""
    mean_convergence: float
    std_convergence: float
    max_deviation: float
    min_deviation: float
    converged_lines: List[str]
    problem_lines: List[str]
    total_lines: int
    tension_distribution: Dict[str, float]
    convergence_percentage: float
    average_tension: float
    tension_range: Tuple[float, float]
    recommendations: List[str] = field(default_factory=list)


@dataclass
class ConvergenceStatus:
    """Convergence status assessment."""
    overall_converged: bool
    convergence_quality: str  # 'excellent', 'good', 'acceptable', 'poor'
    critical_lines: List[str]
    adjustment_needed: Dict[str, float]  # line: adjustment in %
    estimated_iterations: Dict[str, int]
    confidence_level: float  # 0-100%


# =============================================================================
# Stiffness Analysis Models  
# =============================================================================

@dataclass
class StiffnessLineData:
    """Stiffness data for a single mooring line."""
    object_name: str
    k_axial: float  # kN/m
    k_x: float  # kN/m
    k_y: float  # kN/m
    k_z: float  # kN/m
    k_xy: float  # kN/m (cross-coupling)
    k_xz: float  # kN/m (cross-coupling)
    k_yz: float  # kN/m (cross-coupling)
    direction_cosines: Tuple[float, float, float]  # cos_x, cos_y, cos_z
    forces: Tuple[float, float, float]  # Fx, Fy, Fz in kN


@dataclass
class StiffnessData:
    """Complete stiffness analysis data from CSV."""
    filename: Path
    timestamp: datetime
    lines: List[StiffnessLineData]
    raw_dataframe: pd.DataFrame
    
    @property
    def num_lines(self) -> int:
        """Number of mooring lines."""
        return len(self.lines)


@dataclass
class StiffnessMatrix:
    """6-DOF stiffness matrix and properties."""
    matrix: np.ndarray  # 6x6 matrix
    translational: np.ndarray  # 3x3 sub-matrix
    rotational: Optional[np.ndarray] = None  # 3x3 sub-matrix if available
    coupling: Optional[np.ndarray] = None  # 3x3 coupling terms
    eigenvalues: Optional[np.ndarray] = None
    eigenvectors: Optional[np.ndarray] = None
    condition_number: Optional[float] = None
    
    @property
    def is_positive_definite(self) -> bool:
        """Check if matrix is positive definite."""
        if self.eigenvalues is not None:
            return np.all(self.eigenvalues > 0)
        return False
    
    @property
    def principal_stiffnesses(self) -> Optional[Tuple[float, float, float]]:
        """Get principal stiffnesses from eigenvalues."""
        if self.eigenvalues is not None:
            return tuple(self.eigenvalues[:3])
        return None


@dataclass
class StiffnessMetrics:
    """Calculated metrics for stiffness analysis."""
    stiffness_matrix: StiffnessMatrix
    dominant_direction: Tuple[float, float, float]  # unit vector
    stiffness_ratios: Dict[str, float]  # x/y, x/z, y/z ratios
    anisotropy_factor: float
    critical_lines: List[str]  # highest/lowest stiffness
    natural_periods: Optional[Dict[str, float]] = None  # seconds
    system_characteristics: Dict[str, Any] = field(default_factory=dict)
    compliance_matrix: Optional[np.ndarray] = None
    recommendations: List[str] = field(default_factory=list)


# =============================================================================
# Fender Forces Analysis Models
# =============================================================================

@dataclass
class FenderData:
    """Data for a single fender."""
    fender_id: str
    location: Tuple[float, float, float]  # x, y, z coordinates
    forces: List[float]  # Force time series or statistics
    max_force: float  # kN
    mean_force: float  # kN
    contact_events: int
    contact_duration: float  # seconds or percentage
    compression: Optional[float] = None  # m


@dataclass
class FenderForceData:
    """Complete fender force analysis data from CSV."""
    filename: Path
    timestamp: datetime
    fenders: List[FenderData]
    raw_dataframe: pd.DataFrame
    
    @property
    def num_fenders(self) -> int:
        """Number of fenders."""
        return len(self.fenders)
    
    @property
    def total_max_force(self) -> float:
        """Total maximum force across all fenders."""
        return sum(f.max_force for f in self.fenders)


@dataclass
class FenderMetrics:
    """Calculated metrics for fender force analysis."""
    utilization_rates: Dict[str, float]  # fender_id: utilization %
    max_forces: Dict[str, float]  # fender_id: max force
    mean_forces: Dict[str, float]  # fender_id: mean force
    critical_fenders: List[str]
    load_sharing: Dict[str, float]  # fender_id: percentage of total load
    contact_percentages: Dict[str, float]
    force_distribution_stats: Dict[str, float]
    overloaded_fenders: List[str]
    design_margin: Dict[str, float]  # fender_id: remaining capacity %
    recommendations: List[str] = field(default_factory=list)


# =============================================================================
# Group Comparison Models
# =============================================================================

@dataclass
class AnalysisResults:
    """Combined results from all three analysis types."""
    run_id: str
    filename: Path
    context: 'ContextInfo'
    pretension_metrics: Optional[PretensionMetrics] = None
    stiffness_metrics: Optional[StiffnessMetrics] = None
    fender_metrics: Optional[FenderMetrics] = None
    timestamp: datetime = field(default_factory=datetime.now)
    
    @property
    def has_all_analyses(self) -> bool:
        """Check if all three analyses are available."""
        return all([
            self.pretension_metrics is not None,
            self.stiffness_metrics is not None,
            self.fender_metrics is not None
        ])


@dataclass
class GroupStatistics:
    """Statistical metrics for a group of runs."""
    group_name: str
    num_runs: int
    metrics: Dict[str, Dict[str, float]]  # metric_name: {mean, std, min, max, median}
    best_run: str
    worst_run: str
    trends: List[str]
    outliers: List[str]


@dataclass 
class GroupComparison:
    """Comparison results between multiple groups."""
    groups: Dict[str, GroupStatistics]
    rankings: Dict[str, List[str]]  # metric: [ranked group names]
    cross_group_metrics: Dict[str, Any]
    sensitivity_analysis: Dict[str, float]
    recommendations: List[str]
    best_configuration: str
    performance_envelope: Dict[str, Tuple[float, float]]  # metric: (min, max)


# =============================================================================
# Context and Summary Models
# =============================================================================

@dataclass
class ContextInfo:
    """Extracted context from filename and content."""
    vessel_type: Optional[str] = None  # LNGC, FSRU, FPSO, etc.
    water_depth: Optional[float] = None  # meters
    environment: Optional[str] = None  # operational, survival, etc.
    loading_condition: Optional[str] = None  # ballast, loaded, etc.
    return_period: Optional[int] = None  # years
    analysis_type: Optional[str] = None  # static, dynamic
    mooring_configuration: Optional[str] = None
    custom_attributes: Dict[str, Any] = field(default_factory=dict)
    confidence_score: float = 0.0  # 0-1 confidence in extraction


@dataclass
class IndividualSummary:
    """Summary for a single analysis run."""
    run_id: str
    key_findings: List[str]
    metrics_dashboard: Dict[str, Any]
    pass_fail_status: Dict[str, bool]
    critical_issues: List[str]
    recommendations: List[str]
    overall_assessment: str
    compliance_status: Dict[str, bool]


@dataclass
class GroupSummary:
    """Summary for a group of related runs."""
    group_name: str
    num_runs: int
    statistical_summary: GroupStatistics
    comparative_table: pd.DataFrame
    trends_identified: List[str]
    best_practices: List[str]
    group_recommendations: List[str]
    performance_ranking: List[Tuple[str, float]]


@dataclass
class OverallSummary:
    """Overall summary across all groups and runs."""
    total_runs: int
    total_groups: int
    executive_summary: str
    system_conclusions: List[str]
    design_recommendations: List[str]
    risk_assessment: Dict[str, str]
    next_steps: List[str]
    compliance_summary: Dict[str, bool]
    performance_matrix: pd.DataFrame
    critical_actions: List[str]


@dataclass
class ComprehensiveResults:
    """Complete results from comprehensive analysis."""
    config: 'AnalysisConfig'
    individual_results: Dict[str, AnalysisResults]
    group_comparisons: Optional[GroupComparison] = None
    individual_summaries: Dict[str, IndividualSummary] = field(default_factory=dict)
    group_summaries: Dict[str, GroupSummary] = field(default_factory=dict)
    overall_summary: Optional[OverallSummary] = None
    processing_stats: Dict[str, Any] = field(default_factory=dict)
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    
    @property
    def success_rate(self) -> float:
        """Calculate overall success rate."""
        total = len(self.individual_results)
        if total == 0:
            return 0.0
        successful = sum(1 for r in self.individual_results.values() 
                        if r.has_all_analyses)
        return (successful / total) * 100