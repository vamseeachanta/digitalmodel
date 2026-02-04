# ABOUTME: Public API for pipeline wall thickness design analysis
# ABOUTME: Re-exports key symbols from wall_thickness, parametric, and phases modules

from .wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    FabricationType,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
    WallThicknessResult,
)
from .wall_thickness_parametric import (
    API_5L_GRADES,
    ParametricSweep,
    SweepConfig,
    generate_report,
    plot_utilisation_vs_wall_thickness,
)
from .wall_thickness_phases import (
    PhaseAnalysisRunner,
    PhaseComparisonResult,
    PipeDefinition,
    PipelinePhase,
    create_standard_phases,
    generate_phase_report,
)

__all__ = [
    "DesignCode",
    "DesignFactors",
    "DesignLoads",
    "FabricationType",
    "PipeGeometry",
    "PipeMaterial",
    "SafetyClass",
    "WallThicknessAnalyzer",
    "WallThicknessResult",
    "API_5L_GRADES",
    "ParametricSweep",
    "SweepConfig",
    "generate_report",
    "plot_utilisation_vs_wall_thickness",
    "PhaseAnalysisRunner",
    "PhaseComparisonResult",
    "PipeDefinition",
    "PipelinePhase",
    "create_standard_phases",
    "generate_phase_report",
]
