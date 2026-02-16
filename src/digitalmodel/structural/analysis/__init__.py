# ABOUTME: Public API for pipeline wall thickness design analysis
# ABOUTME: Re-exports key symbols from wall_thickness, parametric, phases, codes, lookup, and report modules

from .wall_thickness import (
    CodeEdition,
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
from .wall_thickness_codes import (
    CODE_REGISTRY,
    CodeStrategy,
    register_code,
)
from .wall_thickness_interactive_report import (
    InteractiveReportBuilder,
)
from .wall_thickness_lookup import (
    LookupConfig,
    MTLookupGenerator,
)
from .wall_thickness_parametric import (
    API_5L_GRADES,
    ParametricSweep,
    SweepConfig,
    generate_report,
    plot_utilisation_vs_wall_thickness,
)
from .wall_thickness_comparison import (
    CodeComparisonResult,
    compare_codes,
    generate_comparison_report,
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
    "CodeEdition",
    "DesignCode",
    "DesignFactors",
    "DesignLoads",
    "FabricationType",
    "PipeGeometry",
    "PipeMaterial",
    "SafetyClass",
    "WallThicknessAnalyzer",
    "WallThicknessResult",
    "CODE_REGISTRY",
    "CodeStrategy",
    "register_code",
    "InteractiveReportBuilder",
    "LookupConfig",
    "MTLookupGenerator",
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
    "CodeComparisonResult",
    "compare_codes",
    "generate_comparison_report",
]
