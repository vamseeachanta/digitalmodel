# ABOUTME: ANSYS FEA domain — APDL parsing, generation, analysis setup, post-processing
# ABOUTME: No ANSYS software required; pure Python for AI-agent FEA workflows

"""
ANSYS FEA Domain
================

Parsing:
    APDLReader              — parse .inp materials and sections
    DesignPointReader       — parse DesignPointLog.csv parametric studies
    WBJNReader              — parse .wbjn journal metadata
    ResultsExtractor        — parse ANSYS output files for results

Generation:
    APDLGenerator           — generate APDL command scripts from config
    BatchRunner             — batch parametric run configuration
    PressureVesselGenerator — ASME VIII pressure vessel FEA setup
    BoltAnalysisGenerator   — bolted connection analysis
    ContactAnalysisGenerator— contact element setup
    ThermalAnalysisGenerator— thermal analysis setup

Assessment:
    WeldAssessmentGenerator — weld joint hotspot/linearized stress
    FatiguePostprocessor    — fatigue damage evaluation (S-N, Miner)
    ASMEThicknessCalc       — ASME VIII Div 1 thickness calculations

Reporting:
    ReportGenerator         — auto-generate FEA report sections (markdown)

Data Models:
    APDLMaterial, APDLSection, DesignPoint, ParametricStudy, WBJNJournal
    MaterialConfig, APDLScriptConfig, BatchConfig, SNcurve, FatigueConfig, etc.
"""

from digitalmodel.ansys.apdl_reader import APDLReader
from digitalmodel.ansys.design_points import DesignPointReader
from digitalmodel.ansys.wbjn_reader import WBJNReader
from digitalmodel.ansys.models import (
    APDLMaterial,
    APDLSection,
    DesignPoint,
    ParametricStudy,
    WBJNJournal,
)

# --- New modules ---
from digitalmodel.ansys.apdl_generator import (
    APDLGenerator,
    APDLScriptConfig,
    MaterialConfig,
    ElementTypeConfig,
    MeshConfig,
    BoundaryCondition,
    SolutionConfig,
    PostProcessConfig,
)
from digitalmodel.ansys.batch_runner import (
    BatchRunner,
    BatchConfig,
    ParameterSweep,
    BatchRun,
    LoadCase,
    BatchLoadConfig,
    BatchResultEntry,
    BatchResults,
    RunStatus,
)
from digitalmodel.ansys.results_extractor import (
    ResultsExtractor,
    ResultSummary,
    NodalResult,
    ElementResult,
    ConvergenceRecord,
    StressSummary,
    DisplacementSummary,
    ComparisonRow,
)
from digitalmodel.ansys.pressure_vessel import (
    PressureVesselGenerator,
    ASMEThicknessCalc,
    VesselGeometry,
    DesignConditions,
    NozzleConfig,
    NozzleLoad,
    WindSeismicLoad,
)
from digitalmodel.ansys.bolt_analysis import (
    BoltAnalysisGenerator,
    BoltConfig,
    BoltCircleConfig,
    FlangeConfig,
    GasketConfig,
)
from digitalmodel.ansys.weld_assessment import (
    WeldAssessmentGenerator,
    WeldGeometry,
    HotspotConfig,
    StressClassificationLine,
    LinearizedStress,
    ASMEStressLimits,
)
from digitalmodel.ansys.fatigue_postprocessor import (
    FatiguePostprocessor,
    FatigueConfig,
    FatigueLoadCase,
    FatigueResult,
    SNcurve,
)
from digitalmodel.ansys.contact_analysis import (
    ContactAnalysisGenerator,
    ContactPairConfig,
    InterferenceFitConfig,
    SealConfig,
)
from digitalmodel.ansys.thermal_analysis import (
    ThermalAnalysisGenerator,
    ThermalAnalysisConfig,
    ThermalMaterialConfig,
    ConvectionBC,
    RadiationBC,
    TransientConfig,
)
from digitalmodel.ansys.report_generator import (
    ReportGenerator,
    ReportConfig,
    ModelInfo,
    MeshMetrics,
    BoundaryConditionSummary,
    ResultEntry,
    ConvergenceInfo,
)

__all__ = [
    # Original
    "APDLReader",
    "DesignPointReader",
    "WBJNReader",
    "APDLMaterial",
    "APDLSection",
    "DesignPoint",
    "ParametricStudy",
    "WBJNJournal",
    # APDL Generator
    "APDLGenerator",
    "APDLScriptConfig",
    "MaterialConfig",
    "ElementTypeConfig",
    "MeshConfig",
    "BoundaryCondition",
    "SolutionConfig",
    "PostProcessConfig",
    # Batch Runner
    "BatchRunner",
    "BatchConfig",
    "ParameterSweep",
    "BatchRun",
    "LoadCase",
    "BatchLoadConfig",
    "BatchResultEntry",
    "BatchResults",
    "RunStatus",
    # Results Extractor
    "ResultsExtractor",
    "ResultSummary",
    "NodalResult",
    "ElementResult",
    "ConvergenceRecord",
    "StressSummary",
    "DisplacementSummary",
    "ComparisonRow",
    # Pressure Vessel
    "PressureVesselGenerator",
    "ASMEThicknessCalc",
    "VesselGeometry",
    "DesignConditions",
    "NozzleConfig",
    "NozzleLoad",
    "WindSeismicLoad",
    # Bolt Analysis
    "BoltAnalysisGenerator",
    "BoltConfig",
    "BoltCircleConfig",
    "FlangeConfig",
    "GasketConfig",
    # Weld Assessment
    "WeldAssessmentGenerator",
    "WeldGeometry",
    "HotspotConfig",
    "StressClassificationLine",
    "LinearizedStress",
    "ASMEStressLimits",
    # Fatigue
    "FatiguePostprocessor",
    "FatigueConfig",
    "FatigueLoadCase",
    "FatigueResult",
    "SNcurve",
    # Contact Analysis
    "ContactAnalysisGenerator",
    "ContactPairConfig",
    "InterferenceFitConfig",
    "SealConfig",
    # Thermal Analysis
    "ThermalAnalysisGenerator",
    "ThermalAnalysisConfig",
    "ThermalMaterialConfig",
    "ConvectionBC",
    "RadiationBC",
    "TransientConfig",
    # Report Generator
    "ReportGenerator",
    "ReportConfig",
    "ModelInfo",
    "MeshMetrics",
    "BoundaryConditionSummary",
    "ResultEntry",
    "ConvergenceInfo",
]
