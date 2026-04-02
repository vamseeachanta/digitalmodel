"""OrcaFlex time-domain analysis package.

Includes reporting (HTML report generation from .sim files via OrcFxAPI)
and standalone analysis/design utilities that do NOT require OrcFxAPI:

- model_builder: Build OrcaFlex-compatible model configurations as dicts
- mooring_design: Mooring system preliminary design (catenary equations, sizing)
- riser_config: Riser configuration utilities (SCR, lazy-wave, TTR)
- pipelay_analysis: Pipelay pre-processing (S-lay, J-lay, operability)
- installation_analysis: Subsea installation utilities (DAF, sling, splash zone)
- weather_window: Weather window & operability analysis
- batch_parametric: Parametric study framework
- code_check_engine: Automated code checks (API RP 2RD, DNV-OS-F201, API RP 2SK)
- viv_screening: VIV screening per DNV-RP-C205
- postprocessor: Time series statistics, extreme value analysis
- environment: Environmental data handling (current, wave, wind profiles)
"""

from .reporting import generate_orcaflex_report, ReportConfig
from .qa import run_orcaflex_qa

# Analysis & design modules (no OrcFxAPI dependency)
from .environment import (
    CurrentProfile,
    CurrentProfileType,
    WaveSpectrumParams,
    WaveSpectrumType,
    WindProfile,
    EnvironmentalMatrix,
    EnvironmentalLoadCase,
)
from .model_builder import (
    VesselConfig,
    LineConfig,
    Buoy6DConfig,
    LineSectionProperties,
    LineSection,
    MATERIAL_LIBRARY,
    build_scr_model,
    build_lazy_wave_model,
    build_mooring_line,
)
from .mooring_design import (
    solve_catenary,
    CatenaryResult,
    MooringLineDesign,
    SpreadMooringConfig,
    TurretMooringConfig,
    MOORING_MATERIAL_LIBRARY,
    estimate_line_length,
    calculate_pretension,
)
from .riser_config import (
    RiserPipeProperties,
    SCRDesignInput,
    LazyWaveDesignInput,
    TTRDesignInput,
    calculate_weight_in_water,
    estimate_scr_hang_off_angle,
)
from .pipelay_analysis import (
    SLayConfig,
    JLayConfig,
    LayOperability,
    PipelayPipeProperties,
    calculate_stinger_radius,
)
from .installation_analysis import (
    VesselRAO,
    DAFInput,
    SlingConfig,
    SplashZoneInput,
    WeightManagement,
    WeightItem,
)
from .weather_window import (
    ScatterDiagram,
    OperabilityInput,
    OperabilityTable,
    analyse_persistence,
    PersistenceResult,
    SeasonalOperability,
    WoWEstimate,
)
from .batch_parametric import (
    ParameterSweep,
    ParametricStudy,
    CaseResult,
    ParametricResultsSummary,
)
from .code_check_engine import (
    check_api_rp_2rd,
    check_dnv_os_f201,
    check_mooring_api_2sk,
    APIRP2RDInput,
    DNVOSF201Input,
    APIRP2SKInput,
    UtilisationEnvelope,
    MooringCodeCheckResult,
)
from .viv_screening import (
    VIVScreeningInput,
    BeamProperties,
    viv_screening,
    VIVScreeningResult,
    estimate_response_amplitude,
)
from .postprocessor import (
    compute_time_series_stats,
    TimeSeriesStats,
    generate_range_graph,
    RangeGraphData,
    fit_gumbel,
    fit_weibull,
    ExtremeValueResult,
    process_multi_seed,
    MultiSeedSummary,
)

__all__ = [
    # Reporting (existing)
    "generate_orcaflex_report",
    "ReportConfig",
    "run_orcaflex_qa",
    # Environment
    "CurrentProfile",
    "CurrentProfileType",
    "WaveSpectrumParams",
    "WaveSpectrumType",
    "WindProfile",
    "EnvironmentalMatrix",
    "EnvironmentalLoadCase",
    # Model builder
    "VesselConfig",
    "LineConfig",
    "Buoy6DConfig",
    "LineSectionProperties",
    "LineSection",
    "MATERIAL_LIBRARY",
    "build_scr_model",
    "build_lazy_wave_model",
    "build_mooring_line",
    # Mooring design
    "solve_catenary",
    "CatenaryResult",
    "MooringLineDesign",
    "SpreadMooringConfig",
    "TurretMooringConfig",
    "MOORING_MATERIAL_LIBRARY",
    "estimate_line_length",
    "calculate_pretension",
    # Riser config
    "RiserPipeProperties",
    "SCRDesignInput",
    "LazyWaveDesignInput",
    "TTRDesignInput",
    "calculate_weight_in_water",
    "estimate_scr_hang_off_angle",
    # Pipelay
    "SLayConfig",
    "JLayConfig",
    "LayOperability",
    "PipelayPipeProperties",
    "calculate_stinger_radius",
    # Installation
    "VesselRAO",
    "DAFInput",
    "SlingConfig",
    "SplashZoneInput",
    "WeightManagement",
    "WeightItem",
    # Weather window
    "ScatterDiagram",
    "OperabilityInput",
    "OperabilityTable",
    "analyse_persistence",
    "PersistenceResult",
    "SeasonalOperability",
    "WoWEstimate",
    # Batch parametric
    "ParameterSweep",
    "ParametricStudy",
    "CaseResult",
    "ParametricResultsSummary",
    # Code checks
    "check_api_rp_2rd",
    "check_dnv_os_f201",
    "check_mooring_api_2sk",
    "APIRP2RDInput",
    "DNVOSF201Input",
    "APIRP2SKInput",
    "UtilisationEnvelope",
    "MooringCodeCheckResult",
    # VIV screening
    "VIVScreeningInput",
    "BeamProperties",
    "viv_screening",
    "VIVScreeningResult",
    "estimate_response_amplitude",
    # Post-processing
    "compute_time_series_stats",
    "TimeSeriesStats",
    "generate_range_graph",
    "RangeGraphData",
    "fit_gumbel",
    "fit_weibull",
    "ExtremeValueResult",
    "process_multi_seed",
    "MultiSeedSummary",
]
