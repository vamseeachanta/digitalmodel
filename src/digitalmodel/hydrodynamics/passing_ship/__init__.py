# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""
Passing Ship Forces Calculation Module.

This module implements Wang's methodology for calculating hydrodynamic forces
and moments on moored vessels due to passing ships.
"""

from .formulations import (
    s1_function,
    s2_function,
    ds1_dx,
    ds2_dx,
    f_kernel,
    g_kernel,
    calculate_surge_force_infinite,
    calculate_sway_force_infinite,
    calculate_yaw_moment_infinite,
    finite_depth_correction,
    calculate_forces_with_depth,
)

from .configuration import (
    VesselConfig,
    EnvironmentalConfig,
    CalculationConfig,
    PassingShipConfig,
    YAMLConfigParser,
    UnitConverter,
    ConfigurationMerger,
)

from .calculator import (
    PassingShipCalculator,
    ForceResults,
    ResultCache,
)

from .visualization import (
    ForceDistributionPlotter,
    ParametricStudyVisualizer,
    InteractivePlotManager,
    ComparisonLayoutManager,
    PlotExporter,
    plot_forces,
    create_parametric_study,
    create_comparison_plots,
)

from .input_schemas import (
    PassingShipSpec,
    MooredVesselSpec,
    PassingShipScenario,
    EnvironmentSpec,
    PassingTrack,
    HullSpec,
    MooringLineSpec,
    PassingShipDimensions,
    AnalysisCase,
    OutputSpec,
    FenderSpec,
    ResponseSettings,
    WindLoadSpec,
)

from .force_time_history import (
    ForceTimeHistory,
    generate_time_history,
)

from .mooring_response import MooringResponse, solve_response
from .analysis import AnalysisResult, run_analysis_cases, wind_force

__all__ = [
    "FenderSpec", "ResponseSettings", "WindLoadSpec",
    "MooringResponse", "solve_response",
    "AnalysisResult", "run_analysis_cases", "wind_force",
    # Formulations
    's1_function',
    's2_function',
    'ds1_dx',
    'ds2_dx',
    'f_kernel',
    'g_kernel',
    'calculate_surge_force_infinite',
    'calculate_sway_force_infinite',
    'calculate_yaw_moment_infinite',
    'finite_depth_correction',
    'calculate_forces_with_depth',
    # Configuration
    'VesselConfig',
    'EnvironmentalConfig',
    'CalculationConfig',
    'PassingShipConfig',
    'YAMLConfigParser',
    'UnitConverter',
    'ConfigurationMerger',
    # Calculator
    'PassingShipCalculator',
    'ForceResults',
    'ResultCache',
    # Visualization
    'ForceDistributionPlotter',
    'ParametricStudyVisualizer',
    'InteractivePlotManager',
    'ComparisonLayoutManager',
    'PlotExporter',
    'plot_forces',
    'create_parametric_study',
    'create_comparison_plots',
    # Input schemas (Phase 2)
    'PassingShipSpec',
    'MooredVesselSpec',
    'PassingShipScenario',
    'EnvironmentSpec',
    'PassingTrack',
    'HullSpec',
    'MooringLineSpec',
    'PassingShipDimensions',
    'AnalysisCase',
    'OutputSpec',
    # Force time history (Phase 2)
    'ForceTimeHistory',
    'generate_time_history',
]

__version__ = '1.0.0'
