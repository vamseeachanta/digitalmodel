# ABOUTME: Dynacard analysis module for sucker rod pump diagnostics.
# ABOUTME: Provides physics solvers, calculations, and AI-driven troubleshooting.

from .models import (
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    WellTestData,
    SurveyData,
    MotorProperties,
    CalculationParameters,
    InputParameters,
    DynacardAnalysisContext,
    FluidLoadAnalysis,
    CPIPAnalysis,
    PumpFillageAnalysis,
    ProductionAnalysis,
    TorqueStatistics,
    GearBoxLoadingAnalysis,
    PowerConsumptionAnalysis,
    LiftCapacityAnalysis,
    LoadRatioAnalysis,
    CardGeometryAnalysis,
    RodBucklingAnalysis,
    IdealCardAnalysis,
    TorqueBalanceAnalysis,
    AnalysisResults,
)

from .physics import DynacardPhysicsSolver
from .finite_difference import FiniteDifferenceSolver, solve_wave_equation_fd
from .corners import CornerDetector, calculate_corners, get_corner_loads
from .calculations import (
    calculate_fluid_load,
    calculate_pump_fillage,
    calculate_cpip,
    calculate_theoretical_production,
    run_p1_calculations,
)
from .diagnostics import PumpDiagnostics
from .solver import DynacardWorkflow, perform_well_troubleshooting
from .gear_box_loading import (
    GearBoxLoadingCalculator,
    calculate_gear_box_loading,
)
from .power_consumption import (
    PowerConsumptionCalculator,
    calculate_power_consumption,
    calculate_card_area,
)
from .lift_capacity import (
    LiftCapacityCalculator,
    calculate_lift_capacity,
)
from .load_analysis import (
    LoadRatioCalculator,
    calculate_load_ratios,
    calculate_peak_load_ratio,
    calculate_low_load_ratio,
)
from .pump_efficiency import (
    PumpEfficiencyCalculator,
    calculate_pump_efficiency,
    calculate_efficiency_from_rates,
)
from .geometry import (
    CardGeometryCalculator,
    calculate_card_geometry,
    calculate_card_perimeter,
)
from .rod_buckling import (
    RodBucklingCalculator,
    calculate_rod_buckling,
    estimate_neutral_point,
    calculate_critical_buckling_load,
)
from .ideal_card import (
    IdealCardCalculator,
    generate_ideal_card,
    calculate_shape_similarity,
    calculate_ideal_fluid_load,
    generate_rectangular_pump_card,
)
from .torque_balance import (
    TorqueBalanceCalculator,
    calculate_torque_balance,
    estimate_optimal_counterbalance,
    calculate_torque_objective,
)
from .base import BaseCalculator
from .exceptions import (
    DynacardException,
    ValidationError,
    PhysicsError,
    NumericalError,
    ConfigurationError,
    ConvergenceError,
    DataLoadError,
    missing_data_error,
    invalid_value_error,
    array_length_mismatch_error,
)

__all__ = [
    # Base Class
    "BaseCalculator",
    # Exceptions
    "DynacardException",
    "ValidationError",
    "PhysicsError",
    "NumericalError",
    "ConfigurationError",
    "ConvergenceError",
    "DataLoadError",
    "missing_data_error",
    "invalid_value_error",
    "array_length_mismatch_error",
    # Models
    "CardData",
    "RodSection",
    "PumpProperties",
    "SurfaceUnit",
    "WellTestData",
    "SurveyData",
    "MotorProperties",
    "CalculationParameters",
    "InputParameters",
    "DynacardAnalysisContext",
    "FluidLoadAnalysis",
    "CPIPAnalysis",
    "PumpFillageAnalysis",
    "ProductionAnalysis",
    "TorqueStatistics",
    "GearBoxLoadingAnalysis",
    "PowerConsumptionAnalysis",
    "LiftCapacityAnalysis",
    "LoadRatioAnalysis",
    "CardGeometryAnalysis",
    "RodBucklingAnalysis",
    "IdealCardAnalysis",
    "TorqueBalanceAnalysis",
    "AnalysisResults",
    # Physics Solvers
    "DynacardPhysicsSolver",
    "FiniteDifferenceSolver",
    "solve_wave_equation_fd",
    # Corner Detection
    "CornerDetector",
    "calculate_corners",
    "get_corner_loads",
    # Calculations
    "calculate_fluid_load",
    "calculate_pump_fillage",
    "calculate_cpip",
    "calculate_theoretical_production",
    "run_p1_calculations",
    # Diagnostics
    "PumpDiagnostics",
    # Workflow
    "DynacardWorkflow",
    "perform_well_troubleshooting",
    # Gear Box Loading
    "GearBoxLoadingCalculator",
    "calculate_gear_box_loading",
    # Power Consumption
    "PowerConsumptionCalculator",
    "calculate_power_consumption",
    "calculate_card_area",
    # Lift Capacity
    "LiftCapacityCalculator",
    "calculate_lift_capacity",
    # Load Analysis
    "LoadRatioCalculator",
    "calculate_load_ratios",
    "calculate_peak_load_ratio",
    "calculate_low_load_ratio",
    # Pump Efficiency
    "PumpEfficiencyCalculator",
    "calculate_pump_efficiency",
    "calculate_efficiency_from_rates",
    # Card Geometry
    "CardGeometryCalculator",
    "calculate_card_geometry",
    "calculate_card_perimeter",
    # Rod Buckling
    "RodBucklingCalculator",
    "calculate_rod_buckling",
    "estimate_neutral_point",
    "calculate_critical_buckling_load",
    # Ideal Card
    "IdealCardCalculator",
    "generate_ideal_card",
    "calculate_shape_similarity",
    "calculate_ideal_fluid_load",
    "generate_rectangular_pump_card",
    # Torque Balance
    "TorqueBalanceCalculator",
    "calculate_torque_balance",
    "estimate_optimal_counterbalance",
    "calculate_torque_objective",
]
