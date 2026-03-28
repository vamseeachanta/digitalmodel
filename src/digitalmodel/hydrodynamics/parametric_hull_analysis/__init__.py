"""
ABOUTME: Parametric hull analysis — sweeps hull forms through BEM solvers,
forward speed corrections, passing ship forces, and shallow water effects.
Orchestrates hull_library, capytaine, and passing_ship modules.
"""

__version__ = "0.1.0"

from .models import (
    DepthClassification,
    BankSlopeType,
    SweepConfig,
    PassingShipSweepConfig,
    SweepResultEntry,
    PassingShipSweepEntry,
    BankEffectResult,
    classify_depth,
)
from .sweep import run_parametric_sweep, sweep_to_dataframe
from .forward_speed import (
    encounter_frequency,
    wave_number,
    correct_rao_for_speed,
    strip_theory_speed_correction,
)
from .shallow_water import (
    dnv_shallow_water_factor,
    validate_shallow_water_results,
    pianc_bank_suction_force,
    pianc_bank_clearance_width,
)
from .passing_ship_sweep import (
    run_passing_ship_sweep,
    passing_ship_to_dataframe,
    peak_force_envelope,
    pianc_operability_check,
    hull_profile_to_vessel_config,
)
from .charts import (
    rao_comparison_grid,
    parameter_sensitivity_plot,
    depth_sensitivity_plot,
    passing_ship_contour,
    operability_chart,
    save_figure,
)

__all__ = [
    # Models
    "DepthClassification",
    "BankSlopeType",
    "SweepConfig",
    "PassingShipSweepConfig",
    "SweepResultEntry",
    "PassingShipSweepEntry",
    "BankEffectResult",
    "classify_depth",
    # Core sweep
    "run_parametric_sweep",
    "sweep_to_dataframe",
    # Forward speed
    "encounter_frequency",
    "wave_number",
    "correct_rao_for_speed",
    "strip_theory_speed_correction",
    # Shallow water
    "dnv_shallow_water_factor",
    "validate_shallow_water_results",
    "pianc_bank_suction_force",
    "pianc_bank_clearance_width",
    # Passing ship
    "run_passing_ship_sweep",
    "passing_ship_to_dataframe",
    "peak_force_envelope",
    "pianc_operability_check",
    "hull_profile_to_vessel_config",
    # Charts
    "rao_comparison_grid",
    "parameter_sensitivity_plot",
    "depth_sensitivity_plot",
    "passing_ship_contour",
    "operability_chart",
    "save_figure",
]
