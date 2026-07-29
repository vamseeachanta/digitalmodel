# ABOUTME: Production corrections for rod-pump slippage and fluid shrinkage.
# ABOUTME: Implements the house Patterson equation without assumed inputs.

import math
from typing import Optional

import numpy as np

from .models import (
    CPIPAnalysis,
    DynacardAnalysisContext,
    ProductionAnalysis,
    PumpFillageAnalysis,
)


def patterson_slippage_bpd(
    plunger_diameter_in: float,
    differential_pressure_psi: float,
    clearance_in: float,
    fluid_viscosity_cp: float,
    plunger_length_in: float,
    strokes_per_minute: float,
) -> float:
    """Return plunger/barrel slippage as a continuous-operation BPD rate.

    The equation and coefficients are transcribed from cell B17 of the house
    ``Pump_Slippage_Calculator_SPM_PattersonEq.xls`` workbook:

    ``452 * D * dP * C**1.52 / (mu * L) * (0.14 * SPM + 1)``

    Inputs use the oilfield units named by the workbook: inches, psi,
    centipoise, and strokes per minute.
    """
    values = (
        plunger_diameter_in,
        differential_pressure_psi,
        clearance_in,
        fluid_viscosity_cp,
        plunger_length_in,
        strokes_per_minute,
    )
    if any(value <= 0.0 for value in values):
        raise ValueError("Patterson slippage inputs must all be positive")
    return (
        452.0
        * plunger_diameter_in
        * differential_pressure_psi
        * clearance_in**1.52
        / (fluid_viscosity_cp * plunger_length_in)
        * (0.14 * strokes_per_minute + 1.0)
    )


def _runtime(ctx: DynacardAnalysisContext) -> tuple[float, str]:
    """Prefer card-time runtime; ``ProductionAnalysis`` documents why."""
    if ctx.input_params is not None and ctx.input_params.runtime:
        return ctx.input_params.runtime, "card"
    if ctx.well_test is not None and ctx.well_test.runtime:
        return ctx.well_test.runtime, "well_test"
    return 24.0, "assumed_24h"


def _pump_efficiency(
    ctx: DynacardAnalysisContext,
    theoretical_production: float,
) -> float:
    """Return actual fluid rate as a percentage of uncorrected displacement."""
    if ctx.well_test is None:
        return ctx.pump.efficiency * 100.0
    actual = ctx.well_test.oil_rate + ctx.well_test.water_rate
    if actual <= 0.0:
        return ctx.pump.efficiency * 100.0
    if theoretical_production <= 0.0:
        return 0.0
    return actual / theoretical_production * 100.0


def _correction_inputs(
    ctx: DynacardAnalysisContext,
    cpip: Optional[CPIPAnalysis],
    runtime_fraction: float,
) -> dict:
    """Snapshot every physical input used by the production correction."""
    params = ctx.input_params
    differential = None
    if cpip is not None:
        differential = cpip.pump_discharge_pressure - cpip.pump_intake_pressure
    return {
        "plunger_diameter_in": ctx.pump.diameter,
        "strokes_per_minute": ctx.spm,
        "runtime_fraction": runtime_fraction,
        "plunger_barrel_clearance_in": ctx.pump.plunger_barrel_clearance_in,
        "fluid_viscosity_cp": params.fluid_viscosity_cp if params else None,
        "differential_pressure_psi": differential,
        "plunger_length_in": ctx.pump.plunger_length_in,
        "formation_volume_factor": params.formation_volume_factor if params else None,
    }


def _input_gaps(inputs: dict) -> tuple[list[str], list[str]]:
    """Separate absent inputs from present but non-physical inputs."""
    physical_inputs = {
        name: value for name, value in inputs.items() if name != "runtime_fraction"
    }
    unavailable = [name for name, value in physical_inputs.items() if value is None]
    invalid = [
        name
        for name, value in physical_inputs.items()
        if value is not None and (not math.isfinite(value) or value <= 0.0)
    ]
    return unavailable, invalid


def _slippage(ctx: DynacardAnalysisContext, inputs: dict) -> float:
    """Evaluate Patterson only after its independent inputs are valid."""
    return patterson_slippage_bpd(
        inputs["plunger_diameter_in"],
        inputs["differential_pressure_psi"],
        inputs["plunger_barrel_clearance_in"],
        inputs["fluid_viscosity_cp"],
        inputs["plunger_length_in"],
        inputs["strokes_per_minute"],
    )


def _production_correction(
    ctx: DynacardAnalysisContext,
    cpip: Optional[CPIPAnalysis],
    theoretical_production: float,
    runtime_fraction: float,
) -> dict:
    """Build every reported correction input and term, or name what is missing."""
    inputs = _correction_inputs(ctx, cpip, runtime_fraction)
    unavailable, invalid = _input_gaps(inputs)
    missing = unavailable + invalid
    terms = {
        "slippage_bpd": None,
        "runtime_adjusted_slippage_bpd": None,
        "slippage_corrected_downhole_production": None,
        "corrected_stock_tank_production": None,
        "correction_status": ("invalid_inputs" if invalid else "missing_inputs"),
        "correction_missing_inputs": missing,
    }
    slippage_inputs = missing.copy()
    if "formation_volume_factor" in slippage_inputs:
        slippage_inputs.remove("formation_volume_factor")
    if slippage_inputs:
        return {**inputs, **terms}
    slippage = _slippage(ctx, inputs)
    adjusted_slippage = slippage * runtime_fraction
    if adjusted_slippage > theoretical_production:
        terms.update(
            {
                "slippage_bpd": slippage,
                "runtime_adjusted_slippage_bpd": adjusted_slippage,
                "correction_status": "slippage_exceeds_displacement",
            }
        )
        return {**inputs, **terms}
    corrected_downhole = theoretical_production - adjusted_slippage
    terms.update(
        {
            "slippage_bpd": slippage,
            "runtime_adjusted_slippage_bpd": adjusted_slippage,
            "slippage_corrected_downhole_production": corrected_downhole,
        }
    )
    if "formation_volume_factor" not in missing:
        terms.update(
            {
                "corrected_stock_tank_production": (
                    corrected_downhole / inputs["formation_volume_factor"]
                ),
                "correction_status": "calculated",
            }
        )
    return {**inputs, **terms}


def calculate_theoretical_production(
    ctx: DynacardAnalysisContext,
    fillage: PumpFillageAnalysis,
    cpip: Optional[CPIPAnalysis] = None,
) -> ProductionAnalysis:
    """Calculate displacement and an optional Patterson/FVF-corrected rate."""
    area = np.pi * ctx.pump.diameter**2 / 4.0
    gross = area * fillage.gross_stroke * ctx.spm * 1440.0 / 9702.0
    net = gross * fillage.fillage / 100.0
    runtime, runtime_source = _runtime(ctx)
    runtime_fraction = runtime / 24.0
    displacement = net * runtime_fraction
    correction = _production_correction(ctx, cpip, displacement, runtime_fraction)
    return ProductionAnalysis(
        gross_displacement=float(gross),
        net_displacement=float(net),
        theoretical_production=float(displacement),
        pump_efficiency=float(_pump_efficiency(ctx, displacement)),
        runtime_hours=float(runtime),
        runtime_source=runtime_source,
        **correction,
    )
