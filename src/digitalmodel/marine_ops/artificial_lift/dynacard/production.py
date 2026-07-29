# ABOUTME: Production corrections for rod-pump slippage and fluid shrinkage.
# ABOUTME: Implements the house Patterson equation without assumed inputs.

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
    """Return hours/day and its explicit source."""
    if ctx.input_params is not None and ctx.input_params.runtime:
        return ctx.input_params.runtime, "card"
    if ctx.well_test is not None and ctx.well_test.runtime:
        return ctx.well_test.runtime, "well_test"
    return 24.0, "assumed_24h"


def _pump_efficiency(
    ctx: DynacardAnalysisContext,
    theoretical_production: float,
) -> float:
    """Compare displacement with a well test when one is available."""
    if ctx.well_test is None:
        return ctx.pump.efficiency * 100.0
    actual = ctx.well_test.oil_rate + ctx.well_test.water_rate
    if actual <= 0.0:
        return ctx.pump.efficiency * 100.0
    if theoretical_production <= 0.0:
        return 0.0
    return actual / theoretical_production * 100.0


def _production_correction(
    ctx: DynacardAnalysisContext,
    cpip: Optional[CPIPAnalysis],
    theoretical_production: float,
    runtime_fraction: float,
) -> dict:
    """Build every reported correction input and term, or name what is missing."""
    params = ctx.input_params
    differential = None
    if cpip is not None:
        differential = cpip.pump_discharge_pressure - cpip.pump_intake_pressure
    inputs = {
        "plunger_barrel_clearance_in": ctx.pump.plunger_barrel_clearance_in,
        "fluid_viscosity_cp": params.fluid_viscosity_cp if params else None,
        "differential_pressure_psi": differential,
        "plunger_length_in": ctx.pump.plunger_length_in,
        "formation_volume_factor": params.formation_volume_factor if params else None,
    }
    missing = [
        name for name, value in inputs.items()
        if value is None or value <= 0.0
    ]
    terms = {
        "slippage_bpd": None,
        "runtime_adjusted_slippage_bpd": None,
        "corrected_stock_tank_production": None,
        "correction_missing_inputs": missing,
    }
    if missing:
        return {**inputs, **terms}
    slippage = patterson_slippage_bpd(
        ctx.pump.diameter,
        differential,
        inputs["plunger_barrel_clearance_in"],
        inputs["fluid_viscosity_cp"],
        inputs["plunger_length_in"],
        ctx.spm,
    )
    adjusted_slippage = slippage * runtime_fraction
    corrected_downhole = max(theoretical_production - adjusted_slippage, 0.0)
    terms.update({
        "slippage_bpd": slippage,
        "runtime_adjusted_slippage_bpd": adjusted_slippage,
        "corrected_stock_tank_production": (
            corrected_downhole / inputs["formation_volume_factor"]
        ),
    })
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
    correction = _production_correction(
        ctx, cpip, displacement, runtime_fraction
    )
    return ProductionAnalysis(
        gross_displacement=float(gross),
        net_displacement=float(net),
        theoretical_production=float(displacement),
        pump_efficiency=float(_pump_efficiency(ctx, displacement)),
        runtime_hours=float(runtime),
        runtime_source=runtime_source,
        **correction,
    )
