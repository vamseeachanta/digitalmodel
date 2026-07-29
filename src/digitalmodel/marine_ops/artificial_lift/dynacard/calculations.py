# ABOUTME: Core dynacard calculations: CPIP, Pump Fillage, and Theoretical Production.
# ABOUTME: Ported from Oxy.Cipher.DynaCard with Pydantic models.

import numpy as np
from typing import Tuple
from .models import (
    DynacardAnalysisContext, CardData,
    FluidLoadAnalysis, CPIPAnalysis, PumpFillageAnalysis
)
from .corners import calculate_corners
from .production import calculate_theoretical_production


# =============================================================================
# FLUID LOAD CALCULATION
# =============================================================================

def calculate_fluid_load(
    downhole_card: CardData,
    method: str = 'avg'
) -> FluidLoadAnalysis:
    """
    Calculate the fluid load (F_0) from downhole card.

    Fluid load is the difference between upstroke load and downstroke load
    after the traveling valve opens.

    Args:
        downhole_card: Downhole dynamometer card data
        method: Calculation method - 'avg', 'med', or '2pt'

    Returns:
        FluidLoadAnalysis with upstroke, downstroke, and fluid load
    """
    load = np.array(downhole_card.load)

    # Detect corners
    corners, _ = calculate_corners(downhole_card)
    corners = sorted(corners)

    if method == '2pt':
        # Use corner loads directly
        upstroke_load = load[corners[2]]  # Top right
        downstroke_load = load[corners[0]]  # Bottom left
    elif method == 'avg':
        # Average loads in each region
        upstroke_load = np.mean(load[corners[1]:corners[2]])
        downstroke_load = np.mean(load[corners[3]:])
    elif method == 'med':
        # Median loads in each region
        upstroke_load = np.median(load[corners[1]:corners[2]])
        downstroke_load = np.median(load[corners[3]:])
    else:
        raise ValueError(f"Unknown method: {method}")

    fluid_load = upstroke_load - downstroke_load

    return FluidLoadAnalysis(
        fluid_load=float(fluid_load),
        upstroke_load=float(upstroke_load),
        downstroke_load=float(downstroke_load)
    )


# =============================================================================
# PUMP FILLAGE CALCULATION
# =============================================================================

def calculate_max_plunger_travel(downhole_card: CardData) -> float:
    """
    Calculate gross downhole stroke (maximum plunger travel).

    Args:
        downhole_card: Downhole dynamometer card data

    Returns:
        Gross stroke in inches
    """
    position = np.array(downhole_card.position)
    return float(np.max(position) - np.min(position))


def calculate_effective_plunger_travel(downhole_card: CardData) -> Tuple[float, int]:
    """
    Calculate effective (net) plunger travel.

    The net stroke is determined by the bottom right corner,
    which represents where the traveling valve opens.

    Args:
        downhole_card: Downhole dynamometer card data

    Returns:
        (net_stroke, br_corner_idx): Net stroke in inches and corner index
    """
    position = np.array(downhole_card.position)

    # Get corners
    corners, _ = calculate_corners(downhole_card)
    br_idx = corners[3]  # Bottom right corner

    # Net stroke is position at BR corner relative to minimum
    min_pos = np.min(position)
    net_stroke = position[br_idx] - min_pos

    return float(net_stroke), br_idx


def calculate_pump_fillage(
    downhole_card: CardData
) -> PumpFillageAnalysis:
    """
    Calculate pump fillage percentage.

    Fillage = (Net Stroke / Gross Stroke) * 100

    This represents the percentage of the pump barrel that fills with fluid
    each stroke. Low fillage indicates fluid pound or gas interference.

    Args:
        downhole_card: Downhole dynamometer card data

    Returns:
        PumpFillageAnalysis with gross, net stroke and fillage percentage
    """
    gross_stroke = calculate_max_plunger_travel(downhole_card)
    net_stroke, br_idx = calculate_effective_plunger_travel(downhole_card)

    # Calculate fillage percentage
    if gross_stroke > 0:
        fillage = (net_stroke / gross_stroke) * 100.0
    else:
        fillage = 0.0

    # Clamp to valid range
    fillage = max(0.0, min(100.0, fillage))

    corners, _ = calculate_corners(downhole_card)

    return PumpFillageAnalysis(
        gross_stroke=float(gross_stroke),
        net_stroke=float(net_stroke),
        fillage=float(fillage),
        corners=corners
    )


# =============================================================================
# CPIP (CALCULATED PUMP INTAKE PRESSURE)
# =============================================================================

def calculate_tubing_gradient(
    oil_api: float = 35.0,
    water_sg: float = 1.03,
    water_cut: float = 50.0
) -> Tuple[float, float]:
    """
    Calculate tubing gradient and fluid density.

    TG = 0.433 * SG_w * WC + 0.433 * SG_o * (1 - WC)

    Args:
        oil_api: Oil API gravity
        water_sg: Water specific gravity
        water_cut: Water cut percentage

    Returns:
        (tubing_gradient, fluid_density): psi/ft and lbs/ft^3
    """
    # Calculate oil specific gravity from API
    oil_sg = 141.5 / (oil_api + 131.5)

    # Water cut as fraction
    wc = water_cut / 100.0

    # Mixed fluid specific gravity
    mixed_sg = water_sg * wc + oil_sg * (1.0 - wc)

    # Tubing gradient (psi/ft) = 0.433 * SG
    tubing_gradient = 0.433 * mixed_sg

    # Fluid density (lbs/ft^3) = SG * 62.4
    fluid_density = mixed_sg * 62.4

    return tubing_gradient, fluid_density


def calculate_cpip(
    ctx: DynacardAnalysisContext,
    downhole_card: CardData,
    base_load_ref: float = 0.0
) -> CPIPAnalysis:
    """
    Calculate Pump Intake Pressure (PIP) and Pump Discharge Pressure (PDP).

    PDP = Tubing Pressure + Fluid Density * g * Pump Depth
    PIP = PDP - Fluid Load / Pump Area

    Args:
        ctx: Analysis context with well parameters
        downhole_card: Downhole dynamometer card data
        base_load_ref: Reference base load for calibration

    Returns:
        CPIPAnalysis with PIP, PDP, and fluid density
    """
    # Get well test data for fluid properties
    if ctx.well_test is not None:
        water_cut = ctx.well_test.water_cut
    else:
        water_cut = 50.0  # Default assumption

    # Get tubing pressure from input parameters
    if ctx.input_params is not None and ctx.input_params.tubing_pressure:
        tubing_pressure = ctx.input_params.tubing_pressure
    else:
        tubing_pressure = 100.0  # Default psi

    # Calculate tubing gradient and fluid density
    tubing_gradient, fluid_density = calculate_tubing_gradient(
        oil_api=35.0,
        water_sg=1.03,
        water_cut=water_cut
    )

    # Get pump depth and area
    pump_depth = ctx.pump.depth  # feet
    pump_area = ctx.pump_area  # sq inches

    # Calculate fluid load from downhole card
    fluid_load_analysis = calculate_fluid_load(downhole_card, method='avg')
    fluid_load = fluid_load_analysis.fluid_load

    # Calculate Pump Discharge Pressure
    # PDP = THP + TG * h_pump (tubing head pressure + gradient * depth)
    pdp = tubing_pressure + tubing_gradient * pump_depth

    # Calculate Pump Intake Pressure
    # PIP = PDP - F_0 / A_pump
    pip = pdp - (fluid_load / pump_area)

    return CPIPAnalysis(
        pump_intake_pressure=float(pip),
        pump_discharge_pressure=float(pdp),
        fluid_density=float(fluid_density),
        tubing_gradient=float(tubing_gradient)
    )


# =============================================================================
# COMBINED ANALYSIS
# =============================================================================

def run_p1_calculations(
    ctx: DynacardAnalysisContext,
    downhole_card: CardData
) -> dict:
    """
    Run all P1 (priority 1) calculations.

    Returns dict with:
        - fluid_load: FluidLoadAnalysis
        - fillage: PumpFillageAnalysis
        - cpip: CPIPAnalysis
        - production: ProductionAnalysis
    """
    # Calculate fluid load
    fluid_load = calculate_fluid_load(downhole_card, method='avg')

    # Calculate pump fillage
    fillage = calculate_pump_fillage(downhole_card)

    # Calculate CPIP
    cpip = calculate_cpip(ctx, downhole_card)

    # Calculate theoretical production
    production = calculate_theoretical_production(ctx, fillage, cpip)

    return {
        'fluid_load': fluid_load,
        'fillage': fillage,
        'cpip': cpip,
        'production': production
    }
