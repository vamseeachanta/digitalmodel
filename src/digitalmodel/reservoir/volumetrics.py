# ABOUTME: Volumetric original-oil-in-place (OOIP/STOIIP) and recovery estimates.
# ABOUTME: Field-units OOIP = 7758*A*h*phi*So/Boi, plus primary/secondary recovery.

"""
Reservoir Volumetrics
=====================
Volumetric estimate of original oil in place (OOIP, a.k.a. STOIIP) and the
recoverable volumes implied by a recovery factor, in field units.

OOIP (STB) = 7758 * A * h * phi * So / Boi
  7758 = barrels per acre-foot (43560 ft^3/acre-ft / 5.615 ft^3/bbl)
  A    = areal extent (acres)
  h    = net pay thickness (ft)
  phi  = porosity (fraction)
  So   = oil saturation (fraction) = 1 - Sw
  Boi  = initial oil formation volume factor (reservoir bbl / STB)

All functions return stock-tank barrels (STB). Assumes a uniformly saturated,
fully drained volume within the stated area and net pay.
"""

from __future__ import annotations

from dataclasses import dataclass

BBL_PER_ACRE_FT = 7758.0


@dataclass(frozen=True)
class VolumetricInputs:
    """Inputs for a volumetric OOIP estimate (field units)."""

    area_acres: float
    net_pay_ft: float
    porosity: float            # fraction, 0-1
    oil_saturation: float      # fraction, 0-1 (So = 1 - Sw)
    formation_volume_factor: float  # Boi, reservoir bbl / STB


def ooip_stb(inputs: VolumetricInputs) -> float:
    """Original oil in place (STB) by the volumetric method."""
    return (
        BBL_PER_ACRE_FT
        * inputs.area_acres
        * inputs.net_pay_ft
        * inputs.porosity
        * inputs.oil_saturation
        / inputs.formation_volume_factor
    )


def recoverable_stb(ooip: float, recovery_factor: float) -> float:
    """Recoverable oil (STB) for a given recovery factor (fraction)."""
    return ooip * recovery_factor


def incremental_recovery_stb(
    ooip: float, rf_from: float, rf_to: float
) -> float:
    """
    Extra recoverable oil (STB) from raising the recovery factor `rf_from` -> `rf_to`
    (e.g. the uplift a secondary-recovery project buys over primary alone).
    """
    return ooip * (rf_to - rf_from)
