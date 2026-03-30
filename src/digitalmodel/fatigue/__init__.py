"""
digitalmodel.fatigue — Fatigue analysis module

DNV-RP-C203 S-N curves with pyLife WoehlerCurve backend,
Miner's rule damage accumulation, and thickness correction.
"""

from .sn_curves import get_sn_curve, DNV_CURVES
from .damage import miner_damage, design_life_check, thickness_correction

__all__ = [
    "get_sn_curve",
    "DNV_CURVES",
    "miner_damage",
    "design_life_check",
    "thickness_correction",
]
