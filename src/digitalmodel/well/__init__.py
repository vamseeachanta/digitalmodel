# ABOUTME: Well engineering module — drilling mechanics, ROP models, hydraulics, dysfunction detection
# ABOUTME: Covers Bourgoyne-Young and Warren ROP models, wellbore hydraulics, real-time fault detection

"""
Well Engineering
================

Public API:
    rop_models          — Bourgoyne-Young (8-param) and Warren (power-law) ROP prediction models
    hydraulics          — WellboreHydraulics: ECD, annular velocity, pressure drop, CTR
    dysfunction_detector — Real-time drilling dysfunction detection
"""

from digitalmodel.well.drilling.hydraulics import WellboreHydraulics
from digitalmodel.well.drilling.rop_models import (
    BourgoineYoungROP,
    RopPrediction,
    WarrenROP,
)
from digitalmodel.well.drilling.dysfunction_detector import (
    DysfunctionDetector,
    DysfunctionEvent,
    DysfunctionType,
)

__all__ = [
    "BourgoineYoungROP",
    "RopPrediction",
    "WarrenROP",
    "WellboreHydraulics",
    "DysfunctionDetector",
    "DysfunctionEvent",
    "DysfunctionType",
]
