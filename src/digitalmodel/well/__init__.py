# ABOUTME: Well engineering module — drilling mechanics, ROP models, hydraulics, dysfunction detection
# ABOUTME: Covers Bourgoyne-Young and Warren ROP models, wellbore hydraulics, real-time fault detection

"""
Well Engineering
================

Public API:
    rop_models   — Bourgoyne-Young (8-param) and Warren (power-law) ROP prediction models
"""

from digitalmodel.well.drilling.rop_models import (
    BourgoineYoungROP,
    RopPrediction,
    WarrenROP,
)

__all__ = [
    "BourgoineYoungROP",
    "RopPrediction",
    "WarrenROP",
]
