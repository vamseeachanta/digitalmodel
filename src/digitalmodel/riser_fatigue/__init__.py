"""
digitalmodel.riser_fatigue — SCR/SLWR riser fatigue workflows.

Currently provides the touchdown-zone fatigue assessment (DNV-RP-C203),
a license-free workflow that consumes a stress-range histogram and reuses
the core ``digitalmodel.fatigue`` engine.
"""

from .touchdown import (
    RiserSection,
    TouchdownFatigueInput,
    BinResult,
    TouchdownFatigueResult,
    assess_touchdown_fatigue,
    report_markdown,
)

__all__ = [
    "RiserSection",
    "TouchdownFatigueInput",
    "BinResult",
    "TouchdownFatigueResult",
    "assess_touchdown_fatigue",
    "report_markdown",
]
