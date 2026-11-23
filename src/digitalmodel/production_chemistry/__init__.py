# ABOUTME: Production chemistry package — produced-water mineral-scale prediction
# ABOUTME: (saturation indices, SI trending, brine-mixing compatibility) — issue #1295

"""
Production Chemistry — produced-water mineral-scale prediction
==============================================================

Public API:
    scale_si — Oddo–Tomson-framework saturation indices (calcite, sulfate
               scales, halite), bottomhole→wellhead SI trending, and
               brine-mixing (waterflood compatibility) sweeps.

Basis: Oddo & Tomson, "Why Scale Forms in the Oil Field and Methods To
Predict It", SPE Production & Facilities 9(1), 1994 — the practitioner-
standard conditional-solubility SI framework. See
:mod:`digitalmodel.production_chemistry.scale_si` for the coefficient
honesty statement (defaults are configurable, not verified paper values).
"""

from digitalmodel.production_chemistry.scale_si import (
    BrineComposition,
    CorrelationCoefficients,
    ScaleIndexResult,
    default_coefficients,
    ionic_strength,
    mix_brines,
    mixing_sweep,
    saturation_indices,
    si_profile,
)

__all__ = [
    "BrineComposition",
    "CorrelationCoefficients",
    "ScaleIndexResult",
    "default_coefficients",
    "ionic_strength",
    "mix_brines",
    "mixing_sweep",
    "saturation_indices",
    "si_profile",
]
