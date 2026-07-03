"""Corrosion screening tools — galvanic (dissimilar-metal) compatibility.

Public-domain basis only: MIL-STD-889 galvanic series / anodic-index method
plus widely published environment thresholds. No operator-proprietary data.
"""

from digitalmodel.corrosion.galvanic_screening import (
    ANODIC_INDEX_V,
    ENVIRONMENTS,
    GalvanicScreenResult,
    compatibility_matrix,
    screen_couple,
)

__all__ = [
    "ANODIC_INDEX_V",
    "ENVIRONMENTS",
    "GalvanicScreenResult",
    "compatibility_matrix",
    "screen_couple",
]
