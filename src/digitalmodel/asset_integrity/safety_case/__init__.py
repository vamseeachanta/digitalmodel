"""Safety-case / Major-Accident-Hazard (MAH) support modules for floating assets.

Implements ALARP (As Low As Reasonably Practicable) risk evaluation following the
UK HSE "Tolerability of Risk" (R2P2) framework, NORSOK Z-013 and ISO 17776.

See ``alarp_demonstration`` for the ALARP region classifier and the
gross-disproportion cost-benefit-analysis (CBA) check.
"""

from .alarp_demonstration import (
    ALARPRegion,
    ALARPThresholds,
    ALARPResult,
    GrossDisproportionResult,
    classify_risk,
    gross_disproportion_check,
    risk_from_likelihood_consequence,
)

__all__ = [
    "ALARPRegion",
    "ALARPThresholds",
    "ALARPResult",
    "GrossDisproportionResult",
    "classify_risk",
    "gross_disproportion_check",
    "risk_from_likelihood_consequence",
]
