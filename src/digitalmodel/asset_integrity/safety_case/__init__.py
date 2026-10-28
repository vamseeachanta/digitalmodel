"""Safety-case / Major-Accident-Hazard (MAH) support modules for floating assets.

Implements ALARP (As Low As Reasonably Practicable) risk evaluation following the
UK HSE "Tolerability of Risk" (R2P2) framework, NORSOK Z-013 and ISO 17776.

``mah_register`` provides validated hazard inventory and CSV exchange.
``alarp_demonstration`` provides individual-risk classification and the
gross-disproportion cost-benefit-analysis (CBA) check. Inventory PLL values
(expected fatalities/year) are distinct from individual risk per annum.
``qra_mooring_failure`` combines catenary load evidence with supplied line-break
frequency and consequence. ``qra_vessel_collision`` assesses supplied transit
exposure, collision probability and consequence, retaining passing-ship force
context without treating interaction loads as collision consequences.
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
from .mah_register import (
    EvaluationStatus,
    MAHRegister,
    MAHValidationError,
    MajorAccidentHazard,
)
from .qra_mooring_failure import MooringFailureResult, assess_mooring_failure
from .qra_vessel_collision import (
    PassingShipForceEvidence,
    VesselCollisionInputs,
    VesselCollisionResult,
    assess_vessel_collision,
)

__all__ = [
    "ALARPRegion",
    "ALARPThresholds",
    "ALARPResult",
    "GrossDisproportionResult",
    "classify_risk",
    "gross_disproportion_check",
    "risk_from_likelihood_consequence",
    "EvaluationStatus",
    "MAHRegister",
    "MAHValidationError",
    "MajorAccidentHazard",
    "MooringFailureResult",
    "assess_mooring_failure",
    "PassingShipForceEvidence",
    "VesselCollisionInputs",
    "VesselCollisionResult",
    "assess_vessel_collision",
]
