"""Subsea structure installation analysis.

Provides crane tip motion transfer, splash zone assessment, operability
calculation, multi-vessel screening, and real-time feedback data models
for subsea structure installation engineering.

References:
    DNV-RP-H103 (2011) -- Modelling and Analysis of Marine Operations
    DNV-ST-N001 (2021) -- Marine Operations and Marine Warranty
    DNV-RP-C205 (2021) -- Environmental Conditions and Environmental Loads
"""
from __future__ import annotations

from digitalmodel.marine_ops.installation.models import (
    CraneCurve,
    CraneTipConfig,
    GoNoGoState,
    InstallationCase,
    InstallationCriteria,
    InstallationPhase,
    OperabilityResult,
    SplashZoneResult,
    Structure,
    Vessel,
)
from digitalmodel.marine_ops.installation.crane_tip_motion import (
    crane_tip_raos,
    crane_tip_significant_motion,
    crane_tip_significant_velocity,
)
from digitalmodel.marine_ops.installation.splash_zone import (
    slamming_force,
    splash_zone_assessment,
    varying_buoyancy_force,
)
from digitalmodel.marine_ops.installation.operability import (
    compute_operability,
    hs_limit_for_criterion,
    weather_window_operability,
)
from digitalmodel.marine_ops.installation.vessel_screening import (
    VesselScreeningResult,
    screen_vessels,
)

__all__ = [
    # Models
    "CraneCurve",
    "CraneTipConfig",
    "GoNoGoState",
    "InstallationCase",
    "InstallationCriteria",
    "InstallationPhase",
    "OperabilityResult",
    "SplashZoneResult",
    "Structure",
    "Vessel",
    "VesselScreeningResult",
    # Crane tip motion
    "crane_tip_raos",
    "crane_tip_significant_motion",
    "crane_tip_significant_velocity",
    # Splash zone
    "slamming_force",
    "splash_zone_assessment",
    "varying_buoyancy_force",
    # Operability
    "compute_operability",
    "hs_limit_for_criterion",
    "weather_window_operability",
    # Vessel screening
    "screen_vessels",
]
