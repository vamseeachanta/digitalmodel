"""
ABOUTME: Backward-compat shim — ship_fatigue_analysis moved to base_solvers/marine/ship/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.marine.ship instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.ship_fatigue_analysis is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.marine.ship.ship_fatigue_analysis",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.marine.ship.ship_fatigue_analysis import (  # noqa: F401, E402
    ShipFatigueAnalysis,
)

__all__ = ["ShipFatigueAnalysis"]
