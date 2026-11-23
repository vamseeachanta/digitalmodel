"""
ABOUTME: Backward-compat shim — fatigue_analysis moved to base_solvers/fatigue/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.fatigue instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.fatigue_analysis is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.fatigue.fatigue_analysis",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.fatigue.fatigue_analysis import (  # noqa: F401, E402
    FatigueAnalysis,
)

__all__ = ["FatigueAnalysis"]
