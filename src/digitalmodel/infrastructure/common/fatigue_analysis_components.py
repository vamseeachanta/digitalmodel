"""
ABOUTME: Backward-compat shim — fatigue_analysis_components moved to base_solvers/fatigue/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.fatigue instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.fatigue_analysis_components is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.fatigue.fatigue_analysis_components",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.fatigue.fatigue_analysis_components import (  # noqa: F401, E402
    FatigueAnalysisComponents,
)

__all__ = ["FatigueAnalysisComponents"]
