"""
ABOUTME: Backward-compat shim — viv_analysis_components moved to base_solvers/viv/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.viv instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.viv_analysis_components is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.viv.viv_analysis_components",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.viv.viv_analysis_components import (  # noqa: F401, E402
    VIVAnalysisComponents,
)

__all__ = ["VIVAnalysisComponents"]
