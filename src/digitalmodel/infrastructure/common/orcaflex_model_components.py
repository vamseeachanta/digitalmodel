"""
ABOUTME: Backward-compat shim — orcaflex_model_components moved to base_solvers/marine/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.marine instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.orcaflex_model_components is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.marine.orcaflex_model_components",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.marine.orcaflex_model_components import (  # noqa: F401, E402
    OrcaflexModelComponents,
)

__all__ = ["OrcaflexModelComponents"]
