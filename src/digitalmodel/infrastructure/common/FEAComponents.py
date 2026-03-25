"""
ABOUTME: Backward-compat shim — FEAComponents moved to base_solvers/structural/fea/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.structural.fea instead.
ABOUTME: Note: FEAComponents has a runtime dep on digitalmodel.custom.fea_model (optional).
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.FEAComponents is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.structural.fea.FEAComponents",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.structural.fea.FEAComponents import (  # noqa: F401, E402
    FEAComponents,
)

__all__ = ["FEAComponents"]
