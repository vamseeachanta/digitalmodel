"""
ABOUTME: Backward-compat shim — shear7_model_components moved to base_solvers/viv/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.viv instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.shear7_model_components is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.viv.shear7_model_components",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.viv.shear7_model_components import (  # noqa: F401, E402
    Shear7ModelComponents,
)

__all__ = ["Shear7ModelComponents"]
