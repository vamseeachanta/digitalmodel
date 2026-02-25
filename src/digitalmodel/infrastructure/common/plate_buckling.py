"""
ABOUTME: Backward-compat shim — plate_buckling moved to base_solvers/structural/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.structural instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.plate_buckling is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.structural.plate_buckling",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.structural.plate_buckling import (  # noqa: F401, E402, E501
    PlateBuckling,
)

__all__ = ["PlateBuckling"]
