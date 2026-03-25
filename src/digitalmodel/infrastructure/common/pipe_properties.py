"""
ABOUTME: Backward-compat shim — pipe_properties moved to base_solvers/marine/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.marine instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.pipe_properties is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.marine.pipe_properties",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.marine.pipe_properties import (  # noqa: F401, E402
    PipeProperties,
)

__all__ = ["PipeProperties"]
