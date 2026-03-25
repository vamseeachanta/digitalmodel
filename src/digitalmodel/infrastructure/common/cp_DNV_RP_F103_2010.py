"""
ABOUTME: Backward-compat shim — cp_DNV_RP_F103_2010 moved to base_solvers/hydrodynamics/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.hydrodynamics instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.cp_DNV_RP_F103_2010 is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_F103_2010",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.hydrodynamics.cp_DNV_RP_F103_2010 import (  # noqa: F401, E402
    DNV_RP_F103,
)

__all__ = ["DNV_RP_F103"]
