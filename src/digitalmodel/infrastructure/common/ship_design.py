"""
ABOUTME: Backward-compat shim — ship_design moved to base_solvers/marine/ship/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.marine.ship instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.ship_design is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.marine.ship.ship_design",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.marine.ship.ship_design import (  # noqa: F401, E402
    ShipDesign,
)

__all__ = ["ShipDesign"]
