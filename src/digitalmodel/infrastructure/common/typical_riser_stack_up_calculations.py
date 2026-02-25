"""
ABOUTME: Backward-compat shim — typical_riser_stack_up_calculations moved to base_solvers/marine/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.marine instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.typical_riser_stack_up_calculations is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.marine.typical_riser_stack_up_calculations",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.marine.typical_riser_stack_up_calculations import (  # noqa: F401, E402
    TypicalRiserStackUpCalculations,
)

__all__ = ["TypicalRiserStackUpCalculations"]
