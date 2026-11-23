"""
ABOUTME: Backward-compat shim — wellpath3D moved to base_solvers/well/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.well instead.
ABOUTME: Note: wellpath3D requires tkinter (GUI) which may not be available in all environments.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.wellpath3D is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.well.wellpath3D",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.well.wellpath3D import (  # noqa: F401, E402
    newwell,
    well,
    wellmap,
)

__all__ = ["newwell", "well", "wellmap"]
