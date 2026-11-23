"""
ABOUTME: Backward-compat shim — code_dnvrph103_hydrodynamics_rectangular moved to base_solvers/hydrodynamics/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.hydrodynamics instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.code_dnvrph103_hydrodynamics_rectangular is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.hydrodynamics.code_dnvrph103_hydrodynamics_rectangular import (  # noqa: F401, E402
    DNVRPH103_hydrodynamics_rectangular,
)

__all__ = ["DNVRPH103_hydrodynamics_rectangular"]
