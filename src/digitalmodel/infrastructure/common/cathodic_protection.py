"""
ABOUTME: Backward-compat shim — CathodicProtection moved to base_solvers/hydrodynamics/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.hydrodynamics instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.cathodic_protection is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.hydrodynamics.cathodic_protection import (  # noqa: F401, E402
    CathodicProtection,
)

__all__ = ["CathodicProtection"]
