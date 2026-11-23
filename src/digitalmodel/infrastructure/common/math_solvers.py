"""
ABOUTME: Backward-compat shim — math_solvers moved to base_solvers/pipeline_solvers/.
ABOUTME: Import from digitalmodel.infrastructure.base_solvers.pipeline_solvers instead.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.common.math_solvers is deprecated. "
    "Import from digitalmodel.infrastructure.base_solvers.pipeline_solvers.math_solvers",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.base_solvers.pipeline_solvers.math_solvers import (  # noqa: F401, E402
    Polynomial,
    Scipy_Interpolation,
    FFT_Methods,
    Geometry,
)

__all__ = ["Polynomial", "Scipy_Interpolation", "FFT_Methods", "Geometry"]
