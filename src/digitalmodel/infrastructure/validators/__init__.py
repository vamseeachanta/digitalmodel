# ABOUTME: Backward-compatibility shim for validators package
# Moved to digitalmodel.validation as part of Phase 2D restructuring
# This shim will be removed in a future release

import warnings

warnings.warn(
    "digitalmodel.validators is deprecated. Use digitalmodel.validation instead.",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.validation.data_validator import DataValidator

__all__ = ["DataValidator"]
