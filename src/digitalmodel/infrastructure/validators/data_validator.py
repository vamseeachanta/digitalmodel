# ABOUTME: Backward-compatibility shim for validators.data_validator
# Moved to digitalmodel.validation.data_validator as part of Phase 2D restructuring
# This shim will be removed in a future release

import warnings

warnings.warn(
    "validators.data_validator is deprecated. Use digitalmodel.validation.data_validator instead.",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.validation.data_validator import DataValidator

__all__ = ["DataValidator"]
