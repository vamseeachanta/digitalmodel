"""Backward-compatibility shim for base_configs/config_models (WRK-415 Phase 2A).

ConfigModel has moved to digitalmodel.infrastructure.config.models.
Import from there instead.
"""
import warnings

warnings.warn(
    "digitalmodel.infrastructure.base_configs.config_models is deprecated. "
    "Import from digitalmodel.infrastructure.config.models instead. "
    "Will be removed in a future release.",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.config.models import ConfigModel

__all__ = [
    "ConfigModel",
]
