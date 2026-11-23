"""Backward-compatibility shim for base_configs/config_framework (WRK-415 Phase 2A).

ConfigLoader, SchemaValidator, and ConfigManager have moved to
digitalmodel.infrastructure.config.framework. Import from there instead.
"""
import warnings

warnings.warn(
    "digitalmodel.infrastructure.base_configs.config_framework is deprecated. "
    "Import from digitalmodel.infrastructure.config.framework instead. "
    "Will be removed in a future release.",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.config.framework import (
    ConfigLoader,
    SchemaValidator,
    ConfigManager,
)

__all__ = [
    "ConfigLoader",
    "SchemaValidator",
    "ConfigManager",
]
