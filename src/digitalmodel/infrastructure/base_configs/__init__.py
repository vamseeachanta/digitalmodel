"""
ABOUTME: Backward-compatibility shim for base_configs module (WRK-415 Phase 2A).
ABOUTME: Python logic (ConfigLoader, SchemaValidator, ConfigManager, ConfigModel) has
ABOUTME: moved to infrastructure/config/. This shim re-exports for existing callers.

NOTE: domains/ YAML data directory is intentionally kept here because external
callers reference it via pkgutil.get_data("digitalmodel", "base_configs/domains/...")
filesystem paths and cannot be changed in Phase 2A.
"""

import warnings

warnings.warn(
    "digitalmodel.infrastructure.base_configs is deprecated. "
    "Import from digitalmodel.infrastructure.config instead. "
    "Will be removed in a future release.",
    DeprecationWarning,
    stacklevel=2,
)

from digitalmodel.infrastructure.config.framework import (
    ConfigLoader,
    SchemaValidator,
    ConfigManager,
)
from digitalmodel.infrastructure.config.models import ConfigModel

__all__ = [
    "ConfigLoader",
    "SchemaValidator",
    "ConfigManager",
    "ConfigModel",
]
