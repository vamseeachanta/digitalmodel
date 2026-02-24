"""
ABOUTME: Centralized configuration management for digitalmodel
ABOUTME: Provides Pydantic-based settings with environment variable support and YAML config registry
ABOUTME: Phase 2A (WRK-415): also consolidates ConfigLoader/SchemaValidator/ConfigManager/ConfigModel
"""

from .settings import GlobalSettings, get_settings, override_settings, reset_settings
from .registry import ConfigRegistry, ConfigValidationError
from .compat import load_config
from .framework import ConfigLoader, SchemaValidator, ConfigManager
from .models import ConfigModel

__all__ = [
    'GlobalSettings',
    'get_settings',
    'override_settings',
    'reset_settings',
    'ConfigRegistry',
    'ConfigValidationError',
    'load_config',
    # Consolidated from base_configs/ (Phase 2A)
    'ConfigLoader',
    'SchemaValidator',
    'ConfigManager',
    'ConfigModel',
]

__version__ = '1.0.0'
