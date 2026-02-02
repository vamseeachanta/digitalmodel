"""
ABOUTME: Centralized configuration management for digitalmodel
ABOUTME: Provides Pydantic-based settings with environment variable support and YAML config registry
"""

from .settings import GlobalSettings, get_settings, override_settings, reset_settings
from .registry import ConfigRegistry, ConfigValidationError
from .compat import load_config

__all__ = [
    'GlobalSettings',
    'get_settings',
    'override_settings',
    'reset_settings',
    'ConfigRegistry',
    'ConfigValidationError',
    'load_config',
]

__version__ = '1.0.0'
