# ABOUTME: YAML configuration loader for data procurement
# ABOUTME: Validates config schemas, resolves environment variables, and manages presets

"""
Configuration Loader
====================

YAML configuration parser for data procurement.

Features:
- Load and validate YAML configs
- Environment variable substitution
- Preset management
- Schema validation
- Config inheritance (extends directive)
"""

import logging
import os
from typing import Dict, Any, Optional
from pathlib import Path
import yaml

logger = logging.getLogger(__name__)


class ConfigLoader:
    """
    YAML configuration loader with validation.

    Supports:
    - Environment variable substitution (${ENV_VAR})
    - Config inheritance (extends: @path/to/base.yml)
    - Preset loading (active_preset: preset_name)
    - Schema validation
    """

    def __init__(self, config_path: str):
        """
        Initialize config loader.

        Args:
            config_path: Path to YAML configuration file
        """
        self.config_path = Path(config_path)
        if not self.config_path.exists():
            raise FileNotFoundError(f"Config file not found: {config_path}")

        self.config: Dict[str, Any] = {}
        self._load()
        logger.info(f"Loaded configuration from {config_path}")

    def _load(self) -> None:
        """Load and parse YAML configuration."""
        with open(self.config_path, 'r') as f:
            self.config = yaml.safe_load(f)

        # Handle inheritance
        if 'extends' in self.config:
            self._inherit_config(self.config['extends'])

        # Handle presets
        if 'active_preset' in self.config:
            self._apply_preset(self.config['active_preset'])

        # Substitute environment variables
        self._substitute_env_vars(self.config)

    def _inherit_config(self, parent_path: str) -> None:
        """
        Inherit configuration from parent file.

        Args:
            parent_path: Path to parent config (supports @specs/... syntax)
        """
        # Resolve @specs/... path
        if parent_path.startswith('@'):
            parent_path = parent_path[1:]  # Remove @
            # Resolve relative to repo root or current dir
            parent_path = Path(parent_path)

        if not Path(parent_path).exists():
            logger.warning(f"Parent config not found: {parent_path}, skipping inheritance")
            return

        with open(parent_path, 'r') as f:
            parent_config = yaml.safe_load(f)

        # Merge parent into current (current overrides parent)
        self.config = self._deep_merge(parent_config, self.config)
        logger.debug(f"Inherited config from {parent_path}")

    def _apply_preset(self, preset_name: str) -> None:
        """
        Apply preset configuration.

        Args:
            preset_name: Name of preset in presets section
        """
        if 'presets' not in self.config or preset_name not in self.config['presets']:
            logger.warning(f"Preset '{preset_name}' not found in config")
            return

        preset = self.config['presets'][preset_name]

        # Merge preset into root config
        for key, value in preset.items():
            if key not in self.config:
                self.config[key] = value

        logger.debug(f"Applied preset: {preset_name}")

    def _substitute_env_vars(self, obj: Any) -> None:
        """
        Recursively substitute environment variables.

        Replaces ${ENV_VAR} with os.environ['ENV_VAR'].

        Args:
            obj: Object to process (dict, list, or str)
        """
        if isinstance(obj, dict):
            for key, value in obj.items():
                if isinstance(value, str) and value.startswith('${') and value.endswith('}'):
                    env_var = value[2:-1]
                    obj[key] = os.environ.get(env_var, value)
                    if obj[key] == value:
                        logger.warning(f"Environment variable {env_var} not set")
                else:
                    self._substitute_env_vars(value)

        elif isinstance(obj, list):
            for i, item in enumerate(obj):
                if isinstance(item, str) and item.startswith('${') and item.endswith('}'):
                    env_var = item[2:-1]
                    obj[i] = os.environ.get(env_var, item)
                else:
                    self._substitute_env_vars(item)

    def _deep_merge(self, base: Dict[str, Any], override: Dict[str, Any]) -> Dict[str, Any]:
        """
        Deep merge two dictionaries (override takes precedence).

        Args:
            base: Base dictionary
            override: Override dictionary

        Returns:
            Merged dictionary
        """
        result = base.copy()

        for key, value in override.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                result[key] = self._deep_merge(result[key], value)
            else:
                result[key] = value

        return result

    def get(self, key: str, default: Any = None) -> Any:
        """
        Get configuration value.

        Supports dot notation: 'apis.ERA5.base_url'

        Args:
            key: Configuration key (supports dot notation)
            default: Default value if key not found

        Returns:
            Configuration value
        """
        keys = key.split('.')
        value = self.config

        for k in keys:
            if isinstance(value, dict) and k in value:
                value = value[k]
            else:
                return default

        return value

    def get_api_config(self, provider: str) -> Dict[str, Any]:
        """
        Get API configuration for provider.

        Args:
            provider: API provider name (ERA5, NOAA, etc.)

        Returns:
            API configuration dict
        """
        return self.get(f'apis.{provider}', {})

    def get_output_config(self) -> Dict[str, Any]:
        """Get output configuration."""
        return self.get('output', {})

    def get_caching_config(self) -> Dict[str, Any]:
        """Get caching configuration."""
        return self.get('caching', {})

    def get_validation_config(self) -> Dict[str, Any]:
        """Get validation configuration."""
        return self.get('validation', {})

    def validate_schema(self) -> bool:
        """
        Validate configuration against schema.

        Returns:
            True if valid, False otherwise
        """
        # Required fields check
        required_fields = ['version', 'data_source']

        for field in required_fields:
            if field not in self.config:
                logger.error(f"Missing required field: {field}")
                return False

        # Version check
        if not self.config['version'].startswith('1.'):
            logger.warning(f"Unsupported config version: {self.config['version']}")

        logger.info("Configuration schema validation passed")
        return True

    def to_dict(self) -> Dict[str, Any]:
        """
        Get full configuration as dictionary.

        Returns:
            Configuration dict
        """
        return self.config.copy()

    def __getitem__(self, key: str) -> Any:
        """Allow dict-style access."""
        return self.get(key)

    def __contains__(self, key: str) -> bool:
        """Check if key exists."""
        return self.get(key) is not None
