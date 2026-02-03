"""
ABOUTME: Configuration framework for digitalmodel - unified config management
ABOUTME: Provides YAML loading, schema validation, and centralized config access
"""

import logging
from pathlib import Path
from datetime import datetime
from typing import Any, Dict, Optional, List
from abc import ABC, abstractmethod
from copy import deepcopy

try:
    import yaml
except ImportError:
    yaml = None

try:
    import jsonschema
except ImportError:
    jsonschema = None

logger = logging.getLogger(__name__)


class ConfigLoader:
    """Load YAML configuration files with caching support."""

    def __init__(self, cache_ttl: int = 300):
        """
        Initialize configuration loader.

        Args:
            cache_ttl: Cache time-to-live in seconds (default: 300s)
        """
        self.cache_ttl = cache_ttl
        self._cache: Dict[str, tuple[dict, float]] = {}

    def load(self, config_file: Path | str) -> Dict[str, Any]:
        """
        Load configuration from YAML file with caching.

        Args:
            config_file: Path to YAML configuration file

        Returns:
            Configuration dictionary

        Raises:
            FileNotFoundError: If config file doesn't exist
            yaml.YAMLError: If YAML parsing fails
        """
        config_path = Path(config_file).resolve()

        # Check cache
        cache_key = str(config_path)
        if cache_key in self._cache:
            config_dict, cached_time = self._cache[cache_key]
            if datetime.now().timestamp() - cached_time < self.cache_ttl:
                logger.debug(f"Loaded from cache: {config_file}")
                return config_dict

        # Load from file
        if not config_path.exists():
            raise FileNotFoundError(f"Configuration file not found: {config_file}")

        try:
            with open(config_path, 'r') as f:
                config = yaml.safe_load(f)

            # Handle empty YAML files
            if config is None:
                config = {}

            # Cache the result
            self._cache[cache_key] = (config, datetime.now().timestamp())
            logger.info(f"Loaded configuration from: {config_file}")
            return config

        except yaml.YAMLError as e:
            logger.error(f"Failed to parse YAML file {config_file}: {e}")
            raise

    def load_multiple(self, config_files: List[Path | str]) -> Dict[str, Any]:
        """
        Load and merge multiple configuration files.

        Args:
            config_files: List of configuration file paths

        Returns:
            Merged configuration dictionary
        """
        merged_config = {}

        for config_file in config_files:
            config = self.load(config_file)
            merged_config.update(config)

        logger.info(f"Merged {len(config_files)} configuration files")
        return merged_config

    def clear_cache(self):
        """Clear configuration cache."""
        self._cache.clear()
        logger.debug("Configuration cache cleared")


class SchemaValidator:
    """Validate configuration against JSON schema."""

    BASE_SCHEMA = {
        "$schema": "http://json-schema.org/draft-07/schema#",
        "title": "Unified Configuration Schema",
        "type": "object",
        "required": ["metadata"],
        "properties": {
            "metadata": {
                "type": "object",
                "required": ["name", "version"],
                "properties": {
                    "name": {"type": "string"},
                    "version": {"type": "string"},
                    "description": {"type": "string"},
                }
            },
            "configuration": {
                "type": "object",
                "properties": {
                    "aceengineercode": {"type": "object"},
                    "digitalmodel": {"type": "object"},
                    "shared": {"type": "object"}
                }
            },
            "database": {
                "type": "object",
                "properties": {
                    "engine": {
                        "type": "string",
                        "enum": ["sqlite", "postgresql", "mssql"]
                    },
                    "host": {"type": "string"},
                    "port": {"type": "integer"},
                    "database": {"type": "string"},
                    "pool_size": {"type": "integer"},
                    "max_overflow": {"type": "integer"}
                }
            },
            "logging": {
                "type": "object",
                "properties": {
                    "level": {
                        "type": "string",
                        "enum": ["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"]
                    },
                    "handlers": {"type": "array"},
                    "format": {"type": "string"}
                }
            },
            "performance": {
                "type": "object",
                "properties": {
                    "timeout_seconds": {"type": "number"},
                    "max_workers": {"type": "integer"},
                    "cache_ttl": {"type": "integer"}
                }
            }
        }
    }

    def __init__(self):
        """Initialize schema validator."""
        self.schema = deepcopy(self.BASE_SCHEMA)

    def validate(self, config: Dict[str, Any]) -> tuple[bool, List[str]]:
        """
        Validate configuration against schema.

        Args:
            config: Configuration dictionary to validate

        Returns:
            Tuple of (is_valid, error_messages)
        """
        if jsonschema is None:
            logger.warning("jsonschema not installed, skipping validation")
            return True, []

        errors = []

        try:
            jsonschema.validate(instance=config, schema=self.schema)
            return True, []
        except jsonschema.ValidationError as e:
            errors.append(f"Validation error: {e.message}")
            return False, errors
        except jsonschema.SchemaError as e:
            errors.append(f"Schema error: {e.message}")
            return False, errors

    def get_schema(self) -> Dict[str, Any]:
        """Get current schema."""
        return deepcopy(self.schema)

    def save_schema(self, output_file: Path | str):
        """Save schema to JSON file."""
        import json
        output_path = Path(output_file)
        with open(output_path, 'w') as f:
            json.dump(self.schema, f, indent=2)
        logger.info(f"Schema saved to: {output_file}")

    def extend_schema(self, schema_extension: Dict[str, Any]):
        """
        Extend schema with additional properties.

        Args:
            schema_extension: Schema extension dictionary
        """
        if "properties" not in schema_extension:
            return

        for key, value in schema_extension["properties"].items():
            self.schema["properties"][key] = value

        logger.debug(f"Schema extended with {len(schema_extension.get('properties', {}))} properties")

    @staticmethod
    def create_base_schema() -> Dict[str, Any]:
        """Create base configuration schema."""
        return deepcopy(SchemaValidator.BASE_SCHEMA)

    @staticmethod
    def create_backend_schema() -> Dict[str, Any]:
        """Create backend-specific schema."""
        schema = deepcopy(SchemaValidator.BASE_SCHEMA)
        schema["properties"]["backend"] = {
            "type": "object",
            "properties": {
                "api_port": {"type": "integer"},
                "workers": {"type": "integer"},
                "debug": {"type": "boolean"}
            }
        }
        return schema

    @staticmethod
    def create_solver_schema() -> Dict[str, Any]:
        """Create solver-specific schema."""
        schema = deepcopy(SchemaValidator.BASE_SCHEMA)
        schema["properties"]["solvers"] = {
            "type": "object",
            "properties": {
                "timeout_seconds": {"type": "number"},
                "max_parallel": {"type": "integer"},
                "cache_results": {"type": "boolean"}
            }
        }
        return schema


class ConfigManager:
    """Unified configuration manager orchestrating loader and validator."""

    def __init__(
        self,
        config_file: Optional[Path | str] = None,
        schema_file: Optional[Path | str] = None
    ):
        """
        Initialize configuration manager.

        Args:
            config_file: Path to configuration YAML file
            schema_file: Path to optional schema file
        """
        self.config_loader = ConfigLoader()
        self.config_validator = SchemaValidator()
        self.config: Dict[str, Any] = {}
        self.is_valid_config = False

        if config_file:
            self.load_config(config_file)

    def load_config(self, config_file: Path | str) -> bool:
        """
        Load and validate configuration.

        Args:
            config_file: Path to configuration file

        Returns:
            True if successful, False otherwise
        """
        try:
            self.config = self.config_loader.load(config_file)

            if self.config_validator:
                valid, errors = self.config_validator.validate(self.config)
                if not valid:
                    logger.warning(f"Configuration validation warnings: {errors}")
                self.is_valid_config = valid
            else:
                self.is_valid_config = True

            return True

        except Exception as e:
            logger.error(f"Failed to load configuration: {e}")
            self.is_valid_config = False
            return False

    def get(self, key: str, default: Any = None) -> Any:
        """
        Get configuration value by dot-separated key.

        Args:
            key: Dot-separated key (e.g., "database.host")
            default: Default value if key not found

        Returns:
            Configuration value or default
        """
        keys = key.split('.')
        value = self.config

        for k in keys:
            if isinstance(value, dict):
                value = value.get(k)
            else:
                return default

            if value is None:
                return default

        return value

    def set(self, key: str, value: Any):
        """
        Set configuration value by dot-separated key.

        Args:
            key: Dot-separated key (creates nested dicts as needed)
            value: Value to set
        """
        keys = key.split('.')
        config = self.config

        # Create nested dictionaries as needed
        for k in keys[:-1]:
            if k not in config:
                config[k] = {}
            config = config[k]

        config[keys[-1]] = value
        logger.debug(f"Configuration set: {key} = {value}")

    def get_section(self, section: str) -> Dict[str, Any]:
        """
        Get entire configuration section.

        Args:
            section: Section name

        Returns:
            Section dictionary
        """
        return self.config.get(section, {})

    def validate(self) -> tuple[bool, List[str]]:
        """
        Validate current configuration.

        Returns:
            Tuple of (is_valid, error_messages)
        """
        return self.config_validator.validate(self.config)

    def reload(self, config_file: Path | str) -> bool:
        """
        Reload configuration from file.

        Args:
            config_file: Path to configuration file

        Returns:
            True if successful, False otherwise
        """
        self.config_loader.clear_cache()
        return self.load_config(config_file)

    @property
    def is_valid(self) -> bool:
        """Check if current configuration is valid."""
        return self.is_valid_config

    @property
    def size(self) -> int:
        """Get configuration size (number of top-level keys)."""
        return len(self.config)

    def __repr__(self) -> str:
        """String representation."""
        return (
            f"ConfigManager(size={self.size}, valid={self.is_valid}, "
            f"sections={list(self.config.keys())})"
        )
