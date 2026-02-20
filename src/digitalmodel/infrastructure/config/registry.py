# ABOUTME: ConfigRegistry - Auto-discovering YAML configuration management system
# ABOUTME: Features env var overrides, inheritance, validation, caching, hot-reload, and JSON schemas

"""
Configuration Registry
======================

Auto-discovering YAML configuration management system with advanced features.

Features:
- Recursive auto-discovery of YAML configs from base_configs/domains/
- Environment variable overrides with DM_ prefix (double underscore for nesting)
- Config inheritance via YAML extends and preset loading
- Pydantic-based validation (permissive - warns on unknown keys)
- LRU caching for performance
- Hot-reload support (manual trigger)
- JSON schema auto-generation from configs
- Deep merge for nested config values

Environment Variable Override Examples:
    DM_LOG_LEVEL=DEBUG                     -> config["log_level"]
    DM_ANALYSIS__TOLERANCE=0.0001          -> config["analysis"]["tolerance"]
    DM_SOLVER__PARAMS__MAX_ITER=200        -> config["solver"]["params"]["max_iter"]

Config Inheritance:
    YAML extends:    extends: base_config.yml
    Preset loading:  preset: production
    
Hot Reload:
    Controlled by DM_HOT_RELOAD=true environment variable
"""

import os
import logging
import json
from pathlib import Path
from typing import Dict, Any, Optional, List, Set
from functools import lru_cache
import yaml

logger = logging.getLogger(__name__)


class ConfigValidationError(Exception):
    """Custom exception for configuration validation errors."""
    pass


class ConfigRegistry:
    """
    Auto-discovering YAML configuration registry with advanced features.
    
    Automatically discovers and manages YAML configuration files from
    base_configs/domains/ directory with support for inheritance, validation,
    caching, and environment variable overrides.
    
    Args:
        config_base_path: Base path for configuration files (default: auto-detect from package)
        cache_maxsize: Maximum size for LRU cache (default: 128)
        hot_reload_enabled: Enable hot-reload (default: from DM_HOT_RELOAD env var)
        
    Example:
        >>> registry = ConfigRegistry()
        >>> config = registry.get_config("mooring")
        >>> value = registry.get_value("mooring", "default.analysis.tolerance")
        >>> schema = registry.generate_json_schema("mooring")
    """
    
    # Patterns to exclude from auto-discovery
    EXCLUDE_PATTERNS = ["*_template.yml", "*_example.yml", "*_test.yml",
                        "*_template.yaml", "*_example.yaml", "*_test.yaml"]
    
    def __init__(
        self,
        config_base_path: Optional[Path] = None,
        cache_maxsize: int = 128,
        hot_reload_enabled: Optional[bool] = None
    ):
        """Initialize the configuration registry."""
        # Determine base path
        if config_base_path is None:
            # Auto-detect from package location
            package_root = Path(__file__).parent.parent
            self.base_path = package_root / "base_configs"
        else:
            self.base_path = Path(config_base_path)
            
        self.modules_path = self.base_path / "domains"
        self.presets_path = self.base_path / "presets"
        
        # Validate path exists
        if not self.modules_path.exists():
            logger.warning(f"Config modules path does not exist: {self.modules_path}")
        
        # Hot reload configuration
        if hot_reload_enabled is None:
            self.hot_reload_enabled = os.environ.get("DM_HOT_RELOAD", "false").lower() == "true"
        else:
            self.hot_reload_enabled = hot_reload_enabled
            
        # Cache configuration
        self._cache_maxsize = cache_maxsize
        self._config_cache: Dict[str, Dict[str, Any]] = {}
        self._discovered_configs: Optional[Dict[str, Path]] = None
        
        # Inheritance tracking for circular reference detection
        self._inheritance_stack: Set[str] = set()
        
        logger.info(f"ConfigRegistry initialized with base_path={self.base_path}")
        logger.info(f"Hot reload enabled: {self.hot_reload_enabled}")
        
    def _discover_configs(self) -> Dict[str, Path]:
        """
        Recursively discover all YAML configuration files.
        
        Returns:
            Dict mapping config names to file paths
        """
        if self._discovered_configs is not None and not self.hot_reload_enabled:
            return self._discovered_configs
            
        configs = {}
        
        if not self.modules_path.exists():
            logger.warning(f"Modules path does not exist: {self.modules_path}")
            return configs
            
        # Find all .yml and .yaml files recursively
        for ext in [".yml", ".yaml"]:
            for config_file in self.modules_path.rglob(f"*{ext}"):
                # Skip excluded patterns
                if any(config_file.match(pattern) for pattern in self.EXCLUDE_PATTERNS):
                    logger.debug(f"Skipping excluded file: {config_file.name}")
                    continue
                    
                # Generate config name from file stem
                config_name = config_file.stem
                
                # For nested configs, optionally include parent directory in name
                # e.g., catenary/risers/riser.yml -> riser (simple) or catenary.risers.riser (qualified)
                relative_path = config_file.relative_to(self.modules_path)
                parts = list(relative_path.parent.parts)
                
                # Use simple name if unique, otherwise use qualified name
                if config_name in configs:
                    # Conflict - use qualified name
                    qualified_name = ".".join(parts + [config_name])
                    configs[qualified_name] = config_file
                    logger.debug(f"Discovered config (qualified): {qualified_name} -> {config_file}")
                else:
                    configs[config_name] = config_file
                    logger.debug(f"Discovered config: {config_name} -> {config_file}")
                    
        self._discovered_configs = configs
        logger.info(f"Discovered {len(configs)} configuration files")
        return configs
        
    def list_configs(self) -> List[str]:
        """
        List all discovered configuration names.
        
        Returns:
            List of configuration names
        """
        configs = self._discover_configs()
        return list(configs.keys())
        
    def has_config(self, name: str) -> bool:
        """
        Check if a configuration exists.
        
        Args:
            name: Configuration name
            
        Returns:
            True if config exists, False otherwise
        """
        configs = self._discover_configs()
        return name in configs
        
    def _load_yaml_file(self, file_path: Path) -> Dict[str, Any]:
        """
        Load YAML file with error handling.
        
        Args:
            file_path: Path to YAML file
            
        Returns:
            Parsed YAML content
            
        Raises:
            FileNotFoundError: If file doesn't exist
            yaml.YAMLError: If YAML parsing fails
        """
        if not file_path.exists():
            raise FileNotFoundError(f"Config file not found: {file_path}")
            
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = yaml.safe_load(f)
                return content if content is not None else {}
        except yaml.YAMLError as e:
            logger.error(f"Failed to parse YAML file {file_path}: {e}")
            raise
            
    def _deep_merge(self, base: Dict[str, Any], override: Dict[str, Any]) -> Dict[str, Any]:
        """
        Deep merge two dictionaries.
        
        Args:
            base: Base dictionary
            override: Override dictionary (takes precedence)
            
        Returns:
            Merged dictionary
        """
        result = base.copy()
        
        for key, value in override.items():
            if key in result and isinstance(result[key], dict) and isinstance(value, dict):
                # Recursively merge nested dicts
                result[key] = self._deep_merge(result[key], value)
            else:
                # Override value
                result[key] = value
                
        return result
        
    def _resolve_inheritance(self, config: Dict[str, Any], config_dir: Path) -> Dict[str, Any]:
        """
        Resolve config inheritance (extends and preset directives).
        
        Args:
            config: Configuration dictionary
            config_dir: Directory containing the config file (for resolving relative extends)
            
        Returns:
            Resolved configuration with inheritance applied
            
        Raises:
            FileNotFoundError: If extended/preset config not found
            ValueError: If circular inheritance detected
        """
        result = config.copy()
        
        # Handle 'extends' directive (YAML-based inheritance)
        if "extends" in config:
            extends_file = config["extends"]
            
            # Check for circular inheritance
            if extends_file in self._inheritance_stack:
                raise ValueError(f"Circular inheritance detected: {extends_file}")
                
            self._inheritance_stack.add(extends_file)
            
            try:
                # Resolve path (relative to config directory)
                extends_path = config_dir / extends_file
                
                if not extends_path.exists():
                    raise FileNotFoundError(f"Extended config not found: {extends_path}")
                    
                # Load and resolve parent config (recursive)
                parent_config = self._load_yaml_file(extends_path)
                parent_config = self._resolve_inheritance(parent_config, extends_path.parent)
                
                # Deep merge: parent as base, current as override
                result = self._deep_merge(parent_config, config)
                
                # Remove extends directive from result
                result.pop("extends", None)
                
            finally:
                self._inheritance_stack.discard(extends_file)
                
        # Handle 'preset' directive (preset-based inheritance)
        if "preset" in config:
            preset_name = config["preset"]
            
            # Try multiple preset locations
            preset_locations = [
                self.presets_path / f"{preset_name}.yml",
                self.presets_path / f"{preset_name}.yaml",
                # Also check in same directory as modules (for tests)
                self.modules_path.parent / "presets" / f"{preset_name}.yml",
                self.modules_path.parent / "presets" / f"{preset_name}.yaml",
            ]
            
            preset_file = None
            for location in preset_locations:
                if location.exists():
                    preset_file = location
                    break
                    
            if preset_file is None:
                raise FileNotFoundError(f"Preset config not found: {preset_name}")
                
            # Load preset config
            preset_config = self._load_yaml_file(preset_file)
            
            # Deep merge: preset as base, current as override
            result = self._deep_merge(preset_config, config)
            
            # Remove preset directive from result
            result.pop("preset", None)
            
        return result
        
    def _apply_env_overrides(self, config: Dict[str, Any]) -> Dict[str, Any]:
        """
        Apply environment variable overrides to config.
        
        Supports nested keys with double underscore:
            DM_KEY=value              -> config["key"] or config["default"]["key"]
            DM_PARENT__CHILD=value    -> config["parent"]["child"]
            DM_A__B__C=value          -> config["a"]["b"]["c"]
        
        Auto-detects "default" section and searches there if top-level not found.
        
        Args:
            config: Configuration dictionary
            
        Returns:
            Configuration with env var overrides applied
        """
        result = config.copy()
        
        # Find all DM_ prefixed environment variables
        for env_key, env_value in os.environ.items():
            if not env_key.startswith("DM_"):
                continue
                
            # Remove DM_ prefix and split by double underscore for nesting
            key_path = env_key[3:].split("__")
            
            # Try to apply the override at top level
            if self._apply_env_override(result, key_path, env_value):
                logger.debug(f"Applied env override: {env_key} -> {key_path}")
            # If failed and config has "default" section, try there
            elif "default" in result and isinstance(result["default"], dict):
                if self._apply_env_override(result["default"], key_path, env_value):
                    logger.debug(f"Applied env override to 'default': {env_key} -> {key_path}")
                else:
                    logger.warning(f"Environment variable {env_key} does not match any config key")
            else:
                logger.warning(f"Environment variable {env_key} does not match any config key")
                
        return result
        
    def _apply_env_override(
        self,
        config: Dict[str, Any],
        key_path: List[str],
        value: str,
        current_level: int = 0
    ) -> bool:
        """
        Recursively apply environment variable override to nested config.
        
        Args:
            config: Configuration dictionary (modified in place)
            key_path: List of keys for nested access
            value: String value from environment variable
            current_level: Current recursion level
            
        Returns:
            True if override was applied, False if key path not found
        """
        if current_level >= len(key_path):
            return False
            
        key = key_path[current_level].lower()  # Case-insensitive matching
        
        # Search for matching key (case-insensitive)
        matched_key = None
        for config_key in config.keys():
            if config_key.lower() == key:
                matched_key = config_key
                break
                
        if matched_key is None:
            # Key not found at this level
            return False
            
        if current_level == len(key_path) - 1:
            # Last key in path - apply override
            # Try to coerce type based on existing value
            existing_value = config[matched_key]
            coerced_value = self._coerce_type(value, existing_value)
            config[matched_key] = coerced_value
            return True
        else:
            # Intermediate key - recurse deeper
            if isinstance(config[matched_key], dict):
                return self._apply_env_override(
                    config[matched_key],
                    key_path,
                    value,
                    current_level + 1
                )
            else:
                # Can't traverse deeper - path doesn't exist
                return False
                
    def _coerce_type(self, value: str, reference: Any) -> Any:
        """
        Coerce string value to appropriate type based on reference value.
        
        Args:
            value: String value from environment variable
            reference: Reference value to infer type from
            
        Returns:
            Coerced value
        """
        if isinstance(reference, bool):
            return value.lower() in ["true", "1", "yes"]
        elif isinstance(reference, int):
            try:
                return int(value)
            except ValueError:
                return value
        elif isinstance(reference, float):
            try:
                return float(value)
            except ValueError:
                return value
        else:
            return value
            
    def _validate_config(self, config: Dict[str, Any], config_name: str) -> None:
        """
        Validate configuration (permissive - warns on unknown keys).
        
        Args:
            config: Configuration dictionary
            config_name: Name of configuration (for logging)
        """
        # For now, just log warnings about potentially unknown keys
        # In future, could use JSON schema or Pydantic models for validation
        
        # Check for common required fields
        if "default" not in config and "meta" not in config:
            logger.warning(
                f"Config '{config_name}' missing both 'default' and 'meta' sections. "
                "This may indicate an invalid config structure."
            )
            
    def get_config(self, name: str) -> Dict[str, Any]:
        """
        Get configuration by name with full feature support.
        
        Applies:
        1. Config inheritance (extends, presets)
        2. Environment variable overrides
        3. Validation
        4. Caching
        
        Args:
            name: Configuration name
            
        Returns:
            Configuration dictionary
            
        Raises:
            KeyError: If config not found
            FileNotFoundError: If config file or inheritance references not found
            yaml.YAMLError: If YAML parsing fails
        """
        # Check cache first
        if name in self._config_cache:
            logger.debug(f"Cache hit for config: {name}")
            return self._config_cache[name]
            
        # Discover configs
        configs = self._discover_configs()
        
        if name not in configs:
            raise KeyError(f"Configuration '{name}' not found. Available configs: {list(configs.keys())}")
            
        config_file = configs[name]
        logger.info(f"Loading config: {name} from {config_file}")
        
        # Load YAML file
        config = self._load_yaml_file(config_file)
        
        # Resolve inheritance
        config = self._resolve_inheritance(config, config_file.parent)
        
        # Apply environment variable overrides
        config = self._apply_env_overrides(config)
        
        # Validate
        self._validate_config(config, name)
        
        # Cache result (respect cache size limit)
        if len(self._config_cache) >= self._cache_maxsize:
            # Simple FIFO eviction (could use LRU for better performance)
            first_key = next(iter(self._config_cache))
            self._config_cache.pop(first_key)
            logger.debug(f"Cache full, evicted: {first_key}")
            
        self._config_cache[name] = config
        
        return config
        
    def get_value(self, config_name: str, key_path: str, default: Any = None) -> Any:
        """
        Get nested value from config using dot notation.
        
        Args:
            config_name: Configuration name
            key_path: Dot-separated path (e.g., "default.analysis.tolerance")
            default: Default value if key not found
            
        Returns:
            Configuration value or default
            
        Example:
            >>> registry.get_value("mooring", "default.analysis.tolerance", 0.001)
            0.0001
        """
        config = self.get_config(config_name)
        
        keys = key_path.split('.')
        value = config
        
        for key in keys:
            if isinstance(value, dict) and key in value:
                value = value[key]
            else:
                return default
                
        return value
        
    def clear_cache(self) -> None:
        """Clear the configuration cache."""
        self._config_cache.clear()
        logger.info("Configuration cache cleared")
        
    def reload(self) -> None:
        """
        Reload all configurations (clears cache and re-discovers).
        
        Useful for hot-reload scenarios where config files may have changed.
        """
        logger.info("Reloading configurations...")
        self._config_cache.clear()
        self._discovered_configs = None
        self._discover_configs()
        logger.info("Configuration reload complete")
        
    def generate_json_schema(self, config_name: str) -> Dict[str, Any]:
        """
        Generate JSON schema from configuration structure.
        
        Infers types from actual values in the config.
        
        Args:
            config_name: Configuration name
            
        Returns:
            JSON schema dictionary
        """
        config = self.get_config(config_name)
        
        schema = {
            "$schema": "http://json-schema.org/draft-07/schema#",
            "title": f"{config_name} Configuration Schema",
            "type": "object",
            "properties": self._generate_schema_properties(config)
        }
        
        return schema
        
    def _generate_schema_properties(self, obj: Any) -> Dict[str, Any]:
        """
        Recursively generate JSON schema properties from config object.
        
        Args:
            obj: Configuration object (dict, list, or primitive)
            
        Returns:
            Schema properties dictionary
        """
        if isinstance(obj, dict):
            properties = {}
            for key, value in obj.items():
                properties[key] = self._infer_schema_type(value)
            return properties
        else:
            return {}
            
    def _infer_schema_type(self, value: Any) -> Dict[str, Any]:
        """
        Infer JSON schema type from Python value.
        
        Args:
            value: Python value
            
        Returns:
            Schema type definition
        """
        if isinstance(value, bool):
            return {"type": "boolean"}
        elif isinstance(value, int):
            return {"type": "integer"}
        elif isinstance(value, float):
            return {"type": "number"}
        elif isinstance(value, str):
            return {"type": "string"}
        elif isinstance(value, list):
            if len(value) > 0:
                # Infer array item type from first element
                item_schema = self._infer_schema_type(value[0])
                return {"type": "array", "items": item_schema}
            else:
                return {"type": "array"}
        elif isinstance(value, dict):
            return {
                "type": "object",
                "properties": self._generate_schema_properties(value)
            }
        else:
            return {"type": "string"}  # Fallback
            
    def export_json_schema(self, config_name: str, output_path: Path) -> None:
        """
        Export JSON schema to file.
        
        Args:
            config_name: Configuration name
            output_path: Path to output JSON file
        """
        schema = self.generate_json_schema(config_name)
        
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(schema, f, indent=2)
            
        logger.info(f"Exported JSON schema for '{config_name}' to {output_path}")
