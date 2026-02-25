# ABOUTME: Backward compatibility shim for existing config_loader.py usage
# ABOUTME: Wraps ConfigRegistry to maintain API compatibility with legacy code

"""
Configuration Compatibility Layer
==================================

Provides backward-compatible load_config() function that wraps the new
ConfigRegistry system while maintaining the API of the legacy ConfigLoader.

This allows existing code using config_loader.py to work without changes
while benefiting from the new registry features.

Usage (legacy code works unchanged):
    from digitalmodel.infrastructure.config import load_config
    
    config = load_config("path/to/config.yml")
    value = config["default"]["log_level"]
"""

import logging
from pathlib import Path
from typing import Dict, Any, Union

from .registry import ConfigRegistry

logger = logging.getLogger(__name__)

# Global registry instance for compatibility layer
_global_registry: ConfigRegistry = None


def _get_global_registry() -> ConfigRegistry:
    """
    Get or create global ConfigRegistry instance.
    
    Returns:
        ConfigRegistry instance
    """
    global _global_registry
    
    if _global_registry is None:
        _global_registry = ConfigRegistry()
        logger.debug("Created global ConfigRegistry for compatibility layer")
        
    return _global_registry


def load_config(config_path: Union[str, Path]) -> Dict[str, Any]:
    """
    Load configuration from file path (backward compatibility function).
    
    This function mimics the legacy ConfigLoader.load() API while using
    the new ConfigRegistry underneath. Supports all registry features:
    - Environment variable overrides
    - Config inheritance
    - Caching
    
    Args:
        config_path: Path to YAML configuration file
        
    Returns:
        Configuration dictionary
        
    Raises:
        FileNotFoundError: If config file not found
        yaml.YAMLError: If YAML parsing fails
        
    Example:
        >>> config = load_config("base_configs/domains/mooring/mooring.yml")
        >>> log_level = config["default"]["log_level"]
    """
    config_path = Path(config_path)
    
    if not config_path.exists():
        raise FileNotFoundError(f"Config file not found: {config_path}")
    
    # Extract config name from file stem
    config_name = config_path.stem
    
    # Determine base path from config file location
    # Assume structure: .../base_configs/domains/module_name/config.yml
    try:
        # Find "base_configs" in path
        parts = config_path.parts
        if "base_configs" in parts:
            base_idx = parts.index("base_configs")
            base_path = Path(*parts[:base_idx + 1])
        else:
            # Fallback: use parent directory
            base_path = config_path.parent.parent.parent
            
    except (ValueError, IndexError):
        # Fallback: use global registry with default path
        base_path = None
        
    # Create or get registry for this base path
    if base_path is not None:
        registry = ConfigRegistry(config_base_path=base_path)
    else:
        registry = _get_global_registry()
        
    # Load config through registry (gets all benefits: caching, env vars, etc.)
    try:
        config = registry.get_config(config_name)
        logger.info(f"Loaded config '{config_name}' via compatibility layer")
        return config
    except KeyError:
        # Config not in registry, try loading directly from file
        logger.warning(
            f"Config '{config_name}' not found in registry, "
            f"loading directly from {config_path}"
        )
        return registry._load_yaml_file(config_path)


def load_config_direct(config_path: Union[str, Path]) -> Dict[str, Any]:
    """
    Load configuration directly from file without registry features.
    
    Use this for one-off config loading that doesn't need caching or
    env var overrides. Faster for temporary config loads.
    
    Args:
        config_path: Path to YAML configuration file
        
    Returns:
        Configuration dictionary
        
    Raises:
        FileNotFoundError: If config file not found
        yaml.YAMLError: If YAML parsing fails
    """
    registry = _get_global_registry()
    return registry._load_yaml_file(Path(config_path))
