"""
Path utilities for digitalmodel modules.
Provides consistent path resolution across all modules.
"""

from assetutilities.common.path_resolver import PathResolver


def get_output_directory(cfg, default="output"):
    """
    Get the output directory from config using consistent resolution.
    
    Args:
        cfg: Configuration dictionary
        default: Default path if none found in config
        
    Returns:
        Resolved absolute path to output directory
    """
    return PathResolver.resolve_output_directory(cfg, fallback=default)


def resolve_config_path(path, cfg):
    """
    Resolve a path relative to config directory if available.
    
    Args:
        path: Path to resolve (can be relative or absolute)
        cfg: Configuration dictionary
        
    Returns:
        Resolved absolute path
    """
    return PathResolver.resolve_path(path, cfg)


def ensure_directory_exists(path, cfg=None):
    """
    Ensure a directory exists, creating it if necessary.
    Resolves the path first if cfg is provided.
    
    Args:
        path: Path to directory
        cfg: Configuration dictionary (optional)
        
    Returns:
        Resolved absolute path to directory
    """
    import os
    
    if cfg:
        path = PathResolver.resolve_path(path, cfg)
    
    os.makedirs(path, exist_ok=True)
    return path
