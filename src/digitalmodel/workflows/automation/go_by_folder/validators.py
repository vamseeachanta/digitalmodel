"""
Validation utilities for Create Go-By Folder Tool
"""

import os
from pathlib import Path
from typing import Dict, Any
import logging

logger = logging.getLogger(__name__)

# Constants
MAX_SOURCE_SIZE = 10 * 1024 * 1024 * 1024  # 10GB
MIN_DISK_SPACE = 100 * 1024 * 1024  # 100MB minimum


def validate_arguments(config: Dict[str, Any]) -> None:
    """
    Validate all arguments and configuration.
    
    Args:
        config: Configuration dictionary
        
    Raises:
        ValueError: If validation fails
    """
    # Validate source folder (unless resuming)
    if not config.get('resume', False):
        source = config.get('source_folder')
        if not source:
            raise ValueError("Source folder is required")
        
        source = Path(source)
        if not source.exists():
            raise ValueError(f"Source folder does not exist: {source}")
        
        if not source.is_dir():
            raise ValueError(f"Source path is not a directory: {source}")
        
        if not os.access(source, os.R_OK):
            raise ValueError(f"Source folder is not readable: {source}")
        
        # Check source size
        source_size = get_folder_size(source)
        if source_size > MAX_SOURCE_SIZE:
            size_gb = source_size / (1024 * 1024 * 1024)
            max_gb = MAX_SOURCE_SIZE / (1024 * 1024 * 1024)
            
            # User confirmation needed for large folders
            logger.warning(
                f"Source folder is {size_gb:.1f}GB (recommended max: {max_gb:.0f}GB). "
                "Processing may take significant time and memory."
            )
            # In production, would prompt user for confirmation here
    
    # Validate target folder
    target = config.get('target_folder')
    if not target:
        raise ValueError("Target folder is required")
    
    target = Path(target)
    
    # Check if target exists and overwrite flag
    if target.exists():
        if not config.get('overwrite', False) and not config.get('resume', False):
            raise ValueError(
                f"Target folder already exists: {target}. "
                "Use --overwrite to replace or --resume to continue"
            )
        
        if config.get('overwrite'):
            logger.warning(f"Target folder will be overwritten: {target}")
    
    # Check parent directory exists and is writable
    parent = target.parent
    if not parent.exists():
        raise ValueError(f"Parent directory does not exist: {parent}")
    
    if not os.access(parent, os.W_OK):
        raise ValueError(f"Cannot write to parent directory: {parent}")
    
    # Check disk space
    available_space = get_available_space(parent)
    if available_space < MIN_DISK_SPACE:
        raise ValueError(
            f"Insufficient disk space. Need at least {MIN_DISK_SPACE / (1024*1024):.0f}MB"
        )
    
    # Validate numeric parameters
    max_file_size = config.get('max_file_size', 10240)
    if max_file_size <= 0:
        raise ValueError("max_file_size must be positive")
    
    # Validate patterns
    if config.get('exclude_patterns'):
        validate_patterns(config['exclude_patterns'])
    
    if config.get('include_patterns'):
        validate_patterns(config['include_patterns'])


def get_folder_size(folder: Path) -> int:
    """
    Calculate total size of folder in bytes.
    
    Args:
        folder: Path to folder
        
    Returns:
        Total size in bytes
    """
    total_size = 0
    
    try:
        for dirpath, dirnames, filenames in os.walk(folder):
            for filename in filenames:
                filepath = Path(dirpath) / filename
                try:
                    # Skip symlinks
                    if not filepath.is_symlink():
                        total_size += filepath.stat().st_size
                except (OSError, PermissionError):
                    # Skip files we can't access
                    continue
    except (OSError, PermissionError) as e:
        logger.warning(f"Could not fully calculate folder size: {e}")
    
    return total_size


def get_available_space(path: Path) -> int:
    """
    Get available disk space at path in bytes.
    
    Args:
        path: Path to check
        
    Returns:
        Available space in bytes
    """
    try:
        import shutil
        stat = shutil.disk_usage(path)
        return stat.free
    except Exception as e:
        logger.warning(f"Could not determine available disk space: {e}")
        # Return a large number to avoid false positives
        return 1024 * 1024 * 1024 * 1024  # 1TB


def validate_patterns(patterns: list) -> None:
    """
    Validate glob patterns.
    
    Args:
        patterns: List of glob patterns
        
    Raises:
        ValueError: If pattern is invalid
    """
    import fnmatch
    
    for pattern in patterns:
        try:
            # Test pattern compilation
            fnmatch.translate(pattern)
        except Exception as e:
            raise ValueError(f"Invalid glob pattern '{pattern}': {e}")


def validate_symlink_handling(path: Path) -> Dict[str, Any]:
    """
    Check for symlinks and return handling decision.
    
    Args:
        path: Path to check
        
    Returns:
        Dictionary with symlink information and handling decision
    """
    symlinks = []
    
    for root, dirs, files in os.walk(path, followlinks=False):
        root_path = Path(root)
        
        # Check directories
        for dirname in dirs:
            dirpath = root_path / dirname
            if dirpath.is_symlink():
                symlinks.append({
                    'path': dirpath,
                    'target': dirpath.resolve(),
                    'type': 'directory'
                })
        
        # Check files
        for filename in files:
            filepath = root_path / filename
            if filepath.is_symlink():
                symlinks.append({
                    'path': filepath,
                    'target': filepath.resolve(),
                    'type': 'file'
                })
    
    return {
        'found': len(symlinks) > 0,
        'count': len(symlinks),
        'symlinks': symlinks,
        'handling': 'prompt'  # Default to prompting user
    }


def validate_checkpoint(checkpoint_path: Path) -> bool:
    """
    Validate checkpoint file integrity.
    
    Args:
        checkpoint_path: Path to checkpoint file
        
    Returns:
        True if checkpoint is valid
    """
    if not checkpoint_path.exists():
        return False
    
    try:
        import json
        with open(checkpoint_path, 'r') as f:
            checkpoint = json.load(f)
        
        # Check required fields
        required = ['version', 'timestamp', 'progress', 'config']
        for field in required:
            if field not in checkpoint:
                logger.error(f"Checkpoint missing required field: {field}")
                return False
        
        return True
        
    except Exception as e:
        logger.error(f"Invalid checkpoint file: {e}")
        return False