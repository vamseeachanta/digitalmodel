"""
ABOUTME: Utility functions for modular input validation
ABOUTME: Helper functions for file operations, parameter extraction, and comparisons
"""

from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple
import yaml


def load_yaml_file(file_path: Path) -> Tuple[bool, Optional[Dict], Optional[str]]:
    """
    Load and parse YAML file.

    Args:
        file_path: Path to YAML file

    Returns:
        Tuple of (success, content, error_message)
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = yaml.safe_load(f)
        return True, content, None
    except yaml.YAMLError as e:
        return False, None, f"YAML syntax error: {str(e)}"
    except FileNotFoundError:
        return False, None, f"File not found: {file_path}"
    except Exception as e:
        return False, None, f"Error reading file: {str(e)}"


def extract_includefiles(content: Dict) -> List[str]:
    """
    Recursively extract all includefile references from YAML content.

    Args:
        content: Parsed YAML content

    Returns:
        List of includefile paths
    """
    includefiles = []

    def traverse(obj):
        if isinstance(obj, dict):
            for key, value in obj.items():
                if key == 'includefile' and isinstance(value, str):
                    includefiles.append(value)
                else:
                    traverse(value)
        elif isinstance(obj, list):
            for item in obj:
                traverse(item)

    if content:
        traverse(content)

    return includefiles


def calculate_percentage_difference(actual: float, expected: float) -> float:
    """
    Calculate percentage difference between actual and expected values.

    Args:
        actual: Actual value
        expected: Expected value

    Returns:
        Percentage difference (positive if actual > expected)
    """
    if expected == 0:
        return 100.0 if actual != 0 else 0.0

    return ((actual - expected) / abs(expected)) * 100.0


def is_within_tolerance(actual: float, expected: float, tolerance_percent: float) -> bool:
    """
    Check if actual value is within tolerance of expected value.

    Args:
        actual: Actual value
        expected: Expected value
        tolerance_percent: Tolerance percentage (e.g., 10 for ±10%)

    Returns:
        True if within tolerance
    """
    diff_percent = abs(calculate_percentage_difference(actual, expected))
    return diff_percent <= tolerance_percent


def is_within_range(value: float, min_value: Optional[float], max_value: Optional[float]) -> bool:
    """
    Check if value is within specified range.

    Args:
        value: Value to check
        min_value: Minimum acceptable value (None for no minimum)
        max_value: Maximum acceptable value (None for no maximum)

    Returns:
        True if within range
    """
    if min_value is not None and value < min_value:
        return False
    if max_value is not None and value > max_value:
        return False
    return True


def resolve_include_path(base_file: Path, include_file: str) -> Path:
    """
    Resolve includefile path relative to base file.

    Args:
        base_file: Path to base YAML file
        include_file: Includefile reference (relative or absolute)

    Returns:
        Resolved absolute path
    """
    include_path = Path(include_file)

    # If absolute, return as-is
    if include_path.is_absolute():
        return include_path

    # Resolve relative to base file directory
    return (base_file.parent / include_path).resolve()


def extract_parameter_from_yaml(content: Dict, path: str, default: Any = None) -> Any:
    """
    Extract parameter from nested YAML structure using dot notation.

    Args:
        content: Parsed YAML content
        path: Parameter path (e.g., "Environment.WaveHs")
        default: Default value if path not found

    Returns:
        Parameter value or default
    """
    parts = path.split('.')
    current = content

    for part in parts:
        if isinstance(current, dict) and part in current:
            current = current[part]
        else:
            return default

    return current


def format_file_size(size_bytes: int) -> str:
    """
    Format file size in human-readable format.

    Args:
        size_bytes: Size in bytes

    Returns:
        Formatted string (e.g., "1.5 MB")
    """
    for unit in ['B', 'KB', 'MB', 'GB']:
        if size_bytes < 1024.0:
            return f"{size_bytes:.1f} {unit}"
        size_bytes /= 1024.0
    return f"{size_bytes:.1f} TB"


def sanitize_filename(filename: str) -> str:
    """
    Sanitize filename for safe file system usage.

    Args:
        filename: Original filename

    Returns:
        Sanitized filename
    """
    # Remove or replace invalid characters
    invalid_chars = '<>:"/\\|?*'
    for char in invalid_chars:
        filename = filename.replace(char, '_')

    # Limit length
    max_length = 255
    if len(filename) > max_length:
        name, ext = filename.rsplit('.', 1) if '.' in filename else (filename, '')
        name = name[:max_length - len(ext) - 1]
        filename = f"{name}.{ext}" if ext else name

    return filename


def merge_dicts_deep(base: Dict, override: Dict) -> Dict:
    """
    Deep merge two dictionaries.

    Args:
        base: Base dictionary
        override: Override dictionary

    Returns:
        Merged dictionary
    """
    result = base.copy()

    for key, value in override.items():
        if key in result and isinstance(result[key], dict) and isinstance(value, dict):
            result[key] = merge_dicts_deep(result[key], value)
        else:
            result[key] = value

    return result
