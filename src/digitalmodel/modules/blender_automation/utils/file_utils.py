"""
ABOUTME: File utility functions for Blender automation
Provides file discovery, validation, and system integration utilities.
"""

import subprocess
import shutil
from pathlib import Path
from typing import Optional, List
import logging


logger = logging.getLogger(__name__)


def find_blender_executable() -> Optional[str]:
    """
    Find Blender executable on the system.

    Returns:
        Path to Blender executable, or None if not found
    """
    # Try common installation paths
    common_paths = [
        "blender",  # System PATH
        "/usr/bin/blender",  # Linux
        "/usr/local/bin/blender",  # Linux
        "C:/Program Files/Blender Foundation/Blender/blender.exe",  # Windows
        "C:/Program Files/Blender Foundation/Blender 4.3/blender.exe",
        "C:/Program Files/Blender Foundation/Blender 5.0/blender.exe",
        "/Applications/Blender.app/Contents/MacOS/Blender",  # macOS
    ]

    for path in common_paths:
        if shutil.which(path):
            return path
        if Path(path).exists():
            return path

    # Try which/where command
    try:
        result = subprocess.run(
            ["which", "blender"],
            capture_output=True,
            text=True
        )
        if result.returncode == 0:
            return result.stdout.strip()
    except FileNotFoundError:
        pass

    # Try Windows where command
    try:
        result = subprocess.run(
            ["where", "blender"],
            capture_output=True,
            text=True
        )
        if result.returncode == 0:
            return result.stdout.strip().split('\n')[0]
    except FileNotFoundError:
        pass

    return None


def verify_blender_installation() -> dict:
    """
    Verify Blender installation and get version information.

    Returns:
        Dictionary with installation status and version info
    """
    blender_path = find_blender_executable()

    if not blender_path:
        return {
            "installed": False,
            "path": None,
            "version": None,
            "error": "Blender not found on system"
        }

    try:
        result = subprocess.run(
            [blender_path, "--version"],
            capture_output=True,
            text=True,
            check=True
        )

        version_line = result.stdout.split('\n')[0]

        return {
            "installed": True,
            "path": blender_path,
            "version": version_line,
            "output": result.stdout
        }

    except (subprocess.CalledProcessError, FileNotFoundError) as e:
        return {
            "installed": False,
            "path": blender_path,
            "version": None,
            "error": str(e)
        }


def find_blend_files(
    directory: Path,
    recursive: bool = True
) -> List[Path]:
    """
    Find all .blend files in directory.

    Args:
        directory: Directory to search
        recursive: Search subdirectories

    Returns:
        List of .blend file paths
    """
    directory = Path(directory)

    if not directory.exists():
        raise FileNotFoundError(f"Directory not found: {directory}")

    if recursive:
        return list(directory.rglob("*.blend"))
    else:
        return list(directory.glob("*.blend"))


def find_cad_files(
    directory: Path,
    formats: Optional[List[str]] = None,
    recursive: bool = True
) -> List[Path]:
    """
    Find CAD files in directory.

    Args:
        directory: Directory to search
        formats: List of formats to search for. Default: common CAD formats
        recursive: Search subdirectories

    Returns:
        List of CAD file paths
    """
    if formats is None:
        formats = ["step", "stp", "iges", "igs", "stl", "obj", "fbx"]

    directory = Path(directory)

    if not directory.exists():
        raise FileNotFoundError(f"Directory not found: {directory}")

    files = []

    for fmt in formats:
        if recursive:
            files.extend(directory.rglob(f"*.{fmt}"))
        else:
            files.extend(directory.glob(f"*.{fmt}"))

    return sorted(files)


def get_file_info(file_path: Path) -> dict:
    """
    Get information about a file.

    Args:
        file_path: File path

    Returns:
        Dictionary with file information
    """
    file_path = Path(file_path)

    if not file_path.exists():
        raise FileNotFoundError(f"File not found: {file_path}")

    stat = file_path.stat()

    return {
        "name": file_path.name,
        "stem": file_path.stem,
        "suffix": file_path.suffix,
        "size_bytes": stat.st_size,
        "size_mb": stat.st_size / (1024 * 1024),
        "modified": stat.st_mtime,
        "absolute_path": str(file_path.absolute())
    }


def validate_file_format(file_path: Path, expected_format: str) -> bool:
    """
    Validate file has expected format.

    Args:
        file_path: File path
        expected_format: Expected format (without dot)

    Returns:
        True if format matches
    """
    file_path = Path(file_path)
    actual_format = file_path.suffix.lower().lstrip('.')
    expected = expected_format.lower().lstrip('.')

    return actual_format == expected


def create_output_path(
    input_path: Path,
    output_dir: Path,
    new_suffix: str,
    preserve_structure: bool = False
) -> Path:
    """
    Create output path based on input path.

    Args:
        input_path: Input file path
        output_dir: Output directory
        new_suffix: New file suffix/extension
        preserve_structure: Preserve directory structure

    Returns:
        Output file path
    """
    input_path = Path(input_path)
    output_dir = Path(output_dir)

    if not new_suffix.startswith('.'):
        new_suffix = f".{new_suffix}"

    if preserve_structure and input_path.parent != Path('.'):
        # Recreate directory structure
        relative_dir = input_path.parent
        output_subdir = output_dir / relative_dir
        output_subdir.mkdir(parents=True, exist_ok=True)
        return output_subdir / f"{input_path.stem}{new_suffix}"
    else:
        # Flat output directory
        output_dir.mkdir(parents=True, exist_ok=True)
        return output_dir / f"{input_path.stem}{new_suffix}"
