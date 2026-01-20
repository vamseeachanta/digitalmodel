"""
ABOUTME: Blender Python API wrapper with context management
Provides safe, context-managed access to Blender operations with error handling.
"""

import subprocess
import sys
from pathlib import Path
from typing import Optional, List, Dict, Any
from contextlib import contextmanager
import json


class BlenderWrapper:
    """
    Wrapper for Blender operations with subprocess-based execution.

    Provides a safe interface for running Blender scripts without direct
    bpy import (which requires running inside Blender).
    """

    def __init__(self, blender_path: Optional[str] = None):
        """
        Initialize Blender wrapper.

        Args:
            blender_path: Path to Blender executable. If None, uses system default.
        """
        self.blender_path = blender_path or "blender"
        self._verify_installation()

    def _verify_installation(self) -> None:
        """Verify Blender is installed and accessible."""
        try:
            result = subprocess.run(
                [self.blender_path, "--version"],
                capture_output=True,
                text=True,
                check=True
            )
            self.version = result.stdout.split('\n')[0]
        except (subprocess.CalledProcessError, FileNotFoundError) as e:
            raise RuntimeError(f"Blender not found or not accessible: {e}")

    def run_script(
        self,
        script: str,
        background: bool = True,
        blend_file: Optional[Path] = None,
        output_file: Optional[Path] = None
    ) -> Dict[str, Any]:
        """
        Execute a Python script in Blender.

        Args:
            script: Python script content or path to script file
            background: Run in background mode (no UI)
            blend_file: Optional .blend file to open
            output_file: Optional path to save output .blend file

        Returns:
            Dictionary with execution results
        """
        cmd = [self.blender_path]

        if background:
            cmd.append("--background")

        if blend_file:
            cmd.append(str(blend_file))

        cmd.extend(["--python-expr", script])

        if output_file:
            cmd.extend(["--render-output", str(output_file)])

        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                check=True
            )
            return {
                "success": True,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "returncode": result.returncode
            }
        except subprocess.CalledProcessError as e:
            return {
                "success": False,
                "stdout": e.stdout,
                "stderr": e.stderr,
                "returncode": e.returncode,
                "error": str(e)
            }

    def execute_command(
        self,
        commands: List[str],
        background: bool = True,
        blend_file: Optional[Path] = None
    ) -> Dict[str, Any]:
        """
        Execute multiple Blender commands.

        Args:
            commands: List of Python commands to execute
            background: Run in background mode
            blend_file: Optional .blend file to open

        Returns:
            Dictionary with execution results
        """
        script = "\n".join(commands)
        return self.run_script(script, background, blend_file)

    def import_file(
        self,
        file_path: Path,
        file_format: str,
        output_blend: Optional[Path] = None
    ) -> Dict[str, Any]:
        """
        Import a file into Blender.

        Args:
            file_path: Path to file to import
            file_format: File format (obj, fbx, stl, dae, ply, x3d)
            output_blend: Optional path to save .blend file

        Returns:
            Dictionary with import results
        """
        import_functions = {
            "obj": "bpy.ops.wm.obj_import",
            "fbx": "bpy.ops.import_scene.fbx",
            "stl": "bpy.ops.wm.stl_import",
            "dae": "bpy.ops.wm.collada_import",
            "ply": "bpy.ops.wm.ply_import",
            "x3d": "bpy.ops.import_scene.x3d",
        }

        format_lower = file_format.lower().lstrip('.')
        if format_lower not in import_functions:
            raise ValueError(f"Unsupported format: {file_format}")

        script = f"""
import bpy

# Clear existing scene
bpy.ops.wm.read_factory_settings(use_empty=True)

# Import file
{import_functions[format_lower]}(filepath='{file_path}')

# Save if output specified
"""
        if output_blend:
            script += f"\nbpy.ops.wm.save_as_mainfile(filepath='{output_blend}')"

        return self.run_script(script, background=True)

    def export_file(
        self,
        blend_file: Path,
        output_path: Path,
        file_format: str,
        **export_options
    ) -> Dict[str, Any]:
        """
        Export Blender file to different format.

        Args:
            blend_file: Input .blend file
            output_path: Output file path
            file_format: Export format (obj, fbx, stl, dae, ply, x3d, gltf)
            **export_options: Additional format-specific options

        Returns:
            Dictionary with export results
        """
        export_functions = {
            "obj": "bpy.ops.wm.obj_export",
            "fbx": "bpy.ops.export_scene.fbx",
            "stl": "bpy.ops.wm.stl_export",
            "dae": "bpy.ops.wm.collada_export",
            "ply": "bpy.ops.wm.ply_export",
            "x3d": "bpy.ops.export_scene.x3d",
            "gltf": "bpy.ops.export_scene.gltf",
        }

        format_lower = file_format.lower().lstrip('.')
        if format_lower not in export_functions:
            raise ValueError(f"Unsupported format: {file_format}")

        # Build options string
        options_str = f"filepath='{output_path}'"
        for key, value in export_options.items():
            if isinstance(value, str):
                options_str += f", {key}='{value}'"
            else:
                options_str += f", {key}={value}"

        script = f"""
import bpy

# Export file
{export_functions[format_lower]}({options_str})
"""

        return self.run_script(script, background=True, blend_file=blend_file)


@contextmanager
def BlenderContext(
    blend_file: Optional[Path] = None,
    blender_path: Optional[str] = None
):
    """
    Context manager for Blender operations.

    Usage:
        with BlenderContext() as blender:
            blender.import_file("model.obj", "obj")

    Args:
        blend_file: Optional .blend file to work with
        blender_path: Optional path to Blender executable

    Yields:
        BlenderWrapper instance
    """
    wrapper = BlenderWrapper(blender_path)
    try:
        yield wrapper
    finally:
        pass  # Cleanup if needed
