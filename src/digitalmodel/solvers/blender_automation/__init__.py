"""
ABOUTME: Blender Automation Module - Core initialization and exports
Provides Python API wrappers for Blender automation, batch processing, and CAD integration.
"""

from .core.blender_wrapper import BlenderWrapper, BlenderContext
from .core.scene_manager import SceneManager
from .converters.cad_importer import CADImporter
from .converters.mesh_exporter import MeshExporter
from .utils.batch_processor import BatchProcessor
from .utils.file_utils import find_blender_executable, verify_blender_installation

__version__ = "1.0.0"
__author__ = "DigitalModel Team"

__all__ = [
    "BlenderWrapper",
    "BlenderContext",
    "SceneManager",
    "CADImporter",
    "MeshExporter",
    "BatchProcessor",
    "find_blender_executable",
    "verify_blender_installation",
]
