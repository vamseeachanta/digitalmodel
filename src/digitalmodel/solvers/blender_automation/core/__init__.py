"""
ABOUTME: Core Blender automation components
Provides base classes and context managers for Blender operations.
"""

from .blender_wrapper import BlenderWrapper, BlenderContext
from .scene_manager import SceneManager

__all__ = ["BlenderWrapper", "BlenderContext", "SceneManager"]
