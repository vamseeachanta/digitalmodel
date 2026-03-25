"""
ABOUTME: Scene management utilities for Blender
Provides high-level scene manipulation, object management, and camera/lighting setup.
"""

from pathlib import Path
from typing import Optional, List, Dict, Any, Tuple
from .blender_wrapper import BlenderWrapper


class SceneManager:
    """
    High-level scene management for Blender.

    Provides utilities for managing objects, materials, cameras, and lighting
    without direct bpy access.
    """

    def __init__(self, blender_wrapper: Optional[BlenderWrapper] = None):
        """
        Initialize scene manager.

        Args:
            blender_wrapper: BlenderWrapper instance. Creates new one if None.
        """
        self.blender = blender_wrapper or BlenderWrapper()

    def create_empty_scene(self, output_file: Optional[Path] = None) -> Dict[str, Any]:
        """
        Create a new empty Blender scene.

        Args:
            output_file: Optional path to save the .blend file

        Returns:
            Dictionary with operation results
        """
        script = """
import bpy

# Clear all existing data
bpy.ops.wm.read_factory_settings(use_empty=True)

# Create basic scene with camera and light
bpy.ops.object.camera_add(location=(0, -10, 5))
bpy.ops.object.light_add(type='SUN', location=(0, 0, 10))
"""

        if output_file:
            script += f"\nbpy.ops.wm.save_as_mainfile(filepath='{output_file}')"

        return self.blender.run_script(script, background=True)

    def add_object(
        self,
        blend_file: Path,
        object_type: str,
        location: Tuple[float, float, float] = (0, 0, 0),
        rotation: Tuple[float, float, float] = (0, 0, 0),
        scale: Tuple[float, float, float] = (1, 1, 1),
        name: Optional[str] = None,
        output_file: Optional[Path] = None
    ) -> Dict[str, Any]:
        """
        Add a primitive object to the scene.

        Args:
            blend_file: Input .blend file
            object_type: Type of object (mesh, camera, light, empty)
            location: Object location (x, y, z)
            rotation: Object rotation (x, y, z) in radians
            scale: Object scale (x, y, z)
            name: Optional object name
            output_file: Optional output .blend file path

        Returns:
            Dictionary with operation results
        """
        script = f"""
import bpy
import math

# Add object based on type
if '{object_type}' == 'mesh':
    bpy.ops.mesh.primitive_cube_add(location={location})
elif '{object_type}' == 'camera':
    bpy.ops.object.camera_add(location={location})
elif '{object_type}' == 'light':
    bpy.ops.object.light_add(type='POINT', location={location})
elif '{object_type}' == 'empty':
    bpy.ops.object.empty_add(location={location})
else:
    raise ValueError(f"Unknown object type: {object_type}")

# Get the newly created object
obj = bpy.context.active_object

# Set rotation and scale
obj.rotation_euler = {rotation}
obj.scale = {scale}
"""

        if name:
            script += f"\nobj.name = '{name}'"

        if output_file:
            script += f"\nbpy.ops.wm.save_as_mainfile(filepath='{output_file}')"

        return self.blender.run_script(script, background=True, blend_file=blend_file)

    def setup_camera(
        self,
        blend_file: Path,
        location: Tuple[float, float, float],
        look_at: Tuple[float, float, float] = (0, 0, 0),
        lens: float = 50.0,
        output_file: Optional[Path] = None
    ) -> Dict[str, Any]:
        """
        Setup camera with specific parameters.

        Args:
            blend_file: Input .blend file
            location: Camera location (x, y, z)
            look_at: Point to look at (x, y, z)
            lens: Lens focal length in mm
            output_file: Optional output .blend file path

        Returns:
            Dictionary with operation results
        """
        script = f"""
import bpy
from mathutils import Vector

# Create camera
bpy.ops.object.camera_add(location={location})
camera = bpy.context.active_object

# Set camera to look at target
direction = Vector({look_at}) - camera.location
rot_quat = direction.to_track_quat('-Z', 'Y')
camera.rotation_euler = rot_quat.to_euler()

# Set lens
camera.data.lens = {lens}

# Set as active camera
bpy.context.scene.camera = camera
"""

        if output_file:
            script += f"\nbpy.ops.wm.save_as_mainfile(filepath='{output_file}')"

        return self.blender.run_script(script, background=True, blend_file=blend_file)

    def setup_lighting(
        self,
        blend_file: Path,
        light_type: str = "SUN",
        location: Tuple[float, float, float] = (0, 0, 10),
        energy: float = 1.0,
        color: Tuple[float, float, float] = (1, 1, 1),
        output_file: Optional[Path] = None
    ) -> Dict[str, Any]:
        """
        Setup scene lighting.

        Args:
            blend_file: Input .blend file
            light_type: Type of light (SUN, POINT, SPOT, AREA)
            location: Light location (x, y, z)
            energy: Light intensity
            color: Light color (r, g, b) normalized 0-1
            output_file: Optional output .blend file path

        Returns:
            Dictionary with operation results
        """
        script = f"""
import bpy

# Create light
bpy.ops.object.light_add(type='{light_type}', location={location})
light = bpy.context.active_object

# Set light properties
light.data.energy = {energy}
light.data.color = {color}
"""

        if output_file:
            script += f"\nbpy.ops.wm.save_as_mainfile(filepath='{output_file}')"

        return self.blender.run_script(script, background=True, blend_file=blend_file)

    def apply_material(
        self,
        blend_file: Path,
        object_name: str,
        material_name: str,
        color: Tuple[float, float, float, float] = (0.8, 0.8, 0.8, 1.0),
        metallic: float = 0.0,
        roughness: float = 0.5,
        output_file: Optional[Path] = None
    ) -> Dict[str, Any]:
        """
        Apply a material to an object.

        Args:
            blend_file: Input .blend file
            object_name: Name of object to apply material to
            material_name: Name for the new material
            color: Material color RGBA (0-1 normalized)
            metallic: Metallic value (0-1)
            roughness: Roughness value (0-1)
            output_file: Optional output .blend file path

        Returns:
            Dictionary with operation results
        """
        script = f"""
import bpy

# Get object
obj = bpy.data.objects.get('{object_name}')
if obj is None:
    raise ValueError(f"Object '{object_name}' not found")

# Create material
mat = bpy.data.materials.new(name='{material_name}')
mat.use_nodes = True

# Get principled BSDF node
bsdf = mat.node_tree.nodes.get('Principled BSDF')
if bsdf:
    bsdf.inputs['Base Color'].default_value = {color}
    bsdf.inputs['Metallic'].default_value = {metallic}
    bsdf.inputs['Roughness'].default_value = {roughness}

# Assign material to object
if obj.data.materials:
    obj.data.materials[0] = mat
else:
    obj.data.materials.append(mat)
"""

        if output_file:
            script += f"\nbpy.ops.wm.save_as_mainfile(filepath='{output_file}')"

        return self.blender.run_script(script, background=True, blend_file=blend_file)

    def render_image(
        self,
        blend_file: Path,
        output_image: Path,
        resolution_x: int = 1920,
        resolution_y: int = 1080,
        samples: int = 128,
        file_format: str = "PNG"
    ) -> Dict[str, Any]:
        """
        Render the scene to an image.

        Args:
            blend_file: Input .blend file
            output_image: Output image file path
            resolution_x: Image width in pixels
            resolution_y: Image height in pixels
            samples: Number of render samples
            file_format: Output format (PNG, JPEG, TIFF, etc.)

        Returns:
            Dictionary with render results
        """
        script = f"""
import bpy

# Set render settings
scene = bpy.context.scene
scene.render.resolution_x = {resolution_x}
scene.render.resolution_y = {resolution_y}
scene.render.image_settings.file_format = '{file_format}'
scene.render.filepath = '{output_image}'

# Set samples for Cycles/EEVEE
if scene.render.engine == 'CYCLES':
    scene.cycles.samples = {samples}
elif scene.render.engine == 'BLENDER_EEVEE':
    scene.eevee.taa_render_samples = {samples}

# Render
bpy.ops.render.render(write_still=True)
"""

        return self.blender.run_script(script, background=True, blend_file=blend_file)
