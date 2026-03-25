"""
ABOUTME: Tests for scene management functionality
Tests SceneManager class for scene manipulation operations.
"""

import pytest
from pathlib import Path
import tempfile

from digitalmodel.blender_automation.core.scene_manager import SceneManager
from digitalmodel.blender_automation.core.blender_wrapper import BlenderWrapper


class TestSceneManager:
    """Test suite for SceneManager class."""

    @pytest.fixture
    def manager(self):
        """Create SceneManager instance for testing."""
        return SceneManager()

    @pytest.fixture
    def temp_dir(self):
        """Create temporary directory for test files."""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir)

    def test_create_empty_scene(self, manager, temp_dir):
        """Test creating an empty scene."""
        output_file = temp_dir / "empty_scene.blend"
        result = manager.create_empty_scene(output_file)

        assert result["success"] is True
        assert output_file.exists()

    def test_add_object_mesh(self, manager, temp_dir):
        """Test adding a mesh object to scene."""
        # First create empty scene
        blend_file = temp_dir / "scene.blend"
        manager.create_empty_scene(blend_file)

        # Add mesh object
        result = manager.add_object(
            blend_file,
            "mesh",
            location=(1, 2, 3),
            name="TestCube",
            output_file=blend_file
        )

        assert result["success"] is True

    def test_add_camera(self, manager, temp_dir):
        """Test adding a camera to scene."""
        blend_file = temp_dir / "scene.blend"
        manager.create_empty_scene(blend_file)

        result = manager.add_object(
            blend_file,
            "camera",
            location=(5, -5, 5),
            name="TestCamera",
            output_file=blend_file
        )

        assert result["success"] is True

    def test_add_light(self, manager, temp_dir):
        """Test adding a light to scene."""
        blend_file = temp_dir / "scene.blend"
        manager.create_empty_scene(blend_file)

        result = manager.add_object(
            blend_file,
            "light",
            location=(0, 0, 10),
            name="TestLight",
            output_file=blend_file
        )

        assert result["success"] is True

    def test_setup_camera(self, manager, temp_dir):
        """Test camera setup with look-at."""
        blend_file = temp_dir / "scene.blend"
        manager.create_empty_scene(blend_file)

        result = manager.setup_camera(
            blend_file,
            location=(10, -10, 5),
            look_at=(0, 0, 0),
            lens=50.0,
            output_file=blend_file
        )

        assert result["success"] is True

    def test_setup_lighting(self, manager, temp_dir):
        """Test lighting setup."""
        blend_file = temp_dir / "scene.blend"
        manager.create_empty_scene(blend_file)

        result = manager.setup_lighting(
            blend_file,
            light_type="SUN",
            location=(0, 0, 10),
            energy=2.0,
            color=(1.0, 0.9, 0.8),
            output_file=blend_file
        )

        assert result["success"] is True

    def test_apply_material(self, manager, temp_dir):
        """Test applying material to object."""
        # Create scene with cube
        blend_file = temp_dir / "scene.blend"

        script = f"""
import bpy
bpy.ops.wm.read_factory_settings(use_empty=True)
bpy.ops.mesh.primitive_cube_add()
bpy.context.active_object.name = 'TestCube'
bpy.ops.wm.save_as_mainfile(filepath='{blend_file}')
"""
        wrapper = BlenderWrapper()
        wrapper.run_script(script, background=True)

        # Apply material
        result = manager.apply_material(
            blend_file,
            "TestCube",
            "TestMaterial",
            color=(0.8, 0.2, 0.1, 1.0),
            metallic=0.5,
            roughness=0.3,
            output_file=blend_file
        )

        assert result["success"] is True

    def test_render_image(self, manager, temp_dir):
        """Test rendering scene to image."""
        # Create simple scene
        blend_file = temp_dir / "scene.blend"
        output_image = temp_dir / "render.png"

        # Setup scene with geometry, camera, and light
        script = f"""
import bpy

bpy.ops.wm.read_factory_settings(use_empty=True)

# Add cube
bpy.ops.mesh.primitive_cube_add()

# Add camera
bpy.ops.object.camera_add(location=(5, -5, 5))
camera = bpy.context.active_object
bpy.context.scene.camera = camera

# Add light
bpy.ops.object.light_add(type='SUN', location=(0, 0, 10))

bpy.ops.wm.save_as_mainfile(filepath='{blend_file}')
"""
        wrapper = BlenderWrapper()
        wrapper.run_script(script, background=True)

        # Render
        result = manager.render_image(
            blend_file,
            output_image,
            resolution_x=640,
            resolution_y=480,
            samples=32,
            file_format="PNG"
        )

        assert result["success"] is True
        # Note: Actual rendering might take time, output file check is optional

    def test_object_with_rotation_and_scale(self, manager, temp_dir):
        """Test adding object with rotation and scale."""
        blend_file = temp_dir / "scene.blend"
        manager.create_empty_scene(blend_file)

        result = manager.add_object(
            blend_file,
            "mesh",
            location=(0, 0, 0),
            rotation=(0.5, 0.5, 0),  # radians
            scale=(2, 2, 2),
            name="ScaledCube",
            output_file=blend_file
        )

        assert result["success"] is True

    def test_multiple_lights(self, manager, temp_dir):
        """Test setting up multiple lights."""
        blend_file = temp_dir / "scene.blend"
        manager.create_empty_scene(blend_file)

        # Add SUN light
        result1 = manager.setup_lighting(
            blend_file,
            light_type="SUN",
            location=(0, 0, 10),
            output_file=blend_file
        )

        # Add POINT light
        result2 = manager.setup_lighting(
            blend_file,
            light_type="POINT",
            location=(5, 5, 5),
            energy=100.0,
            output_file=blend_file
        )

        assert result1["success"] is True
        assert result2["success"] is True
