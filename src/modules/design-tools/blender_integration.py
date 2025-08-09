#!/usr/bin/env python3
"""
Blender Integration Module
Provides automation and AI-enhanced visualization for Blender workflows
Part of the FreeCAD + Blender engineering workflow implementation
"""

import sys
import os
import json
import numpy as np
from pathlib import Path
import subprocess
import tempfile

# Blender executable detection
BLENDER_PATHS = [
    '/snap/bin/blender',
    '/usr/bin/blender',
    '/Applications/Blender.app/Contents/MacOS/Blender',
    'C:\\Program Files\\Blender Foundation\\Blender 3.6\\blender.exe'
]

BLENDER_EXECUTABLE = None
for path in BLENDER_PATHS:
    if os.path.exists(path):
        BLENDER_EXECUTABLE = path
        break


class BlenderWorkflow:
    """Main workflow manager for Blender operations"""
    
    def __init__(self, project_name="engineering_viz"):
        self.project_name = project_name
        self.project_path = Path(f"{project_name}.blend")
        self.import_path = Path("exports")  # Where FreeCAD exports are located
        self.render_path = Path("renders")
        self.render_path.mkdir(exist_ok=True)
        
    def execute_blender_script(self, script_content, background=True):
        """
        Execute Python script in Blender environment
        
        Args:
            script_content (str): Python code to execute in Blender
            background (bool): Run in background mode
            
        Returns:
            tuple: (return_code, stdout, stderr)
        """
        if not BLENDER_EXECUTABLE:
            raise RuntimeError("Blender executable not found")
            
        # Create temporary script file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as temp_file:
            temp_file.write(script_content)
            temp_script = temp_file.name
            
        try:
            cmd = [BLENDER_EXECUTABLE]
            if background:
                cmd.extend(['--background'])
            cmd.extend(['--python', temp_script])
            
            result = subprocess.run(cmd, 
                                  capture_output=True, 
                                  text=True, 
                                  timeout=300)  # 5 minute timeout
                                  
            return result.returncode, result.stdout, result.stderr
            
        finally:
            # Clean up temporary file
            try:
                os.unlink(temp_script)
            except:
                pass
                
    def create_engineering_scene(self, import_files=None):
        """
        Create engineering visualization scene with imported CAD models
        
        Args:
            import_files (list): List of files to import (default: all STL in import_path)
            
        Returns:
            tuple: (return_code, output)
        """
        if import_files is None:
            import_files = list(self.import_path.glob("*.stl"))
            
        script = f'''
import bpy
import bmesh
import os
import math

# Clear existing scene
bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete(use_global=False)

# Clear materials
for material in bpy.data.materials:
    bpy.data.materials.remove(material)

# Set up scene for engineering visualization
scene = bpy.context.scene
scene.name = "{self.project_name}"

# Set units to metric
scene.unit_settings.system = 'METRIC'
scene.unit_settings.scale_length = 1.0
scene.unit_settings.length_unit = 'METERS'

# Import CAD models
import_files = {[str(f) for f in import_files]}
imported_objects = []

for file_path in import_files:
    if os.path.exists(file_path):
        # Import STL
        bpy.ops.import_mesh.stl(filepath=file_path)
        
        # Get the imported object (last selected)
        imported_obj = bpy.context.selected_objects[-1] if bpy.context.selected_objects else None
        
        if imported_obj:
            # Scale from mm to meters (FreeCAD exports in mm)
            imported_obj.scale = (0.001, 0.001, 0.001)
            bpy.ops.object.transform_apply(scale=True)
            
            # Clean up mesh
            bpy.context.view_layer.objects.active = imported_obj
            bpy.ops.object.mode_set(mode='EDIT')
            bpy.ops.mesh.select_all(action='SELECT')
            bpy.ops.mesh.remove_doubles()
            bpy.ops.mesh.normals_make_consistent(inside=False)
            bpy.ops.object.mode_set(mode='OBJECT')
            
            imported_objects.append(imported_obj)
            print(f"Imported: {{imported_obj.name}} from {{file_path}}")

# Create engineering materials
def create_industrial_material(name, base_color, metallic=0.8, roughness=0.3):
    mat = bpy.data.materials.new(name=name)
    mat.use_nodes = True
    
    # Clear existing nodes
    mat.node_tree.nodes.clear()
    
    # Add principled BSDF
    bsdf = mat.node_tree.nodes.new(type='ShaderNodeBsdfPrincipled')
    bsdf.inputs['Base Color'].default_value = (*base_color, 1.0)
    bsdf.inputs['Metallic'].default_value = metallic
    bsdf.inputs['Roughness'].default_value = roughness
    
    # Add output node
    output = mat.node_tree.nodes.new(type='ShaderNodeOutputMaterial')
    mat.node_tree.links.new(bsdf.outputs['BSDF'], output.inputs['Surface'])
    
    return mat

# Material library
materials = {{
    'stainless_steel': create_industrial_material('Stainless Steel', (0.8, 0.8, 0.9), 0.9, 0.2),
    'carbon_steel': create_industrial_material('Carbon Steel', (0.3, 0.3, 0.35), 0.8, 0.4),
    'painted_steel': create_industrial_material('Painted Steel', (0.2, 0.4, 0.8), 0.1, 0.6),
    'aluminum': create_industrial_material('Aluminum', (0.9, 0.9, 0.95), 0.9, 0.1),
    'equipment': create_industrial_material('Equipment', (0.6, 0.6, 0.2), 0.3, 0.8)
}}

# Assign materials to objects based on naming
for obj in imported_objects:
    if obj and obj.data:
        obj_name = obj.name.lower()
        
        if 'vessel' in obj_name or 'tank' in obj_name:
            obj.data.materials.append(materials['stainless_steel'])
        elif 'pipe' in obj_name:
            obj.data.materials.append(materials['carbon_steel'])
        elif 'structure' in obj_name or 'platform' in obj_name:
            obj.data.materials.append(materials['painted_steel'])
        elif 'equipment' in obj_name:
            obj.data.materials.append(materials['equipment'])
        else:
            obj.data.materials.append(materials['aluminum'])

# Set up lighting for technical visualization
def setup_technical_lighting():
    # Remove default light
    if 'Light' in bpy.data.objects:
        bpy.data.objects.remove(bpy.data.objects['Light'], do_unlink=True)
    
    # Add key light
    bpy.ops.object.light_add(type='SUN', location=(10, 10, 10))
    key_light = bpy.context.active_object
    key_light.name = 'Key_Light'
    key_light.data.energy = 3.0
    key_light.rotation_euler = (0.785, 0.785, 0)
    
    # Add fill light
    bpy.ops.object.light_add(type='SUN', location=(-5, 5, 8))
    fill_light = bpy.context.active_object
    fill_light.name = 'Fill_Light'
    fill_light.data.energy = 1.5
    fill_light.rotation_euler = (1.2, -0.5, 0)
    
    # Add rim light
    bpy.ops.object.light_add(type='SUN', location=(5, -10, 5))
    rim_light = bpy.context.active_object
    rim_light.name = 'Rim_Light'
    rim_light.data.energy = 2.0
    rim_light.rotation_euler = (1.8, 1.2, 0)

setup_technical_lighting()

# Set up camera for technical view
if 'Camera' in bpy.data.objects:
    camera = bpy.data.objects['Camera']
else:
    bpy.ops.object.camera_add()
    camera = bpy.context.active_object

# Position camera for good overview
if imported_objects:
    # Calculate scene bounds
    min_coords = [float('inf')] * 3
    max_coords = [float('-inf')] * 3
    
    for obj in imported_objects:
        if obj.bound_box:
            for vertex in obj.bound_box:
                world_vertex = obj.matrix_world @ Vector(vertex)
                for i in range(3):
                    min_coords[i] = min(min_coords[i], world_vertex[i])
                    max_coords[i] = max(max_coords[i], world_vertex[i])
    
    # Center of scene
    center = [(min_coords[i] + max_coords[i]) / 2 for i in range(3)]
    size = max([(max_coords[i] - min_coords[i]) for i in range(3)])
    
    # Position camera
    camera.location = (center[0] + size * 1.5, center[1] - size * 1.5, center[2] + size)
    camera.rotation_euler = (1.1, 0, 0.785)

# Set viewport shading to Material Preview
for area in bpy.context.screen.areas:
    if area.type == 'VIEW_3D':
        for space in area.spaces:
            if space.type == 'VIEW_3D':
                space.shading.type = 'MATERIAL'

# Save the scene
bpy.ops.wm.save_as_mainfile(filepath="{str(self.project_path)}")
print(f"Scene created with {{len(imported_objects)}} objects")
print(f"Saved to: {str(self.project_path)}")
'''
        
        return self.execute_blender_script(script)
        
    def render_technical_views(self, output_formats=['PNG']):
        """
        Render technical views of the engineering model
        
        Args:
            output_formats (list): Output formats (PNG, JPEG, OpenEXR)
            
        Returns:
            list: Paths to rendered images
        """
        views = [
            {'name': 'front', 'rotation': (1.5708, 0, 0), 'description': 'Front elevation view'},
            {'name': 'side', 'rotation': (1.5708, 0, 1.5708), 'description': 'Side elevation view'},  
            {'name': 'top', 'rotation': (0, 0, 0), 'description': 'Plan view'},
            {'name': 'iso', 'rotation': (1.1, 0, 0.785), 'description': 'Isometric view'}
        ]
        
        rendered_files = []
        
        for view in views:
            for fmt in output_formats:
                script = f'''
import bpy
import os

# Load the project file
bpy.ops.wm.open_mainfile(filepath="{str(self.project_path)}")

# Set render settings
scene = bpy.context.scene
scene.render.resolution_x = 1920
scene.render.resolution_y = 1080
scene.render.resolution_percentage = 100

# Set output format
if "{fmt.upper()}" == "PNG":
    scene.render.image_settings.file_format = 'PNG'
    scene.render.image_settings.color_mode = 'RGBA'
elif "{fmt.upper()}" == "JPEG":
    scene.render.image_settings.file_format = 'JPEG'
    scene.render.image_settings.quality = 90
elif "{fmt.upper()}" == "OPENEXR":
    scene.render.image_settings.file_format = 'OPEN_EXR'
    
# Set camera position for view
camera = bpy.data.objects.get('Camera')
if camera:
    camera.rotation_euler = {view['rotation']}
    
    # Frame all objects in view
    bpy.context.view_layer.objects.active = camera
    bpy.ops.view3d.camera_to_view_selected()

# Set output path
output_file = "{str(self.render_path / f"{view['name']}.{fmt.lower()}")}"
scene.render.filepath = output_file

# Render
bpy.ops.render.render(write_still=True)
print(f"Rendered {{output_file}} - {view['description']}")
'''
                
                result = self.execute_blender_script(script)
                if result[0] == 0:  # Success
                    rendered_files.append(self.render_path / f"{view['name']}.{fmt.lower()}")
                    
        return rendered_files
        
    def create_assembly_animation(self, duration=120):
        """
        Create assembly/disassembly animation
        
        Args:
            duration (int): Animation duration in frames
            
        Returns:
            str: Path to animation file
        """
        script = f'''
import bpy
import mathutils

# Load the project file
bpy.ops.wm.open_mainfile(filepath="{str(self.project_path)}")

scene = bpy.context.scene
scene.frame_start = 1
scene.frame_end = {duration}
scene.frame_set(1)

# Get all imported objects
objects = [obj for obj in scene.objects if obj.type == 'MESH' and obj.name not in ['Cube', 'Plane']]

if not objects:
    print("No objects found for animation")
else:
    print(f"Creating animation with {{len(objects)}} objects")
    
    # Create disassembly animation
    for i, obj in enumerate(objects):
        # Store original location
        original_loc = obj.location.copy()
        
        # Calculate exploded position
        explosion_factor = 2.0
        exploded_loc = original_loc * explosion_factor
        exploded_loc.z += i * 0.5  # Stack vertically
        
        # Set keyframes
        obj.location = exploded_loc
        obj.keyframe_insert(data_path="location", frame=1)
        
        # Animate to assembled position
        delay = i * 5  # Stagger assembly
        obj.location = original_loc
        obj.keyframe_insert(data_path="location", frame=30 + delay)
        
        # Set interpolation to ease in/out
        if obj.animation_data and obj.animation_data.action:
            for fcurve in obj.animation_data.action.fcurves:
                for keyframe in fcurve.keyframe_points:
                    keyframe.interpolation = 'BEZIER'
                    keyframe.handle_left_type = 'AUTO'
                    keyframe.handle_right_type = 'AUTO'

# Set up rendering for animation
scene.render.resolution_x = 1280
scene.render.resolution_y = 720
scene.render.image_settings.file_format = 'FFMPEG'
scene.render.ffmpeg.format = 'MPEG4'
scene.render.ffmpeg.codec = 'H264'

# Set output path
animation_file = "{str(self.render_path / f"{self.project_name}_assembly.mp4")}"
scene.render.filepath = animation_file

# Render animation
bpy.ops.render.render(animation=True)
print(f"Animation rendered: {{animation_file}}")
'''
        
        result = self.execute_blender_script(script)
        if result[0] == 0:
            return str(self.render_path / f"{self.project_name}_assembly.mp4")
        else:
            return None
            
    def export_for_vr(self, format_type="GLTF"):
        """
        Export model for VR/AR applications
        
        Args:
            format_type (str): Export format (GLTF, FBX, USD)
            
        Returns:
            str: Path to exported VR file
        """
        script = f'''
import bpy

# Load the project file
bpy.ops.wm.open_mainfile(filepath="{str(self.project_path)}")

# Prepare for VR export
scene = bpy.context.scene

# Select all mesh objects
bpy.ops.object.select_all(action='DESELECT')
for obj in scene.objects:
    if obj.type == 'MESH':
        obj.select_set(True)

# Optimize for VR
bpy.context.view_layer.objects.active = bpy.context.selected_objects[0] if bpy.context.selected_objects else None

# Apply modifiers and transforms
for obj in bpy.context.selected_objects:
    bpy.context.view_layer.objects.active = obj
    bpy.ops.object.convert(target='MESH')

# Export based on format
vr_file = "{str(self.render_path / f"{self.project_name}_vr")}"

if "{format_type.upper()}" == "GLTF":
    vr_file += ".gltf"
    bpy.ops.export_scene.gltf(
        filepath=vr_file,
        export_format='GLTF_SEPARATE',
        export_materials='EXPORT',
        export_colors=True,
        export_cameras=True,
        export_lights=True
    )
elif "{format_type.upper()}" == "FBX":
    vr_file += ".fbx"
    bpy.ops.export_scene.fbx(
        filepath=vr_file,
        use_selection=False,
        use_active_collection=False,
        object_types={{'MESH', 'CAMERA', 'LIGHT'}},
        use_mesh_modifiers=True
    )

print(f"Exported for VR: {{vr_file}}")
'''
        
        result = self.execute_blender_script(script)
        if result[0] == 0:
            extension = ".gltf" if format_type.upper() == "GLTF" else ".fbx"
            return str(self.render_path / f"{self.project_name}_vr{extension}")
        else:
            return None


class AIVisualizationAssistant:
    """AI-powered visualization assistance for Blender workflows"""
    
    @staticmethod
    def suggest_camera_angles(scene_bounds, subject_type="equipment"):
        """
        Suggest optimal camera angles based on scene content
        
        Args:
            scene_bounds (dict): Scene bounding box information
            subject_type (str): Type of subject being visualized
            
        Returns:
            list: Recommended camera positions and angles
        """
        suggestions = {
            "equipment": [
                {"name": "Hero Shot", "position": "front-right-elevated", "purpose": "Main presentation view"},
                {"name": "Detail View", "position": "close-up-angled", "purpose": "Component details"},
                {"name": "Context View", "position": "wide-elevated", "purpose": "Environmental context"},
                {"name": "Technical View", "position": "orthographic-front", "purpose": "Technical documentation"}
            ],
            "vessel": [
                {"name": "Full Length", "position": "side-elevated", "purpose": "Show complete vessel"},
                {"name": "End View", "position": "front-center", "purpose": "Show circular sections"},
                {"name": "Cutaway", "position": "three-quarter", "purpose": "Internal structure"},
                {"name": "Installation", "position": "top-down", "purpose": "Crane view for lifting"}
            ],
            "piping": [
                {"name": "Flow Path", "position": "follow-pipe", "purpose": "Show process flow"},
                {"name": "Junction Detail", "position": "close-up-joint", "purpose": "Connection details"},
                {"name": "Support View", "position": "underneath", "purpose": "Support structure"},
                {"name": "Maintenance Access", "position": "operator-view", "purpose": "Accessibility check"}
            ]
        }
        
        return suggestions.get(subject_type, suggestions["equipment"])
        
    @staticmethod
    def analyze_lighting_needs(objects, environment="offshore"):
        """
        Analyze lighting requirements for technical visualization
        
        Args:
            objects (list): Objects to be lit
            environment (str): Environment type
            
        Returns:
            dict: Lighting recommendations
        """
        environments = {
            "offshore": {
                "ambient": {"color": (0.8, 0.9, 1.0), "strength": 0.3},
                "key_light": {"color": (1.0, 0.95, 0.9), "strength": 3.0, "angle": 45},
                "fill_light": {"color": (0.9, 0.95, 1.0), "strength": 1.5, "angle": -30},
                "rim_light": {"color": (1.0, 1.0, 0.9), "strength": 2.0, "angle": 135}
            },
            "industrial": {
                "ambient": {"color": (0.7, 0.7, 0.8), "strength": 0.4},
                "key_light": {"color": (1.0, 1.0, 0.95), "strength": 4.0, "angle": 60},
                "fill_light": {"color": (0.95, 0.95, 1.0), "strength": 2.0, "angle": -45},
                "rim_light": {"color": (1.0, 0.9, 0.8), "strength": 2.5, "angle": 120}
            }
        }
        
        return environments.get(environment, environments["industrial"])


def main():
    """Test the Blender integration functionality"""
    if not BLENDER_EXECUTABLE:
        print("Blender executable not found - cannot run full test")
        return False
        
    try:
        # Create workflow instance
        workflow = BlenderWorkflow("test_engineering_viz")
        
        # Test scene creation (will work with empty import list)
        print("Creating engineering scene...")
        result = workflow.create_engineering_scene([])
        
        if result[0] == 0:
            print("✓ Scene creation successful")
        else:
            print(f"✗ Scene creation failed: {result[2]}")
            return False
            
        # Test camera suggestions
        camera_angles = AIVisualizationAssistant.suggest_camera_angles({}, "equipment")
        print(f"✓ Generated {len(camera_angles)} camera angle suggestions")
        
        # Test lighting analysis
        lighting = AIVisualizationAssistant.analyze_lighting_needs([], "offshore")
        print(f"✓ Analyzed lighting for offshore environment: {len(lighting)} light types")
        
        print("Blender integration test completed successfully!")
        return True
        
    except Exception as e:
        print(f"Test failed: {e}")
        return False


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)