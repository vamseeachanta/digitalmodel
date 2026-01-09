# Blender Automation Module

**ABOUTME:** Comprehensive Python module for Blender automation, batch processing, and CAD file integration.

## Overview

The `blender_automation` module provides a robust Python API for automating Blender operations without requiring direct `bpy` imports. It enables:

- **Subprocess-based Blender execution** - Safe, isolated Blender operations
- **CAD file import/export** - Support for STEP, IGES, STL, OBJ, FBX, GLTF
- **Batch processing** - Parallel and sequential processing of multiple files
- **Scene management** - High-level scene manipulation APIs
- **FreeCAD integration** - STEP/IGES conversion via FreeCAD

## Features

### Core Components

- **BlenderWrapper** - Safe subprocess-based Blender script execution
- **SceneManager** - High-level scene, camera, lighting, and material management
- **CADImporter** - Import CAD files with automatic format conversion
- **MeshExporter** - Export to multiple 3D formats with optimization
- **BatchProcessor** - Parallel batch operations with progress tracking

### Supported Formats

**Import:**
- Direct: STL, OBJ, FBX, Collada (DAE), PLY, X3D
- Via FreeCAD: STEP (.step, .stp), IGES (.iges, .igs)

**Export:**
- STL (binary/ASCII)
- OBJ (with MTL materials)
- FBX (game engines)
- GLTF/GLB (web, AR/VR)
- Collada (DAE)
- PLY, X3D

## Installation

```bash
# Install module
cd /path/to/digitalmodel
pip install -e .

# Verify Blender is installed
python -c "from blender_automation.utils import verify_blender_installation; print(verify_blender_installation())"
```

## Quick Start

### Basic Import/Export

```python
from blender_automation import BlenderWrapper
from pathlib import Path

# Initialize wrapper
blender = BlenderWrapper()

# Import OBJ file
result = blender.import_file(
    Path("models/input.obj"),
    "obj",
    output_blend=Path("output/scene.blend")
)

# Export to STL
result = blender.export_file(
    Path("output/scene.blend"),
    Path("output/model.stl"),
    "stl",
    binary=True
)
```

### Scene Management

```python
from blender_automation import SceneManager
from pathlib import Path

# Create scene manager
manager = SceneManager()

# Create empty scene
blend_file = Path("scene.blend")
manager.create_empty_scene(blend_file)

# Add geometry
manager.add_object(
    blend_file,
    "mesh",
    location=(0, 0, 0),
    name="Cube",
    output_file=blend_file
)

# Setup camera
manager.setup_camera(
    blend_file,
    location=(10, -10, 5),
    look_at=(0, 0, 0),
    lens=50.0,
    output_file=blend_file
)

# Setup lighting
manager.setup_lighting(
    blend_file,
    light_type="SUN",
    location=(0, 0, 10),
    energy=2.0,
    output_file=blend_file
)

# Render
manager.render_image(
    blend_file,
    Path("render.png"),
    resolution_x=1920,
    resolution_y=1080,
    samples=128
)
```

### CAD File Import

```python
from blender_automation import CADImporter
from pathlib import Path

# Initialize importer
importer = CADImporter()

# Import STEP file (requires FreeCAD)
result = importer.import_file(
    Path("models/assembly.step"),
    output_blend=Path("output/assembly.blend"),
    scale=1.0,
    cleanup=True
)

# Batch import multiple files
files = [
    Path("part1.obj"),
    Path("part2.stl"),
    Path("part3.fbx")
]

result = importer.batch_import(
    files,
    output_blend=Path("combined.blend"),
    merge=True
)

print(f"Imported {result['successful']}/{result['total']} files")
```

### Batch Processing

```python
from blender_automation import BatchProcessor
from pathlib import Path

# Initialize processor
processor = BatchProcessor(max_workers=4)

# Convert directory of OBJ files to FBX
result = processor.convert_directory(
    input_dir=Path("models/obj"),
    output_dir=Path("output/fbx"),
    input_format="obj",
    output_format="fbx",
    parallel=True,
    recursive=False
)

print(f"Converted {result['successful']}/{result['total_files']} files")
print(f"Duration: {result['duration_seconds']:.2f} seconds")

# Render multiple scenes
blend_files = list(Path("scenes").glob("*.blend"))

render_result = processor.render_directory(
    blend_files,
    output_dir=Path("renders"),
    resolution=(1920, 1080),
    samples=128,
    parallel=False  # Rendering is resource-intensive
)
```

### Context Manager Usage

```python
from blender_automation import BlenderContext
from pathlib import Path

# Use context manager for safe operations
with BlenderContext() as blender:
    # Import file
    blender.import_file(
        Path("model.obj"),
        "obj",
        Path("scene.blend")
    )

    # Execute custom script
    script = """
import bpy

# Custom operations
for obj in bpy.data.objects:
    if obj.type == 'MESH':
        obj.scale = (2, 2, 2)

bpy.ops.wm.save_as_mainfile(filepath='scene.blend')
"""
    blender.run_script(script, blend_file=Path("scene.blend"))
```

## Advanced Usage

### Custom Blender Scripts

```python
from blender_automation import BlenderWrapper

blender = BlenderWrapper()

# Execute custom Python script
script = """
import bpy
import math

# Create parametric surface
for x in range(-5, 5):
    for y in range(-5, 5):
        z = math.sin(x) * math.cos(y)
        bpy.ops.mesh.primitive_uv_sphere_add(
            location=(x, y, z),
            scale=(0.2, 0.2, 0.2)
        )

bpy.ops.wm.save_as_mainfile(filepath='parametric.blend')
"""

result = blender.run_script(script, background=True)
```

### Material Application

```python
from blender_automation import SceneManager
from pathlib import Path

manager = SceneManager()
blend_file = Path("scene.blend")

# Apply metallic material
manager.apply_material(
    blend_file,
    object_name="Cube",
    material_name="Metal",
    color=(0.8, 0.8, 0.8, 1.0),
    metallic=0.9,
    roughness=0.2,
    output_file=blend_file
)

# Apply colored plastic
manager.apply_material(
    blend_file,
    object_name="Sphere",
    material_name="RedPlastic",
    color=(0.9, 0.1, 0.1, 1.0),
    metallic=0.0,
    roughness=0.5,
    output_file=blend_file
)
```

### Web Optimization

```python
from blender_automation import MeshExporter
from pathlib import Path

exporter = MeshExporter()

# Optimize for web with compression
result = exporter.optimize_for_web(
    blend_file=Path("scene.blend"),
    output_path=Path("web/model.glb"),
    target_size_mb=5.0
)

if result["success"]:
    print(f"Exported: {result['file_size_mb']:.2f} MB")
    print(f"Meets target: {result['meets_target']}")
```

## Integration with Marine Engineering

The module integrates seamlessly with other digitalmodel components:

```python
from blender_automation import CADImporter
from pathlib import Path

# Import marine structure from CAD
importer = CADImporter()

# Import offshore platform STEP model
result = importer.import_file(
    Path("marine_models/platform.step"),
    output_blend=Path("marine_viz/platform.blend"),
    scale=1.0,
    cleanup=True
)

# Add to marine engineering visualization pipeline
# (integrate with marine_engineering module)
```

## API Reference

### Core Classes

- **BlenderWrapper** - Main interface for Blender operations
  - `run_script()` - Execute Python script
  - `execute_command()` - Execute multiple commands
  - `import_file()` - Import 3D file
  - `export_file()` - Export to format

- **SceneManager** - Scene manipulation
  - `create_empty_scene()` - New scene
  - `add_object()` - Add primitive/object
  - `setup_camera()` - Camera configuration
  - `setup_lighting()` - Lighting setup
  - `apply_material()` - Material assignment
  - `render_image()` - Render to image

- **CADImporter** - CAD file import
  - `import_file()` - Import single file
  - `batch_import()` - Import multiple files

- **MeshExporter** - Export utilities
  - `export_file()` - Export to format
  - `export_stl()` - STL export
  - `export_obj()` - OBJ export
  - `export_fbx()` - FBX export
  - `export_gltf()` - GLTF/GLB export
  - `batch_export()` - Export multiple files
  - `optimize_for_web()` - Web optimization

- **BatchProcessor** - Batch operations
  - `process_files()` - Process file list
  - `convert_directory()` - Batch conversion
  - `render_directory()` - Batch rendering
  - `apply_operation_to_directory()` - Custom operations

### Utility Functions

```python
from blender_automation.utils import (
    find_blender_executable,
    verify_blender_installation,
    find_blend_files,
    find_cad_files,
    get_file_info,
    validate_file_format,
    create_output_path
)
```

## Testing

```bash
# Run all tests
pytest tests/blender_automation/

# Run specific test file
pytest tests/blender_automation/test_blender_wrapper.py

# Run with coverage
pytest --cov=src/blender_automation tests/blender_automation/

# Run specific test
pytest tests/blender_automation/test_blender_wrapper.py::TestBlenderWrapper::test_run_simple_script
```

## Requirements

- **Python 3.10+**
- **Blender 4.0+** (5.0+ recommended)
- **FreeCAD** (optional, for STEP/IGES import)

## Performance Considerations

- **Parallel Processing**: Use `max_workers` parameter carefully (default: 4)
- **Rendering**: Disable parallel processing for rendering (resource-intensive)
- **Large Files**: Increase timeout for large CAD file imports
- **Memory**: Monitor memory usage for batch operations

## Troubleshooting

### Blender Not Found

```python
from blender_automation.utils import verify_blender_installation

status = verify_blender_installation()
if not status["installed"]:
    print("Blender not found. Install from: https://www.blender.org/download/")
    print(f"Error: {status['error']}")
```

### STEP/IGES Import Fails

```python
# Install FreeCAD
# Ubuntu: sudo apt install freecad
# macOS: brew install freecad
# Windows: Download from https://www.freecad.org/
```

### Import Errors

```python
# Check Blender version compatibility
from blender_automation import BlenderWrapper

blender = BlenderWrapper()
print(f"Blender version: {blender.version}")
```

## Contributing

See main digitalmodel repository for contribution guidelines.

## License

Part of the digitalmodel project.

## See Also

- [Blender Python API Documentation](https://docs.blender.org/api/current/)
- [FreeCAD Python API](https://wiki.freecad.org/Python)
- [CAD Engineering Module](../digitalmodel/cad_engineering/)
- [Marine Engineering Module](../marine_engineering/)
