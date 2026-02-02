"""
ABOUTME: Basic usage examples for blender_automation module
Demonstrates common operations: import, export, scene creation, rendering.
"""

from pathlib import Path
from digitalmodel.modules.blender_automation import (
    BlenderWrapper,
    BlenderContext,
    SceneManager,
    CADImporter,
    MeshExporter,
    BatchProcessor
)


def example_basic_import_export():
    """Example: Basic file import and export."""
    print("=== Basic Import/Export ===")

    # Initialize wrapper
    blender = BlenderWrapper()

    # Create output directory
    output_dir = Path("examples/output")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Import OBJ file (create a simple one first)
    obj_file = output_dir / "simple.obj"
    obj_file.write_text("""
v 0 0 0
v 1 0 0
v 1 1 0
v 0 1 0
f 1 2 3 4
""")

    # Import to Blender
    blend_file = output_dir / "scene.blend"
    result = blender.import_file(obj_file, "obj", blend_file)

    if result["success"]:
        print(f"✓ Imported {obj_file} successfully")

        # Export to STL
        stl_file = output_dir / "output.stl"
        export_result = blender.export_file(
            blend_file,
            stl_file,
            "stl",
            binary=True
        )

        if export_result["success"]:
            print(f"✓ Exported to {stl_file}")
    else:
        print(f"✗ Import failed: {result.get('error')}")


def example_scene_creation():
    """Example: Create and setup a complete scene."""
    print("\n=== Scene Creation ===")

    manager = SceneManager()
    output_dir = Path("examples/output")
    output_dir.mkdir(parents=True, exist_ok=True)

    blend_file = output_dir / "complete_scene.blend"

    # 1. Create empty scene
    print("Creating empty scene...")
    manager.create_empty_scene(blend_file)

    # 2. Add geometry
    print("Adding geometry...")
    manager.add_object(
        blend_file,
        "mesh",
        location=(0, 0, 0),
        scale=(1, 1, 1),
        name="MainCube",
        output_file=blend_file
    )

    # 3. Setup camera
    print("Setting up camera...")
    manager.setup_camera(
        blend_file,
        location=(7, -7, 5),
        look_at=(0, 0, 0),
        lens=50.0,
        output_file=blend_file
    )

    # 4. Setup lighting
    print("Setting up lighting...")
    manager.setup_lighting(
        blend_file,
        light_type="SUN",
        location=(5, 5, 10),
        energy=2.0,
        color=(1.0, 0.95, 0.9),
        output_file=blend_file
    )

    # 5. Apply material
    print("Applying material...")
    manager.apply_material(
        blend_file,
        "MainCube",
        "GoldMaterial",
        color=(0.9, 0.7, 0.2, 1.0),
        metallic=0.8,
        roughness=0.3,
        output_file=blend_file
    )

    print(f"✓ Scene created: {blend_file}")

    # 6. Render
    print("Rendering scene...")
    output_image = output_dir / "render.png"
    render_result = manager.render_image(
        blend_file,
        output_image,
        resolution_x=1280,
        resolution_y=720,
        samples=64
    )

    if render_result["success"]:
        print(f"✓ Rendered to {output_image}")


def example_cad_import():
    """Example: Import CAD files."""
    print("\n=== CAD File Import ===")

    importer = CADImporter()
    output_dir = Path("examples/output")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Check FreeCAD availability
    if importer.freecad_available:
        print("✓ FreeCAD available - STEP/IGES import enabled")
    else:
        print("⚠ FreeCAD not available - STEP/IGES import disabled")

    # Import STL (always works)
    stl_file = output_dir / "test.stl"
    stl_file.write_text("""
solid test
  facet normal 0 0 1
    outer loop
      vertex 0 0 0
      vertex 1 0 0
      vertex 0 1 0
    endloop
  endfacet
endsolid test
""")

    blend_file = output_dir / "imported_cad.blend"
    result = importer.import_file(
        stl_file,
        output_blend=blend_file,
        scale=1.0,
        cleanup=True
    )

    if result["success"]:
        print(f"✓ Imported {stl_file}")


def example_batch_conversion():
    """Example: Batch convert multiple files."""
    print("\n=== Batch File Conversion ===")

    processor = BatchProcessor(max_workers=2)

    # Create input directory with test files
    input_dir = Path("examples/input")
    input_dir.mkdir(parents=True, exist_ok=True)

    # Create some test OBJ files
    for i in range(3):
        obj_file = input_dir / f"model_{i}.obj"
        obj_file.write_text(f"""
v {i} 0 0
v {i+1} 0 0
v {i} 1 0
f 1 2 3
""")

    # Convert all OBJ to FBX
    output_dir = Path("examples/output/converted")
    result = processor.convert_directory(
        input_dir,
        output_dir,
        "obj",
        "fbx",
        parallel=True
    )

    print(f"✓ Converted {result['successful']}/{result['total_files']} files")
    print(f"  Duration: {result['duration_seconds']:.2f} seconds")

    # Print individual results
    for res in result['results']:
        status = "✓" if res["success"] else "✗"
        print(f"  {status} {res['file']}")


def example_custom_script():
    """Example: Execute custom Blender script."""
    print("\n=== Custom Blender Script ===")

    blender = BlenderWrapper()
    output_dir = Path("examples/output")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Create parametric geometry
    script = f"""
import bpy
import math

# Clear scene
bpy.ops.wm.read_factory_settings(use_empty=True)

# Create parametric surface
for x in range(-3, 4):
    for y in range(-3, 4):
        z = math.sin(x * 0.5) * math.cos(y * 0.5)
        bpy.ops.mesh.primitive_uv_sphere_add(
            location=(x * 1.5, y * 1.5, z * 2),
            scale=(0.3, 0.3, 0.3)
        )

# Add camera and light
bpy.ops.object.camera_add(location=(15, -15, 10))
bpy.context.scene.camera = bpy.context.active_object

bpy.ops.object.light_add(type='SUN', location=(10, 10, 20))

# Save
output_file = '{output_dir / "parametric.blend"}'
bpy.ops.wm.save_as_mainfile(filepath=output_file)

print("Parametric surface created!")
"""

    result = blender.run_script(script, background=True)

    if result["success"]:
        print("✓ Parametric surface created")
        if "Parametric surface created!" in result["stdout"]:
            print("✓ Script output verified")


def example_context_manager():
    """Example: Use context manager for operations."""
    print("\n=== Context Manager Usage ===")

    output_dir = Path("examples/output")
    output_dir.mkdir(parents=True, exist_ok=True)

    with BlenderContext() as blender:
        print(f"✓ Blender initialized: {blender.version}")

        # Create simple OBJ
        obj_file = output_dir / "context_test.obj"
        obj_file.write_text("""
v 0 0 0
v 1 0 0
v 0 1 0
f 1 2 3
""")

        # Import
        blend_file = output_dir / "context_scene.blend"
        result = blender.import_file(obj_file, "obj", blend_file)

        if result["success"]:
            print(f"✓ Imported via context manager")

        # Export
        stl_file = output_dir / "context_output.stl"
        export_result = blender.export_file(
            blend_file,
            stl_file,
            "stl"
        )

        if export_result["success"]:
            print(f"✓ Exported via context manager")


def main():
    """Run all examples."""
    print("Blender Automation Examples")
    print("=" * 50)

    try:
        example_basic_import_export()
        example_scene_creation()
        example_cad_import()
        example_batch_conversion()
        example_custom_script()
        example_context_manager()

        print("\n" + "=" * 50)
        print("✓ All examples completed successfully!")

    except Exception as e:
        print(f"\n✗ Error running examples: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
