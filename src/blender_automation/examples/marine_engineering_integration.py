"""
ABOUTME: Marine engineering integration examples
Demonstrates using blender_automation with marine engineering CAD models.
"""

from pathlib import Path
from blender_automation import (
    BlenderWrapper,
    SceneManager,
    CADImporter,
    MeshExporter,
    BatchProcessor
)


def example_offshore_platform_visualization():
    """
    Example: Import offshore platform STEP model and create visualization.

    This example demonstrates:
    - Importing large STEP assemblies
    - Setting up industrial lighting
    - Creating technical renders
    """
    print("=== Offshore Platform Visualization ===")

    # Initialize components
    importer = CADImporter()
    manager = SceneManager()

    output_dir = Path("examples/marine_output")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Import platform model (assuming STEP file exists)
    platform_step = Path("marine_models/platform.step")

    if platform_step.exists():
        print(f"Importing {platform_step}...")

        blend_file = output_dir / "platform.blend"
        result = importer.import_file(
            platform_step,
            output_blend=blend_file,
            scale=1.0,
            cleanup=True
        )

        if result["success"]:
            print("✓ Platform imported successfully")

            # Setup industrial lighting (bright, high contrast)
            print("Setting up industrial lighting...")
            manager.setup_lighting(
                blend_file,
                light_type="SUN",
                location=(100, 100, 200),
                energy=3.0,
                color=(1.0, 0.98, 0.95),  # Slightly warm
                output_file=blend_file
            )

            # Setup camera for overview shot
            print("Setting up camera...")
            manager.setup_camera(
                blend_file,
                location=(150, -150, 100),
                look_at=(0, 0, 20),
                lens=35.0,  # Wide angle
                output_file=blend_file
            )

            # Render high-quality image
            print("Rendering visualization...")
            render_path = output_dir / "platform_overview.png"
            manager.render_image(
                blend_file,
                render_path,
                resolution_x=3840,  # 4K
                resolution_y=2160,
                samples=256
            )

            print(f"✓ Visualization saved to {render_path}")

    else:
        print(f"⚠ Platform STEP file not found: {platform_step}")
        print("  Create sample STEP file or use existing CAD model")


def example_ship_hull_analysis():
    """
    Example: Import ship hull and export for CFD analysis.

    This example demonstrates:
    - Hull mesh import
    - Mesh optimization for CFD
    - Export to multiple formats
    """
    print("\n=== Ship Hull Analysis Preparation ===")

    importer = CADImporter()
    exporter = MeshExporter()

    output_dir = Path("examples/marine_output/hull_analysis")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Import hull (assuming STL or STEP)
    hull_file = Path("marine_models/hull.stl")

    if hull_file.exists():
        print(f"Importing hull from {hull_file}...")

        blend_file = output_dir / "hull.blend"
        result = importer.import_file(
            hull_file,
            output_blend=blend_file,
            cleanup=True  # Remove doubles, fix normals
        )

        if result["success"]:
            print("✓ Hull imported and cleaned")

            # Export for different analysis tools
            formats = {
                "stl": "Mesh-based CFD (OpenFOAM, Fluent)",
                "obj": "Visualization, mesh analysis",
                "ply": "Point cloud processing"
            }

            for fmt, purpose in formats.items():
                output_file = output_dir / f"hull.{fmt}"
                print(f"Exporting {fmt.upper()} for {purpose}...")

                export_result = exporter.export_file(
                    blend_file,
                    output_file,
                    fmt
                )

                if export_result["success"]:
                    print(f"  ✓ {output_file}")

    else:
        print(f"⚠ Hull file not found: {hull_file}")


def example_mooring_system_visualization():
    """
    Example: Visualize mooring system components.

    This example demonstrates:
    - Batch import of mooring components
    - Assembly in Blender
    - Animation setup
    """
    print("\n=== Mooring System Visualization ===")

    importer = CADImporter()
    manager = SceneManager()

    output_dir = Path("examples/marine_output/mooring")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Component files (anchors, chains, buoys)
    components = {
        "anchor": "mooring_models/anchor.obj",
        "chain": "mooring_models/chain_link.obj",
        "buoy": "mooring_models/buoy.obj",
    }

    # Check if any components exist
    existing_components = [
        name for name, path in components.items()
        if Path(path).exists()
    ]

    if existing_components:
        print(f"Found components: {', '.join(existing_components)}")

        # Create combined scene
        blend_file = output_dir / "mooring_assembly.blend"
        manager.create_empty_scene(blend_file)

        # Import each component
        for name, path in components.items():
            if Path(path).exists():
                print(f"Importing {name}...")
                result = importer.import_file(
                    Path(path),
                    output_blend=blend_file
                )

                if result["success"]:
                    print(f"  ✓ {name} imported")

        # Setup scene
        manager.setup_camera(
            blend_file,
            location=(50, -50, 30),
            look_at=(0, 0, -10),
            output_file=blend_file
        )

        manager.setup_lighting(
            blend_file,
            light_type="SUN",
            location=(30, 30, 50),
            output_file=blend_file
        )

        print(f"✓ Mooring assembly created: {blend_file}")

    else:
        print("⚠ No mooring component files found")
        print("  Place .obj files in mooring_models/ directory")


def example_subsea_equipment_catalog():
    """
    Example: Create visual catalog of subsea equipment.

    This example demonstrates:
    - Batch processing of equipment models
    - Consistent rendering setup
    - Catalog image generation
    """
    print("\n=== Subsea Equipment Catalog ===")

    processor = BatchProcessor(max_workers=2)
    manager = SceneManager()

    # Equipment models directory
    equipment_dir = Path("marine_models/subsea_equipment")
    output_dir = Path("examples/marine_output/equipment_catalog")
    output_dir.mkdir(parents=True, exist_ok=True)

    if equipment_dir.exists():
        # Find all equipment models
        from blender_automation.utils import find_cad_files

        equipment_files = find_cad_files(
            equipment_dir,
            formats=["obj", "stl", "fbx"],
            recursive=False
        )

        if equipment_files:
            print(f"Found {len(equipment_files)} equipment models")

            # Process each equipment piece
            def render_equipment(equipment_file: Path):
                """Render single equipment piece."""
                # Create blend file
                blend_file = output_dir / f"{equipment_file.stem}.blend"

                # Import
                wrapper = BlenderWrapper()
                wrapper.import_file(
                    equipment_file,
                    equipment_file.suffix.lstrip('.'),
                    blend_file
                )

                # Setup consistent lighting and camera
                manager.setup_camera(
                    blend_file,
                    location=(5, -5, 3),
                    look_at=(0, 0, 0),
                    output_file=blend_file
                )

                manager.setup_lighting(
                    blend_file,
                    light_type="AREA",
                    location=(3, -3, 5),
                    energy=100.0,
                    output_file=blend_file
                )

                # Render catalog image
                render_path = output_dir / f"{equipment_file.stem}_catalog.png"
                manager.render_image(
                    blend_file,
                    render_path,
                    resolution_x=1024,
                    resolution_y=1024,
                    samples=128
                )

                return {"equipment": equipment_file.stem, "render": str(render_path)}

            # Batch render all equipment
            result = processor.process_files(
                equipment_files[:5],  # Limit for demo
                render_equipment,
                parallel=True
            )

            print(f"✓ Rendered {result['successful']} catalog images")

        else:
            print("⚠ No equipment models found")

    else:
        print(f"⚠ Equipment directory not found: {equipment_dir}")


def example_marine_assembly_export():
    """
    Example: Export complete marine assembly for external tools.

    This example demonstrates:
    - Complex assembly management
    - Multi-format export
    - Integration with analysis tools
    """
    print("\n=== Marine Assembly Export ===")

    exporter = MeshExporter()

    assembly_blend = Path("examples/marine_output/platform.blend")

    if assembly_blend.exists():
        print(f"Exporting assembly from {assembly_blend}...")

        output_dir = Path("examples/marine_output/exports")
        output_dir.mkdir(parents=True, exist_ok=True)

        # Export to various formats for different tools
        export_configs = [
            {
                "format": "fbx",
                "purpose": "Unity/Unreal visualization",
                "options": {"apply_scale_options": "FBX_SCALE_ALL"}
            },
            {
                "format": "obj",
                "purpose": "General 3D analysis",
                "options": {"export_materials": True}
            },
            {
                "format": "stl",
                "purpose": "FEA/CFD meshing",
                "options": {"binary": True}
            },
            {
                "format": "gltf",
                "purpose": "Web visualization",
                "options": {"export_format": "GLB", "export_draco": True}
            }
        ]

        for config in export_configs:
            fmt = config["format"]
            output_file = output_dir / f"assembly.{fmt}"

            print(f"\nExporting {fmt.upper()} ({config['purpose']})...")

            result = exporter.export_file(
                assembly_blend,
                output_file,
                fmt,
                **config["options"]
            )

            if result["success"]:
                print(f"  ✓ {output_file}")
                # Get file size
                size_mb = output_file.stat().st_size / (1024 * 1024)
                print(f"  Size: {size_mb:.2f} MB")

    else:
        print(f"⚠ Assembly file not found: {assembly_blend}")
        print("  Run offshore_platform_visualization example first")


def main():
    """Run all marine engineering examples."""
    print("Blender Automation - Marine Engineering Integration")
    print("=" * 60)

    try:
        example_offshore_platform_visualization()
        example_ship_hull_analysis()
        example_mooring_system_visualization()
        example_subsea_equipment_catalog()
        example_marine_assembly_export()

        print("\n" + "=" * 60)
        print("✓ Marine engineering examples completed!")
        print("\nNote: Some examples require actual CAD model files.")
        print("Place your marine engineering models in:")
        print("  - marine_models/")
        print("  - mooring_models/")
        print("  - marine_models/subsea_equipment/")

    except Exception as e:
        print(f"\n✗ Error: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()
