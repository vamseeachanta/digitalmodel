"""
ABOUTME: Mesh export utilities for various 3D formats
Handles export from Blender to STL, OBJ, FBX, GLTF, and other formats.
"""

from pathlib import Path
from typing import Optional, Dict, Any, List
import logging

from ..core.blender_wrapper import BlenderWrapper


logger = logging.getLogger(__name__)


class MeshExporter:
    """
    Export Blender scenes to various 3D formats.

    Supports STL, OBJ, FBX, GLTF, Collada, and other common formats.
    Provides options for geometry optimization and format-specific settings.
    """

    SUPPORTED_FORMATS = {
        "stl": {
            "binary": True,
            "ascii": True,
            "use_scene_unit": True
        },
        "obj": {
            "forward_axis": "-Z",
            "up_axis": "Y",
            "export_materials": True,
            "export_uv": True
        },
        "fbx": {
            "use_selection": False,
            "apply_scale_options": "FBX_SCALE_ALL",
            "mesh_smooth_type": "FACE"
        },
        "gltf": {
            "export_format": "GLB",  # or GLTF_SEPARATE
            "export_draco": False,
            "export_materials": "EXPORT"
        },
        "dae": {  # Collada
            "apply_modifiers": True,
            "triangulate": True
        },
        "ply": {
            "export_normals": True,
            "export_uv": True,
            "export_colors": "SRGB"
        },
        "x3d": {
            "use_mesh_modifiers": True,
            "use_triangulate": True
        }
    }

    def __init__(self, blender_wrapper: Optional[BlenderWrapper] = None):
        """
        Initialize mesh exporter.

        Args:
            blender_wrapper: BlenderWrapper instance. Creates new one if None.
        """
        self.blender = blender_wrapper or BlenderWrapper()

    def export_file(
        self,
        blend_file: Path,
        output_path: Path,
        file_format: Optional[str] = None,
        selection_only: bool = False,
        apply_modifiers: bool = True,
        **format_options
    ) -> Dict[str, Any]:
        """
        Export Blender file to specified format.

        Args:
            blend_file: Input .blend file
            output_path: Output file path
            file_format: Output format (auto-detected from extension if None)
            selection_only: Export only selected objects
            apply_modifiers: Apply modifiers before export
            **format_options: Format-specific options

        Returns:
            Dictionary with export results
        """
        output_path = Path(output_path)

        if file_format is None:
            file_format = output_path.suffix.lower().lstrip('.')

        if file_format not in self.SUPPORTED_FORMATS:
            raise ValueError(
                f"Unsupported format: {file_format}. "
                f"Supported: {', '.join(self.SUPPORTED_FORMATS.keys())}"
            )

        # Get default options for format
        default_options = self.SUPPORTED_FORMATS[file_format].copy()
        default_options.update(format_options)

        # Add common options
        if 'use_selection' in default_options:
            default_options['use_selection'] = selection_only
        if 'apply_modifiers' in default_options:
            default_options['apply_modifiers'] = apply_modifiers

        return self.blender.export_file(
            blend_file,
            output_path,
            file_format,
            **default_options
        )

    def export_stl(
        self,
        blend_file: Path,
        output_path: Path,
        binary: bool = True,
        use_scene_unit: bool = True,
        ascii: bool = False
    ) -> Dict[str, Any]:
        """
        Export to STL format (optimized for 3D printing).

        Args:
            blend_file: Input .blend file
            output_path: Output .stl file path
            binary: Use binary STL format (smaller file size)
            use_scene_unit: Use scene unit scale
            ascii: Use ASCII STL format (if binary=False)

        Returns:
            Dictionary with export results
        """
        return self.export_file(
            blend_file,
            output_path,
            "stl",
            binary=binary,
            use_scene_unit=use_scene_unit,
            ascii=ascii
        )

    def export_obj(
        self,
        blend_file: Path,
        output_path: Path,
        export_materials: bool = True,
        export_uv: bool = True,
        forward_axis: str = "-Z",
        up_axis: str = "Y"
    ) -> Dict[str, Any]:
        """
        Export to OBJ format with optional MTL materials.

        Args:
            blend_file: Input .blend file
            output_path: Output .obj file path
            export_materials: Export materials (.mtl file)
            export_uv: Export UV coordinates
            forward_axis: Forward axis mapping
            up_axis: Up axis mapping

        Returns:
            Dictionary with export results
        """
        return self.export_file(
            blend_file,
            output_path,
            "obj",
            export_materials=export_materials,
            export_uv=export_uv,
            forward_axis=forward_axis,
            up_axis=up_axis
        )

    def export_fbx(
        self,
        blend_file: Path,
        output_path: Path,
        apply_scale: str = "FBX_SCALE_ALL",
        mesh_smooth_type: str = "FACE"
    ) -> Dict[str, Any]:
        """
        Export to FBX format (game engines, animation).

        Args:
            blend_file: Input .blend file
            output_path: Output .fbx file path
            apply_scale: Scale application method
            mesh_smooth_type: Smoothing type (FACE, EDGE, OFF)

        Returns:
            Dictionary with export results
        """
        return self.export_file(
            blend_file,
            output_path,
            "fbx",
            apply_scale_options=apply_scale,
            mesh_smooth_type=mesh_smooth_type
        )

    def export_gltf(
        self,
        blend_file: Path,
        output_path: Path,
        format: str = "GLB",
        export_draco: bool = False,
        export_materials: str = "EXPORT"
    ) -> Dict[str, Any]:
        """
        Export to GLTF/GLB format (web, AR/VR).

        Args:
            blend_file: Input .blend file
            output_path: Output .gltf or .glb file path
            format: Export format (GLB or GLTF_SEPARATE)
            export_draco: Use Draco compression
            export_materials: Material export mode

        Returns:
            Dictionary with export results
        """
        return self.export_file(
            blend_file,
            output_path,
            "gltf",
            export_format=format,
            export_draco=export_draco,
            export_materials=export_materials
        )

    def batch_export(
        self,
        blend_files: List[Path],
        output_dir: Path,
        file_format: str,
        **export_options
    ) -> Dict[str, Any]:
        """
        Export multiple Blender files to specified format.

        Args:
            blend_files: List of .blend file paths
            output_dir: Output directory
            file_format: Export format
            **export_options: Format-specific options

        Returns:
            Dictionary with batch export results
        """
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)

        results = []

        for blend_file in blend_files:
            try:
                output_file = output_dir / f"{blend_file.stem}.{file_format}"

                result = self.export_file(
                    blend_file,
                    output_file,
                    file_format,
                    **export_options
                )

                results.append({
                    "input": str(blend_file),
                    "output": str(output_file),
                    "success": result["success"],
                    "error": result.get("error")
                })

            except Exception as e:
                logger.error(f"Failed to export {blend_file}: {e}")
                results.append({
                    "input": str(blend_file),
                    "output": None,
                    "success": False,
                    "error": str(e)
                })

        return {
            "total": len(blend_files),
            "successful": sum(1 for r in results if r["success"]),
            "failed": sum(1 for r in results if not r["success"]),
            "results": results
        }

    def optimize_for_web(
        self,
        blend_file: Path,
        output_path: Path,
        target_size_mb: float = 5.0
    ) -> Dict[str, Any]:
        """
        Optimize and export for web (GLTF with compression).

        Args:
            blend_file: Input .blend file
            output_path: Output .glb file path
            target_size_mb: Target file size in MB

        Returns:
            Dictionary with export results
        """
        # First attempt: standard GLTF
        result = self.export_gltf(
            blend_file,
            output_path,
            format="GLB",
            export_draco=True
        )

        if result["success"]:
            file_size_mb = output_path.stat().st_size / (1024 * 1024)
            result["file_size_mb"] = file_size_mb
            result["meets_target"] = file_size_mb <= target_size_mb

        return result
