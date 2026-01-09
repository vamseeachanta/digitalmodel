"""
ABOUTME: CAD file import utilities for Blender
Handles import of STEP, IGES, STL, and other CAD formats into Blender.
"""

from pathlib import Path
from typing import Optional, Dict, Any, List
import logging

from ..core.blender_wrapper import BlenderWrapper


logger = logging.getLogger(__name__)


class CADImporter:
    """
    Import CAD files into Blender.

    Supports STEP, IGES, STL, OBJ, FBX, and other common formats.
    Can integrate with FreeCAD for STEP/IGES conversion if available.
    """

    SUPPORTED_FORMATS = {
        "step": {"direct": False, "requires": "freecad"},
        "stp": {"direct": False, "requires": "freecad"},
        "iges": {"direct": False, "requires": "freecad"},
        "igs": {"direct": False, "requires": "freecad"},
        "stl": {"direct": True, "requires": None},
        "obj": {"direct": True, "requires": None},
        "fbx": {"direct": True, "requires": None},
        "dae": {"direct": True, "requires": None},
        "ply": {"direct": True, "requires": None},
        "x3d": {"direct": True, "requires": None},
    }

    def __init__(self, blender_wrapper: Optional[BlenderWrapper] = None):
        """
        Initialize CAD importer.

        Args:
            blender_wrapper: BlenderWrapper instance. Creates new one if None.
        """
        self.blender = blender_wrapper or BlenderWrapper()
        self._check_freecad_available()

    def _check_freecad_available(self) -> bool:
        """Check if FreeCAD is available for STEP/IGES import."""
        try:
            import FreeCAD
            self.freecad_available = True
            logger.info("FreeCAD available for STEP/IGES import")
            return True
        except ImportError:
            self.freecad_available = False
            logger.warning("FreeCAD not available. STEP/IGES import will be limited.")
            return False

    def import_file(
        self,
        file_path: Path,
        output_blend: Optional[Path] = None,
        scale: float = 1.0,
        cleanup: bool = True
    ) -> Dict[str, Any]:
        """
        Import a CAD file into Blender.

        Args:
            file_path: Path to CAD file
            output_blend: Optional path to save .blend file
            scale: Scale factor for imported geometry
            cleanup: Whether to cleanup/optimize imported mesh

        Returns:
            Dictionary with import results
        """
        file_path = Path(file_path)
        if not file_path.exists():
            raise FileNotFoundError(f"File not found: {file_path}")

        format_lower = file_path.suffix.lower().lstrip('.')

        if format_lower not in self.SUPPORTED_FORMATS:
            raise ValueError(
                f"Unsupported format: {format_lower}. "
                f"Supported: {', '.join(self.SUPPORTED_FORMATS.keys())}"
            )

        format_info = self.SUPPORTED_FORMATS[format_lower]

        # Direct import for supported formats
        if format_info["direct"]:
            return self._import_direct(file_path, output_blend, scale, cleanup)

        # STEP/IGES require conversion
        if format_info["requires"] == "freecad":
            if not self.freecad_available:
                raise RuntimeError(
                    f"Format {format_lower} requires FreeCAD. "
                    "Install FreeCAD Python bindings."
                )
            return self._import_via_freecad(file_path, output_blend, scale, cleanup)

        raise RuntimeError(f"No import method available for {format_lower}")

    def _import_direct(
        self,
        file_path: Path,
        output_blend: Optional[Path],
        scale: float,
        cleanup: bool
    ) -> Dict[str, Any]:
        """Import file directly using Blender's built-in importers."""
        result = self.blender.import_file(
            file_path,
            file_path.suffix.lstrip('.'),
            output_blend
        )

        if cleanup and result["success"]:
            self._cleanup_mesh(output_blend or file_path)

        return result

    def _import_via_freecad(
        self,
        file_path: Path,
        output_blend: Optional[Path],
        scale: float,
        cleanup: bool
    ) -> Dict[str, Any]:
        """
        Import STEP/IGES via FreeCAD conversion to STL/OBJ.

        Converts STEP/IGES to intermediate format, then imports to Blender.
        """
        import FreeCAD
        import Mesh
        import tempfile

        try:
            # Open STEP/IGES in FreeCAD
            doc = FreeCAD.open(str(file_path))

            # Export to STL (intermediate format)
            temp_stl = Path(tempfile.mktemp(suffix=".stl"))

            # Get all shapes and export
            shapes = []
            for obj in doc.Objects:
                if hasattr(obj, "Shape"):
                    shapes.append(obj.Shape)

            if not shapes:
                raise RuntimeError("No valid shapes found in CAD file")

            # Convert to mesh and export
            mesh = Mesh.Mesh()
            for shape in shapes:
                mesh.addMesh(Mesh.Mesh(shape.tessellate(0.1)))

            mesh.write(str(temp_stl))

            # Import STL into Blender
            result = self.blender.import_file(
                temp_stl,
                "stl",
                output_blend
            )

            # Cleanup temp file
            temp_stl.unlink(missing_ok=True)

            if cleanup and result["success"]:
                self._cleanup_mesh(output_blend or temp_stl)

            return result

        except Exception as e:
            logger.error(f"FreeCAD import failed: {e}")
            return {
                "success": False,
                "error": str(e)
            }

    def _cleanup_mesh(self, blend_file: Path) -> None:
        """
        Cleanup and optimize imported mesh.

        Removes doubles, recalculates normals, etc.
        """
        script = """
import bpy

# Select all mesh objects
bpy.ops.object.select_all(action='DESELECT')
for obj in bpy.data.objects:
    if obj.type == 'MESH':
        obj.select_set(True)
        bpy.context.view_layer.objects.active = obj

# Cleanup operations
if bpy.context.selected_objects:
    # Enter edit mode
    bpy.ops.object.mode_set(mode='EDIT')

    # Remove doubles
    bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.remove_doubles()

    # Recalculate normals
    bpy.ops.mesh.normals_make_consistent(inside=False)

    # Back to object mode
    bpy.ops.object.mode_set(mode='OBJECT')
"""

        self.blender.run_script(script, background=True, blend_file=blend_file)

    def batch_import(
        self,
        file_paths: List[Path],
        output_blend: Path,
        merge: bool = True
    ) -> Dict[str, Any]:
        """
        Import multiple CAD files into a single Blender scene.

        Args:
            file_paths: List of CAD file paths
            output_blend: Output .blend file path
            merge: Whether to merge all into single scene

        Returns:
            Dictionary with batch import results
        """
        results = []

        for i, file_path in enumerate(file_paths):
            try:
                if i == 0:
                    # First import creates the scene
                    result = self.import_file(file_path, output_blend)
                else:
                    # Subsequent imports append to existing scene
                    if merge:
                        # Append to existing blend file
                        script = f"""
import bpy
bpy.ops.wm.append(filepath='{file_path}')
"""
                        result = self.blender.run_script(
                            script,
                            background=True,
                            blend_file=output_blend
                        )
                    else:
                        result = self.import_file(file_path)

                results.append({
                    "file": str(file_path),
                    "success": result["success"],
                    "error": result.get("error")
                })

            except Exception as e:
                logger.error(f"Failed to import {file_path}: {e}")
                results.append({
                    "file": str(file_path),
                    "success": False,
                    "error": str(e)
                })

        return {
            "total": len(file_paths),
            "successful": sum(1 for r in results if r["success"]),
            "failed": sum(1 for r in results if not r["success"]),
            "results": results
        }
