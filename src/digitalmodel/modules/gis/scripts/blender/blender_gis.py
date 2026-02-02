"""Generate Python scripts for Blender 3D visualization of offshore GIS data.

This module produces script *text* that can be executed inside Blender.
It does NOT import bpy itself.
"""
from __future__ import annotations

import textwrap
from pathlib import Path


class BlenderScriptGenerator:
    """Generate Blender Python scripts for offshore GIS visualisation."""

    @classmethod
    def generate_point_cloud_script(
        cls,
        points_data: list[dict],
        scale: float = 1.0,
    ) -> str:
        """Generate a Blender script that creates icosphere meshes at 3D positions.

        Each dict in *points_data* must contain either
        ``{'name', 'x', 'y', 'z'}`` or ``{'name', 'longitude', 'latitude', 'elevation'}``.
        """
        normalised = cls._normalise_points(points_data)
        points_literal = repr(normalised)

        return textwrap.dedent(f"""\
            import bpy

            points = {points_literal}
            scale = {scale!r}

            collection = bpy.data.collections.new("PointCloud")
            bpy.context.scene.collection.children.link(collection)

            for pt in points:
                bpy.ops.mesh.primitive_ico_sphere_add(
                    radius=scale,
                    location=(pt["x"], pt["y"], pt["z"]),
                )
                obj = bpy.context.active_object
                obj.name = pt["name"]
                for col in obj.users_collection:
                    col.objects.unlink(obj)
                collection.objects.link(obj)
        """)

    @classmethod
    def generate_pipeline_script(cls, routes: list[dict]) -> str:
        """Generate a Blender script that creates 3D pipeline curves.

        Each route dict must contain ``{'name', 'coordinates'}``
        where *coordinates* is a list of ``(x, y, z)`` tuples.
        """
        routes_literal = repr(routes)

        return textwrap.dedent(f"""\
            import bpy

            routes = {routes_literal}

            collection = bpy.data.collections.new("Pipelines")
            bpy.context.scene.collection.children.link(collection)

            for route in routes:
                curve_data = bpy.data.curves.new(name=route["name"], type="CURVE")
                curve_data.dimensions = "3D"
                spline = curve_data.splines.new("POLY")
                coords = route["coordinates"]
                spline.points.add(len(coords) - 1)
                for i, (x, y, z) in enumerate(coords):
                    spline.points[i].co = (x, y, z, 1)
                obj = bpy.data.objects.new(route["name"], curve_data)
                collection.objects.link(obj)
        """)

    @classmethod
    def generate_bathymetry_script(cls, grid_data: dict) -> str:
        """Generate a Blender script that creates a mesh surface from bathymetry.

        *grid_data* must contain ``{'vertices': [(x,y,z), ...], 'faces': [(i,j,k,...), ...]}``.
        """
        vertices_literal = repr(grid_data["vertices"])
        faces_literal = repr(grid_data["faces"])

        return textwrap.dedent(f"""\
            import bpy

            vertices = {vertices_literal}
            faces = {faces_literal}

            mesh = bpy.data.meshes.new("BathymetryMesh")
            mesh.from_pydata(vertices, [], faces)
            mesh.update()

            obj = bpy.data.objects.new("Bathymetry", mesh)
            bpy.context.scene.collection.objects.link(obj)
        """)

    @classmethod
    def save_script(cls, script: str, filepath) -> Path:
        """Write *script* text to a ``.py`` file and return its path."""
        path = Path(filepath)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(script, encoding="utf-8")
        return path

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    @classmethod
    def _normalise_points(cls, points_data: list[dict]) -> list[dict]:
        """Normalise point dicts to ``{'name', 'x', 'y', 'z'}``."""
        normalised: list[dict] = []
        for pt in points_data:
            name = pt.get("name", "Point")
            x = pt.get("x", pt.get("longitude", 0.0))
            y = pt.get("y", pt.get("latitude", 0.0))
            z = pt.get("z", pt.get("elevation", 0.0))
            normalised.append({"name": name, "x": x, "y": y, "z": z})
        return normalised
