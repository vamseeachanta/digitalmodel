# ABOUTME: Blender (bpy) fly-through of the onshore tracer scene JSON (#1508).
# ABOUTME: REQUIRES BLENDER — run inside blender, not plain python; stdlib-only imports.
"""Render a short fly-through of an onshore field layout scene in Blender.

REQUIRES BLENDER (tested API target: Blender >= 3.6). This script imports
``bpy`` and therefore CANNOT run under a plain Python interpreter — it must be
executed by Blender itself, headless:

    # 1. produce the scene JSON with the tracer API
    .venv/bin/python -c "
    from digitalmodel.field_development import screen_layout
    screen_layout(
        'src/digitalmodel/field_development/data/onshore_demo_field.yml',
        output_dir='outputs/field_development',
    )"

    # 2. render the ~4 s fly-through (96 frames @ 24 fps, MPEG-4)
    blender --background --python \
        scripts/field_development/onshore_flythrough_bpy.py -- \
        --scene outputs/field_development/onshore_demo_scene.json \
        --out outputs/field_development/onshore_demo_flythrough.mp4

The scene JSON is produced by
``digitalmodel.field_development.onshore_layout.export_scene_json`` (JSON so
Blender's bundled Python needs no third-party packages). The rendered video is
NOT committed to the repo (see .gitignore) — re-render it locally with the
command above.

Scene contents: terrain mesh built from the elevation grid, a cube for the
host/processing pad, cylinders for wells, bevelled curves for flowlines, a sun
lamp, and a camera orbiting the field centre with a slow descent.
"""

from __future__ import annotations

import argparse
import json
import math
import sys
from pathlib import Path

import bpy  # noqa: F401  (only available inside Blender)

FPS = 24
DURATION_S = 4.0
FRAME_COUNT = int(FPS * DURATION_S)
# Vertical exaggeration makes gentle onshore relief readable in the clip.
Z_EXAGGERATION = 2.0
FLOWLINE_BEVEL_M = 8.0
WELL_RADIUS_M = 25.0
WELL_HEIGHT_M = 60.0
HOST_SIZE_M = 120.0


def _parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--scene", required=True, help="scene JSON from export_scene_json"
    )
    parser.add_argument("--out", required=True, help="output video path (.mp4)")
    return parser.parse_args(argv)


def _clear_default_scene() -> None:
    bpy.ops.object.select_all(action="SELECT")
    bpy.ops.object.delete()


def _add_terrain(terrain: dict) -> tuple[float, float, float]:
    """Build the terrain mesh; return the scene centre (x, y, z)."""
    xs, ys, zs = terrain["x_m"], terrain["y_m"], terrain["z_m"]
    nx, ny = len(xs), len(ys)
    z0 = min(min(row) for row in zs)
    verts = [
        (xs[i], ys[j], (zs[j][i] - z0) * Z_EXAGGERATION)
        for j in range(ny)
        for i in range(nx)
    ]
    faces = [
        (j * nx + i, j * nx + i + 1, (j + 1) * nx + i + 1, (j + 1) * nx + i)
        for j in range(ny - 1)
        for i in range(nx - 1)
    ]
    mesh = bpy.data.meshes.new("terrain")
    mesh.from_pydata(verts, [], faces)
    mesh.update()
    obj = bpy.data.objects.new("terrain", mesh)
    bpy.context.collection.objects.link(obj)

    material = bpy.data.materials.new("terrain_mat")
    material.diffuse_color = (0.35, 0.45, 0.25, 1.0)
    obj.data.materials.append(material)

    cx = 0.5 * (xs[0] + xs[-1])
    cy = 0.5 * (ys[0] + ys[-1])
    cz = (max(max(row) for row in zs) - z0) * Z_EXAGGERATION * 0.5
    return cx, cy, cz


def _scene_z(z_m: float, z0: float) -> float:
    return (z_m - z0) * Z_EXAGGERATION


def _add_assets(scene: dict, z0: float) -> None:
    for asset in scene["assets"]:
        z = _scene_z(asset["z_m"], z0)
        if asset["kind"] == "host":
            bpy.ops.mesh.primitive_cube_add(
                size=HOST_SIZE_M,
                location=(asset["x_m"], asset["y_m"], z + HOST_SIZE_M / 2),
            )
        else:
            bpy.ops.mesh.primitive_cylinder_add(
                radius=WELL_RADIUS_M,
                depth=WELL_HEIGHT_M,
                location=(asset["x_m"], asset["y_m"], z + WELL_HEIGHT_M / 2),
            )
        bpy.context.active_object.name = f"asset_{asset['id']}"


def _add_flowlines(scene: dict, z0: float) -> None:
    for flowline in scene["flowlines"]:
        curve = bpy.data.curves.new(f"fl_{flowline['id']}", type="CURVE")
        curve.dimensions = "3D"
        curve.bevel_depth = FLOWLINE_BEVEL_M
        spline = curve.splines.new("POLY")
        path = flowline["path_xyz_m"]
        spline.points.add(len(path) - 1)
        for point, (x, y, z) in zip(spline.points, path):
            point.co = (x, y, _scene_z(z, z0) + FLOWLINE_BEVEL_M, 1.0)
        obj = bpy.data.objects.new(f"fl_{flowline['id']}", curve)
        bpy.context.collection.objects.link(obj)


def _add_camera_flythrough(centre: tuple[float, float, float], radius: float) -> None:
    cx, cy, cz = centre
    target = bpy.data.objects.new("cam_target", None)
    target.location = (cx, cy, cz)
    bpy.context.collection.objects.link(target)

    camera_data = bpy.data.cameras.new("camera")
    camera = bpy.data.objects.new("camera", camera_data)
    bpy.context.collection.objects.link(camera)
    bpy.context.scene.camera = camera
    track = camera.constraints.new(type="TRACK_TO")
    track.target = target

    scene = bpy.context.scene
    scene.frame_start, scene.frame_end = 1, FRAME_COUNT
    scene.render.fps = FPS
    for frame in range(1, FRAME_COUNT + 1):
        t = (frame - 1) / (FRAME_COUNT - 1)
        angle = math.radians(200.0) * t
        height = radius * (0.9 - 0.45 * t)
        camera.location = (
            cx + radius * math.cos(angle),
            cy + radius * math.sin(angle),
            cz + height,
        )
        camera.keyframe_insert(data_path="location", frame=frame)

    sun_data = bpy.data.lights.new("sun", type="SUN")
    sun = bpy.data.objects.new("sun", sun_data)
    sun.location = (cx, cy, cz + 2 * radius)
    sun.rotation_euler = (math.radians(30), 0.0, math.radians(45))
    bpy.context.collection.objects.link(sun)


def _configure_render(out_path: str) -> None:
    scene = bpy.context.scene
    render = scene.render
    render.resolution_x, render.resolution_y = 1280, 720
    render.image_settings.file_format = "FFMPEG"
    render.ffmpeg.format = "MPEG4"
    render.ffmpeg.codec = "H264"
    render.filepath = out_path
    # Prefer EEVEE (fast); engine id differs across Blender versions.
    for engine in ("BLENDER_EEVEE_NEXT", "BLENDER_EEVEE", "BLENDER_WORKBENCH"):
        try:
            render.engine = engine
            break
        except TypeError:
            continue


def main() -> None:
    argv = sys.argv[sys.argv.index("--") + 1 :] if "--" in sys.argv else []
    args = _parse_args(argv)
    with open(args.scene, "r", encoding="utf-8") as fh:
        scene_data = json.load(fh)

    _clear_default_scene()
    terrain = scene_data["terrain"]
    z0 = min(min(row) for row in terrain["z_m"])
    centre = _add_terrain(terrain)
    _add_assets(scene_data, z0)
    _add_flowlines(scene_data, z0)
    span_x = terrain["x_m"][-1] - terrain["x_m"][0]
    span_y = terrain["y_m"][-1] - terrain["y_m"][0]
    _add_camera_flythrough(centre, radius=0.85 * math.hypot(span_x, span_y))

    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    _configure_render(args.out)
    bpy.ops.render.render(animation=True)
    print(f"fly-through rendered to {args.out}")


if __name__ == "__main__":
    main()
