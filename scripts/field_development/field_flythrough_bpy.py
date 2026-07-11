# ABOUTME: Blender (bpy) fly-through of a generalized field-layout scene JSON (#1510).
# ABOUTME: REQUIRES BLENDER — run inside blender, not plain python; stdlib-only imports.
"""Render a fly-through of a generalized field-layout scene in Blender.

REQUIRES BLENDER (tested API target: Blender >= 3.6). This script imports
``bpy`` and therefore CANNOT run under a plain Python interpreter — it must be
executed by Blender itself, headless:

    # 1. produce the scene JSON with the generalized visualization API
    .venv/bin/python -c "
    from digitalmodel.field_development.visualization import (
        render_layout_visualization,
    )
    render_layout_visualization(
        'src/digitalmodel/field_development/data/offshore_demo_field.yml',
        output_dir='outputs/field_development',
    )"

    # 2. render the fly-through (default 96 frames @ 24 fps, MPEG-4)
    blender --background --python \
        scripts/field_development/field_flythrough_bpy.py -- \
        --scene outputs/field_development/offshore_demo_field_scene.json \
        --out outputs/field_development/offshore_demo_flythrough.mp4

The scene JSON is schema ``digitalmodel.field_layout_scene/2``, produced by
``digitalmodel.field_development.visualization.export_layout_scene_json``
(JSON so Blender's bundled Python needs no third-party packages). It types
every asset (host/vessel/well/tree/manifold plus the #1513 renewables kinds
wind_turbine/offshore_substation) and connection
(jumper/flowline/pipeline/umbilical plus array_cable/export_cable), so each
kind gets its own geometry and material here. The onshore #1508 tracer scene
(v1 schema) keeps its own script, ``onshore_flythrough_bpy.py``. Rendered
videos are NOT committed to the repo (see .gitignore) — re-render locally
with the command above.

Scene contents: surface mesh from the elevation grid (bathymetry = negative
elevation), a translucent water plane at z = 0 for subsea scenes, per-kind
asset geometry (pad cubes for hosts, hull boxes for vessels, cylinders for
wells, slimmer/taller cylinders for trees, cones for manifolds, schematic
tower + hub + three-blade rotors for wind turbines, distinct topside boxes
for offshore substations), bevelled curves per connection kind, a sun lamp,
and a camera orbiting the field centre with a slow descent. Camera clip
range is scaled to the scene size — Blender's default clip_end (100 m) would
cull a km-scale field entirely.
"""

from __future__ import annotations

import argparse
import json
import math
import sys
from pathlib import Path

import bpy  # noqa: F401  (only available inside Blender)

# Vertical exaggeration makes gentle relief readable in the clip.
Z_EXAGGERATION = 2.0

# Base asset dimensions [m], scaled up on large domains (see _size_scale).
HOST_SIZE_M = 120.0
VESSEL_DIMS_M = (280.0, 80.0, 40.0)  # hull length, beam, depth
WELL_RADIUS_M = 25.0
WELL_HEIGHT_M = 60.0
TREE_RADIUS_M = 12.0
TREE_HEIGHT_M = 110.0
MANIFOLD_RADIUS_M = 45.0
MANIFOLD_HEIGHT_M = 80.0
# Wind turbine (#1513): schematic tower + hub + three flat blades.
TURBINE_TOWER_RADIUS_M = 6.0
TURBINE_TOWER_HEIGHT_M = 140.0
TURBINE_HUB_RADIUS_M = 12.0
TURBINE_BLADE_LENGTH_M = 100.0
TURBINE_BLADE_CHORD_M = 14.0
TURBINE_BLADE_THICKNESS_M = 3.0
# Offshore substation (#1513): distinct wide topside box.
SUBSTATION_DIMS_M = (110.0, 90.0, 55.0)  # length, width, height
# Reference domain span for the base sizes; larger fields scale up (capped).
REFERENCE_SPAN_M = 3000.0
MAX_SIZE_SCALE = 4.0

# Per-kind flat-shaded material colors (r, g, b, a) and connection bevels [m].
ASSET_COLORS = {
    "host": (0.12, 0.23, 0.37, 1.0),  # dark navy
    "vessel": (0.70, 0.35, 0.11, 1.0),  # dark orange
    "well": (0.13, 0.13, 0.13, 1.0),  # near-black
    "tree": (0.18, 0.44, 0.25, 1.0),  # green
    "manifold": (0.42, 0.25, 0.63, 1.0),  # purple
    "wind_turbine": (0.85, 0.87, 0.90, 1.0),  # off-white (#1513)
    "offshore_substation": (0.55, 0.43, 0.12, 1.0),  # dark gold (#1513)
}
CONNECTION_COLORS = {
    "jumper": (0.88, 0.48, 0.22, 1.0),
    "flowline": (0.70, 0.09, 0.17, 1.0),
    "pipeline": (0.40, 0.00, 0.12, 1.0),
    "umbilical": (0.13, 0.40, 0.67, 1.0),
    "array_cable": (0.25, 0.67, 0.36, 1.0),  # mid green (#1513)
    "export_cable": (0.00, 0.27, 0.11, 1.0),  # darkest green (#1513)
}
CONNECTION_BEVEL_M = {
    "jumper": 6.0,
    "flowline": 8.0,
    "pipeline": 10.0,
    "umbilical": 5.0,
    "array_cable": 5.0,
    "export_cable": 7.0,
}
SURFACE_COLOR_ONSHORE = (0.35, 0.45, 0.25, 1.0)
SURFACE_COLOR_SUBSEA = (0.45, 0.40, 0.30, 1.0)  # seabed sediment
WATER_COLOR = (0.05, 0.22, 0.45, 1.0)
WATER_ALPHA = 0.22


def _parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--scene", required=True, help="scene JSON from export_layout_scene_json"
    )
    parser.add_argument("--out", required=True, help="output video path (.mp4)")
    parser.add_argument(
        "--frames", type=int, default=96, help="frame count (default 96 = 4 s)"
    )
    parser.add_argument("--fps", type=int, default=24, help="frames/s (default 24)")
    parser.add_argument(
        "--res-scale",
        type=int,
        default=100,
        help="render resolution percentage of 1280x720 (default 100)",
    )
    return parser.parse_args(argv)


def _clear_default_scene() -> None:
    bpy.ops.object.select_all(action="SELECT")
    bpy.ops.object.delete()


def _material(name: str, rgba: tuple, alpha: float = 1.0):
    """Simple flat-shaded material; translucent when alpha < 1 (water)."""
    material = bpy.data.materials.new(name)
    material.diffuse_color = (*rgba[:3], alpha)
    material.use_nodes = True
    bsdf = material.node_tree.nodes.get("Principled BSDF")
    if bsdf is not None:
        bsdf.inputs["Base Color"].default_value = rgba
        bsdf.inputs["Roughness"].default_value = 1.0
        # Kill glossy sky reflections (flat shading); input name varies by
        # Blender version (4.x: "Specular IOR Level", 3.x: "Specular").
        for specular_input in ("Specular IOR Level", "Specular"):
            if specular_input in bsdf.inputs:
                bsdf.inputs[specular_input].default_value = 0.0
                break
        if alpha < 1.0:
            bsdf.inputs["Alpha"].default_value = alpha
    if alpha < 1.0:
        material.blend_method = "BLEND"
    return material


def _size_scale(span_m: float) -> float:
    """Scale factor for asset geometry so markers stay visible on big fields."""
    return max(1.0, min(MAX_SIZE_SCALE, span_m / REFERENCE_SPAN_M))


def _scene_z(z_m: float, z0: float) -> float:
    return (z_m - z0) * Z_EXAGGERATION


def _add_surface(surface: dict, subsea: bool) -> tuple[float, float]:
    """Build the surface mesh; return (centre_x, centre_y)."""
    xs, ys, zs = surface["x_m"], surface["y_m"], surface["z_m"]
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
    mesh = bpy.data.meshes.new("surface")
    mesh.from_pydata(verts, [], faces)
    mesh.update()
    obj = bpy.data.objects.new("surface", mesh)
    bpy.context.collection.objects.link(obj)
    color = SURFACE_COLOR_SUBSEA if subsea else SURFACE_COLOR_ONSHORE
    obj.data.materials.append(_material("surface_mat", color))
    return 0.5 * (xs[0] + xs[-1]), 0.5 * (ys[0] + ys[-1])


def _add_water_plane(surface: dict, z0: float) -> float:
    """Translucent water surface at z = 0 (scene z); returns its scene height."""
    xs, ys = surface["x_m"], surface["y_m"]
    cx, cy = 0.5 * (xs[0] + xs[-1]), 0.5 * (ys[0] + ys[-1])
    z_water = _scene_z(0.0, z0)
    span = max(xs[-1] - xs[0], ys[-1] - ys[0])
    bpy.ops.mesh.primitive_plane_add(size=1.2 * span, location=(cx, cy, z_water))
    water = bpy.context.active_object
    water.name = "water_surface"
    water.data.materials.append(_material("water_mat", WATER_COLOR, WATER_ALPHA))
    return z_water


def _asset_scene_z(asset: dict, z0: float) -> float:
    """Scene-space base elevation of one asset (explicit z for off-surface)."""
    return _scene_z(asset["z_m"], z0)


def _add_wind_turbine(asset: dict, x: float, y: float, z: float, scale: float) -> float:
    """Schematic wind turbine (#1513): tower cylinder + hub sphere + 3 blades.

    The rotor is a vertical plane (blades rotated 120 deg apart about the
    field-y axis) — schematic per-kind geometry, not a vendor model. Returns
    the turbine's top (scene z).
    """
    material = _material(f"mat_{asset['id']}", ASSET_COLORS["wind_turbine"])
    tower_height = TURBINE_TOWER_HEIGHT_M * scale
    blade_length = TURBINE_BLADE_LENGTH_M * scale
    hub_z = z + tower_height

    bpy.ops.mesh.primitive_cylinder_add(
        radius=TURBINE_TOWER_RADIUS_M * scale,
        depth=tower_height,
        location=(x, y, z + tower_height / 2),
    )
    tower = bpy.context.active_object
    tower.name = f"asset_{asset['id']}"
    tower.data.materials.append(material)

    bpy.ops.mesh.primitive_uv_sphere_add(
        radius=TURBINE_HUB_RADIUS_M * scale, location=(x, y, hub_z), segments=12
    )
    hub = bpy.context.active_object
    hub.name = f"asset_{asset['id']}_hub"
    hub.data.materials.append(material)

    for blade_index in range(3):
        angle = math.radians(90.0 + 120.0 * blade_index)  # first blade points up
        # Blade centre: half a blade-length from the hub, in the x-z rotor plane.
        cx = x + 0.5 * blade_length * math.cos(angle)
        cz = hub_z + 0.5 * blade_length * math.sin(angle)
        bpy.ops.mesh.primitive_cube_add(size=1.0, location=(cx, y, cz))
        blade = bpy.context.active_object
        blade.name = f"asset_{asset['id']}_blade{blade_index + 1}"
        blade.scale = (
            blade_length,
            TURBINE_BLADE_THICKNESS_M * scale,
            TURBINE_BLADE_CHORD_M * scale,
        )
        blade.rotation_euler = (0.0, -angle, 0.0)  # long axis along the spoke
        blade.data.materials.append(material)
    return hub_z + blade_length


def _add_assets(scene: dict, z0: float, scale: float) -> float:
    """Per-kind asset geometry; returns the highest asset top (scene z)."""
    z_top = 0.0
    for asset in scene["assets"]:
        x, y = asset["x_m"], asset["y_m"]
        z = _asset_scene_z(asset, z0)
        kind = asset["kind"]
        if kind == "wind_turbine":
            z_top = max(z_top, _add_wind_turbine(asset, x, y, z, scale))
            continue
        if kind == "host":
            size = HOST_SIZE_M * scale
            bpy.ops.mesh.primitive_cube_add(size=size, location=(x, y, z + size / 2))
            top = z + size
        elif kind == "vessel":
            length, beam, depth = (d * scale for d in VESSEL_DIMS_M)
            bpy.ops.mesh.primitive_cube_add(size=1.0, location=(x, y, z))
            bpy.context.active_object.scale = (length, beam, depth)
            top = z + depth / 2
        elif kind == "tree":
            radius, height = TREE_RADIUS_M * scale, TREE_HEIGHT_M * scale
            bpy.ops.mesh.primitive_cylinder_add(
                radius=radius, depth=height, location=(x, y, z + height / 2)
            )
            top = z + height
        elif kind == "manifold":
            radius, height = MANIFOLD_RADIUS_M * scale, MANIFOLD_HEIGHT_M * scale
            bpy.ops.mesh.primitive_cone_add(
                radius1=radius, depth=height, location=(x, y, z + height / 2)
            )
            top = z + height
        elif kind == "offshore_substation":
            length, width, height = (d * scale for d in SUBSTATION_DIMS_M)
            bpy.ops.mesh.primitive_cube_add(size=1.0, location=(x, y, z + height / 2))
            bpy.context.active_object.scale = (length, width, height)
            top = z + height
        else:  # well
            radius, height = WELL_RADIUS_M * scale, WELL_HEIGHT_M * scale
            bpy.ops.mesh.primitive_cylinder_add(
                radius=radius, depth=height, location=(x, y, z + height / 2)
            )
            top = z + height
        obj = bpy.context.active_object
        obj.name = f"asset_{asset['id']}"
        color = ASSET_COLORS.get(kind, ASSET_COLORS["well"])
        obj.data.materials.append(_material(f"mat_{asset['id']}", color))
        z_top = max(z_top, top)
    return z_top


def _add_connections(scene: dict, z0: float, scale: float) -> None:
    """Bevelled curve per connection, styled by kind."""
    for conn in scene["connections"]:
        kind = conn["kind"]
        bevel = CONNECTION_BEVEL_M.get(kind, 8.0) * scale
        curve = bpy.data.curves.new(f"conn_{conn['id']}", type="CURVE")
        curve.dimensions = "3D"
        curve.bevel_depth = bevel
        spline = curve.splines.new("POLY")
        path = conn["path_xyz_m"]
        spline.points.add(len(path) - 1)
        for point, (x, y, z) in zip(spline.points, path):
            point.co = (x, y, _scene_z(z, z0) + bevel, 1.0)
        obj = bpy.data.objects.new(f"conn_{conn['id']}", curve)
        bpy.context.collection.objects.link(obj)
        color = CONNECTION_COLORS.get(kind, CONNECTION_COLORS["flowline"])
        obj.data.materials.append(_material(f"mat_conn_{conn['id']}", color))


def _add_camera_flythrough(
    centre: tuple[float, float, float],
    radius: float,
    frame_count: int,
    fps: int,
) -> None:
    cx, cy, cz = centre
    target = bpy.data.objects.new("cam_target", None)
    target.location = (cx, cy, cz)
    bpy.context.collection.objects.link(target)

    camera_data = bpy.data.cameras.new("camera")
    # Field spans are km-scale; Blender's default clip_end (100 m) would cull
    # the entire scene and render only the world background (#1518 lesson).
    camera_data.clip_start = max(0.1, 0.001 * radius)
    camera_data.clip_end = 20.0 * radius
    camera = bpy.data.objects.new("camera", camera_data)
    bpy.context.collection.objects.link(camera)
    bpy.context.scene.camera = camera
    track = camera.constraints.new(type="TRACK_TO")
    track.target = target

    scene = bpy.context.scene
    scene.frame_start, scene.frame_end = 1, frame_count
    scene.render.fps = fps
    for frame in range(1, frame_count + 1):
        t = (frame - 1) / max(1, frame_count - 1)
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


def _configure_render(out_path: str, res_scale: int) -> None:
    scene = bpy.context.scene
    # Filmic/AgX tone-mapping desaturates the flat per-kind colors badly;
    # Standard keeps the schematic palette readable.
    try:
        scene.view_settings.view_transform = "Standard"
    except TypeError:
        pass
    render = scene.render
    render.resolution_x, render.resolution_y = 1280, 720
    render.resolution_percentage = res_scale
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
    surface = scene_data["surface"]
    subsea = bool(scene_data.get("is_subsea"))
    z0 = min(min(row) for row in surface["z_m"])
    cx, cy = _add_surface(surface, subsea)

    span_x = surface["x_m"][-1] - surface["x_m"][0]
    span_y = surface["y_m"][-1] - surface["y_m"][0]
    scale = _size_scale(max(span_x, span_y))

    z_terrain_top = (max(max(row) for row in surface["z_m"]) - z0) * Z_EXAGGERATION
    z_top = max(z_terrain_top, _add_assets(scene_data, z0, scale))
    _add_connections(scene_data, z0, scale)
    if subsea:
        z_top = max(z_top, _add_water_plane(surface, z0))

    centre = (cx, cy, 0.5 * z_top)
    _add_camera_flythrough(
        centre,
        radius=0.85 * math.hypot(span_x, span_y),
        frame_count=args.frames,
        fps=args.fps,
    )

    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    _configure_render(args.out, args.res_scale)
    bpy.ops.render.render(animation=True)
    print(f"fly-through rendered to {args.out}")


if __name__ == "__main__":
    main()
