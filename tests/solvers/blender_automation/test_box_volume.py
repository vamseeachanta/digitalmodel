# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Native export volume checks; missing Blender means NOT_EVALUATED."""

import json

import pytest

from digitalmodel.solvers.blender_automation import BlenderWrapper

from .conftest import requires_blender


@requires_blender
@pytest.mark.parametrize(("side_m", "expected_m3"), [(1.0, 1.0), (2.0, 8.0)])
def test_exported_cube_volume(tmp_path, side_m, expected_m3):
    """Hand geometry: V=s*s*s, so 1*1*1=1 and 2*2*2=8 cubic metres.

    Synthetic metre coordinates are used with no scene scaling. Blender API
    reference, mesh.primitive_cube_add(size), accessed 2026-10-21:
    https://docs.blender.org/api/current/bpy.ops.mesh.html
    Volume is measured after export and reimport, never from the input mesh.
    These integer-coordinate cubes require no engineering acceptance limit.
    """
    wrapper = BlenderWrapper()
    scene, exported, result = (
        tmp_path / "box.blend",
        tmp_path / "box.ply",
        tmp_path / "volume.json",
    )
    create = wrapper.run_script(
        "import bpy\n"
        "bpy.ops.wm.read_factory_settings(use_empty=True)\n"
        f"bpy.ops.mesh.primitive_cube_add(size={side_m!r})\n"
        f"bpy.ops.wm.save_as_mainfile(filepath={str(scene)!r})\n"
    )
    assert create["success"], create["stderr"]
    export = wrapper.export_file(scene, exported, "ply")
    assert export["success"], export["stderr"]
    measure = wrapper.run_script(
        "import bpy, bmesh, json\n"
        "bpy.ops.wm.read_factory_settings(use_empty=True)\n"
        f"bpy.ops.wm.ply_import(filepath={str(exported)!r})\n"
        "mesh = bmesh.new()\n"
        "mesh.from_mesh(bpy.context.object.data)\n"
        f"with open({str(result)!r}, 'w') as stream:\n"
        "    json.dump({'volume_m3': mesh.calc_volume(signed=True)}, stream)\n"
        "mesh.free()\n"
    )
    assert measure["success"], measure["stderr"]
    assert json.loads(result.read_text())["volume_m3"] == expected_m3
