# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline point-script contracts using hand-constructed synthetic data.

Fixture source: independent fractional coordinates, written below as decimal
inputs and fraction-based expectations. Cartesian triples use arbitrary common
length units; geographic aliases use degrees horizontally and the supplied
height unit vertically. The contract copies values into scene coordinates
without a CRS or unit conversion. Scale controls sphere radius only.

Input sequence order is retained, including repeated points. Generation and
inspection never execute the script or import the Blender interface.
"""
from __future__ import annotations

import ast
import os
from copy import deepcopy
from pathlib import Path

import pytest

from digitalmodel.gis.integrations.blender_script_generator import BlenderScriptGenerator


@pytest.fixture(params=("cartesian", "geographic"))
def synthetic_points(request):
    """Return decimal inputs and independently hand-calculated expectations.

    Cartesian source triples: (49/4, -15/2, 25/8), (-19/4, 37/4, -5/2),
    and (5/8, -5/4, 0). Geographic source triples: (-329/4, 107/4, -21/2),
    (91/8, -141/4, 57/4), and (-59/2, 9/8, 0). All are synthetic choices;
    none describes a property. Fractions are exactly representable in binary.
    """
    identifiers = ("feature-z", "feature-a 'quoted'\n\\axis", "feature-μ")
    if request.param == "cartesian":
        points = [
            dict(name=identifiers[0], x=12.25, y=-7.5, z=3.125),
            dict(name=identifiers[1], x=-4.75, y=9.25, z=-2.5),
            dict(name=identifiers[2], x=0.625, y=-1.25, z=0.0),
        ]
        triples = ((49 / 4, -15 / 2, 25 / 8), (-19 / 4, 37 / 4, -5 / 2),
                   (5 / 8, -5 / 4, 0))
    else:
        points = [
            dict(name=identifiers[0], longitude=-82.25, latitude=26.75, elevation=-10.5),
            dict(name=identifiers[1], longitude=11.375, latitude=-35.25, elevation=14.25),
            dict(name=identifiers[2], longitude=-29.5, latitude=1.125, elevation=0.0),
        ]
        triples = ((-329 / 4, 107 / 4, -21 / 2), (91 / 8, -141 / 4, 57 / 4),
                   (-59 / 2, 9 / 8, 0))
    expected = [
        dict(name=name, x=x, y=y, z=z)
        for name, (x, y, z) in zip(identifiers, triples)
    ]
    return points, expected


def _literal_assignment(tree, name):
    """Read one top-level literal assignment without executing generated code."""
    value, = (
        node.value
        for node in tree.body
        if isinstance(node, ast.Assign)
        and any(isinstance(target, ast.Name) and target.id == name
                for target in node.targets)
    )
    return ast.literal_eval(value)


def _assert_expression(node, expected):
    """Compare syntax structure independently of formatting or quote style."""
    assert ast.dump(node) == ast.dump(ast.parse(expected, mode="eval").body)


def test_blender_point_script_preserves_synthetic_coordinates(synthetic_points):
    """Preserve every identifier and coordinate with no scaling or conversion."""
    points, expected = synthetic_points
    # Synthetic radius source: three eighths of a scene length unit.
    script = BlenderScriptGenerator.generate_point_cloud_script(points, scale=0.375)
    tree = ast.parse(script)
    assert _literal_assignment(tree, "points") == expected
    assert _literal_assignment(tree, "scale") == 3 / 8

    sphere_call, = (
        node for node in ast.walk(tree)
        if isinstance(node, ast.Call)
        and ast.unparse(node.func) == "bpy.ops.mesh.primitive_ico_sphere_add"
    )
    keywords = {keyword.arg: keyword.value for keyword in sphere_call.keywords}
    _assert_expression(keywords["location"], '(pt["x"], pt["y"], pt["z"])')
    _assert_expression(keywords["radius"], "scale")
    name_assignment, = (
        node for node in ast.walk(tree)
        if isinstance(node, ast.Assign)
        and any(ast.unparse(target) == "obj.name" for target in node.targets)
    )
    _assert_expression(name_assignment.value, 'pt["name"]')


def test_blender_point_script_is_valid_python_without_execution(synthetic_points):
    """Parse the entire script, including escaped identifiers, without bpy."""
    points, _ = synthetic_points
    script = BlenderScriptGenerator.generate_point_cloud_script(points)
    assert isinstance(ast.parse(script), ast.Module)


def test_blender_point_script_retains_input_order(synthetic_points):
    """Sequence order, repeated entries and the caller's data remain intact."""
    points, expected = synthetic_points
    # Reverse the deliberately nonalphabetic fixture and repeat its last entry.
    ordered = list(reversed(points)) + points[:1]
    snapshot = deepcopy(ordered)
    expected_order = list(reversed(expected)) + expected[:1]

    first = BlenderScriptGenerator.generate_point_cloud_script(ordered)
    second = BlenderScriptGenerator.generate_point_cloud_script(ordered)
    assert first == second
    assert ordered == snapshot
    assert _literal_assignment(ast.parse(first), "points") == expected_order


@pytest.mark.parametrize(
    "line_separator", (None, "\n", "\r\n"), ids=("native", "lf", "crlf"),
)
def test_blender_point_script_writes_only_explicit_destination(
    synthetic_points, tmp_path, monkeypatch, line_separator,
):
    """Write exact UTF-8 text only beneath the explicitly supplied destination."""
    points, expected = synthetic_points
    working_directory = tmp_path / "working"
    working_directory.mkdir()
    sentinel = working_directory / "keep.txt"
    sentinel.write_text("unchanged\n", encoding="utf-8", newline="\n")
    monkeypatch.chdir(working_directory)
    script = BlenderScriptGenerator.generate_point_cloud_script(points)
    destination = tmp_path / "selected" / "nested" / "points.py"

    native_write_text = Path.write_text

    def write_with_platform_newlines(path, text, *, encoding, newline=None):
        if newline is None:
            text = text.replace("\n", line_separator)
        return native_write_text(path, text, encoding=encoding, newline="\n")

    # Retain native writing and exercise both conventions through real files.
    with monkeypatch.context() as platform:
        if line_separator is not None:
            platform.setattr(os, "linesep", line_separator)
            platform.setattr(Path, "write_text", write_with_platform_newlines)
        result = BlenderScriptGenerator.save_script(script, destination)
        expected_bytes = script.replace("\n", os.linesep).encode("utf-8")

    assert result == destination
    assert destination.read_bytes() == expected_bytes
    assert _literal_assignment(ast.parse(destination.read_text(encoding="utf-8")),
                               "points") == expected
    assert sentinel.read_text(encoding="utf-8") == "unchanged\n"
    assert set(tmp_path.rglob("*")) == {
        working_directory, sentinel, destination.parent.parent,
        destination.parent, destination,
    }
