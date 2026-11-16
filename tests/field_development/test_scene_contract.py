# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Consumer-facing tests for the version-2 layout scene contract."""

from copy import deepcopy
import json

import pytest

from digitalmodel.field_development.scene_contract import validate_scene
from digitalmodel.field_development.layout_model import build_layout_model
from digitalmodel.field_development.visualization import (
    export_layout_scene_json,
    layout_scene_dict,
)


@pytest.fixture
def scene():
    return {
        "schema": "digitalmodel.field_layout_scene/2",
        "field_name": "Synthetic example",
        "is_subsea": True,
        "surface": {
            "x_m": [0.0, 10.0],
            "y_m": [0.0, 20.0],
            "z_m": [[-5.0, -5.0], [-5.0, -5.0]],
            "source": "synthetic",
        },
        "assets": [
            {
                "id": key,
                "name": key,
                "kind": kind,
                "subtype": "",
                "x_m": x,
                "y_m": 0.0,
                "z_m": -5.0,
                "on_surface": True,
            }
            for key, kind, x in (("A", "well", 0.0), ("B", "host", 10.0))
        ],
        "connections": [
            {
                "id": "C",
                "kind": "flowline",
                "from": "A",
                "to": "B",
                "waypoints_plan_m": [],
                "route_length_m": 10.0,
                "path_xyz_m": [[0.0, 0.0, -5.0], [10.0, 0.0, -5.0]],
            }
        ],
    }


def test_connected_scene_receipt_and_input_preservation(scene):
    scene["extension"] = {"unverified": ["unknown"]}
    before = deepcopy(scene)
    assert validate_scene(scene) == {
        "schema": scene["schema"],
        "asset_count": 2,
        "connection_count": 1,
    }
    assert scene == before


def replace(scene, path, value):
    parent = scene
    for key in path[:-1]:
        parent = parent[key]
    parent[path[-1]] = value


@pytest.mark.parametrize(
    "path,value",
    [
        (("schema",), "digitalmodel.field_layout_scene/1"),
        (("schema",), None),
        (("field_name",), 42),
        (("is_subsea",), 1),
        (("surface",), []),
        (("surface", "source"), None),
        (("assets",), {}),
        (("connections",), "invalid"),
        (("assets", 0), []),
        (("connections", 0), None),
        (("assets", 0, "name"), False),
        (("assets", 0, "subtype"), []),
        (("assets", 0, "on_surface"), "true"),
    ],
)
def test_rejects_invalid_record_structure(scene, path, value):
    replace(scene, path, value)
    with pytest.raises(ValueError):
        validate_scene(scene)


@pytest.mark.parametrize("value", [None, [], "invalid", 0, True])
def test_rejects_non_mapping_scene(value):
    with pytest.raises(ValueError, match="scene"):
        validate_scene(value)


def test_scene_without_connections(scene):
    scene["connections"] = []
    assert validate_scene(scene)["connection_count"] == 0


@pytest.mark.parametrize("group", ["assets", "connections"])
@pytest.mark.parametrize("value", ["", "  ", None, [], 1])
def test_rejects_empty_or_non_string_ids(scene, group, value):
    scene[group][0]["id"] = value
    with pytest.raises(ValueError, match=group):
        validate_scene(scene)


@pytest.mark.parametrize("group", ["assets", "connections"])
def test_rejects_duplicate_ids_without_echoing_contents(scene, group):
    scene[group][0]["id"] = "sensitive-input-marker"
    scene[group].append(deepcopy(scene[group][0]))
    with pytest.raises(ValueError, match=group) as error:
        validate_scene(scene)
    assert "sensitive-input-marker" not in str(error.value)


@pytest.mark.parametrize("group", ["assets", "connections"])
@pytest.mark.parametrize("value", ["unsupported", [], None])
def test_rejects_unsupported_kinds(scene, group, value):
    scene[group][0]["kind"] = value
    with pytest.raises(ValueError, match="kind"):
        validate_scene(scene)


@pytest.mark.parametrize("endpoint", ["from", "to"])
@pytest.mark.parametrize("value", ["missing", "", None, []])
def test_rejects_unresolved_endpoints(scene, endpoint, value):
    scene["connections"][0][endpoint] = value
    with pytest.raises(ValueError, match=endpoint):
        validate_scene(scene)


@pytest.mark.parametrize(
    "path",
    [
        ("assets", 0, "x_m"),
        ("assets", 0, "y_m"),
        ("assets", 0, "z_m"),
        ("surface", "x_m", 0),
        ("surface", "y_m", 0),
        ("surface", "z_m", 0, 0),
        ("connections", 0, "path_xyz_m", 0, 2),
        ("connections", 0, "route_length_m"),
        ("connections", 0, "waypoints_plan_m", 0, 0),
    ],
)
@pytest.mark.parametrize(
    "value",
    [
        float("nan"),
        float("inf"),
        float("-inf"),
        True,
        "1,000",
        None,
        pytest.param(10**400, id="oversized-integer"),
    ],
)
def test_rejects_nonfinite_or_non_numeric_values(scene, path, value):
    scene["connections"][0]["waypoints_plan_m"] = [[5.0, 0.0]]
    replace(scene, path, value)
    with pytest.raises(ValueError):
        validate_scene(scene)


@pytest.mark.parametrize(
    "path,value",
    [
        (("surface", "x_m"), []),
        (("surface", "x_m"), [0.0, 0.0]),
        (("surface", "y_m"), [20.0, 0.0]),
        (("surface", "y_m"), "invalid"),
        (("surface", "z_m"), [[-5.0, -5.0]]),
        (("surface", "z_m"), [[-5.0], [-5.0, -5.0]]),
        (("surface", "z_m"), [0.0, 1.0]),
        (("connections", 0, "path_xyz_m"), []),
        (("connections", 0, "path_xyz_m"), [[0.0, 0.0, -5.0]]),
        (("connections", 0, "path_xyz_m"), [[0.0, 0.0], [10.0, 0.0]]),
        (("connections", 0, "path_xyz_m"), {"unordered": [0.0, 0.0, 0.0]}),
        (("connections", 0, "waypoints_plan_m"), [[5.0]]),
        (("connections", 0, "waypoints_plan_m"), [[5.0, 0.0, 0.0]]),
        (("connections", 0, "route_length_m"), -1.0),
    ],
)
def test_rejects_malformed_geometry(scene, path, value):
    replace(scene, path, value)
    with pytest.raises(ValueError):
        validate_scene(scene)


def test_zero_length_is_structural_only(scene):
    scene["connections"][0]["route_length_m"] = 0.0
    assert validate_scene(scene)["connection_count"] == 1


REQUIRED_PATHS = (
    [
        (key,)
        for key in (
            "schema",
            "field_name",
            "is_subsea",
            "surface",
            "assets",
            "connections",
        )
    ]
    + [("surface", key) for key in ("x_m", "y_m", "z_m", "source")]
    + [
        ("assets", 0, key)
        for key in ("id", "name", "kind", "subtype", "x_m", "y_m", "z_m", "on_surface")
    ]
    + [
        ("connections", 0, key)
        for key in (
            "id",
            "kind",
            "from",
            "to",
            "waypoints_plan_m",
            "route_length_m",
            "path_xyz_m",
        )
    ]
)


@pytest.mark.parametrize("path", REQUIRED_PATHS)
def test_missing_exporter_field_is_rejected_without_mutation(scene, path):
    parent = scene
    for key in path[:-1]:
        parent = parent[key]
    del parent[path[-1]]
    before = deepcopy(scene)
    with pytest.raises(ValueError):
        validate_scene(scene)
    assert scene == before


def test_existing_exporter_round_trip(tmp_path):
    model = build_layout_model(
        {
            "field": {"name": "Synthetic example"},
            "surface": {
                "kind": "synthetic",
                "x_min_m": 0,
                "x_max_m": 10,
                "y_min_m": 0,
                "y_max_m": 20,
                "nx": 3,
                "ny": 3,
                "base_elevation_m": -5,
                "amplitude_m": 0,
                "x_wavelength_m": 10,
                "y_wavelength_m": 20,
            },
            "assets": [
                {"id": "A", "kind": "well", "x_m": 0, "y_m": 0},
                {"id": "B", "kind": "host", "x_m": 10, "y_m": 0},
            ],
            "connections": [
                {
                    "id": "C",
                    "kind": "flowline",
                    "from": "A",
                    "to": "B",
                    "waypoints_plan_m": [[5, 0]],
                }
            ],
        }
    )
    dest = tmp_path / "scene.json"
    export_layout_scene_json(model, dest)
    exported = json.loads(dest.read_text(encoding="utf-8"))
    assert exported == layout_scene_dict(model)
    assert validate_scene(exported) == {
        "schema": exported["schema"],
        "asset_count": len(model.assets),
        "connection_count": len(model.connections),
    }
