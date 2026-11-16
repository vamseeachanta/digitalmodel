# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Read-only structural validation for version-2 layout scene mappings."""

from collections.abc import Mapping
from math import isfinite
from typing import Any

from .layout_model import ASSET_KINDS, CONNECTION_KINDS

_SCHEMA = "digitalmodel.field_layout_scene/2"


def _mapping(value: Any, field: str) -> Mapping:
    if not isinstance(value, Mapping):
        raise ValueError(f"{field}: expected an object")
    return value


def _array(value: Any, field: str) -> list:
    if not isinstance(value, list):
        raise ValueError(f"{field}: expected an array")
    return value


def _string(value: Any, field: str) -> str:
    if not isinstance(value, str):
        raise ValueError(f"{field}: expected a string")
    return value


def _boolean(value: Any, field: str) -> None:
    if type(value) is not bool:
        raise ValueError(f"{field}: expected a boolean")


def _identifier(value: Any, field: str) -> str:
    identifier = _string(value, field)
    if not identifier.strip():
        raise ValueError(f"{field}: expected a nonempty identifier")
    return identifier


def _number(value: Any, field: str) -> None:
    if type(value) not in (int, float):
        raise ValueError(f"{field}: expected a finite number")
    try:
        finite = isfinite(value)
    except OverflowError:
        raise ValueError(f"{field}: number exceeds supported range") from None
    if not finite:
        raise ValueError(f"{field}: expected a finite number")


def _vector(value: Any, field: str, width: int) -> list:
    values = _array(value, field)
    if len(values) != width:
        raise ValueError(f"{field}: incorrect array width")
    for i, number in enumerate(values):
        _number(number, f"{field}[{i}]")
    return values


def _points(value: Any, field: str, width: int, minimum: int) -> None:
    points = _array(value, field)
    if len(points) < minimum:
        raise ValueError(f"{field}: too few points")
    for i, point in enumerate(points):
        _vector(point, f"{field}[{i}]", width)


def _surface(value: Any) -> None:
    field = "scene.surface"
    surface = _mapping(value, field)
    _string(surface.get("source"), f"{field}.source")
    for key in ("x_m", "y_m"):
        axis = _array(surface.get(key), f"{field}.{key}")
        _vector(axis, f"{field}.{key}", len(axis))
        if not axis or any(a >= b for a, b in zip(axis, axis[1:])):
            raise ValueError(f"{field}.{key}: expected a nonempty increasing axis")
    rows = _array(surface.get("z_m"), f"{field}.z_m")
    if len(rows) != len(surface["y_m"]):
        raise ValueError(f"{field}.z_m: incorrect row count")
    for i, row in enumerate(rows):
        _vector(row, f"{field}.z_m[{i}]", len(surface["x_m"]))


def _records(value: Any, field: str, kinds: frozenset[str]) -> tuple[list, set[str]]:
    records = _array(value, field)
    identifiers: set[str] = set()
    for i, record in enumerate(records):
        path = f"{field}[{i}]"
        record = _mapping(record, path)
        identifier = _identifier(record.get("id"), f"{path}.id")
        if identifier in identifiers:
            raise ValueError(f"{path}.id: duplicate identifier")
        identifiers.add(identifier)
        if _string(record.get("kind"), f"{path}.kind") not in kinds:
            raise ValueError(f"{path}.kind: unsupported kind")
    return records, identifiers


def validate_scene(scene: Any) -> dict[str, Any]:
    """Return structural counts without changing the supplied scene."""
    scene = _mapping(scene, "scene")
    if scene.get("schema") != _SCHEMA:
        raise ValueError("scene.schema: unsupported version")
    _string(scene.get("field_name"), "scene.field_name")
    _boolean(scene.get("is_subsea"), "scene.is_subsea")
    _surface(scene.get("surface"))
    assets, asset_ids = _records(scene.get("assets"), "scene.assets", ASSET_KINDS)
    connections, _ = _records(
        scene.get("connections"), "scene.connections", CONNECTION_KINDS
    )
    for i, asset in enumerate(assets):
        field = f"scene.assets[{i}]"
        asset = _mapping(asset, field)
        for key in ("name", "subtype"):
            _string(asset.get(key), f"{field}.{key}")
        _boolean(asset.get("on_surface"), f"{field}.on_surface")
        for key in ("x_m", "y_m", "z_m"):
            _number(asset.get(key), f"{field}.{key}")
    for i, connection in enumerate(connections):
        field = f"scene.connections[{i}]"
        for key in ("from", "to"):
            endpoint = _identifier(connection.get(key), f"{field}.{key}")
            if endpoint not in asset_ids:
                raise ValueError(f"{field}.{key}: unresolved asset identifier")
        _points(connection.get("path_xyz_m"), f"{field}.path_xyz_m", 3, 2)
        _points(connection.get("waypoints_plan_m"), f"{field}.waypoints_plan_m", 2, 0)
        length = connection.get("route_length_m")
        _number(length, f"{field}.route_length_m")
        if length < 0:
            raise ValueError(f"{field}.route_length_m: expected a nonnegative length")
    return {
        "schema": _SCHEMA,
        "asset_count": len(assets),
        "connection_count": len(connections),
    }
