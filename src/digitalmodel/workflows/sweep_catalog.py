# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Validate planning-only sweep descriptors without expanding or executing cases."""

from collections.abc import Mapping

VARIANT_SOURCES = frozenset({"factorial", "range", "csv", "yaml_matrix"})
UNRESOLVED_FIELDS = frozenset({"vessel_id", "base_input", "grid", "batch_size"})
FAMILY_FIELDS = UNRESOLVED_FIELDS | {
    "id",
    "status",
    "workflow",
    "axes",
    "variants",
    "outputs",
    "consumer",
    "prerequisites",
    "unresolved",
}


def validate_catalog(catalog: Mapping) -> dict:
    """Return pending IDs and prerequisite counts, or raise ValueError.

    Validation establishes descriptor structure only. Even a fully populated
    descriptor remains pending; paths, grid semantics, units and execution
    eligibility require separate review. The input is never modified.
    """
    _fields(catalog, {"schema_version", "families"}, "catalog")
    if type(catalog["schema_version"]) is not int or catalog["schema_version"] != 1:
        raise ValueError("schema_version must be integer 1")
    pending_ids: list[str] = []
    counts: dict[str, int] = {}
    for family in _items(catalog["families"], "families"):
        _validate_family(family)
        family_id = family["id"]
        if family_id in counts:
            raise ValueError(f"duplicate family id: {family_id}")
        pending_ids.append(family_id)
        counts[family_id] = len(family["prerequisites"])
    return {"pending_family_ids": pending_ids, "prerequisite_counts": counts}


def _validate_family(family: Mapping) -> None:
    _fields(family, FAMILY_FIELDS, "family")
    for field in ("id", "workflow", "consumer"):
        _text(family[field], field)
    if family["status"] != "pending":
        raise ValueError("family status must remain pending")
    unresolved = family["unresolved"]
    expected = {field for field in UNRESOLVED_FIELDS if family[field] is None}
    _fields(unresolved, expected, "unresolved")
    for field in expected:
        _text(unresolved[field], f"unresolved.{field}")
    for field in ("vessel_id", "base_input"):
        if family[field] is not None:
            _text(family[field], field)
    grid = family["grid"]
    if grid is not None and (not isinstance(grid, Mapping) or not grid):
        raise ValueError("grid must be a nonempty mapping or unresolved null")
    batch_size = family["batch_size"]
    if batch_size is not None:
        _positive_integer(batch_size, "batch_size")
    _fields(family["variants"], {"source"}, "variants")
    source = family["variants"]["source"]
    if not isinstance(source, str) or source not in VARIANT_SOURCES:
        raise ValueError("variants.source is unsupported")
    _validate_axes(family["axes"])
    for output in _items(family["outputs"], "outputs"):
        _text(output, "output")
    _validate_prerequisites(family["prerequisites"])


def _validate_axes(axes: list) -> None:
    names: set[str] = set()
    for axis in _items(axes, "axes"):
        _fields(axis, {"name", "unit"}, "axis")
        _text(axis["name"], "axis.name")
        _text(axis["unit"], "axis.unit")
        if axis["name"] in names:
            raise ValueError("duplicate axis name")
        names.add(axis["name"])


def _validate_prerequisites(prerequisites: list) -> None:
    issues: set[int] = set()
    for prerequisite in _items(prerequisites, "prerequisites"):
        _fields(prerequisite, {"issue", "reason"}, "prerequisite")
        _positive_integer(prerequisite["issue"], "prerequisite.issue")
        _text(prerequisite["reason"], "prerequisite.reason")
        if prerequisite["issue"] in issues:
            raise ValueError("duplicate prerequisite issue")
        issues.add(prerequisite["issue"])


def _fields(value: Mapping, expected: set | frozenset, label: str) -> None:
    if not isinstance(value, Mapping) or set(value) != expected:
        raise ValueError(f"{label} must contain exactly the documented fields")


def _items(value: list, label: str) -> list:
    if not isinstance(value, list) or not value:
        raise ValueError(f"{label} must be a nonempty list")
    return value


def _text(value: str, label: str) -> None:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{label} must be nonempty text")


def _positive_integer(value: int, label: str) -> None:
    if type(value) is not int or value <= 0:
        raise ValueError(f"{label} must be a positive integer, excluding booleans")
