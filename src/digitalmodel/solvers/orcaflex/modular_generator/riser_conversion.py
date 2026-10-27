# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Lossless bridge between single-line generic input and the riser schema.

Scalar LineType and vessel fields move to typed fields. Compound connection
and section tables remain in generic fallback as layout templates: their
unmapped columns, row widths and native key spelling are retained. Rendering
replaces mapped cells from the typed riser, without adding typed defaults to
the original native mapping. No solver interface is used.
"""

from __future__ import annotations

from copy import deepcopy
from typing import Any

from .schema.generic import GenericModel, TYPED_FIELD_MAP
from .schema.riser import Riser

# Native property names from the existing riser and generic schema/builders.
_LINE_TYPE_FIELDS = {
    "outer_diameter": "OD",
    "inner_diameter": "ID",
    "mass_per_length": "MassPerUnitLength",
    "bending_stiffness": "EI",
    "axial_stiffness": "EA",
    "torsional_stiffness": "GJ",
    "poisson_ratio": "PoissonRatio",
    "drag_coefficient": "Cd",
    "added_mass_coefficient": "Ca",
    "axial_drag_coefficient": "CdAxial",
    "contact_diameter": "OuterContactDiameter",
    "allowable_tension": "AllowableTension",
    "stress_od": "StressOD",
    "stress_id": "StressID",
}
_VESSEL_FIELDS = {
    "type_name": "VesselType",
    "position": "InitialPosition",
    "length": "Length",
    "primary_motion": "PrimaryMotion",
    "superimposed_motion": "SuperimposedMotion",
    "included_in_statics": "IncludedInStatics",
}
_SECTION_FIELDS = {
    "line_type": "LineType",
    "length": "Length",
    "segment_length": "TargetSegmentLength",
}
_END_FIELDS = {
    "azimuth": "ConnectionAzimuth",
    "declination": "ConnectionDeclination",
    "gamma": "ConnectionGamma",
}
_POSITION_COLUMNS = ("ConnectionX", "ConnectionY", "ConnectionZ")


def _table(properties: dict, required: set[str]) -> tuple[str, list[str], list]:
    """Find one unambiguous compound table, accepting reordered columns."""
    matches = []
    for key, rows in properties.items():
        if not isinstance(key, str):
            continue
        columns = [column.strip() for column in key.split(",")]
        if required.issubset(columns):
            matches.append((key, columns, rows))
    if len(matches) != 1:
        raise ValueError(f"Expected one table containing {sorted(required)}")
    key, columns, rows = matches[0]
    if len(set(columns)) != len(columns) or not isinstance(rows, list) or not rows:
        raise ValueError(f"Invalid table: {key}")
    for row in rows:
        if not isinstance(row, list) or len(row) > len(columns):
            raise ValueError(f"Invalid row in table: {key}")
        if not required.issubset(columns[:len(row)]):
            raise ValueError(f"Missing required cell in table: {key}")
    return key, columns, rows


def _extract_scalars(obj: dict, fields: dict[str, str]) -> dict[str, Any]:
    """Move effective scalar properties, respecting generic typed precedence."""
    properties = obj.setdefault("properties", {})
    effective = dict(properties)
    for field, native in TYPED_FIELD_MAP.items():
        if field in obj:
            effective[native] = obj[field]
    extracted = {"name": obj["name"]}
    for field, native in fields.items():
        if native not in effective:
            continue
        extracted[field] = effective[native]
        properties.pop(native, None)
        for generic_field, generic_native in TYPED_FIELD_MAP.items():
            if generic_native == native:
                obj.pop(generic_field, None)
    return extracted


def extract_riser(generic: dict) -> Riser:
    """Extract the supported single-line subset, modifying only a deep copy.

    The caller owns the copied fallback dictionary. Required dimensions must
    be scalar values supported by RiserLineType; no engineering values are
    inferred. Multiple lines or vessels are deliberately rejected.
    """
    lines = generic.get("lines") or []
    vessels = generic.get("vessels") or []
    if len(lines) != 1 or len(vessels) != 1:
        raise ValueError("Riser conversion requires one line and one vessel")
    line = lines[0]
    props = line.get("properties") or {}
    _, section_columns, section_rows = _table(props, set(_SECTION_FIELDS.values()))
    sections = [
        {field: row[section_columns.index(native)]
         for field, native in _SECTION_FIELDS.items()}
        for row in section_rows
    ]
    _, columns, rows = _table(props, {"Connection", *_POSITION_COLUMNS})
    if len(rows) != 2:
        raise ValueError("Riser conversion requires exactly two end connections")
    vessel_name = vessels[0]["name"]
    buoy_names = {obj["name"] for obj in generic.get("buoys_6d", []) or []}
    buoy_names.update(obj["name"] for obj in generic.get("buoys_3d", []) or [])
    ends = []
    for row in rows:
        cells = dict(zip(columns, row))
        name = cells["Connection"]
        end = {"position": [cells[column] for column in _POSITION_COLUMNS]}
        if name == vessel_name:
            end.update(type="vessel", name=name)
        elif name in buoy_names:
            end.update(type="buoy", name=name)
        elif name in {"Anchored", "Fixed", "Free"}:
            end["type"] = "anchor" if name == "Anchored" else name.lower()
        else:
            raise ValueError("Unsupported riser end connection")
        end.update({field: cells[native] for field, native in _END_FIELDS.items()
                    if native in cells})
        ends.append(end)
    referenced_types = {section["line_type"] for section in sections}
    source_types = generic.get("line_types", []) or []
    if len({obj["name"] for obj in source_types}) != len(source_types):
        raise ValueError("Duplicate generic line type names")
    line_types = [_extract_scalars(obj, _LINE_TYPE_FIELDS)
                  for obj in source_types if obj["name"] in referenced_types]
    vessel = _extract_scalars(vessels[0], _VESSEL_FIELDS)
    return Riser.model_validate({
        "vessel": vessel,
        "line_types": line_types,
        "lines": [{"name": line["name"], "sections": sections,
                   "end_a": ends[0], "end_b": ends[1]}],
    })


def _restore_scalars(obj: dict, typed: Any, fields: dict[str, str]) -> None:
    """Overlay authored typed fields only; schema defaults add no native keys."""
    properties = obj.setdefault("properties", {})
    obj["name"] = typed.name
    for field, native in fields.items():
        if field in typed.model_fields_set:
            properties[native] = getattr(typed, field)


def render_riser(riser: Riser, fallback: GenericModel) -> GenericModel:
    """Rebuild one generic rendering view with authoritative typed values.

    Native tables supply layout and unmapped values only. Section counts and
    line-type identities must remain compatible with the retained templates.
    The supplied validated objects are not mutated.
    """
    generic = deepcopy(fallback.model_dump(exclude_unset=True))
    lines = generic.get("lines") or []
    vessels = generic.get("vessels") or []
    if len(lines) != 1 or len(vessels) != 1 or len(riser.lines) != 1:
        raise ValueError("Riser fallback requires one line and one vessel")
    if riser.links or riser.clump_types:
        raise ValueError("Typed links and clumps are not supported by riser fallback")
    type_templates = {obj["name"]: obj for obj in generic.get("line_types", [])}
    if len(type_templates) != len(generic.get("line_types", [])) or not {
        obj.name for obj in riser.line_types
    }.issubset(type_templates):
        raise ValueError("Riser line types must match the generic fallback")
    for line_type in riser.line_types:
        _restore_scalars(type_templates[line_type.name], line_type, _LINE_TYPE_FIELDS)
    _restore_scalars(vessels[0], riser.vessel, _VESSEL_FIELDS)
    line = lines[0]
    typed_line = riser.lines[0]
    line["name"] = typed_line.name
    props = line["properties"]
    _, columns, rows = _table(props, set(_SECTION_FIELDS.values()))
    if len(rows) != len(typed_line.sections):
        raise ValueError("Riser section count must match the generic fallback")
    for row, section in zip(rows, typed_line.sections, strict=True):
        for field, native in _SECTION_FIELDS.items():
            row[columns.index(native)] = getattr(section, field)
    _, columns, rows = _table(props, {"Connection", *_POSITION_COLUMNS})
    if len(rows) != 2:
        raise ValueError("Riser fallback requires exactly two end connections")
    for row, end in zip(rows, (typed_line.end_a, typed_line.end_b), strict=True):
        name = {"anchor": "Anchored", "fixed": "Fixed", "free": "Free"}.get(
            end.type.value, end.name
        )
        row[columns.index("Connection")] = name
        for column, value in zip(_POSITION_COLUMNS, end.position, strict=True):
            row[columns.index(column)] = value
        for field, native in _END_FIELDS.items():
            if native in columns[:len(row)]:
                row[columns.index(native)] = getattr(end, field)
    return GenericModel.model_validate(generic)
