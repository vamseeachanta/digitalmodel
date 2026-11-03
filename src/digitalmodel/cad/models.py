# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Immutable, dependency-free geometry exchanged by the CAD readers."""

from dataclasses import dataclass
from math import isfinite
from typing import Literal, cast

LengthUnit = Literal["m", "mm", "cm", "in", "ft"]
Point3D = tuple[float, float, float]
EntityKind = Literal["point", "line", "polyline", "triangle", "polygon"]
CadFormat = Literal["dxf", "stl", "obj", "dwg"]
UnitBasis = Literal["file", "caller", "file_and_caller"]


class CadReadError(ValueError):
    """Input geometry cannot be read without losing required information."""


class UnitsError(CadReadError):
    """Length units are missing, unsupported or in conflict."""


@dataclass(frozen=True)
class UnitDecision:
    """Unit basis and exact scale applied to the source coordinates."""

    source_units: LengthUnit
    metres_per_unit: float
    basis: UnitBasis


@dataclass(frozen=True)
class GeometryEntity:
    """An indexed primitive; a closed polyline repeats its first index."""

    kind: EntityKind
    indices: tuple[int, ...]
    layer: str | None = None
    source_id: str | None = None


@dataclass(frozen=True)
class CadGeometry:
    """World-coordinate geometry in metres, without welding or mesh repair.

    Entity indices address points. Winding and source vertex order are retained;
    disconnected and coincident points are permitted. This record certifies
    neither watertightness nor suitability for an analysis.
    """

    points: tuple[Point3D, ...]
    entities: tuple[GeometryEntity, ...]
    unit_decision: UnitDecision
    format: CadFormat
    units: Literal["m"] = "m"


def normalized_geometry(
    points: list[Point3D],
    entities: list[GeometryEntity],
    decision: UnitDecision,
    format: CadFormat,
) -> CadGeometry:
    """Apply the resolved length scale and refuse non-finite coordinates."""
    if not points or not entities:
        raise CadReadError("Geometry is empty")
    # Dimensional identity: x_m = x_source * metres_per_unit; unit factors are
    # sourced in units.py (NIST SP 811, 2008, Appendix B.8 and SI Prefixes).
    scaled = tuple(
        tuple(float(value) * decision.metres_per_unit for value in point)
        for point in points
    )
    if any(len(point) != 3 or not all(map(isfinite, point)) for point in scaled):
        raise CadReadError("Coordinates must be finite world-coordinate triples")
    for entity in entities:
        if not entity.indices or any(
            index < 0 or index >= len(scaled) for index in entity.indices
        ):
            raise CadReadError("Entity contains invalid vertex indices")
    return CadGeometry(
        cast(tuple[Point3D, ...], scaled), tuple(entities), decision, format
    )
