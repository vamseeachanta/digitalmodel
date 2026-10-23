# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Typed CAD results retain native topology and explicit length units."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Literal

if TYPE_CHECKING:
    from ezdxf.document import Drawing
    from meshio import Mesh
    from OCP.TopoDS import TopoDS_Shape

LengthUnit = Literal["m", "mm", "cm", "in", "ft"]
CadFormat = Literal["dxf", "step", "iges", "stl", "obj", "dwg"]


class CadReadError(ValueError):
    """A drawing cannot be read without losing the reader contract."""


class UnitsError(CadReadError):
    """Length units are missing, unsupported, or contradictory."""


@dataclass(frozen=True)
class UnitInfo:
    """Coordinate units can differ from the original file's declared units."""

    coordinate_unit: LengthUnit
    metres_per_unit: float
    declared_units: tuple[LengthUnit, ...]
    source: Literal["file", "caller"]


@dataclass(frozen=True)
class DxfGeometry:
    """Complete ezdxf document, including entities, layers and blocks."""

    document: Drawing


@dataclass(frozen=True)
class MeshGeometry:
    """meshio vertices and connectivity; no repair or welding is applied."""

    mesh: Mesh


@dataclass(frozen=True)
class BrepGeometry:
    """OpenCASCADE shape with curves, faces and any available solids."""

    shape: TopoDS_Shape


@dataclass(frozen=True)
class CadModel:
    """Geometry and its units must travel together to downstream consumers."""

    format: CadFormat
    geometry: DxfGeometry | MeshGeometry | BrepGeometry
    units: UnitInfo
