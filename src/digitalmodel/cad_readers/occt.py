# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""OpenCASCADE STEP and IGES adapters preserve boundary representation geometry."""

from __future__ import annotations

from pathlib import Path
from threading import RLock
from typing import TYPE_CHECKING, Literal

from .models import BrepGeometry, CadModel, CadReadError, LengthUnit, UnitsError
from .units import METRES_PER_UNIT, resolve_units

if TYPE_CHECKING:
    from OCP.STEPControl import STEPControl_Reader

_LOCK = RLock()

# OCCT 7.9 IGESData_BasicEditor::UnitFlagName / UnitFlagValue.
# Flag 3 is user-defined and is deliberately not accepted as millimetres.
# https://github.com/Open-Cascade-SAS/OCCT/blob/V7_9_0/src/IGESData/IGESData_BasicEditor.cxx
_IGES_UNITS: dict[int, LengthUnit] = {1: "in", 2: "mm", 4: "ft", 6: "m", 10: "cm"}
_STEP_NAMES: dict[str, LengthUnit] = {
    "metre": "m",
    "meter": "m",
    "m": "m",
    "millimetre": "mm",
    "millimeter": "mm",
    "mm": "mm",
    "centimetre": "cm",
    "centimeter": "cm",
    "cm": "cm",
    "inch": "in",
    "in": "in",
    "foot": "ft",
    "feet": "ft",
    "ft": "ft",
}


def read_brep(
    path: Path, fmt: Literal["step", "iges"], units: LengthUnit | None
) -> CadModel:
    """Translate into explicit millimetres, restoring kernel settings afterwards."""
    from OCP.IFSelect import IFSelect_RetDone
    from OCP.IGESControl import IGESControl_Reader
    from OCP.Interface import Interface_Static
    from OCP.STEPControl import STEPControl_Reader

    # OCCT 7.9 STEP/IGES guides, xstep.cascade.unit: target defaults to MM.
    # Explicitly set and restore it; serialize calls made by this package.
    # https://dev.opencascade.org/doc/overview/html/occt_user_guides__iges.html
    with _LOCK:
        reader = STEPControl_Reader() if fmt == "step" else IGESControl_Reader()
        previous = Interface_Static.CVal_s("xstep.cascade.unit")
        try:
            Interface_Static.SetCVal_s("xstep.cascade.unit", "MM")
            if reader.ReadFile(str(path)) != IFSelect_RetDone:
                raise CadReadError("OpenCASCADE could not read the CAD file")
            declared = _step_units(reader) if fmt == "step" else _iges_units(path)
            info = resolve_units(declared, units, "mm")
            if fmt == "step":
                # STEPControl_Reader::SetSystemLengthUnit uses mm per unit.
                reader.SetSystemLengthUnit(
                    METRES_PER_UNIT["mm"] / METRES_PER_UNIT["mm"]
                )
            elif not declared:
                # Supply missing IGES units before transfer, never after it.
                section = reader.IGESModel().GlobalSection()
                section.SetUnitFlag(
                    next(k for k, v in _IGES_UNITS.items() if v == units)
                )
                reader.IGESModel().SetGlobalSection(section)
            roots = reader.NbRootsForTransfer()
            transferred = reader.TransferRoots()
            # XSControl_Reader API: TransferRoots counts successful roots.
            if roots <= 0 or transferred != roots or reader.NbShapes() <= 0:
                raise CadReadError("OpenCASCADE transfer was empty or incomplete")
            shape = reader.OneShape()
            if shape.IsNull():
                raise CadReadError("OpenCASCADE returned null geometry")
            if fmt == "step" and not declared:
                assert units is not None  # resolve_units already required it.
                # With no unit context OCCT assumes its target unit (MM).
                # Apply the explicitly required caller interpretation.
                from OCP.BRepBuilderAPI import BRepBuilderAPI_Transform
                from OCP.gp import gp_Pnt, gp_Trsf

                transform = gp_Trsf()
                transform.SetScale(
                    gp_Pnt(), METRES_PER_UNIT[units] / METRES_PER_UNIT["mm"]
                )
                shape = BRepBuilderAPI_Transform(shape, transform, True).Shape()
            return CadModel(fmt, BrepGeometry(shape), info)
        finally:
            Interface_Static.SetCVal_s("xstep.cascade.unit", previous)


def _step_units(reader: STEPControl_Reader) -> tuple[LengthUnit, ...]:
    from OCP.TColStd import TColStd_SequenceOfAsciiString

    lengths, angles, solids = (TColStd_SequenceOfAsciiString() for _ in range(3))
    reader.FileUnits(lengths, angles, solids)
    names = [
        lengths.Value(i).ToCString().lower() for i in range(1, lengths.Length() + 1)
    ]  # OCCT sequences are 1-based.
    if any(name not in _STEP_NAMES for name in names):
        raise UnitsError("Unsupported STEP file units")
    return tuple(dict.fromkeys(_STEP_NAMES[name] for name in names))


def _iges_units(path: Path) -> tuple[LengthUnit, ...]:
    from .iges_header import original_unit_flag

    flag = original_unit_flag(path)
    if flag == 0:  # OCCT IGESData_BasicEditor: zero is an undefined unit flag.
        return ()
    if flag not in _IGES_UNITS:
        raise UnitsError("Unsupported or user-defined IGES file units")
    return (_IGES_UNITS[flag],)
