# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Read STEP and IGES boundary representations through the optional OCP kernel.

Units are decoded from the exchange-file text by this module before the kernel
is used. The kernel is then told to return coordinates in that source unit, so
the shared metre conversion in ``models.normalized_geometry`` is applied once.
"""

import re
import threading
from contextlib import contextmanager
from dataclasses import replace
from math import isclose, sqrt
from pathlib import Path
from typing import Any, Iterator

from .models import (
    BrepSummary,
    CadFormat,
    CadGeometry,
    CadReadError,
    GeometryEntity,
    LengthUnit,
    Point3D,
    UnitsError,
    normalized_geometry,
)
from .units import METRES_PER_UNIT, resolve_units

# OCCT XSTEP unit names for xstep.cascade.unit and the IGES global section.
# OpenCASCADE 7.9, STEP and IGES translator user guides, "xstep.cascade.unit".
OCCT_UNIT: dict[LengthUnit, str] = {
    "m": "M",
    "mm": "MM",
    "cm": "CM",
    "in": "INCH",
    "ft": "FT",
}

# IGES 5.3 (1996), section 2.2.4 (Global section), parameters 14 (units flag)
# and 15 (units name). Flag 3 defers to the name. Flags 5, 7, 8, 9 and 11 (mile,
# kilometre, mil, micron, microinch) are outside the package unit set.
IGES_UNIT_FLAGS: dict[int, LengthUnit] = {1: "in", 2: "mm", 4: "ft", 6: "m", 10: "cm"}
IGES_UNIT_FLAG_FOR: dict[LengthUnit, int] = {
    unit: flag for flag, unit in IGES_UNIT_FLAGS.items()
}
IGES_UNIT_NAMES: dict[str, LengthUnit] = {
    "IN": "in",
    "INCH": "in",
    "MM": "mm",
    "FT": "ft",
    "M": "m",
    "CM": "cm",
}

# ISO 10303-41 SI_UNIT prefixes accepted for .METRE.; $ denotes no prefix.
STEP_SI_PREFIXES: dict[str, LengthUnit] = {"$": "m", ".MILLI.": "mm", ".CENTI.": "cm"}
STEP_CONVERSION_NAMES: dict[str, LengthUnit] = {"inch": "in", "foot": "ft"}
# Unit names reported by the OCCT 7.9 STEP reader (FileUnits), for cross-check.
OCCT_STEP_UNIT_NAMES: dict[str, LengthUnit] = {
    "metre": "m",
    "meter": "m",
    "millimetre": "mm",
    "millimeter": "mm",
    "centimetre": "cm",
    "centimeter": "cm",
    "inch": "in",
    "foot": "ft",
}

# Display tessellation only: the triangles approximate curved faces within this
# fraction of the bounding-box diagonal. Volume, area and the bounding box are
# computed from the exact B-rep, not from these triangles. 0.5 rad is the
# OCCT BRepMesh default angular deflection.
RELATIVE_LINEAR_DEFLECTION = 1e-3
ANGULAR_DEFLECTION_RAD = 0.5

_OCP_LOCK = threading.Lock()
_P21_TOKEN = re.compile(r"'(?:[^']|'')*'|/\*.*?\*/|;", re.S)
_P21_INSTANCE = re.compile(r"\s*#(\d+)\s*=\s*")
_P21_REFERENCE = re.compile(r"#(\d+)")
_UNIT_CONTEXT = re.compile(r"\bGLOBAL_UNIT_ASSIGNED_CONTEXT\s*\(\s*\(([^()]*)\)")
_LENGTH_UNIT = re.compile(r"\bLENGTH_UNIT\s*\(")
_SI_LENGTH = re.compile(r"\bSI_UNIT\s*\(\s*(\$|\.[A-Z]+\.)\s*,\s*\.METRE\.\s*\)")
_CONVERSION = re.compile(
    r"\bCONVERSION_BASED_UNIT\s*\(\s*'((?:[^']|'')*)'\s*,\s*#(\d+)\s*\)"
)
_MEASURE = re.compile(
    r"\bLENGTH_MEASURE_WITH_UNIT\s*\(\s*(?:LENGTH_MEASURE\s*\(\s*)?"
    r"([-+]?[0-9.]+(?:[ED][-+]?[0-9]+)?)\s*\)?\s*,\s*#(\d+)\s*\)"
)
_HOLLERITH = re.compile(r" *(\d+)H")


def _p21_instances(text: str) -> dict[int, str]:
    """Split ISO 10303-21 text into instance bodies, keeping quoted strings."""
    instances: dict[int, str] = {}
    pieces: list[str] = []
    position = 0
    for match in _P21_TOKEN.finditer(text):
        pieces.append(text[position : match.start()])
        position = match.end()
        token = match.group()
        if token.startswith("/*"):
            pieces.append(" ")
        elif token != ";":
            pieces.append(token)
        else:
            statement = "".join(pieces)
            pieces = []
            head = _P21_INSTANCE.match(statement)
            if head:
                instances[int(head.group(1))] = statement[head.end() :]
    return instances


def _step_length_unit(body: str, instances: dict[int, str], nested: bool) -> LengthUnit:
    si = _SI_LENGTH.search(body)
    if si and "CONVERSION_BASED_UNIT" not in body:
        if si.group(1) not in STEP_SI_PREFIXES:
            raise UnitsError("Unsupported STEP length unit prefix")
        return STEP_SI_PREFIXES[si.group(1)]
    conversion = _CONVERSION.search(body)
    if conversion is None or nested:
        raise UnitsError("Unsupported STEP length unit")
    name = conversion.group(1).replace("''", "'").strip().casefold()
    if name not in STEP_CONVERSION_NAMES:
        raise UnitsError("Unsupported STEP conversion-based length unit")
    measure = _MEASURE.search(instances.get(int(conversion.group(2)), ""))
    if measure is None:
        raise UnitsError("STEP conversion-based unit has no length factor")
    base = _step_length_unit(
        instances.get(int(measure.group(2)), ""), instances, nested=True
    )
    metres = float(measure.group(1).replace("D", "E")) * METRES_PER_UNIT[base]
    unit = STEP_CONVERSION_NAMES[name]
    # The named unit and its stated factor are independent declarations; a
    # disagreement is a contradiction, not a rounding question.
    if not isclose(metres, METRES_PER_UNIT[unit], rel_tol=1e-9):
        raise UnitsError("STEP conversion-based unit name and factor conflict")
    return unit


def step_declared_units(text: str) -> LengthUnit | None:
    """Return the length unit of the STEP representation contexts, if declared.

    Only units assigned through GLOBAL_UNIT_ASSIGNED_CONTEXT count. Contexts
    with different length units are refused rather than reconciled.
    """
    instances = _p21_instances(text)
    found: set[LengthUnit] = set()
    for body in instances.values():
        for context in _UNIT_CONTEXT.finditer(body):
            for reference in _P21_REFERENCE.findall(context.group(1)):
                unit_body = instances.get(int(reference), "")
                if _LENGTH_UNIT.search(unit_body):
                    found.add(_step_length_unit(unit_body, instances, nested=False))
    if len(found) > 1:
        raise UnitsError("STEP declares more than one length unit")
    return found.pop() if found else None


def _iges_parameter(
    section: str, position: int, delimiter: str, terminator: str
) -> tuple[str | None, int]:
    """Return one Global parameter and the index of the separator after it."""
    hollerith = _HOLLERITH.match(section, position)
    if hollerith:
        end = hollerith.end() + int(hollerith.group(1))
        if end > len(section):
            raise CadReadError("IGES Global section string is truncated")
        following = len(section) - len(section[end:].lstrip(" "))
        return section[hollerith.end() : end], following
    ends = [
        index
        for index in (
            section.find(delimiter, position),
            section.find(terminator, position),
        )
        if index >= 0
    ]
    if not ends:
        raise CadReadError("IGES Global section is not terminated")
    return section[position : min(ends)].strip() or None, min(ends)


def iges_global_parameters(text: str) -> list[str | None]:
    """Parse the IGES Global section (columns 1-72 of lines flagged G)."""
    section = "".join(
        line[:72] for line in text.splitlines() if len(line) >= 73 and line[72] == "G"
    )
    if not section.strip():
        raise CadReadError("IGES Global section is missing")
    parameters: list[str | None] = []
    delimiter, terminator = ",", ";"
    position = 0
    while True:
        value, position = _iges_parameter(section, position, delimiter, terminator)
        if position >= len(section):
            raise CadReadError("IGES Global section is not terminated")
        separator = section[position]
        position += 1
        # Parameters 1 and 2 redefine the delimiter and the terminator.
        if value is not None and len(parameters) < 2:
            if len(value) != 1:
                raise CadReadError("IGES Global section delimiter is invalid")
            if parameters:
                terminator = value
            else:
                delimiter = value
        parameters.append(value)
        if len(parameters) > 1 and separator == terminator:
            return parameters
        if separator != delimiter:
            raise CadReadError("IGES Global section has an invalid delimiter")


def _iges_scale_is_unity(scale: str | None) -> None:
    if scale is None:
        return
    try:
        ratio = float(scale.replace("D", "E"))
    except ValueError:
        raise CadReadError("IGES model space scale is not a number") from None
    if ratio != 1.0:
        raise CadReadError("IGES model space scale other than 1 is unsupported")


def _iges_flag_unit(flag_text: str | None) -> tuple[int | None, LengthUnit | None]:
    if flag_text is None:
        return None, None
    try:
        flag = int(flag_text)
    except ValueError:
        raise UnitsError("Unsupported IGES units flag") from None
    if flag == 3:
        return flag, None
    if flag not in IGES_UNIT_FLAGS:
        raise UnitsError("Unsupported IGES units flag")
    return flag, IGES_UNIT_FLAGS[flag]


def iges_declared_units(text: str) -> LengthUnit | None:
    """Return the IGES model-space length unit, or None when the flag is blank.

    IGES 5.3 defaults a blank units flag to inches; that default is not treated
    as a declaration here, so the caller must state the units.
    """
    parameters = iges_global_parameters(text) + [None] * 15
    _iges_scale_is_unity(parameters[12])
    flag, by_flag = _iges_flag_unit(parameters[13])
    name = (parameters[14] or "").strip().upper()
    if name and name not in IGES_UNIT_NAMES:
        raise UnitsError("Unsupported IGES units name")
    by_name = IGES_UNIT_NAMES.get(name)
    if flag == 3 and by_name is None:
        raise UnitsError("IGES units flag 3 requires a units name")
    if by_flag is not None and by_name is not None and by_flag != by_name:
        raise UnitsError("IGES units flag and units name conflict")
    return by_flag if by_flag is not None else by_name


@contextmanager
def _kernel_session(unit: LengthUnit) -> Iterator[None]:
    """Set the kernel output unit and quiet native messages; restore both.

    OCCT translator settings are process-wide, so reads are serialised here.
    """
    from OCP.IGESControl import IGESControl_Controller
    from OCP.Interface import Interface_Static
    from OCP.Message import Message, Message_Fail
    from OCP.STEPControl import STEPControl_Controller

    with _OCP_LOCK:
        STEPControl_Controller.Init_s()
        IGESControl_Controller.Init_s()
        previous = Interface_Static.CVal_s("xstep.cascade.unit")
        printers = [
            (printer, printer.GetTraceLevel())
            for printer in Message.DefaultMessenger_s().Printers()
        ]
        try:
            Interface_Static.SetCVal_s("xstep.cascade.unit", OCCT_UNIT[unit])
            for printer, _ in printers:
                printer.SetTraceLevel(Message_Fail)
            yield
        finally:
            Interface_Static.SetCVal_s("xstep.cascade.unit", previous)
            for printer, level in printers:
                printer.SetTraceLevel(level)


def _read_step_shape(path: Path, declared: LengthUnit | None) -> Any:
    from OCP.IFSelect import IFSelect_RetDone
    from OCP.STEPControl import STEPControl_Reader
    from OCP.TColStd import TColStd_SequenceOfAsciiString

    reader = STEPControl_Reader()
    if reader.ReadFile(str(path)) != IFSelect_RetDone:
        raise CadReadError("STEP file cannot be read")
    lengths = TColStd_SequenceOfAsciiString()
    reader.FileUnits(
        lengths, TColStd_SequenceOfAsciiString(), TColStd_SequenceOfAsciiString()
    )
    names = {
        lengths.Value(index).ToCString().strip().casefold()
        for index in range(1, lengths.Length() + 1)
    } - {""}
    kernel = {OCCT_STEP_UNIT_NAMES.get(name) for name in names}
    # The kernel converts each context from its own unit; its reading must
    # agree with the header parse, or the identity scale below is not valid.
    if kernel != ({declared} if declared else set()):
        raise UnitsError("STEP length unit is interpreted inconsistently")
    if reader.TransferRoots() == 0:
        raise CadReadError("STEP file has no transferable shape")
    return reader.OneShape()


def _read_iges_shape(path: Path, unit: LengthUnit) -> Any:
    from OCP.IFSelect import IFSelect_RetDone
    from OCP.IGESControl import IGESControl_Reader
    from OCP.TCollection import TCollection_HAsciiString

    reader = IGESControl_Reader()
    if reader.ReadFile(str(path)) != IFSelect_RetDone:
        raise CadReadError("IGES file cannot be read")
    # The resolved unit is written into the kernel's copy of the Global section
    # so a blank flag is not read with the IGES default of inches.
    model = reader.IGESModel()
    section = model.GlobalSection()
    section.SetUnitFlag(IGES_UNIT_FLAG_FOR[unit])
    section.SetUnitName(TCollection_HAsciiString(OCCT_UNIT[unit]))
    model.SetGlobalSection(section)
    if reader.TransferRoots() == 0:
        raise CadReadError("IGES file has no transferable shape")
    return reader.OneShape()


def _count(shape: Any, kind: Any) -> Any:
    from OCP.TopExp import TopExp
    from OCP.TopTools import TopTools_IndexedMapOfShape

    shapes = TopTools_IndexedMapOfShape()
    TopExp.MapShapes_s(shape, kind, shapes)
    return shapes


def _summarise(shape: Any, scale: float) -> tuple[BrepSummary, float]:
    from OCP.Bnd import Bnd_Box
    from OCP.BRepBndLib import BRepBndLib
    from OCP.BRepCheck import BRepCheck_Analyzer
    from OCP.BRepGProp import BRepGProp
    from OCP.GProp import GProp_GProps
    from OCP.TopAbs import TopAbs_FACE, TopAbs_SHELL, TopAbs_SOLID

    solids = _count(shape, TopAbs_SOLID)
    faces = _count(shape, TopAbs_FACE)
    if faces.Extent() == 0:
        raise CadReadError("Shape has no faces; wireframe-only input is unsupported")
    box = Bnd_Box()
    BRepBndLib.AddOptimal_s(shape, box, False, False)
    if box.IsVoid():
        raise CadReadError("Shape has no bounded geometry")
    low, high = box.CornerMin(), box.CornerMax()
    corners = ((low.X(), low.Y(), low.Z()), (high.X(), high.Y(), high.Z()))
    diagonal = sqrt(sum((b - a) ** 2 for a, b in zip(*corners)))
    if not diagonal > 0:
        raise CadReadError("Shape has a degenerate bounding box")
    volume = None
    if solids.Extent():
        volume = 0.0
        for index in range(1, solids.Extent() + 1):
            properties = GProp_GProps()
            BRepGProp.VolumeProperties_s(solids.FindKey(index), properties)
            volume += properties.Mass()
    properties = GProp_GProps()
    BRepGProp.SurfaceProperties_s(shape, properties)
    deflection = diagonal * RELATIVE_LINEAR_DEFLECTION
    bounding_box: tuple[Point3D, Point3D] = (
        (corners[0][0] * scale, corners[0][1] * scale, corners[0][2] * scale),
        (corners[1][0] * scale, corners[1][1] * scale, corners[1][2] * scale),
    )
    summary = BrepSummary(
        solids=solids.Extent(),
        shells=_count(shape, TopAbs_SHELL).Extent(),
        faces=faces.Extent(),
        bounding_box=bounding_box,
        volume_m3=None if volume is None else volume * scale**3,
        surface_area_m2=properties.Mass() * scale**2,
        is_valid=bool(BRepCheck_Analyzer(shape).IsValid()),
        linear_deflection_m=deflection * scale,
    )
    return summary, deflection


def _triangles(
    shape: Any, deflection: float
) -> tuple[list[Point3D], list[GeometryEntity]]:
    from OCP.BRep import BRep_Tool
    from OCP.BRepMesh import BRepMesh_IncrementalMesh
    from OCP.TopAbs import TopAbs_FACE, TopAbs_REVERSED
    from OCP.TopExp import TopExp_Explorer
    from OCP.TopLoc import TopLoc_Location
    from OCP.TopoDS import TopoDS

    BRepMesh_IncrementalMesh(shape, deflection, False, ANGULAR_DEFLECTION_RAD, False)
    points: list[Point3D] = []
    entities: list[GeometryEntity] = []
    explorer = TopExp_Explorer(shape, TopAbs_FACE)
    face_number = 0
    while explorer.More():
        face = TopoDS.Face_s(explorer.Current())
        explorer.Next()
        face_number += 1
        location = TopLoc_Location()
        triangulation = BRep_Tool.Triangulation_s(face, location)
        if triangulation is None or triangulation.NbTriangles() == 0:
            raise CadReadError(f"Face {face_number} could not be tessellated")
        transform = location.Transformation()
        base = len(points) - 1  # Poly_Triangulation nodes are one-based.
        for index in range(1, triangulation.NbNodes() + 1):
            node = triangulation.Node(index).Transformed(transform)
            points.append((node.X(), node.Y(), node.Z()))
        flip = face.Orientation() == TopAbs_REVERSED
        for index in range(1, triangulation.NbTriangles() + 1):
            triangle = triangulation.Triangle(index)
            a, b, c = (triangle.Value(k) for k in (1, 2, 3))
            if flip:
                b, c = c, b
            entities.append(
                GeometryEntity(
                    "triangle",
                    (base + a, base + b, base + c),
                    source_id=f"face {face_number}",
                )
            )
    return points, entities


def read_brep(path: Path, units: LengthUnit | None) -> CadGeometry:
    """Read a STEP or IGES shape; units come from the file or the caller.

    Points and triangle entities are a display tessellation, grouped by B-rep
    face in ``source_id``. ``CadGeometry.brep`` carries exact kernel properties.
    """
    try:
        import OCP  # noqa: F401
    except ImportError:
        raise ImportError(
            "STEP and IGES reading requires digitalmodel[cad-occ]"
        ) from None
    format: CadFormat = (
        "step" if path.suffix.casefold() in {".step", ".stp"} else "iges"
    )
    try:
        # ISO 10303-21 and IGES are ASCII formats; latin-1 decodes any byte so
        # malformed input is refused by the parsers rather than the decoder.
        text = path.read_text(encoding="latin-1")
    except OSError:
        raise CadReadError(f"{format.upper()} file cannot be read") from None
    declared = (
        step_declared_units(text) if format == "step" else iges_declared_units(text)
    )
    decision = resolve_units(declared, units)
    with _kernel_session(decision.source_units):
        if format == "step":
            shape = _read_step_shape(path, declared)
        else:
            shape = _read_iges_shape(path, decision.source_units)
        if shape is None or shape.IsNull():
            raise CadReadError(f"{format.upper()} file has no shape")
        summary, deflection = _summarise(shape, decision.metres_per_unit)
        points, entities = _triangles(shape, deflection)
    geometry = normalized_geometry(points, entities, decision, format)
    return replace(geometry, brep=summary)
