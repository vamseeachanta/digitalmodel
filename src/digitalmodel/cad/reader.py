# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Format dispatch for the dependency-optional CAD reader package."""

from pathlib import Path

from .converter import DwgConverter
from .models import CadGeometry, CadReadError, LengthUnit
from .units import resolve_units

# SolidWorks part/assembly and Inventor part/assembly suffixes. No licensed or
# documented reader for these formats is used by this package.
NATIVE_SOLID_MODELLER_SUFFIXES = frozenset({".sldprt", ".sldasm", ".ipt", ".iam"})


def read_cad(
    path: str | Path,
    *,
    units: LengthUnit | None = None,
    converter: DwgConverter | None = None,
) -> CadGeometry:
    """Read geometry in metres, refusing absent or contradictory units.

    ``units`` states the source coordinate unit; it fills missing file metadata.
    STEP and IGES are read through the optional OCP kernel. Native SolidWorks
    and Inventor files are refused; they are exported to STEP first.
    """
    path = Path(path)
    if not path.is_file():
        raise CadReadError("Input is not a regular file")
    suffix = path.suffix.casefold()
    if converter is not None and suffix != ".dwg":
        raise CadReadError("A converter is only used for DWG input")
    if units is not None:
        resolve_units(None, units)
    if suffix == ".dxf":
        from .dxf import read_dxf

        return read_dxf(path, units)
    if suffix in {".stl", ".obj"}:
        from .mesh import read_mesh

        return read_mesh(path, units)
    if suffix == ".dwg":
        if converter is None:
            raise CadReadError("DWG requires an explicit converter configuration")
        from .converter import read_dwg

        return read_dwg(path, units, converter)
    if suffix in {".step", ".stp", ".iges", ".igs"}:
        from .brep import read_brep

        return read_brep(path, units)
    if suffix in NATIVE_SOLID_MODELLER_SUFFIXES:
        raise CadReadError(
            "Native SolidWorks and Inventor files are not read; "
            "export STEP or IGES from the authoring CAD system"
        )
    raise CadReadError(
        "Unsupported format; supported: DXF, STL, OBJ, STEP, IGES, DWG via converter"
    )
