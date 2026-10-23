# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Format routing with lazy optional dependencies and mandatory unit checks."""

from dataclasses import replace
from pathlib import Path

from .converter import Converter
from .models import (
    CadFormat,
    CadModel,
    CadReadError,
    DxfGeometry,
    LengthUnit,
    MeshGeometry,
)
from .units import resolve_units, validate_unit


def read_cad(
    path: str | Path,
    *,
    units: LengthUnit | None = None,
    converter: Converter | None = None,
    converted_path: str | Path | None = None,
) -> CadModel:
    """Read a supported file, requiring a declaration wherever units are absent.

    DWG requires an explicit converter and a new caller-owned DXF output path.
    Translator behavior and geometry limits are described in the reader README.
    """
    validate_unit(units)
    path = Path(path).resolve()
    formats: dict[str, CadFormat] = {
        ".dxf": "dxf",
        ".step": "step",
        ".stp": "step",
        ".iges": "iges",
        ".igs": "iges",
        ".stl": "stl",
        ".obj": "obj",
        ".dwg": "dwg",
    }
    fmt = formats.get(path.suffix.lower())
    if fmt is None:
        raise CadReadError("Unsupported CAD format")
    if not path.is_file():
        raise CadReadError("CAD input must be an existing file")
    if fmt == "dwg":
        if converter is None or converted_path is None:
            raise CadReadError("DWG requires a converter and converted_path")
        output = Path(converted_path).absolute()
        converter.convert(path, output)
        return replace(read_cad(output, units=units), format="dwg")
    try:
        if fmt == "dxf":
            return _read_dxf(path, units)
        if fmt == "step" or fmt == "iges":
            from .occt import read_brep

            return read_brep(path, fmt, units)
        import meshio
        import numpy as np

        info = resolve_units((), units)
        # meshio 5.3 probes ASCII STL bytes as a uint32 triangle count.
        # NumPy 2 can overflow that format probe; geometry is checked below.
        with np.errstate(over="ignore"):
            mesh = meshio.read(path, file_format=fmt)
        if (
            not len(mesh.points)
            or not any(len(c.data) for c in mesh.cells)
            or not np.isfinite(mesh.points).all()
        ):
            raise CadReadError("Mesh has no usable finite geometry")
        if any(
            np.any(c.data < 0) or np.any(c.data >= len(mesh.points)) for c in mesh.cells
        ):
            raise CadReadError("Mesh connectivity references missing vertices")
        return CadModel(fmt, MeshGeometry(mesh), info)
    except CadReadError:
        raise
    except ImportError as exc:
        raise CadReadError("Missing CAD dependency; see the reader README") from exc
    except (Exception, SystemExit) as exc:
        # meshio uses SystemExit for some unrecognised malformed inputs.
        raise CadReadError(f"Cannot read {fmt.upper()} geometry") from exc


def _read_dxf(path: Path, units: LengthUnit | None) -> CadModel:
    import ezdxf

    doc = ezdxf.readfile(path)
    # ezdxf stable Units documentation, INSUNITS table; named constants avoid
    # embedding DXF numeric group values. INSUNITS=0 is not metres.
    # https://ezdxf.readthedocs.io/en/stable/concepts/units.html
    codes: dict[int, LengthUnit] = {
        ezdxf.units.M: "m",
        ezdxf.units.MM: "mm",
        ezdxf.units.CM: "cm",
        ezdxf.units.IN: "in",
        ezdxf.units.FT: "ft",
    }
    declared: tuple[LengthUnit, ...] = ()
    if doc.units != 0:  # ezdxf Units table: 0 means unitless.
        if doc.units not in codes:
            from .models import UnitsError

            raise UnitsError("Unsupported DXF file units")
        declared = (codes[doc.units],)
    info = resolve_units(declared, units)
    if not len(doc.modelspace()):
        raise CadReadError("DXF modelspace contains no entities")
    return CadModel("dxf", DxfGeometry(doc), info)
