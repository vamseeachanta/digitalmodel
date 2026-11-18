# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Read STL/OBJ vertex positions and faces through the optional mesh extra."""

from contextlib import redirect_stderr, redirect_stdout
from io import StringIO
from pathlib import Path
from typing import Literal

from .models import (
    CadGeometry,
    CadReadError,
    EntityKind,
    GeometryEntity,
    LengthUnit,
    normalized_geometry,
)
from .units import resolve_units


def _obj_buffer(path: Path) -> StringIO:
    # meshio 5.3.5 OBJ reader handles polygon faces but ignores unknown records.
    # Only position/face data and non-geometric display records are accepted.
    # meshio 5.3.5, published source distribution, meshio/obj/_obj.py.
    allowed = {"v", "vt", "vn", "f", "o", "g", "s", "usemtl", "mtllib"}
    records = []
    vertex_count = 0
    try:
        with path.open(encoding="utf-8") as stream:
            for line in stream:
                record = line.partition("#")[0].strip()
                if not record:
                    continue
                if record.split()[0] not in allowed or record.endswith("\\"):
                    raise CadReadError("OBJ contains unsupported geometry records")
                fields = record.split()
                if fields[0] == "v":
                    vertex_count += 1
                    records.append(record)
                elif fields[0] == "f":
                    indices = [int(item.split("/")[0]) for item in fields[1:]]
                    if 0 in indices:
                        raise CadReadError("OBJ face has invalid indices")
                    # meshio expects positive one-based OBJ indices. Resolve
                    # negative references at the face, before later vertices.
                    # Library of Congress OBJ description (2025-02-18), Notes,
                    # Potential interoperability issues: -1 is the latest vertex.
                    indices = [i if i > 0 else vertex_count + i + 1 for i in indices]
                    if any(i <= 0 for i in indices):
                        raise CadReadError("OBJ face has invalid indices")
                    records.append("f " + " ".join(map(str, indices)))
    except CadReadError:
        raise
    except (OSError, UnicodeError, ValueError):
        raise CadReadError("OBJ file cannot be read") from None
    # Appearance records are deliberately excluded: their independent indexing
    # is not required to reconstruct position/topology and is not returned.
    return StringIO("\n".join(records) + "\n")


def read_mesh(path: Path, units: LengthUnit | None) -> CadGeometry:
    """Read source faces without triangulation; explicit units are mandatory.

    STL and OBJ comments, materials and object names are not unit declarations.
    Normals, texture coordinates, groups and materials are not returned.
    """
    decision = resolve_units(None, units)
    format: Literal["obj", "stl"] = "obj" if path.suffix.casefold() == ".obj" else "stl"
    source = _obj_buffer(path) if format == "obj" else path
    try:
        import meshio
    except ImportError:
        raise ImportError("Mesh reading requires digitalmodel[mesh]") from None
    try:
        # meshio.read may print input diagnostics and raise SystemExit on failure.
        # Keep failures within this API and do not reproduce untrusted payloads.
        with redirect_stdout(StringIO()), redirect_stderr(StringIO()):
            mesh = meshio.read(source, file_format=format)
    except (
        SystemExit,
        meshio.ReadError,
        OSError,
        ValueError,
        KeyError,
        IndexError,
        UnicodeError,
        OverflowError,
    ):
        raise CadReadError("Mesh file cannot be read") from None
    entities = []
    for block in mesh.cells:
        if block.type not in {"triangle", "quad", "polygon"}:
            raise CadReadError("Mesh contains unsupported cell geometry")
        for face in block.data:
            if len(face) < 3:
                raise CadReadError("Mesh face needs at least three vertices")
            kind: EntityKind = "triangle" if len(face) == 3 else "polygon"
            entities.append(GeometryEntity(kind, tuple(int(i) for i in face)))
    return normalized_geometry(mesh.points.tolist(), entities, decision, format)
