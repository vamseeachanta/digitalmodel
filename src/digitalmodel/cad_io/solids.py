# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""STEP solid checks, surface exports and optional tetrahedral meshing."""

from __future__ import annotations

import tempfile
from pathlib import Path
from typing import Any


def _mesh(path: Path, target: Path) -> dict[str, Any]:
    """Create one coarse first-order tetrahedral mesh and an INP export."""
    import gmsh

    if gmsh.isInitialized():
        raise RuntimeError("gmsh is already initialised; mesh in a separate process")
    gmsh.initialize(["cad_io"], readConfigFiles=False)
    try:
        gmsh.option.setNumber("General.Terminal", 0)
        gmsh.option.setNumber("General.NumThreads", 1)
        gmsh.option.setNumber("Mesh.MaxNumThreads1D", 1)
        gmsh.option.setNumber("Mesh.MaxNumThreads2D", 1)
        gmsh.option.setNumber("Mesh.MaxNumThreads3D", 1)
        gmsh.model.add("part")
        gmsh.model.occ.importShapes(str(path.resolve()))
        gmsh.model.occ.synchronize()
        volumes = gmsh.model.getEntities(3)
        if not volumes:
            raise ValueError("STEP contains no solid volumes")
        bounds = gmsh.model.getBoundingBox(-1, -1)
        span = max(bounds[i + 3] - bounds[i] for i in range(3))
        gmsh.option.setNumber("Mesh.MeshSizeMin", span / 8)
        gmsh.option.setNumber("Mesh.MeshSizeMax", span / 4)
        gmsh.option.setNumber("Mesh.ElementOrder", 1)
        gmsh.model.addPhysicalGroup(3, [tag for _, tag in volumes], name="SOLID")
        gmsh.model.mesh.generate(3)
        nodes = len(gmsh.model.mesh.getNodes()[0])
        elements = sum(len(tags) for tags in gmsh.model.mesh.getElements(3)[1])
        if not nodes or not elements:
            raise ValueError("gmsh produced no volume mesh")
        gmsh.write(str(target.resolve()))
        return {"nodes": nodes, "elements": elements, "element_scope": "volume"}
    finally:
        gmsh.finalize()


def step_checks(
    path: str | Path, out_dir: str | Path | None = None, *, mesh: bool = True,
) -> dict[str, Any]:
    """Check a STEP solid and independently attempt glTF, STL and INP exports.

    Coordinates and volumes use build123d's imported millimetres and cubic
    millimetres. Binary glTF is exported as part.glb. Mesh counts cover volume
    elements. Meshing does not constitute a finite-element quality assessment.
    """
    import build123d as cad

    source = Path(path)
    folder = Path(out_dir) if out_dir is not None else Path(tempfile.mkdtemp(prefix="cad-"))
    folder.mkdir(parents=True, exist_ok=True)
    if any((folder / name).exists() for name in ("part.glb", "part.stl", "part.inp")):
        raise FileExistsError("solid export already exists; use a fresh directory")
    shape = cad.import_step(source)
    bounds = shape.bounding_box()
    valid = shape.is_valid
    if callable(valid):
        valid = valid()
    result: dict[str, Any] = {
        "valid": bool(valid), "volume": float(shape.volume),
        "length_unit": "mm", "volume_unit": "mm^3",
        "bounding_box": {"min": list(bounds.min), "max": list(bounds.max),
                         "size": list(bounds.size)},
        "steps": {},
    }
    if not result["valid"] or not shape.solids() or result["volume"] <= 0:
        raise ValueError("STEP does not contain valid solids with positive volume")
    for name in ("gltf", "stl", "mesh"):
        if name == "mesh" and not mesh:
            result["steps"][name] = {"status": "skipped", "reason": "meshing not requested"}
            continue
        try:
            if name == "mesh":
                data = _mesh(source, folder / "part.inp")
            else:
                target = folder / ("part.glb" if name == "gltf" else "part.stl")
                ok = (cad.export_gltf(shape, target, binary=True) if name == "gltf"
                      else cad.export_stl(shape, target))
                if not ok or not target.is_file() or not target.stat().st_size:
                    raise RuntimeError("geometry export produced no output")
                data = {}
            result["steps"][name] = {"status": "ok", **data}
        except Exception as exc:
            reason = ("gmsh module or shared library is absent" if name == "mesh"
                      and isinstance(exc, (ImportError, OSError)) else type(exc).__name__)
            result["steps"][name] = {"status": "failed", "reason": reason}
    return result
