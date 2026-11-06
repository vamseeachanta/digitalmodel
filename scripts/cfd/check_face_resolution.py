#!/usr/bin/env python3
"""POST-MESH GATE between meshing and solving (#2033).

    usage: check_face_resolution.py <case-dir> [--patch hull]
                                    [--finest-cell M] [--k FACTOR]

Measures the largest face on the hull patch of the written ``constant/polyMesh``
and refuses the mesh when it exceeds ``k * (finest in-plane cell)^2``.

WHY IT SITS HERE. The mesh phase already ends with a ``checkMesh`` verdict, and
that verdict is necessary and not sufficient: the mesh that invalidated a
resistance campaign was reported "Mesh OK" with zero failed checks, because
none of checkMesh's criteria is about SIZE on a named patch. The layer-coverage
figure alongside it read 95-96 %, correctly, about a surface whose bow had
never been resolved. This is the check that separated them, and it costs one
traversal of the boundary against a solve measured in days.

EXIT CODES
    0  the patch is inside the limit; the measured ratio is printed
    1  it is not, or the gate could not be run

FAILING CLOSED IS THE POINT. "Could not determine the target cell size" exits
1, it does not skip. An absent check reads greener than a failing one, and
this whole defect class is made of signals that were absent rather than red.
Cases built outside the hull case builders carry no ``case_provenance.json``;
give them ``--finest-cell`` or ``DM_CFD_FINEST_CELL_M``.
"""
from __future__ import annotations

import argparse
import importlib.util
import os
import sys
from pathlib import Path

# Load the canonical, standard-library-only reader directly. Importing its
# package would execute unrelated solver modules and require their dependencies.
_CANDIDATES = [
    Path(os.environ.get("DM_CFD_SRC", "")) if os.environ.get("DM_CFD_SRC") else None,
    Path(__file__).resolve().parents[2] / "dm_src",
    Path(__file__).resolve().parents[2] / "src",
]
for _candidate in _CANDIDATES:
    if _candidate is not None:
        _path = _candidate / "digitalmodel/solvers/openfoam/hull_face_resolution.py"
        if _path.is_file():
            _spec = importlib.util.spec_from_file_location("_hull_face_resolution", _path)
            _reader = importlib.util.module_from_spec(_spec)
            # dataclasses resolves annotations through the defining module.
            sys.modules[_spec.name] = _reader
            try:
                _spec.loader.exec_module(_reader)
            except (ImportError, OSError) as _exc:
                sys.exit(f"FACE RESOLUTION VERDICT: FAIL -- cannot load mesh reader: {_exc}")
            break
else:
    sys.exit(
        "FACE RESOLUTION VERDICT: FAIL -- cannot locate mesh reader; "
        "set DM_CFD_SRC to a source directory containing "
        "digitalmodel/solvers/openfoam/hull_face_resolution.py."
    )

DEFAULT_FACE_AREA_FACTOR = _reader.DEFAULT_FACE_AREA_FACTOR
HullFaceResolutionError = _reader.HullFaceResolutionError
assert_patch_face_resolution = _reader.assert_patch_face_resolution
finest_in_plane_cell_m = _reader.finest_in_plane_cell_m
patch_face_areas = _reader.patch_face_areas


def _float_env(name: str) -> float | None:
    raw = os.environ.get(name)
    if raw is None or not raw.strip():
        return None
    try:
        return float(raw)
    except ValueError:
        raise HullFaceResolutionError(f"{name}={raw!r} is not a number") from None


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("case", type=Path)
    parser.add_argument("--patch", default=os.environ.get("DM_CFD_HULL_PATCH", "hull"))
    parser.add_argument("--finest-cell", type=float, default=None)
    parser.add_argument("--k", type=float, default=None)
    args = parser.parse_args(argv)

    try:
        factor = args.k or _float_env("DM_CFD_FACE_AREA_K") or DEFAULT_FACE_AREA_FACTOR
        finest = (
            args.finest_cell
            or _float_env("DM_CFD_FINEST_CELL_M")
            or finest_in_plane_cell_m(args.case)
        )
        areas = patch_face_areas(args.case / "constant" / "polyMesh", args.patch)
        ratio = assert_patch_face_resolution(areas, finest, factor=factor)
    except HullFaceResolutionError as exc:
        print(f"FACE RESOLUTION VERDICT: FAIL -- {exc}", file=sys.stderr)
        return 1

    print(
        f"FACE RESOLUTION VERDICT: PASS -- patch {areas.patch!r}: "
        f"{areas.n_faces} faces, max {areas.max_area_m2:.4g} m2 = "
        f"{ratio:.3g}x the target cell area ({finest:.4g} m)^2, "
        f"limit {factor:g}x; mean face {areas.mean_area_m2:.4g} m2"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
