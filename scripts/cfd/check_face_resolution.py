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
import os
import sys
from pathlib import Path

# WHERE THE PACKAGE COMES FROM. The chain is deployed to the solve host as a
# copy of scripts/cfd alone, so "it imports on my checkout" is not evidence.
# Three routes, in order, and an actionable refusal if none of them work --
# never a silent skip, because a gate that cannot run must not look like a
# gate that passed.
_CANDIDATES = [
    Path(__file__).resolve().parents[2] / "src",   # running from a checkout
    Path(__file__).resolve().parents[2] / "dm_src",  # deployed lane: deploy_lane.sh ships the
                                                     # package to <lane cfd root>/dm_src beside
                                                     # <campaign>/scripts, two levels up from here
    Path(os.environ.get("DM_CFD_SRC", "")) if os.environ.get("DM_CFD_SRC") else None,
]
for _candidate in _CANDIDATES:
    if _candidate is not None and (_candidate / "digitalmodel").is_dir():
        sys.path.insert(0, str(_candidate))

try:
    from digitalmodel.solvers.openfoam.hull_face_resolution import (  # noqa: E402
        DEFAULT_FACE_AREA_FACTOR,
        HullFaceResolutionError,
        assert_patch_face_resolution,
        finest_in_plane_cell_m,
        patch_face_areas,
    )
except ImportError as _exc:  # pragma: no cover - deployment failure path
    sys.exit(
        f"FACE RESOLUTION VERDICT: FAIL -- cannot import digitalmodel "
        f"({_exc}). Install the package on this host or point DM_CFD_SRC at a "
        f"checkout's src/ directory. The gate is not optional: skipping it "
        f"releases a solve onto a mesh nobody has measured."
    )


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
