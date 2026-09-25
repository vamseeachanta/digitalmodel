"""
ABOUTME: Run HullProd on a set of hull meshes and collect the recommended signature into one CSV.
Reproduces results/signatures.csv. Inputs are STL/OBJ/PLY (use gdf_to_stl.py for GDF panel files).

Usage: python screen_hulls.py <out_dir> <mesh1> [<mesh2> ...]
Requires: pip install hullprod   (Python 3.10-3.12)
"""

from __future__ import annotations

import csv
import json
import sys
from pathlib import Path

from hullprod import assess


def row_for(out_dir: Path, name: str, sig: dict) -> dict:
    """Flatten the recommended signature plus mesh-quality provenance into one CSV row."""
    data = json.load(open(out_dir / "signature.json"))
    mq = data["diagnostics"]["mesh_quality"]
    topo = mq["topology"]
    return {
        "hull": name,
        "lref": round(data["reference_length"]["value"], 2),
        "lref_mode": data["reference_length"]["mode"],
        "I_D": round(sig["I_D"], 3),
        "I_D_plus": round(sig["I_D_plus"], 3),
        "I_D_minus": round(sig["I_D_minus"], 3),
        "a_flat": round(sig["a_C"]["flat"], 3),
        "a_single": round(sig["a_C"]["single"], 3),
        "a_elliptic": round(sig["a_C"]["elliptic"], 3),
        "a_saddle": round(sig["a_C"]["saddle"], 3),
        "watertight": topo["is_watertight"],
        "components": topo["connected_face_component_count"],
        "reliability": mq["curvature_reliability"]["status"],
        "status": data["validity"]["I_D"]["status"],
    }


def main() -> None:
    out_root = Path(sys.argv[1])
    rows = []
    for mesh in sys.argv[2:]:
        name = Path(mesh).stem
        out_dir = out_root / f"out_{name}"
        result = assess(mesh, out_dir=out_dir, plots=True, overwrite=True)
        rows.append(row_for(out_dir, name, result.signature))
        print(name, rows[-1]["I_D"], rows[-1]["a_saddle"], rows[-1]["reliability"])
    with open(out_root / "signatures.csv", "w", newline="") as fh:
        writer = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


if __name__ == "__main__":
    main()
