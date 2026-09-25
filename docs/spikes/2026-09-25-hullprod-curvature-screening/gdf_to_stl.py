"""
ABOUTME: Convert a WAMIT low-order GDF panel file to a triangulated STL that HullProd can read.
Mirrors ISX/ISY symmetry so HullProd sees the full hull, merges shared vertices, drops degenerate faces.

Usage: python gdf_to_stl.py <input.gdf> <output.stl>
Requires: numpy, trimesh (both are HullProd dependencies).
"""

from __future__ import annotations

import sys

import numpy as np
import trimesh


def read_gdf(path: str) -> tuple[np.ndarray, int, int, float]:
    """Return (panels[n,4,3], isx, isy, ulen) from a low-order GDF."""
    lines = open(path, encoding="latin-1").read().splitlines()
    ulen = float(lines[1].split()[0])
    isx, isy = (int(float(v)) for v in lines[2].split()[:2])
    n_panels = int(lines[3].split()[0])
    coords = np.array(
        [[float(v) for v in line.split()[:3]] for line in lines[4:] if line.strip()],
        dtype=float,
    )
    return coords[: n_panels * 4].reshape(n_panels, 4, 3), isx, isy, ulen


def quads_to_mesh(quads: np.ndarray) -> trimesh.Trimesh:
    """Split each quad into two triangles; collapsed quads (v3 == v4) become one triangle."""
    vertices = quads.reshape(-1, 3)
    faces: list[list[int]] = []
    for i in range(len(quads)):
        a, b, c, d = 4 * i, 4 * i + 1, 4 * i + 2, 4 * i + 3
        if np.allclose(quads[i, 2], quads[i, 3]):
            faces.append([a, b, c])
        else:
            faces.extend([[a, b, c], [a, c, d]])
    mesh = trimesh.Trimesh(vertices, np.asarray(faces), process=True)
    mesh.merge_vertices()
    mesh.update_faces(mesh.nondegenerate_faces())
    return mesh


def mirror(quads: np.ndarray, isx: int, isy: int) -> np.ndarray:
    """Apply GDF symmetry flags; reverse vertex order on the mirrored copy to keep normals outward."""
    if isy:
        quads = np.concatenate([quads, quads[:, ::-1] * [1, -1, 1]])
    if isx:
        quads = np.concatenate([quads, quads[:, ::-1] * [-1, 1, 1]])
    return quads


def main() -> None:
    src, dst = sys.argv[1], sys.argv[2]
    quads, isx, isy, ulen = read_gdf(src)
    mesh = quads_to_mesh(mirror(quads, isx, isy))
    print(
        f"{src}: panels={len(quads)} isx={isx} isy={isy} ulen={ulen} -> "
        f"faces={len(mesh.faces)} verts={len(mesh.vertices)} "
        f"watertight={mesh.is_watertight} extents={mesh.extents.round(2)}"
    )
    mesh.export(dst)


if __name__ == "__main__":
    main()
