"""
ABOUTME: Convert a WAMIT low-order GDF panel file to a triangulated STL that HullProd can read.
Uses the shortest 3-D quad diagonal, alternating by panel index on ties.
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
    with open(path, encoding="latin-1") as stream:
        lines = stream.read().splitlines()
    ulen = float(lines[1].split()[0])
    isx, isy = (int(float(v)) for v in lines[2].split()[:2])
    n_panels = int(lines[3].split()[0])
    coords = np.array(
        [[float(v) for v in line.split()[:3]] for line in lines[4:] if line.strip()],
        dtype=float,
    )
    return coords[: n_panels * 4].reshape(n_panels, 4, 3), isx, isy, ulen


def quads_to_mesh(quads: np.ndarray) -> trimesh.Trimesh:
    """Split on the shortest diagonal; ties alternate, collapsed quads stay single."""
    vertices = quads.reshape(-1, 3)
    faces: list[list[int]] = []
    for i in range(len(quads)):
        a, b, c, d = 4 * i, 4 * i + 1, 4 * i + 2, 4 * i + 3
        if np.allclose(quads[i, 2], quads[i, 3]):
            faces.append([a, b, c])
        else:
            d02 = float(np.sum((quads[i, 0] - quads[i, 2]) ** 2))
            d13 = float(np.sum((quads[i, 1] - quads[i, 3]) ** 2))
            other = i % 2 if np.isclose(d02, d13, rtol=1e-12, atol=0) else d13 < d02
            faces.extend([[a, b, d], [b, c, d]] if other else [[a, b, c], [a, c, d]])
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


def mirror_mesh(mesh: trimesh.Trimesh, isx: int, isy: int) -> trimesh.Trimesh:
    """Reflect already selected triangles, preserving tie diagonals and winding."""
    vertices, faces = np.asarray(mesh.vertices), np.asarray(mesh.faces)
    for enabled, axis in ((isx, 0), (isy, 1)):
        if enabled:
            mirrored = vertices.copy()
            mirrored[:, axis] *= -1
            faces = np.vstack([faces, faces[:, ::-1] + len(vertices)])
            vertices = np.vstack([vertices, mirrored])
    return trimesh.Trimesh(vertices, faces, process=True)


def main() -> None:
    src, dst = sys.argv[1], sys.argv[2]
    quads, isx, isy, ulen = read_gdf(src)
    mesh = mirror_mesh(quads_to_mesh(quads), isx, isy)
    print(
        f"{src}: panels={len(quads)} isx={isx} isy={isy} ulen={ulen} -> "
        f"faces={len(mesh.faces)} verts={len(mesh.vertices)} "
        f"watertight={mesh.is_watertight} extents={mesh.extents.round(2)}"
    )
    mesh.export(dst)


if __name__ == "__main__":
    main()
