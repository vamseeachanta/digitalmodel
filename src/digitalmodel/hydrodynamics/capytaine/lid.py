# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Resolve native interior panels without changing their covered domain.

Capytaine 3.0 Mesh.generate_lid retains grid cells wholly inside the waterline.
Boundary refinement reduces the missing strip; coplanar cell merging retains
exactly the generated area and avoids refining the whole interior indefinitely.
"""

from typing import Any

import numpy as np


def resolved_lid(mesh: Any, z: float, initial_radius: float) -> Any:
    """Refine native lid boundaries until area differs by at most five percent.

    Args:
        mesh: Immersed native mesh, optionally reflection symmetric.
        z: Interior-panel elevation.
        initial_radius: Initial native panel-radius limit in metres.

    Returns:
        A native lid whose interior cells are merged without enlarging its area.

    Raises:
        ValueError: If native generation cannot meet the coverage criterion.
    """
    import capytaine as cpt
    if isinstance(mesh, cpt.ReflectionSymmetricMesh):
        return cpt.ReflectionSymmetricMesh(
            resolved_lid(mesh.half, z, initial_radius), plane=mesh.plane,
        )
    immersed = mesh.immersed_part(free_surface=z)
    area = float(cpt.FloatingBody(mesh=immersed).waterplane_area)
    if area <= 0:
        raise ValueError('No positive waterplane area is available for the lid')
    radius = initial_radius
    for _ in range(8):
        lid = _component_lids(immersed, z, radius, 2 * initial_radius)
        if abs(float(sum(lid.faces_areas)) / area - 1) <= 0.05:
            return lid
        radius /= 2
    raise ValueError('Native lid coverage does not meet the five-percent criterion')


def _component_lids(mesh: Any, z: float, radius: float, maximum_edge: float) -> Any:
    """Limit native candidate grids to each disconnected waterline component."""
    from capytaine.meshes.geometry import connected_components_of_waterline
    import capytaine as cpt
    lids = []
    for crown in connected_components_of_waterline(mesh, z=z):
        lower, upper = crown.vertices.min(axis=0), crown.vertices.max(axis=0)
        part = mesh
        for axis in (0, 1):
            normal = np.eye(3)[axis]
            part = part.clipped(normal=normal, origin=upper)
            part = part.clipped(normal=-normal, origin=lower)
        lid = part.generate_lid(z=z, faces_max_radius=radius)
        if lid.nb_faces:
            lids.append(merge_lid_cells(lid, maximum_edge))
    if not lids:
        return cpt.Mesh(None, None)
    vertices, faces, offset = [], [], 0
    for lid in lids:
        vertices.append(lid.vertices)
        faces.append(lid.faces + offset)
        offset += len(lid.vertices)
    return cpt.Mesh(np.concatenate(vertices), np.concatenate(faces))


def merge_lid_cells(mesh: Any, maximum_edge: float) -> Any:
    """Merge adjacent rectangular lid cells while retaining area and normals.

    Args:
        mesh: Native axis-aligned planar rectangular cells.
        maximum_edge: Maximum merged edge length in metres.

    Returns:
        A native mesh covering exactly the same grid cells.
    """
    import capytaine as cpt
    x = np.unique(mesh.vertices[:, 0])
    y = np.unique(mesh.vertices[:, 1])
    rows: dict[int, set[int]] = {}
    for face in mesh.vertices[mesh.faces]:
        i = int(np.searchsorted(x, min(face[:, 0])))
        j = int(np.searchsorted(y, min(face[:, 1])))
        rows.setdefault(j, set()).add(i)
    rectangles = _grid_rectangles(rows, x, y, maximum_edge)
    vertices: list[tuple[float, float, float]] = []
    faces: list[list[int]] = []
    z = float(mesh.vertices[0, 2])
    for i0, i1, j0, j1 in rectangles:
        start = len(vertices)
        vertices.extend([(x[i0], y[j0], z), (x[i0], y[j1], z),
                         (x[i1], y[j1], z), (x[i1], y[j0], z)])
        faces.append([start, start + 1, start + 2, start + 3])
    return cpt.Mesh(np.asarray(vertices), np.asarray(faces))


def _grid_rectangles(rows: dict[int, set[int]], x: np.ndarray,
                     y: np.ndarray, maximum_edge: float) -> list[tuple[int, int, int, int]]:
    """Cover occupied grid cells with bounded rectangles without adding cells."""
    rectangles = []
    for j in sorted(rows):
        while rows[j]:
            i = min(rows[j])
            end = i + 1
            while end in rows[j] and x[end + 1] - x[i] <= maximum_edge:
                end += 1
            cells = set(range(i, end))
            top = j + 1
            while (top in rows and cells <= rows[top]
                   and y[top + 1] - y[j] <= maximum_edge):
                top += 1
            for row in range(j, top):
                rows[row].difference_update(cells)
            rectangles.append((i, end, j, top))
    return rectangles
