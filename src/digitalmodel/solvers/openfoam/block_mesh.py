#!/usr/bin/env python3
"""
ABOUTME: Deterministic blockMeshDict body rendering for an OpenFOAM case domain,
split out of case_builder.py so the builder holds orchestration only (#1575).
"""

from __future__ import annotations

from .models import DomainConfig

__all__ = ["render_block_mesh_dict_body"]


# The single hex block and its six boundary patches are fixed; only the vertex
# coordinates and the cell counts vary with the requested domain.
_BLOCK_MESH_TEMPLATE = """
convertToMeters 1;

vertices
(
    {vertices}
);

blocks
(
    hex (0 1 2 3 4 5 6 7) ({nx} {ny} {nz}) simpleGrading (1 1 1)
);

edges
(
);

boundary
(
    inlet
    {{
        type patch;
        faces
        (
            (0 4 7 3)
        );
    }}
    outlet
    {{
        type patch;
        faces
        (
            (1 2 6 5)
        );
    }}
    bottom
    {{
        type wall;
        faces
        (
            (0 1 2 3)
        );
    }}
    top
    {{
        type patch;
        faces
        (
            (4 5 6 7)
        );
    }}
    sides
    {{
        type symmetry;
        faces
        (
            (0 1 5 4)
            (3 7 6 2)
        );
    }}
);

mergePatchPairs
(
);

"""


def render_block_mesh_dict_body(domain: DomainConfig) -> str:
    """Render the ``system/blockMeshDict`` body for ``domain``.

    The returned string excludes the FoamFile header and the trailing rule; the
    caller composes those, matching the ``render_*_dict_body`` convention used
    by :mod:`.motion` and :mod:`.partial_fill`.

    The emitted block uses the domain's own extents and cell counts, so a
    caller-requested domain reaches the mesh rather than a builder default.
    """
    nx, ny, nz = domain.cell_counts()
    vertices = "\n    ".join(
        f"( {v[0]:>10.4f}  {v[1]:>10.4f}  {v[2]:>10.4f} )"
        for v in domain.block_mesh_vertices()
    )
    return _BLOCK_MESH_TEMPLATE.format(vertices=vertices, nx=nx, ny=ny, nz=nz)
