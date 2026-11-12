#!/usr/bin/env python3
"""
ABOUTME: Deterministic blockMeshDict body rendering for an OpenFOAM case domain,
split out of case_builder.py so the builder holds orchestration only (#1575).
"""

from __future__ import annotations

from .models import BoundaryCondition, DomainConfig

__all__ = ["boundary_patch_types", "render_block_mesh_dict_body"]


def boundary_patch_types(
    boundary_conditions: list[BoundaryCondition] | None = None,
) -> dict[str, str]:
    """Return mesh types consistent with the effective no-slip velocity BCs.

    OpenFOAM v2312 (2023), noSlip and nutkWallFunction, Usage: both require
    ``type wall`` in the mesh boundary. Other patches retain their original
    topology. Keep names stable for probes, manifests and boundary overrides.
    https://doc.openfoam.com/2312/tools/processing/boundary-conditions/rtm/derived/wall/noSlip/
    """
    patch_types = dict(
        inlet="patch", outlet="patch", bottom="wall", top="patch", sides="symmetry"
    )
    # Match _boundary_field: the last BC for a patch and field is effective,
    # including a type supplied through BoundaryCondition.extra.
    velocity_types = {
        bc.patch_name: bc.to_foam_dict()[bc.patch_name]["type"]
        for bc in (boundary_conditions or []) if bc.field == "U"
    }
    for patch, bc_type in velocity_types.items():
        if patch in patch_types and bc_type == "noSlip":
            patch_types[patch] = "wall"
    return patch_types


# The single hex block and its six boundary patches are fixed; only the vertex
# coordinates, cell counts and no-slip patch types vary with the case.
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
        type {inlet_type};
        faces
        (
            (0 4 7 3)
        );
    }}
    outlet
    {{
        type {outlet_type};
        faces
        (
            (1 2 6 5)
        );
    }}
    bottom
    {{
        type {bottom_type};
        faces
        (
            (0 1 2 3)
        );
    }}
    top
    {{
        type {top_type};
        faces
        (
            (4 5 6 7)
        );
    }}
    sides
    {{
        type {sides_type};
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


def render_block_mesh_dict_body(
    domain: DomainConfig,
    boundary_conditions: list[BoundaryCondition] | None = None,
) -> str:
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
    patch_types = {
        f"{patch}_type": kind
        for patch, kind in boundary_patch_types(boundary_conditions).items()
    }
    return _BLOCK_MESH_TEMPLATE.format(
        vertices=vertices, nx=nx, ny=ny, nz=nz, **patch_types
    )
