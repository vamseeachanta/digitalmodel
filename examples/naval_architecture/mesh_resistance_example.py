# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic box: mesh -> friction -> independent arithmetic check.

Run from the repository with PYTHONPATH=src. All physical inputs are declared
synthetic choices, not seawater properties or experimental measurements.
The hand calculation is in docs/domains/ship-design/mesh-resistance-research.md.
"""

from __future__ import annotations

import json
import math

from digitalmodel.naval_architecture.hull_fixtures import box_mesh
from digitalmodel.naval_architecture.mesh_hydrostatics import TriMesh
from digitalmodel.naval_architecture.mesh_resistance import (
    FlowConditions,
    mesh_friction,
)


def worked_example() -> dict:
    """Return evidence without writing files or running a solver."""
    vertices, faces = box_mesh(length=10, beam=2, depth=2)
    mesh = TriMesh(vertices, faces, units="m", axes=("forward", "port", "up"))
    conditions = FlowConditions(
        speed_m_s=1,
        density_kg_m3=1000,
        viscosity_m2_s=1e-5,
        gravity_m_s2=10,
        draft_m=1,
        trim_m=0,
        water_depth_m=None,
        free_surface="calm",
        roughness="smooth",
        appendages="absent",
    )
    record = mesh_friction(
        mesh,
        conditions,
        form_factor_k=0.2,
        implementation_revision="worked-example-v1",
        cite=False,
    )
    # Independent arithmetic: S=10*2+2*10*1+2*2*1=44; Re=1e6;
    # CF: ITTC 7.5-03-02-04 (2024 rev 02), unnumbered tutorial, printed page 8.
    # RF: ITTC 7.5-02-03-01.4 (2021 rev 05), section 2.4.1.
    # CF=.075/(6-2)^2; RF=CF*1000*44/2=103.125 N.
    # This absolute roundoff budget checks synthetic arithmetic only.
    reference_force = 103.125
    return {
        "resistance": record,
        "arithmetic_reference_N": reference_force,
        "arithmetic_matches": math.isclose(
            record["components"]["friction"]["force_N"],
            reference_force,
            rel_tol=0,
            abs_tol=1e-10,
        ),
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2, allow_nan=False))
