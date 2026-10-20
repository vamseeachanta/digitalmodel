# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic box: mesh -> friction -> independent comparison -> evidence gate.

Run from the repository with PYTHONPATH=src. All physical inputs are declared
synthetic choices, not seawater properties or experimental measurements.
The hand calculation is in docs/domains/ship-design/mesh-resistance-research.md.
"""

from __future__ import annotations

from dataclasses import replace
import hashlib
import json

from digitalmodel.naval_architecture.analytical_verification import (
    Candidate,
    assess_candidate,
)
from digitalmodel.naval_architecture.hull_fixtures import box_mesh
from digitalmodel.naval_architecture.mesh_hydrostatics import TriMesh
from digitalmodel.naval_architecture.mesh_resistance import (
    FlowConditions,
    mesh_friction,
)
from digitalmodel.naval_architecture.resistance_validation import (
    BenchmarkCase,
    compare_cases,
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
    # ITTC 2024 rev 02, sec 4: CF=.075/(6-2)^2; RF=CF*1000*44/2=103.125 N.
    derivation = b"S=44;Re=1e6;CF=.075/16;RF=103.125"
    reference = BenchmarkCase(
        case_id="analytic-box",
        family="analytic-box",
        geometry_digest=mesh.source_digest,
        source_digest=hashlib.sha256(derivation).hexdigest(),
        source_revision="independent-hand-calculation-v1",
        rights_basis="Original synthetic derivation; repository MIT license",
        redistribution_allowed=True,
        role="test",
        conditions=conditions,
        length_m=10,
        wetted_area_m2=44,
        observable="friction_force",
        normalization="dimensional",
        units="N",
        value=103.125,
    )
    prediction = replace(
        reference,
        case_id="mesh-box",
        value=record["components"]["friction"]["force_N"],
        source_revision="worked-example-v1",
        source_digest=hashlib.sha256(
            json.dumps(record, sort_keys=True, allow_nan=False).encode()
        ).hexdigest(),
    )
    comparison = compare_cases(
        prediction,
        reference,
        absolute_tolerance=1e-10,
        relative_tolerance=0,
        tolerance_source="Synthetic arithmetic roundoff budget; not a physical accuracy limit",
    )
    candidate = Candidate(
        expression="RF = rho*U^2*S*CF/2",
        method_class="empirical_correlation",
        assumptions=("ITTC-1957 smooth surface correlation",),
        provenance="ITTC 7.5-03-02-04 (2024 rev 02), section 4",
    )
    assessment = assess_candidate(candidate, {}, {"Re": (1e6, 1e6)}, {"Re": 1e6})
    return {
        "resistance": record,
        "arithmetic_comparison": comparison,
        "analytical_evidence": assessment,
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2, allow_nan=False))
