"""Worked synthetic resistance example; no CFD result or solver invocation.

Independent hand calculation (all dimensions and forces are invented):
L=4 m, B=1 m, draft=0.6 m, depth=1 m. The submerged rectangular solid has
volume L*B*T=2.4 m3 and wetted area L*B+2*(L+B)*T=10 m2.
With rho=1000 kg/m3 and U=2 m/s, rho*S*U**2/2=20000 N.
Half-body forces -10, -13 and -25 N give Ct=.001, .0013 and .0025.
For representative spacings 1, 2 and 4 m, Ct=.0009+.0001*h**2: p=2,
extrapolated Ct=.0009, absolute GCI=1.25*.0001=.000125.
GCI method and safety factor: NASA grid-convergence tutorial (2021),
'Richardson Extrapolation' and 'Grid Convergence Index'.
All policy settings below are illustrative inputs with no design authority.
The coarse spacings demonstrate arithmetic only, not a usable CFD mesh.
"""

from __future__ import annotations

import argparse
import json
import math
from pathlib import Path

from digitalmodel.naval_architecture.hull_fixtures import box_mesh
from digitalmodel.naval_architecture.hull_ingest import ingest_triangles
from digitalmodel.solvers.openfoam.hull_case import HullCaseConfig
from digitalmodel.solvers.openfoam.hull_manifest import HullManifest
from digitalmodel.solvers.openfoam.resistance_campaign import (
    CampaignPolicy,
    ResistanceEvidence,
    assess_resistance_campaign,
    prepare_resistance_case,
)


def run_example(output: Path) -> dict:
    """Write all generated files beneath a new caller-selected output folder."""
    output.mkdir(parents=True, exist_ok=False)
    vertices, faces = box_mesh(4, 1, 1)  # Independent geometry calculation above.
    triangles = [tuple(tuple(vertices[i]) for i in face) for face in faces]
    ingested = ingest_triangles(
        triangles,
        output / "geometry",
        source_name="synthetic_box",
        units_in="m",
        forward="+x",
        draft_m=0.6,
    )
    config = HullCaseConfig(
        manifest=HullManifest.from_dict(ingested.to_dict()),
        stl_path=output / "geometry" / ingested.stl_file,
        velocity=2,
        density=1000,
        kinematic_viscosity=1e-6,
        ranks=1,
        free_surface_cells_per_wavelength=None,
    )
    policy = CampaignPolicy(
        window=20,
        minimum_samples=3,
        maximum_relative_span=0.01,
        force_balance_relative_tolerance=1e-10,
        refinement_ratio_relative_tolerance=1e-10,
        source="Independent worked arithmetic example; illustrative settings only",
    )
    evidence = []
    # Invented forces, spacings and cell metadata from the docstring calculation.
    for index, (spacing, total, cells) in enumerate(
        ((1, -10, 512), (2, -13, 64), (4, -25, 8))
    ):
        case = prepare_resistance_case(config.replace(name=f"grid_{index}"), output)
        force_file = case / "synthetic_force.dat"
        force_file.write_text(
            "# Time total_x total_y total_z pressure_x pressure_y pressure_z "
            "viscous_x viscous_y viscous_z\n"
            + "".join(
                f"{time} ({total} 0 0) (-4 0 0) ({total + 4} 0 0)\n"
                for time in range(4)
            )
        )
        evidence.append(ResistanceEvidence(case, force_file, cells, spacing))
    result = assess_resistance_campaign(evidence, policy)
    result["data_basis"] = "Synthetic arithmetic demonstration; no solver was run"
    (output / "prediction.json").write_text(
        json.dumps(result, indent=2, sort_keys=True, allow_nan=False) + "\n"
    )
    return result


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "output", type=Path, help="New directory for generated example files"
    )
    result = run_example(parser.parse_args().output)
    # Human display is rounded; prediction.json retains the computed values.
    grid = result["grid"]
    # Round the displayed uncertainty upward so its magnitude is not understated.
    display_step = 10 ** (math.floor(math.log10(grid["absolute_gci"])) - 1)
    displayed_gci = math.ceil(grid["absolute_gci"] / display_step) * display_step
    print(
        json.dumps(
            {
                "ct": f"{result['levels'][0]['ct']:.1e}",
                "observed_order": f"{grid['observed_order']:.1f}",
                "extrapolated_ct": f"{grid['extrapolated_ct']:.1e}",
                "absolute_gci": f"{displayed_gci:.1e}",
                "validation": result["validation"]["status"],
            },
            indent=2,
        )
    )
