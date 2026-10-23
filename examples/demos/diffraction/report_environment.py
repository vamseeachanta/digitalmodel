"""Compute report GM from explicit SI inputs without running a solver.

Method: OrcaWave help, Results: Hydrostatics / Hydrostatic stiffness matrix,
online edition accessed 2026-10-22; C44=rho*g*V*GM_T, likewise C55 for GM_L.
Inputs must describe the same equilibrium and reference frame. No density,
gravity or acceptance threshold is assumed.
"""

import argparse
import json

from digitalmodel.hydrodynamics.diffraction.report_computations import compute_stability
from digitalmodel.hydrodynamics.diffraction.report_data_models import HydrostaticData


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("density", "gravity", "volume", "c44", "c55"):
        parser.add_argument(f"--{name}", type=float, required=True)
    inputs = parser.parse_args()
    # Six rigid-body DOFs in the report contract; the other entries are unused.
    restoring = [[0.0] * 6 for _ in range(6)]
    restoring[3][3] = inputs.c44
    restoring[4][4] = inputs.c55
    hydrostatics = HydrostaticData(
        volume=inputs.volume,
        mass=inputs.density * inputs.volume,
        centre_of_buoyancy=[0.0, 0.0, 0.0],
        centre_of_mass=[0.0, 0.0, 0.0],
        waterplane_area=0.0,
        restoring_matrix=restoring,
    )
    result = compute_stability(hydrostatics, rho=inputs.density, g=inputs.gravity)
    print(json.dumps({key: result[key] for key in ("gm_transverse", "gm_longitudinal")}))


if __name__ == "__main__":
    main()
