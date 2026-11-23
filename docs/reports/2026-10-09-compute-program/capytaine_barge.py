"""Capytaine leg for the solver baseline barge (digitalmodel #2300 pack).

Solves the same 100 x 20 x 8 m box barge, frequencies, headings, depth and
mass properties as the OrcaWave and AQWA cases in
``digitalmodel.solvers.benchmark.cases`` and writes heave added mass (te) and
heave RAO at 0 deg for every frequency, with and without an interior lid
(irregular-frequency removal).

Usage: python capytaine_barge.py <digitalmodel-src-dir> <out.json>
"""

from __future__ import annotations

import json
import sys
import time
from pathlib import Path

import numpy as np
import xarray as xr

src, out = Path(sys.argv[1]), Path(sys.argv[2])
sys.path.insert(0, str(src / "digitalmodel" / "solvers"))
from benchmark import cases  # noqa: E402  (stdlib-only pack module)

import capytaine as cpt  # noqa: E402

RHO, G, DEPTH = 1025.0, 9.80665, 200.0
L, B, T = (cases.BARGE[k] for k in ("length_m", "beam_m", "draft_m"))
MASS = L * B * T * RHO
COG = (0.0, 0.0, -2.0)
KXX, KYY, KZZ = 7.0, 29.0, 29.0
OMEGAS = np.array(cases.DIFFRACTION_FREQUENCIES, dtype=float)
HEADINGS = np.radians(cases.DIFFRACTION_HEADINGS)


def barge_mesh() -> "cpt.Mesh":
    quads = np.array(cases._barge_panels(), dtype=float)  # (n, 4, 3)
    flat = quads.reshape(-1, 3).round(6)
    verts, inverse = np.unique(flat, axis=0, return_inverse=True)
    return cpt.Mesh(verts, inverse.reshape(-1, 4), name="baseline_barge")


def solve(with_lid: bool) -> dict:
    mesh = barge_mesh()
    lid = mesh.generate_lid(z=-0.5) if with_lid else None
    body = cpt.FloatingBody(mesh=mesh, lid_mesh=lid,
                            dofs=cpt.rigid_body_dofs(rotation_center=COG),
                            center_of_mass=COG, name="baseline_barge")
    inertia = np.diag([MASS, MASS, MASS,
                       MASS * KXX**2, MASS * KYY**2, MASS * KZZ**2])
    dofs = list(body.dofs)
    body.inertia_matrix = xr.DataArray(
        inertia, dims=["influenced_dof", "radiating_dof"],
        coords={"influenced_dof": dofs, "radiating_dof": dofs})
    body.hydrostatic_stiffness = body.compute_hydrostatic_stiffness(rho=RHO, g=G)

    test = xr.Dataset(coords={
        "omega": OMEGAS, "wave_direction": HEADINGS, "radiating_dof": dofs,
        "water_depth": [DEPTH], "rho": [RHO], "g": [G]})
    t0 = time.perf_counter()
    data = cpt.BEMSolver().fill_dataset(test, body, n_jobs=8)
    solve_s = time.perf_counter() - t0
    rao = cpt.post_pro.rao(data, wave_direction=0.0)

    a33 = data["added_mass"].sel(radiating_dof="Heave", influenced_dof="Heave")
    b33 = data["radiation_damping"].sel(radiating_dof="Heave", influenced_dof="Heave")
    heave = np.abs(rao.sel(radiating_dof="Heave"))
    return {
        "lid": with_lid,
        "panels": int(mesh.nb_faces),
        "solve_s": round(solve_s, 2),
        "a33_te": [round(float(v) / 1000.0, 4) for v in a33.values.ravel()],
        "b33_te_per_s": [round(float(v) / 1000.0, 4) for v in b33.values.ravel()],
        "heave_rao_0deg": [round(float(v), 6) for v in heave.values.ravel()],
        "c33_kN_per_m": round(float(body.hydrostatic_stiffness.sel(
            influenced_dof="Heave", radiating_dof="Heave")) / 1000.0, 3),
    }


result = {
    "solver": "capytaine", "solver_version": cpt.__version__,
    "barge": cases.BARGE, "panels_expected": cases.BARGE_PANELS,
    "omega_rad_s": OMEGAS.tolist(),
    "sample_indices": cases.DIFFRACTION_SAMPLE_INDICES,
    "rho": RHO, "g": G, "water_depth_m": DEPTH,
    "runs": [solve(False), solve(True)],
}
out.write_text(json.dumps(result, indent=1) + "\n")
print(json.dumps({k: v for k, v in result.items() if k != "runs"}))
for run in result["runs"]:
    idx = cases.DIFFRACTION_SAMPLE_INDICES
    print("lid" if run["lid"] else "no-lid", run["solve_s"], "s",
          "A33@samples", [run["a33_te"][i] for i in idx],
          "RAO@samples", [run["heave_rao_0deg"][i] for i in idx])
