# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""First-order panel hydrodynamics with an internal irregular-frequency lid."""
from __future__ import annotations

from typing import TYPE_CHECKING

import numpy as np
import xarray as xr

from .basis import aggregate
from .geometry import hull_mesh

if TYPE_CHECKING:
    import capytaine as cpt

DOFS = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]


def low_frequency_solver() -> cpt.BEMSolver:
    """Use the supported finite-depth kernel for small depth/wavelength ratios."""
    import capytaine as cpt
    return cpt.BEMSolver(green_function=cpt.FinGreen3D())


def cylinder_added_mass(segments: int) -> float:
    """Compute heave added mass for a 10 m diameter, 20 m submerged cylinder."""
    import capytaine as cpt
    mesh = cpt.mesh_vertical_cylinder(radius=5, length=20, center=(0, 0, -10),
                                      resolution=(4, segments, 8)).immersed_part()
    mesh = cpt.Mesh(mesh.vertices, [face for face, center in zip(mesh.faces, mesh.faces_centers)
                                    if center[2] < -1e-8])
    body = cpt.FloatingBody(mesh=mesh, dofs=cpt.rigid_body_dofs(),
                            center_of_mass=(0, 0, -10),
                            lid_mesh=mesh.generate_lid(z=-0.5))
    problem = cpt.RadiationProblem(body=body, radiating_dof="Heave",
                                   omega=2*np.pi*0.049, water_depth=200, rho=1025, g=9.81)
    result = cpt.BEMSolver().solve(problem)
    return float(result.added_mass["Heave"])


def solve_hydrodynamics(basis: dict, step: float, omega: np.ndarray,
                        headings: np.ndarray) -> tuple:
    """Solve all six radiation modes and selected first-order wave headings."""
    import capytaine as cpt
    mesh = hull_mesh(basis, step)
    data = aggregate(basis, basis["system"]["adopted_reading"])
    cg = data["cg"].copy()
    cg[1] = 0.0
    lid = mesh.generate_lid(z=-0.5, faces_max_radius=step/2)
    body = cpt.FloatingBody(
        mesh=mesh, dofs=cpt.rigid_body_dofs(rotation_center=(0, 0, 0)),
        center_of_mass=cg, mass=data["mass"],
        lid_mesh=lid, name="reference_platform")
    environment = basis["environment"]
    rho, gravity = environment["water_density"]["value"], environment["gravity"]["value"]
    hydrostatics = body.compute_hydrostatics(rho=rho, g=gravity)
    stiffness = body.compute_hydrostatic_stiffness(rho=rho, g=gravity).sel(
        influenced_dof=DOFS, radiating_dof=DOFS).values
    test = xr.Dataset(coords={
        "omega": omega, "wave_direction": np.radians(headings), "radiating_dof": DOFS,
        "water_depth": [environment["water_depth"]["value"]], "rho": [rho], "g": [gravity]})
    regular = test.sel(omega=omega[omega >= 0.05])
    solved = cpt.BEMSolver().fill_dataset(regular, body, n_jobs=1, n_threads=8)
    if np.any(omega < 0.05):
        low = low_frequency_solver().fill_dataset(
            test.sel(omega=omega[omega < 0.05]), body, n_jobs=1, n_threads=8)
        solved = low.combine_first(solved).sortby("omega")
    if len(solved.omega) != len(omega):
        raise ValueError("BEM did not return all requested frequencies")
    return solved, {"panels": mesh.nb_faces, "volume": float(mesh.volume),
                    "center_of_buoyancy": np.asarray(hydrostatics["center_of_buoyancy"]).tolist(),
                    "maximum_panel_radius": float(mesh.faces_radiuses.max()),
                    "maximum_lid_panel_radius": float(body.lid_mesh.faces_radiuses.max()),
                    "panel_size": step,
                    "waterplane_area": float(hydrostatics["waterplane_area"]),
                    "stiffness": stiffness.tolist()}


def coefficient_arrays(dataset: xr.Dataset) -> tuple:
    """Extract frequency/mode tensors with explicit axes independent of storage."""
    added = dataset["added_mass"].squeeze().sel(influenced_dof=DOFS, radiating_dof=DOFS).transpose(
        "omega", "influenced_dof", "radiating_dof").values
    radiation = dataset["radiation_damping"].squeeze().sel(influenced_dof=DOFS, radiating_dof=DOFS).transpose(
        "omega", "influenced_dof", "radiating_dof").values
    excitation = dataset["excitation_force"].squeeze().sel(influenced_dof=DOFS).transpose(
        "omega", "wave_direction", "influenced_dof").values
    return added, radiation, excitation
