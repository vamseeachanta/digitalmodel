# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Solve Capytaine diffraction and radiation boundary-element problems.

Hydrodynamic coefficients are extracted into the existing BEMResult model.
"""

from __future__ import annotations

import logging
from typing import Dict, List, Optional, Tuple, Any

import numpy as np
import xarray as xr

from .models import BEMResult, BodyConfig, DOF, SolverConfig, WaveConditions
from .mesh_adapter import create_floating_body

logger = logging.getLogger(__name__)


def _import_capytaine():
    try:
        import capytaine as cpt
        return cpt
    except ImportError as e:
        raise ImportError(
            "Capytaine is required. Activate the capytaine-env."
        ) from e


class CapytaineSolver:
    """Wrapper around Capytaine's BEMSolver for hydrodynamic analysis.

    Follows the digitalmodel solver pattern: configure → validate → solve → extract.
    Implements diffraction + radiation BEM analysis per potential flow theory
    (DNV-RP-C205 §7.1).
    """

    def __init__(
        self,
        body_config: BodyConfig,
        wave_conditions: WaveConditions,
        solver_config: Optional[SolverConfig] = None,
    ):
        """Configure the body, wave conditions and solver settings.

        Args:
            body_config: Mesh, rigid-body DOFs and mass properties.
            wave_conditions: Frequencies, headings and fluid properties.
            solver_config: Optional numerical solver settings.
        """
        self.body_config = body_config
        self.wave_conditions = wave_conditions
        self.solver_config = solver_config or SolverConfig()
        self._body = None
        self._solver = None
        self._dataset: Optional[xr.Dataset] = None

    def validate_inputs(self) -> Tuple[bool, List[str]]:
        """Validate input parameters before solving."""
        errors = []

        wc = self.wave_conditions
        if len(wc.omega_array) == 0:
            errors.append("No wave frequencies specified")
        if np.any(wc.omega_array <= 0):
            errors.append("All frequencies must be positive")
        if wc.water_depth <= 0:
            errors.append("Water depth must be positive (use np.inf for deep water)")
        if wc.rho <= 0:
            errors.append("Water density must be positive")
        if len(wc.headings) == 0:
            errors.append("At least one wave heading required")

        bc = self.body_config
        if bc.mesh.path is None and bc.mesh.predefined_shape is None:
            errors.append("No mesh source specified (path or predefined_shape)")
        if len(bc.dofs) == 0:
            errors.append("At least one DOF must be specified")

        return (len(errors) == 0, errors)

    def solve(self, compute_hydrostatics: bool = True) -> BEMResult:
        """Run the BEM analysis and return structured results.

        Args:
            compute_hydrostatics: If True, compute hydrostatic stiffness and
                rigid body inertia from the mesh (DNV-RP-C205 §7.2.1).

        Returns:
            BEMResult with hydrodynamic coefficients.

        Raises:
            ValueError: If input validation fails.
            RuntimeError: If the solver encounters an error.
        """
        cpt = _import_capytaine()

        # Validate
        valid, errors = self.validate_inputs()
        if not valid:
            raise ValueError(f"Input validation failed: {'; '.join(errors)}")

        # Create body
        self._body = create_floating_body(self.body_config)

        # Attach hydrostatics if needed for later RAO computation
        if compute_hydrostatics:
            self._compute_and_attach_hydrostatics(cpt)

        # Create solver
        self._solver = self._build_solver(cpt)

        # Build test matrix and solve
        self._dataset = self._run_fill_dataset(cpt, compute_hydrostatics)

        # Extract and return structured results
        return self._extract_results()

    def _compute_and_attach_hydrostatics(self, cpt):
        """Compute and attach hydrostatic properties to the body."""
        rho = self.wave_conditions.rho
        prescribed = (
            self._configured_inertia()
            if self.body_config.inertia_matrix is not None else None
        )
        try:
            if prescribed is None:
                self._body.inertia_matrix = self._body.compute_rigid_body_inertia(rho=rho)
            else:
                self._body.inertia_matrix = prescribed
            self._body.hydrostatic_stiffness = self._body.compute_hydrostatic_stiffness(
                rho=rho, g=self.wave_conditions.g,
            )
            logger.info("Computed hydrostatic stiffness and inertia for '%s'", self._body.name)
        except Exception as e:
            logger.warning("Could not compute hydrostatics: %s", e)

    def _configured_inertia(self) -> xr.DataArray:
        """Label the user-provided six-DOF matrix and select the active DOFs."""
        matrix = np.asarray(self.body_config.inertia_matrix, dtype=float)
        if matrix.shape != (6, 6) or not np.all(np.isfinite(matrix)):
            raise ValueError("The configured inertia matrix must be finite and 6 by 6")
        names = [d.value for d in DOF.all_dofs()]
        selected = [d.value for d in self.body_config.dofs]
        return xr.DataArray(
            matrix, dims=("influenced_dof", "radiating_dof"),
            coords={"influenced_dof": names, "radiating_dof": names},
        ).sel(influenced_dof=selected, radiating_dof=selected)

    def _build_solver(self, cpt):
        """Construct the Capytaine BEMSolver with configured options."""
        sc = self.solver_config

        # Green function
        gf_map = {
            "Delhommeau": cpt.Delhommeau,
        }
        gf_cls = gf_map.get(sc.green_function)
        gf = gf_cls() if gf_cls else cpt.Delhommeau()

        # Engine
        # Capytaine (2026), 3.0 changelog, Internals: the Green function
        # belongs to DefaultMatrixEngine; the previous engine was removed.
        # https://capytaine.org/stable/changelog.html
        engine = cpt.DefaultMatrixEngine(
            green_function=gf,
            linear_solver=sc.linear_solver,
        )

        solver = cpt.BEMSolver(engine=engine)
        logger.info(
            "BEMSolver created: gf=%s, engine=%s, method=%s",
            sc.green_function, type(engine).__name__, sc.method,
        )
        return solver

    def _run_fill_dataset(self, cpt, hydrostatics: bool) -> xr.Dataset:
        """Build test matrix and run solver via fill_dataset."""
        wc = self.wave_conditions
        dof_names = [d.value for d in self.body_config.dofs]

        test_matrix = xr.Dataset(coords={
            "omega": wc.omega_array,
            "wave_direction": wc.headings,
            "radiating_dof": dof_names,
            "water_depth": [wc.water_depth],
            "rho": [wc.rho],
            "g": [wc.g],
        })

        n_problems = len(wc.omega_array) * (len(wc.headings) + len(dof_names))
        logger.info(
            "Solving %d problems (%d omegas x [%d headings + %d DOFs])",
            n_problems, len(wc.omega_array), len(wc.headings), len(dof_names),
        )

        dataset = self._solver.fill_dataset(
            test_matrix,
            self._body,
            hydrostatics=hydrostatics,
            n_jobs=self.solver_config.n_jobs,
        )

        logger.info("BEM solve complete. Dataset variables: %s", list(dataset.data_vars))
        return dataset

    def _extract_results(self) -> BEMResult:
        """Extract numpy arrays from xarray dataset into BEMResult."""
        ds = self._dataset
        if ds is None:
            raise ValueError("No solved dataset is available")
        wc = self.wave_conditions
        dof_names = [d.value for d in self.body_config.dofs]

        requested = {
            "omega": wc.omega_array, "wave_direction": wc.headings,
            "radiating_dof": dof_names, "influenced_dof": dof_names,
        }
        ds = ds.sel({key: value for key, value in requested.items() if key in ds.coords})

        result = BEMResult(
            dataset=ds,
            omegas=wc.omega_array,
            periods=wc.period_array,
            headings=wc.headings,
            dof_names=dof_names,
            body_name=self.body_config.mesh.name,
            water_depth=wc.water_depth,
            rho=wc.rho,
            g=wc.g,
        )

        groups = [
            (("added_mass", "radiation_damping"), ("omega", "influenced_dof", "radiating_dof")),
            (("excitation_force", "diffraction_force", "Froude_Krylov_force"),
             ("omega", "wave_direction", "influenced_dof")),
            (("hydrostatic_stiffness", "inertia_matrix"), ("influenced_dof", "radiating_dof")),
        ]
        for variables, dimensions in groups:
            for name in variables:
                if name in ds:
                    attribute = "froude_krylov_force" if name == "Froude_Krylov_force" else name
                    setattr(result, attribute, _ordered_values(ds, name, dimensions))

        return result

    @property
    def dataset(self) -> Optional[xr.Dataset]:
        """Access the raw xarray dataset after solving."""
        return self._dataset

    @property
    def body(self):
        """Access the Capytaine FloatingBody after solving."""
        return self._body


def _ordered_values(dataset: xr.Dataset, name: str, dimensions: Tuple[str, ...]) -> np.ndarray:
    """Retain physical axes and remove only singleton environmental dimensions."""
    array = dataset[name]
    for dimension in array.dims:
        if dimension not in dimensions:
            if array.sizes[dimension] != 1:
                raise ValueError(f"Unexpected non-singleton dimension in {name}: {dimension}")
            array = array.isel({dimension: 0}, drop=True)
    return np.asarray(array.transpose(*dimensions).values)


def run_bem_analysis(
    body_config: BodyConfig,
    wave_conditions: WaveConditions,
    solver_config: Optional[SolverConfig] = None,
    compute_hydrostatics: bool = True,
) -> BEMResult:
    """Convenience function: create solver, validate, solve, return results.

    Args:
        body_config: Floating body specification.
        wave_conditions: Wave environment (DNV-RP-C205 §3.3).
        solver_config: Optional solver tuning parameters.
        compute_hydrostatics: Compute stiffness/inertia from mesh geometry.

    Returns:
        BEMResult with all hydrodynamic coefficients.
    """
    solver = CapytaineSolver(body_config, wave_conditions, solver_config)
    return solver.solve(compute_hydrostatics=compute_hydrostatics)
