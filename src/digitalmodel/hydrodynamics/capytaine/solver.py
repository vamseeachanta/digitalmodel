"""
ABOUTME: BEM solver wrapper for Capytaine hydrodynamic analysis.
ABOUTME: Runs diffraction + radiation problems and extracts hydrodynamic coefficients.
"""

from __future__ import annotations

import logging
from typing import Dict, List, Optional, Tuple, Any

import numpy as np
import xarray as xr

from .models import BEMResult, BodyConfig, SolverConfig, WaveConditions
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
        try:
            self._body.inertia_matrix = self._body.compute_rigid_body_inertia(rho=rho)
            self._body.hydrostatic_stiffness = self._body.compute_hydrostatic_stiffness(rho=rho)
            logger.info("Computed hydrostatic stiffness and inertia for '%s'", self._body.name)
        except Exception as e:
            logger.warning("Could not compute hydrostatics: %s", e)

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
        engine = cpt.BasicMatrixEngine(
            matrix_cache_size=sc.matrix_cache_size,
            linear_solver=sc.linear_solver,
        )

        solver = cpt.BEMSolver(green_function=gf, engine=engine)
        logger.info(
            "BEMSolver created: gf=%s, engine=%s, method=%s",
            sc.green_function, sc.engine, sc.method,
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
        wc = self.wave_conditions
        dof_names = [d.value for d in self.body_config.dofs]

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

        # Added mass: (omega, radiating_dof, influenced_dof) → (n_omega, n_dof, n_dof)
        if "added_mass" in ds:
            result.added_mass = ds["added_mass"].values

        # Radiation damping
        if "radiation_damping" in ds:
            result.radiation_damping = ds["radiation_damping"].values

        # Excitation force = diffraction + Froude-Krylov (DNV-RP-C205 §7.3.1)
        if "excitation_force" in ds:
            result.excitation_force = ds["excitation_force"].values
        if "diffraction_force" in ds:
            result.diffraction_force = ds["diffraction_force"].values
        if "Froude_Krylov_force" in ds:
            result.froude_krylov_force = ds["Froude_Krylov_force"].values

        # Hydrostatics
        if "hydrostatic_stiffness" in ds:
            result.hydrostatic_stiffness = ds["hydrostatic_stiffness"].values
        if "inertia_matrix" in ds:
            result.inertia_matrix = ds["inertia_matrix"].values

        return result

    @property
    def dataset(self) -> Optional[xr.Dataset]:
        """Access the raw xarray dataset after solving."""
        return self._dataset

    @property
    def body(self):
        """Access the Capytaine FloatingBody after solving."""
        return self._body


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
