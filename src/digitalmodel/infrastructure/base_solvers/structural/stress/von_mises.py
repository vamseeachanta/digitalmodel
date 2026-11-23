"""
ABOUTME: Von Mises stress analysis solver
ABOUTME: Implements linear elastic stress analysis with Von Mises failure criterion
"""

import numpy as np
from typing import Dict, Any, List, Tuple
import logging

from ..base import StructuralSolver

logger = logging.getLogger(__name__)


class VonMisesSolver(StructuralSolver):
    """
    Von Mises stress solver for linear elastic structural analysis.

    Performs linear elastic finite element analysis and computes:
    - Nodal displacements
    - Element stresses (Von Mises)
    - Safety factors against yield
    - Critical locations for failure

    Theory:
    - Linear elasticity: σ = E * ε
    - Von Mises equivalent stress: σ_vm = sqrt((σ_x - σ_y)² + (σ_y - σ_z)² + (σ_z - σ_x)²)/√2 + 3*(τ_xy² + τ_yz² + τ_zx²)
    - Safety factor: SF = σ_y / σ_vm
    """

    def __init__(self, name: str = "VonMisesSolver", version: str = "1.0.0"):
        """
        Initialize Von Mises stress solver.

        Args:
            name: Solver name
            version: Solver version
        """
        super().__init__(name=name, version=version)
        self.stress_results = {}
        self.safety_factors = {}
        self.critical_elements = []
        self.max_stress = 0.0
        self.min_safety_factor = float('inf')

    def validate_inputs(self) -> Tuple[bool, List[str]]:
        """
        Validate input data for Von Mises analysis.

        Returns:
            Tuple of (is_valid, error_list)
        """
        errors = []

        # Check elements
        if len(self.elements) == 0:
            errors.append("No elements defined")

        # Check nodes
        if len(self.nodes) == 0:
            errors.append("No nodes defined")

        # Check boundary conditions
        if len(self.boundary_conditions) == 0:
            errors.append("No boundary conditions applied")

        # Check loads
        if len(self.loads) == 0:
            errors.append("No loads applied")

        # Check material properties
        if 'E' not in self.material_properties or self.material_properties['E'] <= 0:
            errors.append("Invalid Young's modulus")

        if 'nu' not in self.material_properties or not (0 <= self.material_properties['nu'] <= 0.5):
            errors.append("Invalid Poisson's ratio")

        if 'sigma_y' not in self.material_properties or self.material_properties['sigma_y'] <= 0:
            errors.append("Invalid yield strength")

        logger.debug("Validation errors: %d", len(errors))
        return len(errors) == 0, errors

    def solve(self) -> Dict[str, Any]:
        """
        Solve Von Mises stress analysis problem.

        Returns:
            Dictionary with analysis results including:
            - status: "completed" or "failed"
            - displacements: nodal displacement vector
            - stresses: element stress dictionary
            - safety_factors: safety factor for each element
            - max_stress: maximum Von Mises stress
            - min_safety_factor: minimum safety factor
            - critical_elements: elements with SF < 1.0
        """
        logger.info("Starting Von Mises stress analysis")

        # Validate inputs
        is_valid, errors = self.validate_inputs()
        if not is_valid:
            logger.error("Validation failed: %s", errors)
            raise ValueError(f"Input validation failed: {errors}")

        try:
            # Assemble global system
            logger.info("Assembling global stiffness matrix")
            K_global = self.assemble_global_matrix()

            # Assemble load vector
            logger.info("Assembling load vector")
            F = self._assemble_load_vector()

            # Apply boundary conditions
            logger.info("Applying boundary conditions")
            K_modified, F_modified = self.apply_boundary_conditions(K_global, F)

            # Solve system
            logger.info("Solving linear system")
            displacements = self.solve_system(K_modified, F_modified)

            # Compute stresses
            logger.info("Computing element stresses")
            self._compute_all_stresses(displacements)

            # Compute safety factors
            logger.info("Computing safety factors")
            self._compute_safety_factors()

            # Identify critical elements
            logger.info("Identifying critical elements")
            self._identify_critical_elements()

            # Store results
            self.results = {
                'displacements': displacements,
                'stresses': self.stress_results,
                'safety_factors': self.safety_factors,
                'max_stress': self.max_stress,
                'min_safety_factor': self.min_safety_factor,
                'critical_elements': self.critical_elements
            }

            logger.info(
                "Von Mises analysis completed - Max stress: %.2e Pa, Min SF: %.3f",
                self.max_stress,
                self.min_safety_factor
            )

            return {
                'status': 'completed',
                'displacements': displacements,
                'stresses': self.stress_results,
                'safety_factors': self.safety_factors,
                'max_stress': self.max_stress,
                'min_safety_factor': self.min_safety_factor,
                'critical_elements': self.critical_elements,
                'num_elements': len(self.elements),
                'num_nodes': len(self.nodes),
                'yield_strength': self.material_properties['sigma_y']
            }

        except Exception as e:
            logger.error("Analysis failed: %s", str(e))
            return {
                'status': 'failed',
                'error': str(e)
            }

    def _assemble_load_vector(self) -> np.ndarray:
        """
        Assemble global load vector from applied loads.

        Returns:
            Global load vector
        """
        n_nodes = len(self.nodes)
        n_dof = n_nodes * 2  # 2D analysis
        F = np.zeros(n_dof)

        for load in self.loads:
            node_id = load['node']
            direction = load['direction']  # 'x' or 'y'
            magnitude = load['magnitude']

            dof_index = node_id * 2 + (0 if direction == 'x' else 1)
            if 0 <= dof_index < n_dof:
                F[dof_index] += magnitude

        logger.debug("Load vector assembled with %.2e Pa total load magnitude", np.linalg.norm(F))
        return F

    def _compute_all_stresses(self, displacements: np.ndarray) -> None:
        """
        Compute Von Mises stresses for all elements.

        Args:
            displacements: Nodal displacement vector
        """
        self.stress_results = {}
        self.max_stress = 0.0

        for elem_id, element in enumerate(self.elements):
            # Get element displacements
            dof_indices = element.get_global_dof_indices()
            elem_displacements = displacements[dof_indices]

            # Compute element stresses
            element_stresses = element.compute_element_stresses(
                elem_displacements,
                self.material_properties
            )

            # Store results
            self.stress_results[elem_id] = {
                'axial_stress': float(element_stresses['axial_stress']),
                'bending_stress_top': float(element_stresses['bending_stress_top']),
                'bending_stress_bottom': float(element_stresses['bending_stress_bottom']),
                'max_stress': float(element_stresses['max_stress']),
                'von_mises': float(element_stresses['max_stress'])  # For beam, von Mises = max stress
            }

            # Track maximum
            if element_stresses['max_stress'] > self.max_stress:
                self.max_stress = element_stresses['max_stress']

        logger.debug("All stresses computed - Max Von Mises: %.2e Pa", self.max_stress)

    def _compute_safety_factors(self) -> None:
        """
        Compute safety factors for all elements.

        Safety Factor = σ_y / σ_vm

        Elements with SF < 1.0 are overstressed.
        """
        self.safety_factors = {}
        self.min_safety_factor = float('inf')

        yield_strength = self.material_properties['sigma_y']
        logger.debug("Computing safety factors with yield strength: %.2e Pa", yield_strength)

        for elem_id, stresses in self.stress_results.items():
            von_mises = stresses['von_mises']

            # Compute safety factor (avoid division by zero)
            if von_mises > 1e-12:  # Essentially zero stress
                sf = yield_strength / von_mises
            else:
                sf = float('inf')

            self.safety_factors[elem_id] = sf

            # Track minimum
            if sf < self.min_safety_factor:
                self.min_safety_factor = sf

        logger.debug("Safety factors computed - Min SF: %.3f", self.min_safety_factor)

    def _identify_critical_elements(self) -> None:
        """
        Identify elements with safety factor < 1.0 (overstressed).
        """
        self.critical_elements = []

        for elem_id, sf in self.safety_factors.items():
            if sf < 1.0:
                self.critical_elements.append({
                    'element_id': elem_id,
                    'safety_factor': sf,
                    'stress': self.stress_results[elem_id]['von_mises'],
                    'status': 'overstressed'
                })

        # Sort by safety factor (most critical first)
        self.critical_elements.sort(key=lambda x: x['safety_factor'])

        if self.critical_elements:
            logger.warning(
                "%d elements are overstressed (SF < 1.0)",
                len(self.critical_elements)
            )
        else:
            logger.info("All elements are safe (SF >= 1.0)")

    def get_solver_metadata(self) -> Dict[str, Any]:
        """
        Get solver metadata.

        Returns:
            Dictionary with solver information
        """
        return {
            'name': self.name,
            'version': self.version,
            'type': 'structural',
            'analysis_type': 'static_linear_elastic',
            'solver_name': 'Von Mises Stress Analysis',
            'theory': 'Linear elasticity with Von Mises equivalent stress',
            'dof_per_node': 2,
            'dimension': 2
        }

    def get_results_summary(self) -> Dict[str, Any]:
        """
        Get summary of analysis results.

        Returns:
            Dictionary with key results
        """
        if 'displacements' not in self.results:
            return {'status': 'not_solved'}

        return {
            'status': 'completed',
            'num_elements': len(self.elements),
            'num_nodes': len(self.nodes),
            'max_displacement': float(np.max(np.abs(self.results['displacements']))),
            'max_von_mises_stress': self.max_stress,
            'yield_strength': self.material_properties['sigma_y'],
            'min_safety_factor': self.min_safety_factor,
            'overstressed_elements': len(self.critical_elements),
            'youngs_modulus': self.material_properties['E'],
            'poissons_ratio': self.material_properties['nu']
        }
