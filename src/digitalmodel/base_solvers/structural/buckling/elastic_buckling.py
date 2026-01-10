"""
ABOUTME: Elastic buckling (linear stability) analysis solver
ABOUTME: Solves eigenvalue problem for critical loads and mode shapes
"""

import numpy as np
from scipy.linalg import eig
from typing import Dict, Any, List, Tuple
import logging

from ..base import StructuralSolver

logger = logging.getLogger(__name__)


class ElasticBucklingSolver(StructuralSolver):
    """
    Linear elastic buckling analysis solver.

    Performs linear stability analysis to determine:
    - Critical buckling loads
    - Mode shapes
    - Buckling factors (ratios to applied load)

    Theory:
    - Eigenvalue problem: (K + λ*Kg)*φ = 0
    - K: Linear elastic stiffness matrix
    - Kg: Geometric stiffness matrix (load-dependent)
    - λ: Buckling factor (critical load ratio)
    - φ: Mode shape (buckling pattern)
    """

    def __init__(self, name: str = "ElasticBucklingSolver", version: str = "1.0.0"):
        """
        Initialize elastic buckling solver.

        Args:
            name: Solver name
            version: Solver version
        """
        super().__init__(name=name, version=version)
        self.num_modes = 5  # Number of buckling modes to extract
        self.eigenvalues = np.array([])
        self.eigenvectors = np.array([])
        self.critical_loads = {}
        self.mode_shapes = {}

    def set_num_modes(self, num_modes: int) -> None:
        """
        Set number of buckling modes to extract.

        Args:
            num_modes: Number of modes (1-100)

        Raises:
            ValueError: If num_modes is invalid
        """
        if not isinstance(num_modes, int) or num_modes < 1 or num_modes > 100:
            raise ValueError(f"num_modes must be int between 1 and 100, got {num_modes}")

        self.num_modes = num_modes
        logger.info("Number of buckling modes set to %d", num_modes)

    def validate_inputs(self) -> Tuple[bool, List[str]]:
        """
        Validate input data for buckling analysis.

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

        # Check loads (for geometric stiffness)
        if len(self.loads) == 0:
            errors.append("No loads applied (needed for geometric stiffness)")

        # Check material properties
        if 'E' not in self.material_properties or self.material_properties['E'] <= 0:
            errors.append("Invalid Young's modulus")

        # Check num_modes
        if self.num_modes < 1:
            errors.append("num_modes must be >= 1")

        logger.debug("Validation errors: %d", len(errors))
        return len(errors) == 0, errors

    def solve(self) -> Dict[str, Any]:
        """
        Solve linear elastic buckling problem.

        Returns:
            Dictionary with buckling analysis results including:
            - status: "completed" or "failed"
            - critical_loads: Buckling factors for each mode
            - mode_shapes: Eigenvectors (buckling patterns)
            - eigenvalues: λ values for each mode
            - first_critical_load: Lowest buckling factor
        """
        logger.info("Starting elastic buckling analysis")

        # Validate inputs
        is_valid, errors = self.validate_inputs()
        if not is_valid:
            logger.error("Validation failed: %s", errors)
            raise ValueError(f"Input validation failed: {errors}")

        try:
            # Assemble matrices
            logger.info("Assembling stiffness matrix")
            K = self.assemble_global_matrix()

            logger.info("Assembling geometric stiffness matrix")
            Kg = self._assemble_geometric_stiffness_matrix()

            # Apply boundary conditions
            logger.info("Applying boundary conditions")
            K_mod, _ = self.apply_boundary_conditions(K, np.zeros(K.shape[0]))
            Kg_mod, _ = self.apply_boundary_conditions(Kg, np.zeros(Kg.shape[0]))

            # Solve eigenvalue problem
            logger.info("Solving eigenvalue problem: (K - λKg)φ = 0")
            eigenvalues, eigenvectors = self._solve_eigenvalue_problem(K_mod, Kg_mod)

            # Store results
            self._store_results(eigenvalues, eigenvectors)

            logger.info(
                "Buckling analysis completed - First critical load factor: %.3f",
                self.critical_loads[0] if len(self.critical_loads) > 0 else 0
            )

            return {
                'status': 'completed',
                'critical_loads': self.critical_loads,
                'mode_shapes': self.mode_shapes,
                'eigenvalues': self.eigenvalues.tolist(),
                'first_critical_load': self.critical_loads[0] if len(self.critical_loads) > 0 else 0,
                'num_modes': len(self.critical_loads),
                'num_elements': len(self.elements),
                'num_nodes': len(self.nodes)
            }

        except Exception as e:
            logger.error("Buckling analysis failed: %s", str(e))
            return {
                'status': 'failed',
                'error': str(e)
            }

    def _assemble_geometric_stiffness_matrix(self) -> np.ndarray:
        """
        Assemble geometric stiffness matrix from element axial forces.

        The geometric stiffness matrix accounts for the stiffening or
        destabilizing effects of applied loads on structural rigidity.

        For a beam element with axial force N:
        Kg = (N/L) * geometry_matrix

        Returns:
            Global geometric stiffness matrix (n_dof × n_dof)
        """
        n_nodes = len(self.nodes)
        n_dof = n_nodes * 2
        Kg = np.zeros((n_dof, n_dof))

        logger.debug("Assembling geometric stiffness matrix from %d elements", len(self.elements))

        for elem_id, element in enumerate(self.elements):
            # Estimate element axial force from loads
            # Simple approach: assume load distribution proportional to element
            # In production, would compute from preliminary linear analysis
            N_elem = self._estimate_element_axial_force(elem_id, element)

            if abs(N_elem) > 1e-12:
                # Compute element geometric stiffness
                Kg_elem = self._compute_element_geometric_stiffness(element, N_elem)

                # Add to global matrix
                dof_indices = element.get_global_dof_indices()
                for i, dof_i in enumerate(dof_indices):
                    for j, dof_j in enumerate(dof_indices):
                        if 0 <= dof_i < n_dof and 0 <= dof_j < n_dof:
                            Kg[dof_i, dof_j] += Kg_elem[i, j]

        logger.debug("Geometric stiffness matrix assembled")
        return Kg

    def _estimate_element_axial_force(self, elem_id: int, element: Any) -> float:
        """
        Estimate axial force in element from applied loads.

        For simplicity, assumes uniform load distribution.
        Production code would compute from preliminary linear analysis.

        Args:
            elem_id: Element ID
            element: Element object

        Returns:
            Estimated axial force (N)
        """
        # Compute total load magnitude
        total_load = sum(abs(load['magnitude']) for load in self.loads)

        # Distribute proportionally to elements
        num_elements = len(self.elements)
        if num_elements > 0:
            return total_load / num_elements * np.cos(element.angle)
        else:
            return 0.0

    def _compute_element_geometric_stiffness(
        self,
        element: Any,
        axial_force: float
    ) -> np.ndarray:
        """
        Compute element geometric stiffness matrix.

        For a beam element:
        Kg = (N/L) * [0, 0, 0, 0]
                      [0, 6/5, 0, 1/10]
                      [0, 0, 0, 0]
                      [0, 1/10, 0, 6/5]

        Args:
            element: Element object
            axial_force: Axial force in element (N)

        Returns:
            4×4 element geometric stiffness matrix
        """
        L = element.L
        N_over_L = axial_force / L

        # Simplified geometric stiffness (for bending)
        kg = N_over_L * np.array([
            [0.0, 0.0, 0.0, 0.0],
            [0.0, 6.0/5.0, 0.0, 1.0/10.0],
            [0.0, 0.0, 0.0, 0.0],
            [0.0, 1.0/10.0, 0.0, 6.0/5.0]
        ])

        return kg

    def _solve_eigenvalue_problem(
        self,
        K: np.ndarray,
        Kg: np.ndarray
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Solve eigenvalue problem (K - λKg)φ = 0.

        Rearranged as: Kg⁻¹*K*φ = (1/λ)*φ
        Which gives: solve_eig(K, Kg) → λ

        Args:
            K: Linear stiffness matrix
            Kg: Geometric stiffness matrix

        Returns:
            Tuple of (eigenvalues, eigenvectors)

        Raises:
            ValueError: If eigenvalue solution fails
        """
        try:
            # Solve generalized eigenvalue problem
            # Returns eigenvalues where λ = critical load factor
            logger.debug("Solving generalized eigenvalue problem")
            eigenvalues, eigenvectors = eig(K, Kg)

            # Filter to real positive eigenvalues
            # (complex eigenvalues indicate numerical issues)
            real_mask = np.isreal(eigenvalues) & (np.real(eigenvalues) > 0)
            eigenvalues = np.real(eigenvalues[real_mask])
            eigenvectors = np.real(eigenvectors[:, real_mask])

            # Sort by eigenvalue (ascending)
            sort_indices = np.argsort(eigenvalues)
            eigenvalues = eigenvalues[sort_indices]
            eigenvectors = eigenvectors[:, sort_indices]

            # Keep requested number of modes
            n_modes = min(self.num_modes, len(eigenvalues))
            eigenvalues = eigenvalues[:n_modes]
            eigenvectors = eigenvectors[:, :n_modes]

            logger.debug("Eigenvalue problem solved - %d modes found", len(eigenvalues))
            logger.debug("Eigenvalues (critical load factors): %s", eigenvalues)

            return eigenvalues, eigenvectors

        except Exception as e:
            logger.error("Eigenvalue solution failed: %s", str(e))
            raise ValueError(f"Failed to solve eigenvalue problem: {e}")

    def _store_results(self, eigenvalues: np.ndarray, eigenvectors: np.ndarray) -> None:
        """
        Store buckling analysis results.

        Args:
            eigenvalues: Critical load factors (λ)
            eigenvectors: Mode shapes (φ)
        """
        self.eigenvalues = eigenvalues
        self.eigenvectors = eigenvectors
        self.critical_loads = {}
        self.mode_shapes = {}

        for mode_id, (eigenvalue, eigenvector) in enumerate(zip(eigenvalues, eigenvectors.T)):
            self.critical_loads[mode_id] = float(eigenvalue)
            self.mode_shapes[mode_id] = eigenvector.astype(float)

        logger.debug("Results stored for %d buckling modes", len(self.critical_loads))

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
            'analysis_type': 'linear_buckling',
            'solver_name': 'Elastic Buckling Analysis',
            'theory': 'Eigenvalue analysis of (K - λKg)φ = 0',
            'dof_per_node': 2,
            'dimension': 2
        }

    def get_results_summary(self) -> Dict[str, Any]:
        """
        Get summary of buckling analysis results.

        Returns:
            Dictionary with key results
        """
        if len(self.critical_loads) == 0:
            return {'status': 'not_solved'}

        return {
            'status': 'completed',
            'num_modes': len(self.critical_loads),
            'first_critical_load': self.critical_loads[0],
            'critical_loads': self.critical_loads,
            'num_elements': len(self.elements),
            'num_nodes': len(self.nodes)
        }

    def get_mode_shape(self, mode_id: int) -> np.ndarray:
        """
        Get mode shape for specified buckling mode.

        Args:
            mode_id: Mode number (0-indexed)

        Returns:
            Mode shape vector

        Raises:
            IndexError: If mode_id is out of range
        """
        if mode_id not in self.mode_shapes:
            raise IndexError(f"Mode {mode_id} not found")

        return self.mode_shapes[mode_id]
