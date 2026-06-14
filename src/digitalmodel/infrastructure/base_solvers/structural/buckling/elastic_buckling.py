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
            K_mod, Kg_mod, free_dofs = self._reduce_buckling_matrices(K, Kg)

            # Solve eigenvalue problem
            logger.info("Solving eigenvalue problem: (K - λKg)φ = 0")
            eigenvalues, eigenvectors = self._solve_eigenvalue_problem(K_mod, Kg_mod)
            eigenvectors = self._expand_eigenvectors(
                eigenvectors, free_dofs, K.shape[0]
            )

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
        if hasattr(element, "axial_force"):
            return float(element.axial_force)

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

        For a Euler-Bernoulli beam-column element with transverse displacement
        and rotation DOFs, positive axial_force is compressive reference load:

        Kg = N/(30L) * [ 36,   3L, -36,   3L]
                      [ 3L, 4L², -3L,  -L²]
                      [-36,  -3L,  36,  -3L]
                      [ 3L, -L², -3L, 4L²]

        Args:
            element: Element object
            axial_force: Axial force in element (N)

        Returns:
            4×4 element geometric stiffness matrix
        """
        L = element.L
        L2 = L * L

        kg = (axial_force / (30.0 * L)) * np.array([
            [36.0, 3.0 * L, -36.0, 3.0 * L],
            [3.0 * L, 4.0 * L2, -3.0 * L, -L2],
            [-36.0, -3.0 * L, 36.0, -3.0 * L],
            [3.0 * L, -L2, -3.0 * L, 4.0 * L2],
        ])

        return kg

    def _reduce_buckling_matrices(
        self, K: np.ndarray, Kg: np.ndarray
    ) -> Tuple[np.ndarray, np.ndarray, List[int]]:
        """
        Reduce K and Kg to free DOFs for a generalized buckling eigenproblem.

        Constrained DOFs must be removed rather than identity-stamped into both
        matrices; otherwise fixed DOFs create artificial eigenvalues of 1.0.
        """
        n_dof = K.shape[0]
        constrained = self._constrained_dof_indices(n_dof)
        free_dofs = [index for index in range(n_dof) if index not in constrained]
        if not free_dofs:
            raise ValueError("All DOFs are constrained; buckling eigenproblem is empty")

        selector = np.ix_(free_dofs, free_dofs)
        return K[selector], Kg[selector], free_dofs

    def _constrained_dof_indices(self, n_dof: int) -> set[int]:
        constrained: set[int] = set()
        dofs_per_node = 2

        for node in self.boundary_conditions.get("fixed_nodes", []):
            node_id = int(node)
            constrained.update(
                index
                for index in range(
                    node_id * dofs_per_node,
                    node_id * dofs_per_node + dofs_per_node,
                )
                if 0 <= index < n_dof
            )

        for node in self.boundary_conditions.get("pinned_nodes", []):
            dof_index = int(node) * dofs_per_node
            if 0 <= dof_index < n_dof:
                constrained.add(dof_index)

        for item in self.boundary_conditions.get("fixed_dofs", []):
            node_id = int(item["node"])
            for dof_name in item["dofs"]:
                offset = self._dof_offset(str(dof_name))
                dof_index = node_id * dofs_per_node + offset
                if 0 <= dof_index < n_dof:
                    constrained.add(dof_index)

        return constrained

    def _dof_offset(self, dof_name: str) -> int:
        normalized = dof_name.lower()
        if normalized in {"y", "uy", "translation", "transverse"}:
            return 0
        if normalized in {"theta", "rotation", "rz"}:
            return 1
        raise ValueError(f"Unsupported beam DOF name for buckling BC: {dof_name}")

    def _expand_eigenvectors(
        self,
        eigenvectors: np.ndarray,
        free_dofs: List[int],
        n_dof: int,
    ) -> np.ndarray:
        expanded = np.zeros((n_dof, eigenvectors.shape[1]))
        for reduced_index, dof_index in enumerate(free_dofs):
            expanded[dof_index, :] = eigenvectors[reduced_index, :]
        return expanded

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
            real_mask = (
                np.isreal(eigenvalues)
                & np.isfinite(np.real(eigenvalues))
                & (np.real(eigenvalues) > 0)
            )
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

            if len(eigenvalues) == 0:
                raise ValueError("No finite positive buckling eigenvalues found")

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
