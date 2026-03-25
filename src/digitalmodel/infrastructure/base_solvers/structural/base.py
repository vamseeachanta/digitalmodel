"""
ABOUTME: Abstract structural solver base class
ABOUTME: Provides structural-specific analysis methods and interfaces
"""

from abc import abstractmethod
from typing import Any, Dict, List, Tuple, Optional
import numpy as np
import logging

from ..base import AnalysisSolver

logger = logging.getLogger(__name__)


class StructuralSolver(AnalysisSolver):
    """
    Abstract base class for structural analysis solvers.

    Extends AnalysisSolver with structural-specific methods:
    - Element handling and management
    - Boundary condition application
    - Load specification and assembly
    - Material property management
    - Global matrix assembly
    - Stress computation and post-processing
    - Mesh quality validation
    """

    def __init__(self, name: str, version: str, config: Optional[Dict[str, Any]] = None):
        """
        Initialize structural solver.

        Args:
            name: Solver name
            version: Solver version
            config: Optional configuration dictionary
        """
        super().__init__(name, version, config)
        self.elements: List[Any] = []
        self.nodes: Dict[int, np.ndarray] = {}
        self.boundary_conditions: Dict[str, Any] = {}
        self.loads: List[Dict[str, Any]] = []
        self.material_properties: Dict[str, Any] = {}
        self.global_stiffness_matrix: Optional[np.ndarray] = None
        self.displacements: Optional[np.ndarray] = None
        self.stresses: Dict[int, np.ndarray] = {}
        logger.debug("StructuralSolver initialized: %s v%s", name, version)

    def set_elements(self, elements: List[Any]) -> None:
        """
        Set elements for structural analysis.

        Args:
            elements: List of element objects
        """
        if not isinstance(elements, list):
            raise TypeError("Elements must be a list")
        if len(elements) == 0:
            raise ValueError("At least one element is required")

        self.elements = elements
        logger.info("Set %d elements", len(elements))

    def set_nodes(self, nodes: Dict[int, np.ndarray]) -> None:
        """
        Set node coordinates for the mesh.

        Args:
            nodes: Dictionary mapping node IDs to coordinate arrays
        """
        if not isinstance(nodes, dict):
            raise TypeError("Nodes must be a dictionary")
        if len(nodes) == 0:
            raise ValueError("At least one node is required")

        self.nodes = nodes.copy()
        logger.info("Set %d nodes", len(nodes))

    def set_boundary_conditions(self, bcs: Dict[str, Any]) -> None:
        """
        Set boundary conditions for the problem.

        Args:
            bcs: Dictionary of boundary condition specifications
                Example: {
                    'fixed_nodes': [0, 1],  # Fixed support
                    'pinned_nodes': [10],   # Pinned support
                    'prescribed_displacements': {5: {'ux': 0.1}}
                }
        """
        if not isinstance(bcs, dict):
            raise TypeError("Boundary conditions must be a dictionary")

        self.boundary_conditions = bcs.copy()
        logger.info("Set boundary conditions: %s", list(bcs.keys()))

    def set_loads(self, loads: List[Dict[str, Any]]) -> None:
        """
        Set loads for the structural problem.

        Args:
            loads: List of load dictionaries
                Example: [
                    {'node': 5, 'fx': 1000.0},  # Point load
                    {'elements': [0, 1], 'q': 50.0}  # Distributed load
                ]
        """
        if not isinstance(loads, list):
            raise TypeError("Loads must be a list")

        self.loads = [load.copy() for load in loads]
        logger.info("Set %d load cases", len(loads))

    def set_material_properties(self, material: Dict[str, Any]) -> None:
        """
        Set material properties for the analysis.

        Args:
            material: Dictionary of material properties
                Example: {
                    'E': 2.1e11,  # Young's modulus (Pa)
                    'nu': 0.3,    # Poisson's ratio
                    'rho': 7850,  # Density (kg/m³)
                    'sigma_y': 2.5e8  # Yield strength (Pa)
                }
        """
        if not isinstance(material, dict):
            raise TypeError("Material properties must be a dictionary")

        required_properties = ['E', 'nu', 'rho', 'sigma_y']
        for prop in required_properties:
            if prop not in material:
                raise ValueError(f"Missing required material property: {prop}")

        self.material_properties = material.copy()
        logger.info("Set material properties: E=%.2e, nu=%.2f, rho=%.1f, sigma_y=%.2e",
                   material['E'], material['nu'], material['rho'], material['sigma_y'])

    def assemble_global_matrix(self) -> np.ndarray:
        """
        Assemble global stiffness matrix from elements.

        Returns:
            Global stiffness matrix (n_dof × n_dof)

        Raises:
            ValueError: If elements or nodes not set
        """
        if len(self.elements) == 0:
            raise ValueError("Elements must be set before assembly")
        if len(self.nodes) == 0:
            raise ValueError("Nodes must be set before assembly")

        # Determine total DOFs (2 per node for 2D, 3 per node for 3D)
        n_nodes = len(self.nodes)
        n_dof = n_nodes * 2  # Assume 2D for now

        K_global = np.zeros((n_dof, n_dof))

        logger.debug("Assembling global stiffness matrix (%d x %d)", n_dof, n_dof)

        # Add element stiffness contributions
        for elem_id, element in enumerate(self.elements):
            K_elem = element.compute_global_stiffness_matrix()
            dof_indices = element.get_global_dof_indices()

            # Assembly loop
            for i, dof_i in enumerate(dof_indices):
                for j, dof_j in enumerate(dof_indices):
                    if 0 <= dof_i < n_dof and 0 <= dof_j < n_dof:
                        K_global[dof_i, dof_j] += K_elem[i, j]

        self.global_stiffness_matrix = K_global
        logger.info("Global stiffness matrix assembled: condition number = %.2e",
                   np.linalg.cond(K_global))

        return K_global

    def apply_boundary_conditions(self, K: np.ndarray, F: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """
        Apply boundary conditions to stiffness matrix and load vector.

        Args:
            K: Global stiffness matrix
            F: Global load vector

        Returns:
            Tuple of (modified K, modified F) with BCs applied

        Raises:
            ValueError: If boundary conditions not set
        """
        if len(self.boundary_conditions) == 0:
            raise ValueError("Boundary conditions must be set")

        K_mod = K.copy()
        F_mod = F.copy()
        n_dof = K.shape[0]

        # Handle fixed nodes (both DOFs = 0)
        if 'fixed_nodes' in self.boundary_conditions:
            fixed_nodes = self.boundary_conditions['fixed_nodes']
            for node in fixed_nodes:
                for dof in range(2):  # 2 DOFs per node
                    dof_idx = node * 2 + dof
                    if 0 <= dof_idx < n_dof:
                        K_mod[dof_idx, :] = 0
                        K_mod[:, dof_idx] = 0
                        K_mod[dof_idx, dof_idx] = 1.0
                        F_mod[dof_idx] = 0.0

        logger.debug("Boundary conditions applied")
        return K_mod, F_mod

    def solve_system(self, K: np.ndarray, F: np.ndarray) -> np.ndarray:
        """
        Solve linear system K*u = F for displacements.

        Args:
            K: Stiffness matrix (with BCs applied)
            F: Load vector (with BCs applied)

        Returns:
            Displacement vector
        """
        try:
            u = np.linalg.solve(K, F)
            self.displacements = u
            logger.info("System solved: displacement norm = %.6e", np.linalg.norm(u))
            return u
        except np.linalg.LinAlgError as e:
            logger.error("Failed to solve system: %s", str(e))
            raise ValueError("Stiffness matrix is singular or ill-conditioned") from e

    def compute_stresses(self, displacements: np.ndarray) -> Dict[int, np.ndarray]:
        """
        Compute stresses from displacements.

        Args:
            displacements: Nodal displacement vector

        Returns:
            Dictionary mapping element IDs to stress arrays

        Raises:
            NotImplementedError: Must be implemented by subclasses
        """
        raise NotImplementedError("Subclasses must implement compute_stresses()")

    def validate_mesh(self) -> Tuple[bool, List[str]]:
        """
        Validate mesh quality and connectivity.

        Returns:
            Tuple of (is_valid, error_list)
            - is_valid: True if mesh passes validation
            - error_list: List of validation error messages
        """
        errors = []

        # Check nodes exist
        if len(self.nodes) < 3:
            errors.append("Mesh must have at least 3 nodes")

        # Check elements exist
        if len(self.elements) < 1:
            errors.append("Mesh must have at least 1 element")

        # Check element connectivity
        valid_node_ids = set(self.nodes.keys())
        for elem_id, element in enumerate(self.elements):
            elem_nodes = element.get_node_ids()
            for node_id in elem_nodes:
                if node_id not in valid_node_ids:
                    errors.append(f"Element {elem_id} references non-existent node {node_id}")

        # Check for duplicate nodes
        node_coords = np.array(list(self.nodes.values()))
        for i in range(len(node_coords)):
            for j in range(i + 1, len(node_coords)):
                dist = np.linalg.norm(node_coords[i] - node_coords[j])
                if dist < 1e-10:
                    errors.append(f"Duplicate or coincident nodes detected (distance = {dist})")

        is_valid = len(errors) == 0
        logger.info("Mesh validation: %s (%d errors)", "VALID" if is_valid else "INVALID", len(errors))

        return is_valid, errors

    def get_results(self) -> Dict[str, Any]:
        """
        Get analysis results.

        Returns:
            Dictionary with displacements, stresses, and metadata
        """
        results = super().get_results()

        results.update({
            'displacements': self.displacements.copy() if self.displacements is not None else None,
            'stresses': {k: v.copy() for k, v in self.stresses.items()},
            'num_elements': len(self.elements),
            'num_nodes': len(self.nodes),
        })

        return results
