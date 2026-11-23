"""
ABOUTME: 2-node Euler-Bernoulli beam element implementation
ABOUTME: Computes local stiffness matrices and transformations
"""

import numpy as np
from typing import Dict, Any, List, Tuple
import logging

logger = logging.getLogger(__name__)


class BeamElement:
    """
    2-node beam element using Euler-Bernoulli theory.

    Degrees of freedom per node:
    - 2D: vertical displacement (y) and rotation (θ)
    - Total: 4 DOFs per element

    Properties:
    - E: Young's modulus
    - I: Second moment of inertia
    - A: Cross-sectional area
    - L: Element length
    """

    def __init__(
        self,
        elem_id: int,
        node1: int,
        node2: int,
        coord1: np.ndarray,
        coord2: np.ndarray,
        E: float,
        I: float,
        A: float
    ):
        """
        Initialize beam element.

        Args:
            elem_id: Element identifier
            node1: First node ID
            node2: Second node ID
            coord1: First node coordinates [x, y]
            coord2: Second node coordinates [x, y]
            E: Young's modulus (Pa)
            I: Second moment of inertia (m^4)
            A: Cross-sectional area (m^2)
        """
        self.elem_id = elem_id
        self.node1 = node1
        self.node2 = node2
        self.coord1 = np.array(coord1, dtype=float)
        self.coord2 = np.array(coord2, dtype=float)

        # Material and geometric properties
        self.E = float(E)  # Young's modulus
        self.I = float(I)  # Second moment of inertia
        self.A = float(A)  # Cross-sectional area

        # Compute element length and orientation
        dx = self.coord2[0] - self.coord1[0]
        dy = self.coord2[1] - self.coord1[1]
        self.L = np.sqrt(dx**2 + dy**2)

        if self.L < 1e-10:
            raise ValueError(f"Element {elem_id}: nodes are too close (L < 1e-10)")

        # Angle with respect to horizontal
        self.angle = np.arctan2(dy, dx)

        # Precompute trigonometric values
        self.c = np.cos(self.angle)
        self.s = np.sin(self.angle)

        logger.debug(
            "BeamElement %d: nodes (%d, %d), L=%.4f, angle=%.2f°, E=%.2e, I=%.4e",
            elem_id, node1, node2, self.L, np.degrees(self.angle), E, I
        )

    def compute_local_stiffness_matrix(self) -> np.ndarray:
        """
        Compute local stiffness matrix for beam element.

        Returns:
            4x4 local stiffness matrix

        Formula (Euler-Bernoulli):
        K_local = [k11  k12  k13  k14]
                  [k21  k22  k23  k24]
                  [k31  k32  k33  k34]
                  [k41  k42  k43  k44]

        Where:
        k11 = EI/L³ * 12
        k12 = EI/L² * 6
        k13 = -EI/L³ * 12
        k14 = EI/L² * 6
        ... (symmetric matrix)
        """
        L = self.L
        L2 = L * L
        L3 = L * L * L
        EI = self.E * self.I

        # Local stiffness matrix coefficients
        k11 = 12.0 * EI / L3
        k12 = 6.0 * EI / L2
        k13 = -12.0 * EI / L3
        k14 = 6.0 * EI / L2

        k22 = 4.0 * EI / L
        k23 = -6.0 * EI / L2
        k24 = 2.0 * EI / L

        k33 = 12.0 * EI / L3
        k34 = -6.0 * EI / L2

        k44 = 4.0 * EI / L

        # Assemble symmetric matrix
        K_local = np.array([
            [k11,  k12,  k13,  k14],
            [k12,  k22,  k23,  k24],
            [k13,  k23,  k33,  k34],
            [k14,  k24,  k34,  k44]
        ])

        return K_local

    def compute_transformation_matrix(self) -> np.ndarray:
        """
        Compute transformation matrix from local to global coordinates.

        Returns:
            4x4 transformation matrix T such that K_global = T^T * K_local * T
        """
        c = self.c
        s = self.s

        # Transformation matrix for 2D beam element
        # Local DOFs: [uy1, θ1, uy2, θ2]
        # Global DOFs: [ux1, uy1, θ1, ux2, uy2, θ2]
        T = np.array([
            [c,  s,  0,  0],
            [-s, c,  0,  0],
            [0,  0,  1,  0],
            [0,  0,  0,  1]
        ])

        return T

    def compute_global_stiffness_matrix(self) -> np.ndarray:
        """
        Compute global stiffness matrix.

        Returns:
            4x4 global stiffness matrix in global coordinates
        """
        K_local = self.compute_local_stiffness_matrix()
        T = self.compute_transformation_matrix()

        # K_global = T^T * K_local * T
        K_global = T.T @ K_local @ T

        return K_global

    def get_node_ids(self) -> List[int]:
        """Get node IDs for this element."""
        return [self.node1, self.node2]

    def get_global_dof_indices(self, dofs_per_node: int = 2) -> List[int]:
        """
        Get global DOF indices for this element.

        Args:
            dofs_per_node: DOFs per node (typically 2 for 2D beam)

        Returns:
            List of 4 global DOF indices
        """
        dof1_x = self.node1 * dofs_per_node
        dof1_y = self.node1 * dofs_per_node + 1
        dof2_x = self.node2 * dofs_per_node
        dof2_y = self.node2 * dofs_per_node + 1

        return [dof1_x, dof1_y, dof2_x, dof2_y]

    def compute_element_strains(
        self,
        nodal_displacements: np.ndarray
    ) -> Dict[str, np.ndarray]:
        """
        Compute strains from nodal displacements.

        Args:
            nodal_displacements: 4-element displacement vector

        Returns:
            Dictionary with strain information
        """
        L = self.L

        # Local displacements
        u_local = self.compute_transformation_matrix() @ nodal_displacements

        # Axial strain
        axial_strain = u_local[0] / L

        # Curvature (bending strain)
        curvature = (u_local[3] - u_local[1]) / L

        # Shear strain (average)
        shear_strain = (u_local[2] - u_local[0]) / L

        return {
            'axial': axial_strain,
            'curvature': curvature,
            'shear': shear_strain,
            'local_displacements': u_local
        }

    def compute_element_stresses(
        self,
        nodal_displacements: np.ndarray,
        material_properties: Dict[str, float]
    ) -> Dict[str, Any]:
        """
        Compute stresses from nodal displacements.

        Args:
            nodal_displacements: 4-element displacement vector
            material_properties: Material properties dictionary

        Returns:
            Dictionary with stress information
        """
        strains = self.compute_element_strains(nodal_displacements)
        E = material_properties.get('E', self.E)

        # Axial stress
        axial_stress = E * strains['axial']

        # Bending stress (at top and bottom of section)
        y_top = 0.5 * np.sqrt(self.I / (self.A / 12.0))  # Approximate
        bending_stress_top = E * strains['curvature'] * y_top
        bending_stress_bot = -bending_stress_top

        # Maximum bending stress
        max_bending_stress = np.max(np.abs([bending_stress_top, bending_stress_bot]))

        # Maximum stress
        max_stress = np.max(np.abs([
            axial_stress,
            max_bending_stress
        ]))

        return {
            'axial_stress': axial_stress,
            'bending_stress_top': bending_stress_top,
            'bending_stress_bottom': bending_stress_bot,
            'max_bending_stress': max_bending_stress,
            'max_stress': max_stress,
            'element_id': self.elem_id
        }

    def compute_local_internal_forces(
        self,
        nodal_displacements: np.ndarray
    ) -> np.ndarray:
        """
        Compute internal forces in local coordinates.

        Args:
            nodal_displacements: Global displacement vector

        Returns:
            4-element vector of local internal forces
        """
        K_local = self.compute_local_stiffness_matrix()
        T = self.compute_transformation_matrix()

        # Transform global displacements to local
        u_local = T @ nodal_displacements

        # Compute local forces
        f_local = K_local @ u_local

        return f_local

    def __repr__(self) -> str:
        """String representation of element."""
        return (
            f"BeamElement(id={self.elem_id}, "
            f"nodes=({self.node1}, {self.node2}), "
            f"L={self.L:.4f}m, "
            f"angle={np.degrees(self.angle):.2f}°)"
        )
