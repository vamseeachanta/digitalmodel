"""
ABOUTME: 2D frame solver with 3-DOF/node frame elements
ABOUTME: Axial + bending (ux, uy, theta) per node; assembles and solves K*u=F
"""

import math
from dataclasses import dataclass, field
from typing import Dict, List, Tuple

import numpy as np


@dataclass
class FrameResult:
    """Results from frame analysis."""
    displacements: np.ndarray
    reactions: Dict[int, Dict[str, float]]
    member_forces: List[Dict]


class FrameElement2D:
    """
    2D frame element: 3 DOFs per node (ux, uy, theta).
    6 DOFs total per element. Euler-Bernoulli bending + axial.
    """

    def __init__(
        self,
        elem_id: int,
        node_i: int,
        node_j: int,
        xi: float, yi: float,
        xj: float, yj: float,
        E: float,
        A: float,
        I: float,
    ):
        self.elem_id = elem_id
        self.node_i = node_i
        self.node_j = node_j
        dx = xj - xi
        dy = yj - yi
        self.L = math.sqrt(dx**2 + dy**2)
        if self.L < 1e-12:
            raise ValueError(f"Element {elem_id}: zero length")
        self.E = E
        self.A = A
        self.I = I
        self.c = dx / self.L
        self.s = dy / self.L

    def local_stiffness(self) -> np.ndarray:
        """6x6 local stiffness matrix [u1,v1,θ1,u2,v2,θ2]."""
        L = self.L
        EA_L = self.E * self.A / L
        EI = self.E * self.I
        L2 = L * L
        L3 = L * L * L

        k = np.zeros((6, 6))
        # Axial terms
        k[0, 0] = EA_L
        k[0, 3] = -EA_L
        k[3, 0] = -EA_L
        k[3, 3] = EA_L
        # Bending terms
        k[1, 1] = 12 * EI / L3
        k[1, 2] = 6 * EI / L2
        k[1, 4] = -12 * EI / L3
        k[1, 5] = 6 * EI / L2
        k[2, 1] = 6 * EI / L2
        k[2, 2] = 4 * EI / L
        k[2, 4] = -6 * EI / L2
        k[2, 5] = 2 * EI / L
        k[4, 1] = -12 * EI / L3
        k[4, 2] = -6 * EI / L2
        k[4, 4] = 12 * EI / L3
        k[4, 5] = -6 * EI / L2
        k[5, 1] = 6 * EI / L2
        k[5, 2] = 2 * EI / L
        k[5, 4] = -6 * EI / L2
        k[5, 5] = 4 * EI / L
        return k

    def transformation_matrix(self) -> np.ndarray:
        """6x6 rotation matrix from local to global coordinates."""
        c, s = self.c, self.s
        T = np.zeros((6, 6))
        T[0, 0] = c;  T[0, 1] = s
        T[1, 0] = -s; T[1, 1] = c
        T[2, 2] = 1.0
        T[3, 3] = c;  T[3, 4] = s
        T[4, 3] = -s; T[4, 4] = c
        T[5, 5] = 1.0
        return T

    def global_stiffness(self) -> np.ndarray:
        """6x6 global stiffness: K_g = T^T * K_l * T."""
        kl = self.local_stiffness()
        T = self.transformation_matrix()
        return T.T @ kl @ T

    def dof_indices(self) -> List[int]:
        """Global DOF indices for this element (3 per node)."""
        return [
            self.node_i * 3, self.node_i * 3 + 1, self.node_i * 3 + 2,
            self.node_j * 3, self.node_j * 3 + 1, self.node_j * 3 + 2,
        ]

    def member_end_forces(self, u_global: np.ndarray) -> np.ndarray:
        """Compute local member-end forces from global displacements."""
        dofs = self.dof_indices()
        u_elem = np.array([u_global[d] for d in dofs])
        T = self.transformation_matrix()
        u_local = T @ u_elem
        kl = self.local_stiffness()
        return kl @ u_local


def solve_frame(
    nodes: Dict[int, Tuple[float, float]],
    members: List[Dict],
    fixed_nodes: List[int],
    forces: Dict[int, Tuple[float, float, float]],
    E: float,
) -> FrameResult:
    """
    Solve a 2D frame for displacements, reactions, and member forces.

    Args:
        nodes: {node_id: (x, y)} coordinates
        members: list of dicts with 'id', 'nodes' (i, j), 'section' {'A', 'I'}
        fixed_nodes: node IDs with all 3 DOFs restrained
        forces: {node_id: (fx, fy, mz)} applied forces/moments
        E: Young's modulus (consistent units with geometry)

    Returns:
        FrameResult with displacements, reactions, member_forces
    """
    # Map node IDs to sequential indices
    sorted_ids = sorted(nodes.keys())
    id_map = {nid: idx for idx, nid in enumerate(sorted_ids)}
    n_nodes = len(sorted_ids)
    n_dof = n_nodes * 3

    # Build elements
    elements = []
    for m in members:
        ni, nj = m["nodes"]
        xi, yi = nodes[ni]
        xj, yj = nodes[nj]
        idx_i = id_map[ni]
        idx_j = id_map[nj]
        elem = FrameElement2D(
            elem_id=m["id"],
            node_i=idx_i, node_j=idx_j,
            xi=xi, yi=yi, xj=xj, yj=yj,
            E=E,
            A=m["section"]["A"],
            I=m["section"]["I"],
        )
        elements.append(elem)

    # Assemble global stiffness
    K = np.zeros((n_dof, n_dof))
    for elem in elements:
        kg = elem.global_stiffness()
        dofs = elem.dof_indices()
        for i_loc, i_glob in enumerate(dofs):
            for j_loc, j_glob in enumerate(dofs):
                K[i_glob, j_glob] += kg[i_loc, j_loc]

    # Assemble load vector
    F = np.zeros(n_dof)
    for nid, (fx, fy, mz) in forces.items():
        idx = id_map[nid]
        F[idx * 3] += fx
        F[idx * 3 + 1] += fy
        F[idx * 3 + 2] += mz

    # Apply fixed BCs (penalty method for clarity)
    K_mod = K.copy()
    F_mod = F.copy()
    fixed_dofs = []
    for nid in fixed_nodes:
        idx = id_map[nid]
        for d in range(3):
            fixed_dofs.append(idx * 3 + d)

    for dof in fixed_dofs:
        K_mod[dof, :] = 0.0
        K_mod[:, dof] = 0.0
        K_mod[dof, dof] = 1.0
        F_mod[dof] = 0.0

    # Solve
    u = np.linalg.solve(K_mod, F_mod)

    # Compute reactions: R = K_original * u - F_applied
    R = K @ u - F
    reactions = {}
    for nid in fixed_nodes:
        idx = id_map[nid]
        reactions[nid] = {
            "fx": R[idx * 3],
            "fy": R[idx * 3 + 1],
            "mz": R[idx * 3 + 2],
        }

    # Compute member-end forces
    member_forces = []
    for elem, m in zip(elements, members):
        f_local = elem.member_end_forces(u)
        member_forces.append({
            "id": m["id"],
            "label": m.get("label", f"member_{m['id']}"),
            "nodes": m["nodes"],
            "axial_i": f_local[0],
            "shear_i": f_local[1],
            "moment_i": f_local[2],
            "axial_j": f_local[3],
            "shear_j": f_local[4],
            "moment_j": f_local[5],
        })

    return FrameResult(
        displacements=u,
        reactions=reactions,
        member_forces=member_forces,
    )
