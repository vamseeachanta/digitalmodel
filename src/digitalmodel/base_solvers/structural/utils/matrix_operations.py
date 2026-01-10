"""
ABOUTME: Matrix operations utility functions for FEM
ABOUTME: Provides sparse assembly, submatrix extraction, BC enforcement
"""

import numpy as np
from typing import Tuple, List
from scipy.sparse import csr_matrix, lil_matrix
import logging

logger = logging.getLogger(__name__)


def assemble_sparse_matrix(
    elements_contributions: List[Tuple[List[int], List[int], np.ndarray]],
    size: int
) -> csr_matrix:
    """
    Assemble global sparse matrix from element contributions.

    More efficient than dense assembly for large problems.

    Args:
        elements_contributions: List of (dof_i, dof_j, K_elem) tuples
        size: Size of square matrix (n_dof × n_dof)

    Returns:
        Global sparse matrix in CSR format
    """
    K_sparse = lil_matrix((size, size))

    for dof_i, dof_j, K_elem in elements_contributions:
        for i, i_idx in enumerate(dof_i):
            for j, j_idx in enumerate(dof_j):
                if 0 <= i_idx < size and 0 <= j_idx < size:
                    K_sparse[i_idx, j_idx] += K_elem[i, j]

    logger.debug("Assembled sparse matrix: %d x %d, %d non-zeros", size, size, K_sparse.nnz)
    return K_sparse.tocsr()


def extract_submatrix(
    matrix: np.ndarray,
    free_dof: List[int]
) -> np.ndarray:
    """
    Extract submatrix for free (unconstrained) degrees of freedom.

    Used to remove rows/columns corresponding to constrained DOFs.

    Args:
        matrix: Full matrix (n_dof × n_dof)
        free_dof: Indices of free degrees of freedom

    Returns:
        Reduced matrix (n_free × n_free)
    """
    free_dof = np.array(free_dof)
    submatrix = matrix[np.ix_(free_dof, free_dof)]

    logger.debug(
        "Extracted submatrix: %d x %d → %d x %d",
        matrix.shape[0],
        matrix.shape[1],
        submatrix.shape[0],
        submatrix.shape[1]
    )
    return submatrix


def enforce_boundary_conditions(
    K: np.ndarray,
    F: np.ndarray,
    fixed_dof: List[int]
) -> Tuple[np.ndarray, np.ndarray]:
    """
    Enforce boundary conditions by matrix modification.

    Sets rows/columns for fixed DOFs to enforce displacement = 0.

    Args:
        K: Stiffness matrix (n_dof × n_dof)
        F: Load vector (n_dof,)
        fixed_dof: Indices of fixed degrees of freedom

    Returns:
        Tuple of (K_modified, F_modified)
    """
    K_mod = K.copy()
    F_mod = F.copy()

    for dof in fixed_dof:
        if 0 <= dof < K_mod.shape[0]:
            # Zero out row and column
            K_mod[dof, :] = 0.0
            K_mod[:, dof] = 0.0

            # Set diagonal to 1
            K_mod[dof, dof] = 1.0

            # Zero out load
            F_mod[dof] = 0.0

    logger.debug("Boundary conditions enforced for %d DOFs", len(fixed_dof))
    return K_mod, F_mod


def condition_number(matrix: np.ndarray) -> float:
    """
    Compute condition number of matrix.

    High condition numbers indicate numerical instability.
    CN > 1e10 suggests ill-conditioned system.

    Args:
        matrix: Square matrix

    Returns:
        Condition number (L2 norm)
    """
    try:
        # Use SVD for numerical stability
        U, s, Vt = np.linalg.svd(matrix)
        cn = s[0] / s[-1] if s[-1] > 1e-15 else float('inf')

        logger.debug("Matrix condition number: %.2e", cn)
        return cn

    except Exception as e:
        logger.warning("Failed to compute condition number: %s", str(e))
        return float('inf')
