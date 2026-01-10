"""
ABOUTME: Structural analysis utility modules
ABOUTME: Provides matrix operations, material library, and post-processing
"""

from .matrix_operations import (
    assemble_sparse_matrix,
    extract_submatrix,
    enforce_boundary_conditions,
    condition_number
)
from .post_processing import (
    compute_von_mises,
    compute_safety_factors,
    extract_critical_elements
)

__all__ = [
    'assemble_sparse_matrix',
    'extract_submatrix',
    'enforce_boundary_conditions',
    'condition_number',
    'compute_von_mises',
    'compute_safety_factors',
    'extract_critical_elements',
]
