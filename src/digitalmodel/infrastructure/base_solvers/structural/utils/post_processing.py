"""
ABOUTME: Post-processing utility functions for structural analysis
ABOUTME: Provides stress computation, safety factors, critical element identification
"""

import numpy as np
from typing import Dict, List, Tuple
import logging

logger = logging.getLogger(__name__)


def compute_von_mises(
    sigma_xx: float,
    sigma_yy: float,
    sigma_zz: float = 0.0,
    tau_xy: float = 0.0,
    tau_yz: float = 0.0,
    tau_zx: float = 0.0
) -> float:
    """
    Compute Von Mises equivalent stress.

    σ_vm = sqrt((σ_x - σ_y)² + (σ_y - σ_z)² + (σ_z - σ_x)²)/√2 + 3*(τ_xy² + τ_yz² + τ_zx²))

    For 2D stress state (σ_zz = 0, τ_yz = 0, τ_zx = 0):
    σ_vm = sqrt(σ_x² - σ_x*σ_y + σ_y² + 3*τ_xy²)

    Args:
        sigma_xx: Normal stress in x direction
        sigma_yy: Normal stress in y direction
        sigma_zz: Normal stress in z direction (default 0 for 2D)
        tau_xy: Shear stress in xy plane
        tau_yz: Shear stress in yz plane (default 0 for 2D)
        tau_zx: Shear stress in zx plane (default 0 for 2D)

    Returns:
        Von Mises equivalent stress
    """
    # Compute principal stress differences
    sigma_diff_xy = sigma_xx - sigma_yy
    sigma_diff_yz = sigma_yy - sigma_zz
    sigma_diff_zx = sigma_zz - sigma_xx

    # Von Mises formula
    sigma_vm = np.sqrt(
        (sigma_diff_xy**2 + sigma_diff_yz**2 + sigma_diff_zx**2) / 2.0 +
        3.0 * (tau_xy**2 + tau_yz**2 + tau_zx**2)
    )

    return float(sigma_vm)


def compute_safety_factors(
    von_mises_stresses: Dict[int, float],
    yield_strength: float
) -> Dict[int, float]:
    """
    Compute safety factors for each element.

    Safety Factor = σ_y / σ_vm

    Args:
        von_mises_stresses: Dictionary of {element_id: von_mises_stress}
        yield_strength: Yield strength of material (Pa)

    Returns:
        Dictionary of {element_id: safety_factor}
    """
    safety_factors = {}

    for elem_id, sigma_vm in von_mises_stresses.items():
        if sigma_vm > 1e-12:  # Avoid division by zero
            sf = yield_strength / sigma_vm
        else:
            sf = float('inf')

        safety_factors[elem_id] = float(sf)

    logger.debug(
        "Computed safety factors for %d elements",
        len(safety_factors)
    )
    return safety_factors


def extract_critical_elements(
    safety_factors: Dict[int, float],
    threshold: float = 1.0
) -> List[Dict]:
    """
    Extract elements with safety factor below threshold.

    Args:
        safety_factors: Dictionary of {element_id: safety_factor}
        threshold: Critical safety factor threshold (default 1.0 for yield)

    Returns:
        List of critical element dictionaries, sorted by severity
    """
    critical_elements = []

    for elem_id, sf in safety_factors.items():
        if sf < threshold:
            critical_elements.append({
                'element_id': elem_id,
                'safety_factor': float(sf),
                'margin': float(threshold - sf),
                'status': 'critical' if sf < 0.5 else 'warning'
            })

    # Sort by safety factor (most critical first)
    critical_elements.sort(key=lambda x: x['safety_factor'])

    if critical_elements:
        logger.warning(
            "%d elements are critical (SF < %.2f)",
            len(critical_elements),
            threshold
        )
    else:
        logger.info("No critical elements found (all SF >= %.2f)", threshold)

    return critical_elements


def compute_displacement_magnitude(
    displacement_vector: np.ndarray,
    dof_per_node: int = 2
) -> np.ndarray:
    """
    Compute magnitude of displacement at each node.

    Args:
        displacement_vector: Full displacement vector (all DOFs)
        dof_per_node: Degrees of freedom per node (2 for 2D, 3 for 3D)

    Returns:
        Array of displacement magnitudes per node
    """
    n_nodes = len(displacement_vector) // dof_per_node
    magnitudes = np.zeros(n_nodes)

    for i in range(n_nodes):
        dof_indices = range(i * dof_per_node, (i + 1) * dof_per_node)
        magnitudes[i] = np.linalg.norm(displacement_vector[list(dof_indices)])

    return magnitudes


def compute_strain_energy(
    K: np.ndarray,
    u: np.ndarray
) -> float:
    """
    Compute total strain energy in the structure.

    U = (1/2) * u^T * K * u

    Args:
        K: Stiffness matrix (n_dof × n_dof)
        u: Displacement vector (n_dof,)

    Returns:
        Total strain energy (J)
    """
    U = 0.5 * np.dot(u, np.dot(K, u))
    return float(U)


def compute_compliance(
    K: np.ndarray,
    F: np.ndarray,
    u: np.ndarray
) -> float:
    """
    Compute structural compliance (flexibility).

    Compliance = F^T * u = u^T * K * u

    Args:
        K: Stiffness matrix
        F: Load vector
        u: Displacement vector

    Returns:
        Compliance (m/N)
    """
    compliance = np.dot(F, u)
    return float(compliance)


def extract_element_results(
    results: Dict,
    element_id: int
) -> Dict:
    """
    Extract results for a specific element.

    Args:
        results: Full analysis results dictionary
        element_id: Element ID

    Returns:
        Dictionary with element-specific results
    """
    element_results = {}

    # Extract stresses
    if 'stresses' in results and element_id in results['stresses']:
        element_results['stresses'] = results['stresses'][element_id]

    # Extract safety factor
    if 'safety_factors' in results and element_id in results['safety_factors']:
        element_results['safety_factor'] = results['safety_factors'][element_id]

    # Extract mode shape (for buckling)
    if 'mode_shapes' in results and element_id in results['mode_shapes']:
        element_results['mode_shape'] = results['mode_shapes'][element_id]

    return element_results
