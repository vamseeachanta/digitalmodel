"""
ABOUTME: RAO (Response Amplitude Operator) computation from BEM results.
ABOUTME: Solves the frequency-domain equation of motion for vessel response.
"""

from __future__ import annotations

import logging
from typing import Optional

import numpy as np

from .models import BEMResult, RAOResult

logger = logging.getLogger(__name__)


def compute_rao(bem_result: BEMResult) -> RAOResult:
    """Compute RAOs from BEM hydrodynamic coefficients using Capytaine's post-processing.

    Uses the impedance method: RAO = F_exc / Z where
    Z = -omega^2 * (M + A) + j*omega*B + K  (DNV-RP-C205 §7.2.5)

    Args:
        bem_result: Completed BEM analysis results with hydrostatics.

    Returns:
        RAOResult with complex RAO, amplitude, and phase.

    Raises:
        ValueError: If hydrostatic data is missing from the BEM result.
    """
    ds = bem_result.dataset
    if ds is None:
        raise ValueError("No dataset in BEM result — run solver first")

    if "hydrostatic_stiffness" not in ds or "inertia_matrix" not in ds:
        raise ValueError(
            "Hydrostatic stiffness and inertia matrix required for RAO computation. "
            "Re-run solver with compute_hydrostatics=True."
        )

    try:
        from capytaine.post_pro import rao as cpt_rao
    except ImportError:
        from capytaine.post_pro.rao import rao as cpt_rao

    rao_ds = cpt_rao(ds)

    # Extract RAO values
    rao_complex = rao_ds["RAO"].values if "RAO" in rao_ds else rao_ds.values
    rao_amp = np.abs(rao_complex)
    rao_phase = np.rad2deg(np.angle(rao_complex))

    result = RAOResult(
        rao=rao_complex,
        rao_amplitude=rao_amp,
        rao_phase=rao_phase,
        omegas=bem_result.omegas,
        periods=bem_result.periods,
        headings=bem_result.headings,
        dof_names=bem_result.dof_names,
        dataset=rao_ds,
    )

    logger.info("RAO computation complete: shape=%s", rao_complex.shape)
    return result


def compute_rao_manual(
    bem_result: BEMResult,
    mass_matrix: np.ndarray,
    stiffness_matrix: np.ndarray,
    damping_extra: Optional[np.ndarray] = None,
) -> RAOResult:
    """Compute RAOs manually from provided mass/stiffness matrices.

    For cases where the user supplies their own structural properties rather
    than computing from the mesh geometry (e.g., known vessel mass distribution).

    RAO_j = F_exc_j / sum_k[ (-omega^2*(M_jk + A_jk) + j*omega*(B_jk + B_ext_jk) + K_jk) ]
    (DNV-RP-C205 §7.2.5, Eq 7.2.1)

    Args:
        bem_result: BEM results with added_mass, radiation_damping, excitation_force.
        mass_matrix: 6x6 (or n_dof x n_dof) mass/inertia matrix [kg, kg.m, kg.m^2].
        stiffness_matrix: 6x6 hydrostatic restoring stiffness [N/m, N.m/rad].
        damping_extra: Optional additional damping (viscous, mooring) [N.s/m].

    Returns:
        RAOResult with complex RAO per frequency, heading, and DOF.
    """
    A = bem_result.added_mass  # (n_omega, n_dof, n_dof)
    B = bem_result.radiation_damping  # (n_omega, n_dof, n_dof)
    F_exc = bem_result.excitation_force  # (n_omega, n_heading, n_dof)
    omegas = bem_result.omegas

    if A is None or B is None or F_exc is None:
        raise ValueError("BEM result must contain added_mass, radiation_damping, and excitation_force")

    n_omega = len(omegas)
    n_heading = F_exc.shape[1]
    n_dof = A.shape[1]

    M = np.asarray(mass_matrix, dtype=float)
    K = np.asarray(stiffness_matrix, dtype=float)
    B_ext = np.zeros_like(M) if damping_extra is None else np.asarray(damping_extra, dtype=float)

    rao_complex = np.zeros((n_omega, n_heading, n_dof), dtype=complex)

    for i, omega in enumerate(omegas):
        # Impedance matrix: Z = -omega^2*(M+A) + j*omega*(B+B_ext) + K
        Z = (-omega**2 * (M + A[i]) + 1j * omega * (B[i] + B_ext) + K)

        for h in range(n_heading):
            # Solve Z * xi = F_exc for each heading
            try:
                rao_complex[i, h, :] = np.linalg.solve(Z, F_exc[i, h, :])
            except np.linalg.LinAlgError:
                logger.warning("Singular impedance at omega=%.3f, heading=%d — using pseudoinverse", omega, h)
                rao_complex[i, h, :] = np.linalg.lstsq(Z, F_exc[i, h, :], rcond=None)[0]

    return RAOResult(
        rao=rao_complex,
        rao_amplitude=np.abs(rao_complex),
        rao_phase=np.rad2deg(np.angle(rao_complex)),
        omegas=omegas,
        periods=bem_result.periods,
        headings=bem_result.headings,
        dof_names=bem_result.dof_names,
    )
