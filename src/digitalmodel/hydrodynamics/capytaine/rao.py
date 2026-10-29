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

    Uses the impedance method: RAO = F_exc / Z where, in Capytaine's
    exp(-j*omega*t) convention, Z = -omega^2 * (M + A) - j*omega*B + K
    (DNV-RP-C205 §7.2.5). The operator is Capytaine's own; this function
    delegates to it.

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

    The coupled system is solved, not divided row by row:

        sum_k Z_jk RAO_k = F_exc_j,   i.e.  RAO = solve(Z, F_exc)
        Z = -omega^2*(M + A) - j*omega*(B + B_ext) + K

    (DNV-RP-C205 §7.2.5, Eq 7.2.1, written in Capytaine's exp(-j*omega*t)
    convention, which its excitation force is expressed in -- hence -j*omega*B.)

    All supplied numeric arrays must contain finite values and have the exact
    shapes below. Frequency, heading, and DOF axes must be nonempty. Complex
    excitation is supported; angular frequencies retain their supplied signs.

    Args:
        bem_result: BEM results with omegas shaped (n_omega,), added_mass and
            radiation_damping shaped (n_omega, n_dof, n_dof), and complex
            excitation_force shaped (n_omega, n_heading, n_dof).
        mass_matrix: Mass/inertia shaped (n_dof, n_dof) [kg, kg.m, kg.m^2].
        stiffness_matrix: Restoring stiffness shaped (n_dof, n_dof) [N/m, N.m/rad].
        damping_extra: Optional additional damping shaped (n_dof, n_dof)
            [N.s/m]. Defaults to zero additional damping.

    Returns:
        RAOResult with complex RAO per frequency, heading, and DOF.

    Raises:
        ValueError: If a required numeric array is missing, has incompatible
            dimensions, or contains nonfinite values. The message identifies
            the input field; validation completes before any linear solve.
    """
    def finite_array(name, value):
        try:
            array = np.asarray(value)
            finite = np.isfinite(array).all()
        except (TypeError, ValueError):
            raise ValueError(f"{name} must be a finite numeric array") from None
        if not finite:
            raise ValueError(f"{name} must contain only finite values")
        return array

    omegas = finite_array("omegas", bem_result.omegas)
    if omegas.ndim != 1 or omegas.size == 0:
        raise ValueError("omegas must have shape (n_omega,) with n_omega > 0")
    n_omega = omegas.size

    A = finite_array("added_mass", bem_result.added_mass)
    if (A.ndim != 3 or A.shape[0] != n_omega or A.shape[1] == 0
            or A.shape[1] != A.shape[2]):
        raise ValueError("added_mass must have shape (n_omega, n_dof, n_dof) with n_dof > 0")
    n_dof = A.shape[1]

    B = finite_array("radiation_damping", bem_result.radiation_damping)
    if B.shape != A.shape:
        raise ValueError("radiation_damping must have shape (n_omega, n_dof, n_dof)")

    F_exc = finite_array("excitation_force", bem_result.excitation_force)
    if (F_exc.ndim != 3 or F_exc.shape[0] != n_omega or F_exc.shape[1] == 0
            or F_exc.shape[2] != n_dof):
        raise ValueError("excitation_force must have shape (n_omega, n_heading, n_dof) with n_heading > 0")
    n_heading = F_exc.shape[1]

    structural = {}
    for name, value in (("mass_matrix", mass_matrix),
                        ("stiffness_matrix", stiffness_matrix),
                        ("damping_extra", damping_extra)):
        if name == "damping_extra" and value is None:
            continue
        array = finite_array(name, value)
        if array.shape != (n_dof, n_dof):
            raise ValueError(f"{name} must have shape (n_dof, n_dof)")
        structural[name] = np.asarray(array, dtype=float)

    M = structural["mass_matrix"]
    K = structural["stiffness_matrix"]
    B_ext = structural.get("damping_extra", np.zeros_like(M))

    rao_complex = np.zeros((n_omega, n_heading, n_dof), dtype=complex)

    for i, omega in enumerate(omegas):
        # Impedance matrix in exp(-j*omega*t): Z = -omega^2*(M+A) - j*omega*(B+B_ext) + K.
        # +j*omega*B is the other time convention; in coupled modes it changes
        # the response magnitudes, not only the phase (#2147).
        Z = (-omega**2 * (M + A[i]) - 1j * omega * (B[i] + B_ext) + K)

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
