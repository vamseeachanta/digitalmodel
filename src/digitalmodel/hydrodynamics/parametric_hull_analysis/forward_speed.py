"""
ABOUTME: Forward speed corrections for hydrodynamic coefficients.

Implements DNV-RP-C205 §7.4 encounter frequency transformation and
strip-theory speed corrections for added mass and damping.  These are
analytical corrections applied post-solve — no Capytaine wrapper
modification required.

References
----------
DNV-RP-C205 (2021), §7.4 Forward Speed Effects.
Salvesen, Tuck & Faltinsen (1970), strip theory speed corrections.
"""

from __future__ import annotations

import logging
from typing import Optional

import numpy as np

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# Wave number (dispersion relation)
# ---------------------------------------------------------------------------


def wave_number(
    omega: np.ndarray,
    water_depth: float = np.inf,
    g: float = 9.81,
    max_iter: int = 50,
    tol: float = 1e-10,
) -> np.ndarray:
    """Solve the linear dispersion relation for wave number k.

    Deep water::

        k = omega^2 / g

    Finite depth (Newton iteration)::

        omega^2 = g * k * tanh(k * h)

    Parameters
    ----------
    omega : ndarray
        Angular frequencies [rad/s].  Shape (n_omega,).
    water_depth : float
        Water depth [m].  Use ``np.inf`` for deep water.
    g : float
        Gravitational acceleration [m/s²].
    max_iter : int
        Maximum Newton iterations for finite depth.
    tol : float
        Convergence tolerance for Newton iteration.

    Returns
    -------
    ndarray
        Wave numbers [1/m].  Same shape as *omega*.
    """
    omega = np.atleast_1d(np.asarray(omega, dtype=float))
    k_deep = omega**2 / g

    if not np.isfinite(water_depth):
        return k_deep

    # Finite depth: Newton iteration on f(k) = omega^2 - g*k*tanh(k*h) = 0
    h = water_depth
    k = k_deep.copy()  # Initial guess: deep water

    for _ in range(max_iter):
        tanh_kh = np.tanh(k * h)
        f = omega**2 - g * k * tanh_kh
        # df/dk = -g * (tanh(kh) + k*h*(1 - tanh^2(kh)))
        df = -g * (tanh_kh + k * h * (1.0 - tanh_kh**2))
        # Protect against zero derivative
        mask = np.abs(df) > 1e-30
        delta = np.zeros_like(k)
        delta[mask] = f[mask] / df[mask]
        k -= delta
        k = np.maximum(k, 1e-12)  # Ensure positive

        if np.max(np.abs(delta)) < tol:
            break

    return k


# ---------------------------------------------------------------------------
# Encounter frequency
# ---------------------------------------------------------------------------


def encounter_frequency(
    omega: np.ndarray,
    forward_speed: float,
    heading_rad: np.ndarray,
    water_depth: float = np.inf,
    g: float = 9.81,
) -> np.ndarray:
    """Compute encounter frequency array.

    DNV-RP-C205 §7.4.1::

        omega_e = omega - k * U * cos(beta)

    where k is the wave number from the dispersion relation and beta
    is the wave heading relative to the vessel (0 = head seas, following
    the DNV convention).

    Parameters
    ----------
    omega : ndarray
        Wave angular frequencies [rad/s].  Shape (n_omega,).
    forward_speed : float
        Forward speed [m/s].
    heading_rad : ndarray
        Wave headings [rad].  Shape (n_heading,).
    water_depth : float
        Water depth [m].  Use ``np.inf`` for deep water.
    g : float
        Gravitational acceleration [m/s²].

    Returns
    -------
    ndarray
        Encounter frequencies [rad/s].  Shape (n_omega, n_heading).

    Notes
    -----
    Head seas (beta=0): omega_e > omega (frequency increases)
    Following seas (beta=pi): omega_e < omega (frequency decreases)
    """
    omega = np.atleast_1d(np.asarray(omega, dtype=float))
    heading_rad = np.atleast_1d(np.asarray(heading_rad, dtype=float))

    k = wave_number(omega, water_depth, g)

    # omega_e[i, j] = omega[i] - k[i] * U * cos(heading[j])
    omega_e = omega[:, np.newaxis] - (
        k[:, np.newaxis] * forward_speed * np.cos(heading_rad[np.newaxis, :])
    )

    return omega_e


# ---------------------------------------------------------------------------
# RAO speed correction
# ---------------------------------------------------------------------------


def correct_rao_for_speed(
    rao_result: object,
    forward_speed: float,
    water_depth: float = np.inf,
    g: float = 9.81,
    n_encounter_freq: Optional[int] = None,
) -> object:
    """Re-map RAO from wave frequency to encounter frequency domain.

    For each heading, computes the encounter frequency mapping and
    interpolates RAO amplitudes onto a regular encounter frequency grid.

    Parameters
    ----------
    rao_result : RAOResult
        Capytaine RAO result (wave-frequency domain).
    forward_speed : float
        Forward speed [m/s].
    water_depth : float
        Water depth [m].
    g : float
        Gravitational acceleration [m/s²].
    n_encounter_freq : int, optional
        Number of encounter frequency points.  Defaults to same as
        input frequency count.

    Returns
    -------
    RAOResult
        New RAOResult with encounter-frequency coordinates.
        The ``omegas`` field contains encounter frequencies.

    Notes
    -----
    Regions where the encounter frequency mapping is non-monotonic
    (following seas near critical speed) are handled by retaining only
    the portion where omega_e is increasing.
    """
    from digitalmodel.hydrodynamics.capytaine.models import RAOResult

    omegas = rao_result.omegas
    headings = rao_result.headings
    amp = rao_result.rao_amplitude   # (n_omega, n_heading, n_dof)
    phase = rao_result.rao_phase     # (n_omega, n_heading, n_dof)

    if omegas is None or amp is None:
        logger.warning("RAO result has no data, returning unchanged")
        return rao_result

    n_omega = len(omegas)
    n_heading = len(headings) if headings is not None else 1
    n_dof = amp.shape[-1]
    n_out = n_encounter_freq or n_omega

    # Compute encounter frequency grid
    omega_e = encounter_frequency(omegas, forward_speed, headings, water_depth, g)

    # Build regular encounter frequency grid
    omega_e_min = float(np.nanmin(omega_e[omega_e > 0])) if np.any(omega_e > 0) else 0.01
    omega_e_max = float(np.nanmax(omega_e))
    omega_e_grid = np.linspace(omega_e_min, omega_e_max, n_out)

    # Interpolate RAO for each heading and DOF
    new_amp = np.zeros((n_out, n_heading, n_dof))
    new_phase = np.zeros((n_out, n_heading, n_dof))

    for j in range(n_heading):
        oe_col = omega_e[:, j] if omega_e.ndim > 1 else omega_e[:, 0]

        # Find monotonically increasing region
        mono_mask = np.ones(n_omega, dtype=bool)
        for i in range(1, n_omega):
            if oe_col[i] <= oe_col[i - 1]:
                mono_mask[i:] = False
                break

        oe_mono = oe_col[mono_mask]
        if len(oe_mono) < 2:
            continue

        for d in range(n_dof):
            amp_mono = amp[mono_mask, j, d]
            phase_mono = phase[mono_mask, j, d] if phase is not None else np.zeros_like(amp_mono)

            new_amp[:, j, d] = np.interp(
                omega_e_grid, oe_mono, amp_mono,
                left=0.0, right=0.0,
            )
            new_phase[:, j, d] = np.interp(
                omega_e_grid, oe_mono, phase_mono,
                left=0.0, right=0.0,
            )

    # Build new RAOResult
    periods_e = np.where(omega_e_grid > 0, 2 * np.pi / omega_e_grid, np.inf)

    return RAOResult(
        rao=None,  # Complex RAO not trivially remapped
        rao_amplitude=new_amp,
        rao_phase=new_phase,
        omegas=omega_e_grid,
        periods=periods_e,
        headings=headings,
        dof_names=rao_result.dof_names,
        dataset=None,
    )


# ---------------------------------------------------------------------------
# Strip theory speed corrections for A and B
# ---------------------------------------------------------------------------


def strip_theory_speed_correction(
    added_mass: np.ndarray,
    damping: np.ndarray,
    omegas: np.ndarray,
    forward_speed: float,
    length_bp: float,
) -> tuple[np.ndarray, np.ndarray]:
    """Apply first-order strip-theory speed-dependent corrections.

    Simplified Salvesen-Tuck-Faltinsen corrections for added mass
    and radiation damping at forward speed.

    The primary effect is a coupling between heave-pitch (indices 2,4)
    and surge-pitch (indices 0,4) that grows linearly with speed::

        A_35(U) = A_35(0) - U * A_33(0) / omega
        B_35(U) = B_35(0) - U * B_33(0) / omega

    Parameters
    ----------
    added_mass : ndarray
        Zero-speed added mass, shape (n_omega, 6, 6).
    damping : ndarray
        Zero-speed radiation damping, shape (n_omega, 6, 6).
    omegas : ndarray
        Angular frequencies [rad/s], shape (n_omega,).
    forward_speed : float
        Forward speed [m/s].
    length_bp : float
        Length between perpendiculars [m] (for normalisation).

    Returns
    -------
    (corrected_added_mass, corrected_damping)
        Both shape (n_omega, 6, 6).

    References
    ----------
    Salvesen, Tuck & Faltinsen (1970), Ship Motions and Sea Loads.
    DNV-RP-C205 (2021) §7.4.
    """
    A = added_mass.copy()
    B = damping.copy()

    if abs(forward_speed) < 1e-6:
        return A, B

    U = forward_speed
    n_omega = len(omegas)

    for i in range(n_omega):
        w = omegas[i]
        if abs(w) < 1e-10:
            continue

        # Heave-pitch coupling correction (indices 2=heave, 4=pitch)
        A[i, 2, 4] -= U * A[i, 2, 2] / w
        A[i, 4, 2] -= U * A[i, 2, 2] / w
        B[i, 2, 4] -= U * B[i, 2, 2] / w
        B[i, 4, 2] -= U * B[i, 2, 2] / w

        # Surge-pitch coupling correction (indices 0=surge, 4=pitch)
        A[i, 0, 4] -= U * A[i, 0, 0] / w
        B[i, 0, 4] -= U * B[i, 0, 0] / w

        # Sway-yaw coupling correction (indices 1=sway, 5=yaw)
        A[i, 1, 5] -= U * A[i, 1, 1] / w
        B[i, 1, 5] -= U * B[i, 1, 1] / w

    return A, B


__all__ = [
    "encounter_frequency",
    "wave_number",
    "correct_rao_for_speed",
    "strip_theory_speed_correction",
]
