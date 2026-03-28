"""Crane tip motion transfer function.

Computes motion at an arbitrary point on a vessel (e.g. crane tip) from
vessel RAOs at the centre of gravity using a geometric transfer function.

For small-angle rotations, the crane tip displacement is:

    x_tip = x_cog + R x r

where R is the rotation matrix and r is the position vector from CoG to
crane tip. In the frequency domain this becomes:

    RAO_tip_heave(w) = RAO_heave(w) - y*RAO_roll(w) + x*RAO_pitch(w)
    RAO_tip_surge(w) = RAO_surge(w) + z*RAO_pitch(w) - y*RAO_yaw(w)
    RAO_tip_sway(w)  = RAO_sway(w)  - z*RAO_roll(w)  + x*RAO_yaw(w)

where (x, y, z) is the crane tip position relative to vessel CoG and
rotational RAOs are in radians/m.

Reference: DNV-RP-C205 §7.2, standard motion transfer approach.
"""
from __future__ import annotations

import numpy as np
from .models import Vessel, CraneTipConfig


def _complex_rao(amplitude: np.ndarray, phase_deg: np.ndarray) -> np.ndarray:
    """Convert amplitude + phase (deg) to complex RAO."""
    return amplitude * np.exp(1j * np.deg2rad(phase_deg))


def _decompose_complex(z: np.ndarray) -> tuple[np.ndarray, np.ndarray]:
    """Convert complex RAO back to amplitude + phase (deg)."""
    return np.abs(z), np.rad2deg(np.angle(z))


def _rotational_to_rad(amplitude: np.ndarray, unit: str = "deg/m") -> np.ndarray:
    """Convert rotational RAO amplitude from deg/m to rad/m if needed."""
    if unit == "deg/m":
        return np.deg2rad(amplitude)
    return amplitude


def crane_tip_raos(
    vessel: Vessel,
    crane_tip: CraneTipConfig | None = None,
) -> dict[str, dict[str, np.ndarray]]:
    """Compute RAOs at crane tip from vessel CoG RAOs.

    Parameters
    ----------
    vessel : Vessel
        Vessel with complete 6-DOF RAOs.
    crane_tip : CraneTipConfig, optional
        Override crane tip position. Defaults to vessel.crane_tip.

    Returns
    -------
    dict
        Crane tip RAOs with keys 'surge', 'sway', 'heave'.
        Each value: {'amplitude': (n_freq, n_hdg), 'phase': (n_freq, n_hdg)}.

    Raises
    ------
    ValueError
        If vessel does not have complete 6-DOF RAOs.
    """
    if not vessel.has_complete_raos():
        raise ValueError(f"Vessel '{vessel.name}' missing required DOFs")

    tip = crane_tip or vessel.crane_tip
    x, y, z = tip.x_m, tip.y_m, tip.z_m
    rao = vessel.rao_data

    # Build complex RAOs for translational DOFs
    surge_c = _complex_rao(rao["surge"]["amplitude"], rao["surge"]["phase"])
    sway_c = _complex_rao(rao["sway"]["amplitude"], rao["sway"]["phase"])
    heave_c = _complex_rao(rao["heave"]["amplitude"], rao["heave"]["phase"])

    # Build complex RAOs for rotational DOFs, converting deg/m -> rad/m
    roll_amp_rad = _rotational_to_rad(rao["roll"]["amplitude"])
    pitch_amp_rad = _rotational_to_rad(rao["pitch"]["amplitude"])
    yaw_amp_rad = _rotational_to_rad(rao["yaw"]["amplitude"])

    roll_c = roll_amp_rad * np.exp(1j * np.deg2rad(rao["roll"]["phase"]))
    pitch_c = pitch_amp_rad * np.exp(1j * np.deg2rad(rao["pitch"]["phase"]))
    yaw_c = yaw_amp_rad * np.exp(1j * np.deg2rad(rao["yaw"]["phase"]))

    # Geometric transfer: CoG -> crane tip
    # heave_tip = heave - y*roll + x*pitch
    # surge_tip = surge + z*pitch - y*yaw
    # sway_tip  = sway  - z*roll  + x*yaw
    heave_tip_c = heave_c - y * roll_c + x * pitch_c
    surge_tip_c = surge_c + z * pitch_c - y * yaw_c
    sway_tip_c = sway_c - z * roll_c + x * yaw_c

    result = {}
    for name, complex_rao in [
        ("surge", surge_tip_c),
        ("sway", sway_tip_c),
        ("heave", heave_tip_c),
    ]:
        amp, phase = _decompose_complex(complex_rao)
        result[name] = {"amplitude": amp, "phase": phase}

    return result


def crane_tip_significant_motion(
    tip_rao_amplitude: np.ndarray,
    hs_m: float,
    tp_s: float,
    frequencies: np.ndarray,
    spectrum_type: str = "jonswap",
    gamma: float = 3.3,
) -> float:
    """Significant single-amplitude crane tip motion from spectral analysis.

    Integrates tip RAO^2 * S(w) to get m0, then returns 2*sqrt(m0) as the
    significant double-amplitude motion.

    Parameters
    ----------
    tip_rao_amplitude : np.ndarray
        Crane tip RAO amplitude [m/m] at each frequency, shape (n_freq,).
    hs_m : float
        Significant wave height [m].
    tp_s : float
        Peak spectral period [s].
    frequencies : np.ndarray
        Frequencies [rad/s], shape (n_freq,).
    spectrum_type : str
        Wave spectrum: 'jonswap' (default) or 'pm' (Pierson-Moskowitz).
    gamma : float
        JONSWAP peak enhancement factor (default 3.3).

    Returns
    -------
    float
        Significant single-amplitude motion [m].
    """
    wp = 2 * np.pi / tp_s  # Peak frequency rad/s

    # JONSWAP / PM spectrum
    S = _jonswap_spectrum(frequencies, hs_m, wp, gamma if spectrum_type == "jonswap" else 1.0)

    # Response spectrum
    m0 = np.trapz(tip_rao_amplitude**2 * S, frequencies)
    return 2.0 * np.sqrt(m0)


def crane_tip_significant_velocity(
    tip_rao_amplitude: np.ndarray,
    hs_m: float,
    tp_s: float,
    frequencies: np.ndarray,
    spectrum_type: str = "jonswap",
    gamma: float = 3.3,
) -> float:
    """Significant single-amplitude crane tip velocity [m/s].

    v_sig = 2 * sqrt(m2) where m2 = integral of w^2 * RAO^2 * S(w) dw.
    """
    wp = 2 * np.pi / tp_s
    S = _jonswap_spectrum(frequencies, hs_m, wp, gamma if spectrum_type == "jonswap" else 1.0)
    m2 = np.trapz(frequencies**2 * tip_rao_amplitude**2 * S, frequencies)
    return 2.0 * np.sqrt(m2)


def _jonswap_spectrum(
    omega: np.ndarray,
    hs: float,
    wp: float,
    gamma: float = 3.3,
) -> np.ndarray:
    """JONSWAP wave spectrum S(w) [m^2 s/rad].

    S(w) = (alpha * g^2 / w^5) * exp(-5/4 * (wp/w)^4) * gamma^a

    Parametric form with alpha derived from Hs.
    """
    g = 9.80665

    # Spectral width parameter
    sigma = np.where(omega <= wp, 0.07, 0.09)

    # Peak enhancement
    a = np.exp(-0.5 * ((omega - wp) / (sigma * wp)) ** 2)

    # Pierson-Moskowitz base
    alpha_pm = (5.0 / 16.0) * hs**2 * wp**4 / g**2
    # Normalisation factor for gamma
    C_gamma = 1.0 - 0.287 * np.log(gamma)

    with np.errstate(divide="ignore", invalid="ignore"):
        S_pm = (alpha_pm * g**2 / omega**5) * np.exp(-1.25 * (wp / omega) ** 4)
        S = (S_pm / C_gamma) * gamma**a

    S = np.nan_to_num(S, nan=0.0, posinf=0.0, neginf=0.0)
    return S
