"""
ABOUTME: Mean drift and slowly-varying drift force calculations for OrcaWave.
Implements Newman's approximation for QTF, full QTF double-sum integration,
mean drift force coefficient handling, and wind/current drift estimation.
"""

from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class MeanDriftCoefficients(BaseModel):
    """Frequency-dependent mean drift force coefficients for one heading."""

    heading_deg: float = Field(..., description="Wave heading in degrees")
    frequencies_rad_s: List[float] = Field(..., description="Frequency vector")
    surge_kn_m2: List[float] = Field(
        ...,
        description="Mean drift force coefficient in surge (kN/m^2)",
    )
    sway_kn_m2: List[float] = Field(
        ...,
        description="Mean drift force coefficient in sway (kN/m^2)",
    )
    yaw_knm_m2: List[float] = Field(
        ...,
        description="Mean drift moment coefficient in yaw (kN·m/m^2)",
    )


class QTFMatrix(BaseModel):
    """Quadratic Transfer Function matrix for one DOF and heading."""

    dof: str = Field(..., description="Degree of freedom (surge, sway, yaw)")
    heading_deg: float
    frequencies_rad_s: List[float] = Field(
        ...,
        description="Frequency vector (same for both axes)",
    )
    real_part: List[List[float]] = Field(
        ...,
        description="Real part of QTF matrix [n_freq x n_freq]",
    )
    imag_part: List[List[float]] = Field(
        ...,
        description="Imaginary part of QTF matrix [n_freq x n_freq]",
    )


class DriftForceResult(BaseModel):
    """Mean and slowly-varying drift force results."""

    heading_deg: float
    mean_surge_kn: float = Field(..., description="Mean drift force in surge (kN)")
    mean_sway_kn: float = Field(..., description="Mean drift force in sway (kN)")
    mean_yaw_knm: float = Field(..., description="Mean drift moment in yaw (kN·m)")
    sv_surge_std_kn: float = Field(
        default=0.0,
        description="Standard deviation of slowly-varying surge force (kN)",
    )
    sv_sway_std_kn: float = Field(
        default=0.0,
        description="Standard deviation of slowly-varying sway force (kN)",
    )


class WindCurrentDrift(BaseModel):
    """Steady drift force from wind and current."""

    wind_speed_m_s: float
    current_speed_m_s: float
    heading_deg: float
    wind_force_surge_kn: float
    wind_force_sway_kn: float
    wind_moment_yaw_knm: float
    current_force_surge_kn: float
    current_force_sway_kn: float
    current_moment_yaw_knm: float
    total_surge_kn: float
    total_sway_kn: float
    total_yaw_knm: float


# ---------------------------------------------------------------------------
# Mean drift forces
# ---------------------------------------------------------------------------

def compute_mean_drift_force(
    drift_coeffs: MeanDriftCoefficients,
    wave_spectrum: np.ndarray,
    omega: np.ndarray,
) -> Tuple[float, float, float]:
    """Compute mean drift forces from coefficients and wave spectrum.

    F_drift = integral( 2 * D(omega) * S(omega) d_omega )

    where D(omega) is the mean drift coefficient and S(omega) the wave spectrum.

    Args:
        drift_coeffs: Frequency-dependent drift coefficients for one heading.
        wave_spectrum: Wave spectral density S(omega).
        omega: Frequency vector (rad/s).

    Returns:
        (surge_kN, sway_kN, yaw_kNm) mean drift forces.
    """
    freqs = np.array(drift_coeffs.frequencies_rad_s)
    d_surge = np.interp(omega, freqs, drift_coeffs.surge_kn_m2)
    d_sway = np.interp(omega, freqs, drift_coeffs.sway_kn_m2)
    d_yaw = np.interp(omega, freqs, drift_coeffs.yaw_knm_m2)

    f_surge = float(np.trapz(2.0 * d_surge * wave_spectrum, omega))
    f_sway = float(np.trapz(2.0 * d_sway * wave_spectrum, omega))
    m_yaw = float(np.trapz(2.0 * d_yaw * wave_spectrum, omega))

    return f_surge, f_sway, m_yaw


# ---------------------------------------------------------------------------
# Newman's approximation
# ---------------------------------------------------------------------------

def newman_approximation(
    drift_coeffs: MeanDriftCoefficients,
    wave_spectrum: np.ndarray,
    omega: np.ndarray,
    dof: str = "surge",
) -> np.ndarray:
    """Newman's approximation for the slowly-varying QTF.

    QTF_ij ~ 0.5 * (D(omega_i) + D(omega_j))

    where D(omega) is the diagonal (mean drift) coefficient.

    Args:
        drift_coeffs: Mean drift coefficients.
        wave_spectrum: Wave spectral density.
        omega: Frequency vector.
        dof: Degree of freedom ('surge', 'sway', or 'yaw').

    Returns:
        Slowly-varying force time series standard deviation spectrum.
    """
    freqs = np.array(drift_coeffs.frequencies_rad_s)

    if dof == "surge":
        d_vals = np.array(drift_coeffs.surge_kn_m2)
    elif dof == "sway":
        d_vals = np.array(drift_coeffs.sway_kn_m2)
    else:
        d_vals = np.array(drift_coeffs.yaw_knm_m2)

    d_interp = np.interp(omega, freqs, d_vals)

    n = len(omega)
    # Build Newman QTF matrix
    qtf_newman = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            qtf_newman[i, j] = 0.5 * (d_interp[i] + d_interp[j])

    # Slowly-varying force spectrum (difference-frequency)
    # S_sv(mu) = 8 * integral( QTF(omega, omega+mu)^2 * S(omega) * S(omega+mu) d_omega )
    d_omega = omega[1] - omega[0] if len(omega) > 1 else 1.0
    sv_spectrum = np.zeros(n)
    for k in range(n):
        total = 0.0
        for i in range(n - k):
            j = i + k
            total += qtf_newman[i, j] ** 2 * wave_spectrum[i] * wave_spectrum[j]
        sv_spectrum[k] = 8.0 * total * d_omega

    return sv_spectrum


def full_qtf_slowly_varying(
    qtf: QTFMatrix,
    wave_spectrum: np.ndarray,
    omega: np.ndarray,
) -> float:
    """Compute slowly-varying drift force std using full QTF integration.

    sigma_sv^2 = 8 * double_sum( |QTF(omega_i, omega_j)|^2
                                  * S(omega_i) * S(omega_j) * d_omega^2 )

    Args:
        qtf: Full QTF matrix.
        wave_spectrum: Wave spectral density.
        omega: Frequency vector.

    Returns:
        Standard deviation of slowly-varying drift force.
    """
    freqs = np.array(qtf.frequencies_rad_s)
    qr = np.array(qtf.real_part)
    qi = np.array(qtf.imag_part)
    qtf_mag2 = qr**2 + qi**2

    # Interpolate wave spectrum to QTF frequency grid
    s_interp = np.interp(freqs, omega, wave_spectrum)

    d_omega = freqs[1] - freqs[0] if len(freqs) > 1 else 1.0

    # Double sum
    n = len(freqs)
    variance = 0.0
    for i in range(n):
        for j in range(n):
            variance += qtf_mag2[i, j] * s_interp[i] * s_interp[j]
    variance *= 8.0 * d_omega**2

    return float(np.sqrt(max(variance, 0.0)))


# ---------------------------------------------------------------------------
# Wind and current drift
# ---------------------------------------------------------------------------

def compute_wind_current_drift(
    wind_speed_m_s: float,
    current_speed_m_s: float,
    heading_deg: float,
    projected_wind_area_m2: float = 3000.0,
    projected_current_area_m2: float = 1500.0,
    vessel_length_m: float = 250.0,
    cx_wind: float = 1.0,
    cy_wind: float = 0.8,
    cx_current: float = 0.08,
    cy_current: float = 1.2,
    rho_air: float = 1.225,
    rho_water: float = 1025.0,
) -> WindCurrentDrift:
    """Estimate steady drift forces from wind and current.

    Uses standard drag-type formulations:
        F = 0.5 * rho * C * A * V^2

    Args:
        wind_speed_m_s: Wind speed (m/s).
        current_speed_m_s: Current speed (m/s).
        heading_deg: Environmental heading (deg, 0=head, 90=beam).
        projected_wind_area_m2: Projected above-water area (m^2).
        projected_current_area_m2: Projected below-water area (m^2).
        vessel_length_m: Vessel length for yaw moment arm.
        cx_wind: Wind drag coefficient in surge direction.
        cy_wind: Wind drag coefficient in sway direction.
        cx_current: Current drag coefficient in surge direction.
        cy_current: Current drag coefficient in sway direction.
        rho_air: Air density (kg/m^3).
        rho_water: Water density (kg/m^3).

    Returns:
        WindCurrentDrift with all force components.
    """
    theta = np.deg2rad(heading_deg)
    cos_t = np.cos(theta)
    sin_t = np.sin(theta)

    # Wind forces
    q_wind = 0.5 * rho_air * wind_speed_m_s**2
    fw_surge = q_wind * cx_wind * projected_wind_area_m2 * abs(cos_t) * np.sign(cos_t)
    fw_sway = q_wind * cy_wind * projected_wind_area_m2 * abs(sin_t) * np.sign(sin_t)
    # Yaw moment: assume moment arm ~ 0.1 * vessel length
    mw_yaw = fw_sway * 0.1 * vessel_length_m

    # Current forces
    q_curr = 0.5 * rho_water * current_speed_m_s**2
    fc_surge = q_curr * cx_current * projected_current_area_m2 * abs(cos_t) * np.sign(cos_t)
    fc_sway = q_curr * cy_current * projected_current_area_m2 * abs(sin_t) * np.sign(sin_t)
    mc_yaw = fc_sway * 0.1 * vessel_length_m

    # Convert to kN
    fw_surge_kn = float(fw_surge / 1000.0)
    fw_sway_kn = float(fw_sway / 1000.0)
    mw_yaw_knm = float(mw_yaw / 1000.0)
    fc_surge_kn = float(fc_surge / 1000.0)
    fc_sway_kn = float(fc_sway / 1000.0)
    mc_yaw_knm = float(mc_yaw / 1000.0)

    return WindCurrentDrift(
        wind_speed_m_s=wind_speed_m_s,
        current_speed_m_s=current_speed_m_s,
        heading_deg=heading_deg,
        wind_force_surge_kn=fw_surge_kn,
        wind_force_sway_kn=fw_sway_kn,
        wind_moment_yaw_knm=mw_yaw_knm,
        current_force_surge_kn=fc_surge_kn,
        current_force_sway_kn=fc_sway_kn,
        current_moment_yaw_knm=mc_yaw_knm,
        total_surge_kn=fw_surge_kn + fc_surge_kn,
        total_sway_kn=fw_sway_kn + fc_sway_kn,
        total_yaw_knm=mw_yaw_knm + mc_yaw_knm,
    )
