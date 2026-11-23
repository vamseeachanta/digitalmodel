"""
ABOUTME: Mean drift and slowly-varying drift force calculations for OrcaWave.
Implements Newman's approximation for QTF, full QTF double-sum integration,
mean drift force coefficient handling, and wind/current drift estimation.
"""

from typing import List, Literal, Tuple

import numpy as np
from pydantic import BaseModel, Field

_trapezoid = getattr(np, "trapezoid", None) or np.trapz

# NumPy 2.4 reference, numpy.isclose: documented default relative tolerance.
# This is a numerical comparison policy, not an engineering acceptance limit.
# https://numpy.org/doc/2.4/reference/generated/numpy.isclose.html
_VALIDATION_RTOL = 1e-5


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------


class MeanDriftCoefficients(BaseModel):
    """Frequency-dependent mean drift force coefficients for one heading."""

    heading_deg: float = Field(
        ..., ge=0, lt=360, description="Direction from, clockwise from bow"
    )
    frame: Literal["x_forward_y_port_z_up"]
    heading_convention: Literal["from_clockwise_bow"]
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

    frame: str = "x_forward_y_port_z_up"
    heading_convention: str = "from_clockwise_bow"
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

    f_surge = float(_trapezoid(2.0 * d_surge * wave_spectrum, omega))
    f_sway = float(_trapezoid(2.0 * d_sway * wave_spectrum, omega))
    m_yaw = float(_trapezoid(2.0 * d_yaw * wave_spectrum, omega))

    return f_surge, f_sway, m_yaw


# ---------------------------------------------------------------------------
# Newman's approximation
# ---------------------------------------------------------------------------


def _validate_rtol(value: float, name: str) -> None:
    """Require a relative tolerance that cannot accept a missing partner."""
    # In |a-b| <= rtol*|b| with a=0, b!=0, rtol>=1 would accept missing data.
    if not np.isfinite(value) or not 0 <= value < 1:
        raise ValueError(f"{name} must be finite and in [0, 1)")


def _frequency_grid(values, name: str, *, uniform: bool, rtol: float) -> np.ndarray:
    """Validate coordinates before interpolation or discrete lag integration."""
    frequencies = np.asarray(values, dtype=float)
    if frequencies.ndim != 1 or frequencies.size == 0:
        raise ValueError(f"{name} must be a nonempty one-dimensional array")
    if not np.all(np.isfinite(frequencies)):
        raise ValueError(f"{name} must be finite")
    if np.any(frequencies <= 0):
        raise ValueError(f"{name} must be positive")
    spacing = np.diff(frequencies)
    if np.any(spacing <= 0):
        raise ValueError(f"{name} must be strictly increasing")
    # No absolute floor: frequency units must not change the comparison.
    if uniform and spacing.size and not np.allclose(
        spacing, spacing[0], rtol=rtol, atol=0,
    ):
        raise ValueError(f"{name} must have uniform spacing within grid_rtol={rtol}")
    return frequencies


def _wave_inputs(omega, wave_spectrum, grid_rtol):
    """Validate the common input domain of the two slow-drift methods."""
    _validate_rtol(grid_rtol, "grid_rtol")
    omega = _frequency_grid(omega, "omega", uniform=True, rtol=grid_rtol)
    spectrum = np.asarray(wave_spectrum, dtype=float)
    if spectrum.shape != omega.shape:
        raise ValueError("wave_spectrum must have the same one-dimensional shape as omega")
    if not np.all(np.isfinite(spectrum)):
        raise ValueError("wave_spectrum must be finite")
    if np.any(spectrum < 0):
        raise ValueError("wave_spectrum must be nonnegative")
    return omega, spectrum


def _complete_hermitian_qtf(qtf: QTFMatrix, n: int, rtol: float):
    """Reject incomplete, nonfinite or non-Hermitian difference-frequency data."""
    try:
        real = np.asarray(qtf.real_part, dtype=float)
        imag = np.asarray(qtf.imag_part, dtype=float)
    except ValueError as exc:
        raise ValueError("QTF real_part and imag_part must be complete square matrices") from exc
    if real.shape != (n, n) or imag.shape != (n, n):
        raise ValueError("QTF real_part and imag_part must be complete square matrices")
    if not np.all(np.isfinite(real)) or not np.all(np.isfinite(imag)):
        raise ValueError("QTF real_part and imag_part must be finite")
    # Orcina WebHelp, "Vessel types: Wave drift and sum frequency QTFs",
    # "Full QTFs", symmetry equation (accessed 2026-10-23): Q_ij=conj(Q_ji).
    # https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseltypes,WavedriftandsumfrequencyQTFs.htm
    if np.any(np.diag(imag) != 0):
        raise ValueError("Difference-frequency QTF must have a real diagonal")
    # Separate real and imaginary checks prevent a large real term from hiding
    # a missing imaginary partner. Zero absolute tolerance rejects either lone
    # triangle even for small coefficients. No entries are filled or averaged.
    if not (
        np.allclose(real, real.T, rtol=rtol, atol=0)
        and np.allclose(imag, -imag.T, rtol=rtol, atol=0)
    ):
        raise ValueError(
            "QTF must be complete and Hermitian within qtf_rtol; "
            "supply both conjugate triangles explicitly"
        )
    return real, imag


def newman_approximation(
    drift_coeffs: MeanDriftCoefficients,
    wave_spectrum: np.ndarray,
    omega: np.ndarray,
    dof: str = "surge",
    *,
    grid_rtol: float = _VALIDATION_RTOL,
) -> np.ndarray:
    """Newman's approximation for the slowly-varying QTF.

    QTF_ij ~ 0.5 * (D(omega_i) + D(omega_j))

    where D(omega) is the diagonal (mean drift) coefficient.

    Args:
        drift_coeffs: Mean drift coefficients.
        wave_spectrum: Wave spectral density.
        omega: Frequency vector.
        dof: Degree of freedom ('surge', 'sway', or 'yaw').
        grid_rtol: Relative spacing tolerance; finite and in [0, 1).
            Default is NumPy's numerical isclose relative tolerance, with no
            absolute floor. Does not specify an engineering accuracy criterion.

    Returns:
        Discrete one-sided slowly-varying force PSD at mu[k] = k * d_omega.
        For a uniform increasing omega grid, variance is sum(result) * d_omega
        (full bin weights, including the last bin). The zero-lag bin is zero:
        equal-frequency pairs give constant mean load, not fluctuation variance.
        This excludes the mean-load bin previously included in this function.

    Raises:
        ValueError: Nonpositive, nonfinite, nonuniform or unordered omega;
            mismatched, negative or nonfinite spectrum; invalid coefficients,
            dof or tolerance. A singleton grid returns zero (no beat pairs).
    """
    omega, wave_spectrum = _wave_inputs(omega, wave_spectrum, grid_rtol)
    freqs = _frequency_grid(
        drift_coeffs.frequencies_rad_s, "Drift coefficient frequencies",
        uniform=False, rtol=grid_rtol,
    )
    if dof == "surge":
        d_vals = np.array(drift_coeffs.surge_kn_m2)
    elif dof == "sway":
        d_vals = np.array(drift_coeffs.sway_kn_m2)
    elif dof == "yaw":
        d_vals = np.array(drift_coeffs.yaw_knm_m2)
    else:
        raise ValueError("dof must be 'surge', 'sway', or 'yaw'")
    if d_vals.shape != freqs.shape:
        raise ValueError("Drift coefficients must have the same shape as their frequencies")
    if not np.all(np.isfinite(d_vals)):
        raise ValueError("Drift coefficients must be finite")

    d_interp = np.interp(omega, freqs, d_vals)
    n = len(omega)
    if n == 1:
        return np.zeros(n)
    # Build Newman QTF matrix
    qtf_newman = 0.5 * (d_interp[:, None] + d_interp[None, :])
    # Slowly-varying force spectrum (difference-frequency)
    # S_sv(mu) = 8 * integral( QTF(omega, omega+mu)^2 * S(omega) * S(omega+mu) d_omega )
    d_omega = omega[1] - omega[0]
    sv_spectrum = np.zeros(n)
    # k=0 contains only the constant diagonal loads for fixed wave amplitudes.
    for k in range(1, n):
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
    *,
    grid_rtol: float = _VALIDATION_RTOL,
    qtf_rtol: float = _VALIDATION_RTOL,
) -> float:
    """Compute slowly-varying drift force std using full QTF integration.

    sigma_sv^2 = 8 * sum_{i<j}( |QTF(omega_i, omega_j)|^2
                               * S(omega_i) * S(omega_j) * d_omega^2 )

    Uses a complete Hermitian difference-frequency QTF on a uniform increasing
    frequency grid and fixed wave-component amplitudes sqrt(2*S*d_omega).
    Diagonal terms give the mean drift load and are excluded from this std.
    Compared to the former all-pairs sum, variance is approximately halved on
    fine grids; a diagonal-only QTF now gives zero slowly-varying force.

    Args:
        qtf: Complete Hermitian QTF matrix (both triangles, same frequency grid).
        wave_spectrum: Wave spectral density.
        omega: Frequency vector.
        grid_rtol: Relative spacing tolerance for both frequency grids.
        qtf_rtol: Relative tolerance for real symmetry and imaginary
            antisymmetry separately. The imaginary diagonal must be zero.
            Both tolerances use NumPy's numerical isclose relative default,
            with no absolute floor, and must be finite and in [0, 1).

    Returns:
        Standard deviation of slowly-varying drift force.

    Raises:
        ValueError: Invalid grids or spectrum (as in newman_approximation),
            incomplete, nonfinite or non-Hermitian QTF, invalid tolerances,
            or a spectrum grid that does not cover the QTF frequencies.
            No triangle is inferred or symmetrised. A singleton returns zero.
    """
    omega, wave_spectrum = _wave_inputs(omega, wave_spectrum, grid_rtol)
    _validate_rtol(qtf_rtol, "qtf_rtol")
    freqs = _frequency_grid(
        qtf.frequencies_rad_s, "QTF frequencies", uniform=True, rtol=grid_rtol,
    )
    qr, qi = _complete_hermitian_qtf(qtf, len(freqs), qtf_rtol)
    if freqs[0] < omega[0] or freqs[-1] > omega[-1]:
        raise ValueError("Spectrum frequency range must cover all QTF frequencies")
    if len(freqs) == 1:
        return 0.0
    qtf_mag2 = qr**2 + qi**2

    # Interpolate wave spectrum to QTF frequency grid
    s_interp = np.interp(freqs, omega, wave_spectrum)

    d_omega = freqs[1] - freqs[0]

    # With a_i = sqrt(2*S_i*d_omega), Hermitian ordered terms (i,j) and (j,i)
    # combine into ONE real sinusoid of amplitude 2*a_i*a_j*|Q_ij|. Its phase
    # variance is amplitude^2/2 = 8*S_i*S_j*d_omega^2*|Q_ij|^2. Distinct
    # unordered pairs have zero phase-ensemble covariance, so visit i<j once.
    # i=j is constant in time and belongs to mean load, not this variance.
    # Convention: Orcina, "Vessel theory: Wave drift and sum frequency loads",
    # "Second order load theory" (F = Re sum_ij Q_ij a_i a_j exp(i*delta_w*t)).
    n = len(freqs)
    variance = 0.0
    for i in range(n):
        for j in range(i + 1, n):
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
        heading_deg: Direction FROM, clockwise from bow in [0,360).
            Frame: +x forward, +y port, +z up; 0=head, 90=from starboard.
            Positive yaw is bow to port. Lever arm is assumed +0.1*L in x.
            Imported diffraction coefficients must be transformed to this frame.
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
    if not np.isfinite(heading_deg) or not 0 <= heading_deg < 360:
        raise ValueError("Heading must be finite in [0, 360)")
    theta = np.deg2rad(heading_deg)
    cos_t = np.cos(theta)
    sin_t = np.sin(theta)

    # Wind forces
    q_wind = 0.5 * rho_air * wind_speed_m_s**2
    fw_surge = -q_wind * cx_wind * projected_wind_area_m2 * abs(cos_t) * np.sign(cos_t)
    fw_sway = q_wind * cy_wind * projected_wind_area_m2 * abs(sin_t) * np.sign(sin_t)
    # Yaw moment: assume moment arm ~ 0.1 * vessel length
    mw_yaw = fw_sway * 0.1 * vessel_length_m

    # Current forces
    q_curr = 0.5 * rho_water * current_speed_m_s**2
    fc_surge = (
        -q_curr * cx_current * projected_current_area_m2 * abs(cos_t) * np.sign(cos_t)
    )
    fc_sway = (
        q_curr * cy_current * projected_current_area_m2 * abs(sin_t) * np.sign(sin_t)
    )
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
