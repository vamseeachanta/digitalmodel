"""Offline scalar slow-drift response; force units are kN, length units m.

References: Lande-Sudall and Stansby (2026), Applied Ocean Research 176,
105292, doi:10.1016/j.apor.2026.105292, Eqs. 14, 20–21; Naess (1985),
Journal of Ship Research 29(4), 270–284, doi:10.5957/jsr.1985.29.4.270.
"""

from dataclasses import dataclass

import numpy as np


@dataclass(frozen=True)
class SurgeResponse:
    """Discrete one-sided response PSD per rad/s and separate static offset."""

    frequencies_rad_s: np.ndarray
    spectrum_m2_s_rad: np.ndarray
    std_m: float
    mean_offset_m: float


@dataclass(frozen=True)
class SurgeKernel:
    """Centered Kac–Siegert kernel in m; two real quadratures per eigenvalue.

    X = mean_offset_m + sum(lambda_j*(U_j²+V_j²)), with independent
    standard normal quadratures, represents the off-diagonal response.
    Thus dynamic mean=2*sum(lambda)=0 and variance=4*sum(lambda²).
    Fixed-amplitude diagonal loads are retained only in mean_offset_m.
    """

    matrix_m: np.ndarray
    eigenvalues_m: np.ndarray
    mean_offset_m: float

    @property
    def variance_m2(self):
        return float(4 * np.sum(self.eigenvalues_m**2))


def _real_array(values, name):
    if np.iscomplexobj(values):
        raise ValueError(f"{name} must be real")
    return np.asarray(values, dtype=float)


def _uniform_grid(values, name, *, starts_at_zero=False):
    grid = _real_array(values, name)
    if grid.ndim != 1 or grid.size < 2 or not np.all(np.isfinite(grid)):
        raise ValueError(f"{name} must be a finite vector with at least two bins")
    steps = np.diff(grid)
    if grid[0] < 0 or steps[0] <= 0 or not np.allclose(
        steps, steps[0], rtol=1e-8, atol=1e-12 * steps[0],
    ):
        raise ValueError(f"{name} must be nonnegative, uniform and increasing")
    if starts_at_zero and grid[0] != 0:
        raise ValueError(f"{name} must start at zero")
    return grid, float(steps[0])


def _spectrum(values, grid):
    spectrum = _real_array(values, "spectrum")
    if (spectrum.shape != grid.shape or not np.all(np.isfinite(spectrum))
            or np.any(spectrum < 0)):
        raise ValueError("spectrum must match the grid and be finite and nonnegative")
    return spectrum


def _oscillator(mass_kg, stiffness_kn_m, damping_kn_s_m, damping_ratio):
    if (not np.isfinite(mass_kg) or mass_kg <= 0
            or not np.isfinite(stiffness_kn_m) or stiffness_kn_m <= 0):
        raise ValueError("mass_kg and stiffness_kn_m must be finite and positive")
    if (damping_kn_s_m is None) == (damping_ratio is None):
        raise ValueError("supply exactly one of damping_kn_s_m and damping_ratio")
    mass = mass_kg / 1000.0  # tonne = kN s²/m
    damping = damping_kn_s_m
    if damping_ratio is not None:
        damping = 2 * damping_ratio * np.sqrt(mass * stiffness_kn_m)
    if not np.isfinite(damping) or damping <= 0:
        raise ValueError("linear damping must be finite and positive")
    return mass, float(damping)


def _qtf_inputs(qtf, wave_spectrum, omega):
    grid, width = _uniform_grid(omega, "omega")
    spectrum = _spectrum(wave_spectrum, grid)
    qgrid = np.asarray(qtf.frequencies_rad_s, dtype=float)
    if qgrid.shape != grid.shape or not np.allclose(qgrid, grid, rtol=1e-10, atol=0):
        raise ValueError("QTF and wave spectrum must use the same frequency grid")
    real = np.asarray(qtf.real_part)
    imag = np.asarray(qtf.imag_part)
    if real.shape != (grid.size, grid.size) or imag.shape != real.shape:
        raise ValueError("both QTF parts must be square matrices matching omega")
    values = real + 1j * imag
    if not np.all(np.isfinite(values)):
        raise ValueError("QTF must be a finite square matrix matching omega")
    scale = float(np.max(np.abs(values)))
    if not np.allclose(values, values.conj().T, rtol=1e-10, atol=1e-12 * scale):
        raise ValueError("QTF must be complete and Hermitian, with real diagonal")
    return grid, width, spectrum, values


def full_qtf_force_spectrum(qtf, wave_spectrum, omega):
    """One-sided kN² s/rad PSD at mu[k]=k*domega, DC excluded.

    The complete Hermitian QTF and nonnegative wave PSD must share one
    uniform increasing grid. Each unordered pair contributes once; variance
    is sum(result)*domega. No extrapolation or implicit matrix completion.
    """
    grid, width, spectrum, values = _qtf_inputs(qtf, wave_spectrum, omega)
    result = np.zeros(grid.size)
    for lag in range(1, grid.size):
        result[lag] = 8 * width * np.sum(
            np.abs(np.diag(values, lag))**2 * spectrum[:-lag] * spectrum[lag:]
        )
    return result


def low_frequency_surge_response(
    force_spectrum, mu_rad_s, *, mass_kg, stiffness_kn_m,
    damping_kn_s_m=None, damping_ratio=None, mean_force_kn=0.0,
):
    """Filter a full-bin difference-frequency PSD by |K-M*mu²+i*C*mu|⁻².

    ``force_spectrum`` is kN² s/rad at mu[k]=k*dmu; its DC bin must be
    zero. Variance uses sum(S)*dmu, including the last bin, matching
    drift_forces.newman_approximation. Mass includes added mass. Damping
    includes all linearized contributions and must be positive. The model
    is uncoupled with constant coefficients; it supplies no aerodynamic model.
    """
    mu, width = _uniform_grid(mu_rad_s, "mu_rad_s", starts_at_zero=True)
    force = _spectrum(force_spectrum, mu)
    if force[0] != 0:
        raise ValueError("force spectrum DC must be zero; supply mean_force_kn separately")
    if not np.isfinite(mean_force_kn):
        raise ValueError("mean_force_kn must be finite")
    mass, damping = _oscillator(
        mass_kg, stiffness_kn_m, damping_kn_s_m, damping_ratio,
    )
    denominator = (stiffness_kn_m - mass * mu**2)**2 + (damping * mu)**2
    response = force / denominator
    return SurgeResponse(
        mu.copy(), response, float(np.sqrt(response.sum() * width)),
        float(mean_force_kn / stiffness_kn_m),
    )


def surge_response_kernel(
    qtf, wave_spectrum, omega, *, mass_kg, stiffness_kn_m,
    damping_kn_s_m=None, damping_ratio=None,
):
    """Build the centered surge kernel using complex, signed receptance.

    With deterministic amplitudes a_i=sqrt(2*S_i*domega), the physical
    ordered-pair matrix is A_ij=a_i*a_j*Q_ij*H(omega_i-omega_j).
    Quadrature covariance is 1/2, so the Kac–Siegert kernel is A/2.
    This factor is required before using its eigenvalues in the Naess
    squared-standard-normal convention; deterministic amplitudes must not
    be treated as unit-variance Gaussian quadratures. The diagonal is
    removed before diagonalization and its mean is returned separately.
    Phase rotations act by unitary similarity and cannot alter eigenvalues.
    """
    grid, width, spectrum, values = _qtf_inputs(qtf, wave_spectrum, omega)
    if qtf.dof != "surge":
        raise ValueError("a surge QTF is required")
    mass, damping = _oscillator(
        mass_kg, stiffness_kn_m, damping_kn_s_m, damping_ratio,
    )
    delta = grid[:, None] - grid[None, :]
    receptance = 1 / (stiffness_kn_m - mass * delta**2 + 1j * damping * delta)
    rms_amplitudes = np.sqrt(spectrum * width)
    kernel = rms_amplitudes[:, None] * rms_amplitudes[None, :] * values * receptance
    mean_offset = float(2 * np.trace(kernel).real)
    np.fill_diagonal(kernel, 0.)
    return SurgeKernel(kernel, np.linalg.eigvalsh(kernel), mean_offset)


def naess_expected_extreme(
    eigenvalues_m, upcrossing_rate_hz, duration_s, *, mean_offset_m=0., tail="upper",
):
    """Naess asymptotic expected signed excursion, in m, plus static offset.

    Lande-Sudall and Stansby (2026), Eq. 21:
    E[max X] ~ lambda1*(q + log(q) + 2*EulerGamma*(1-q**(-1/2))),
    q=2*log(n_plus*T). The omitted remainder is O(q**(-2/3)).
    ``eigenvalues_m`` use the squared-standard-normal convention of
    surge_response_kernel, not deterministic pair-matrix eigenvalues.
    For the lower tail the same expression is applied to -X.

    This is a long-duration, lightly damped approximation, not a quantile,
    response height or guaranteed finite-duration bound. Supply an observed
    zero upcrossing rate of the centered response in Hz. A Gaussian spectral
    crossing-rate estimate is not generally exact for quadratic responses.
    This API requires q>=1 (n_plus*T>=sqrt(e)) to avoid evaluating the
    expansion at a small or negative logarithm; this minimal domain check
    does not establish asymptotic accuracy. Validate each use.
    """
    eigenvalues = _real_array(eigenvalues_m, "eigenvalues_m")
    if (eigenvalues.ndim != 1 or eigenvalues.size == 0
            or not np.all(np.isfinite(eigenvalues))):
        raise ValueError("eigenvalues_m must be a nonempty finite vector")
    if tail not in ("upper", "lower"):
        raise ValueError("tail must be upper or lower")
    if (not np.isfinite(upcrossing_rate_hz) or upcrossing_rate_hz <= 0
            or not np.isfinite(duration_s) or duration_s <= 0
            or not np.isfinite(mean_offset_m)):
        raise ValueError("rate and duration must be finite and positive; mean must be finite")
    log_count = np.log(upcrossing_rate_hz) + np.log(duration_s)
    if log_count < .5:
        raise ValueError("duration too short: expected upcrossing count must be >=sqrt(e)")
    sign = 1 if tail == "upper" else -1
    largest = max(float(np.max(sign * eigenvalues)), 0.)
    q = 2 * log_count
    amplitude = largest * (q + np.log(q) + 2 * np.euler_gamma * (1 - q**-.5))
    return float(mean_offset_m + sign * amplitude)
