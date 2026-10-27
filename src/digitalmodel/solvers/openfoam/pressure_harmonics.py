"""Fit dimensional pressure at the first three blade-passage harmonics.

The model is ``p(t) = mean + sum(A[k] * cos(2*pi*f[k]*t + phi[k]))``,
where ``f[k] = k * blade_count * rpm / 60`` for orders one through three.
The conversion uses one minute = 60 seconds (NIST SP 330, section 4:
https://www.nist.gov/pml/special-publication-330/sp-330-section-4).

For cosine and sine coefficients c and s, the angle-addition identity gives
``A = hypot(c, s)`` and ``phi = atan2(-s, c)``. Amplitudes are peak amplitudes
in Pa, not peak-to-peak or RMS. Phases are radians in [-pi, pi], referred to
the supplied time coordinate's zero; this is not a blade-angle reference.
Phase has no physical meaning when the corresponding amplitude is zero.

This is an equal-sample-weight least-squares fit, including an intercept.
The fitted mean is the intercept, which need not equal the sample average
over a partial cycle. Nonuniform times are accepted without interpolation.
Known frequencies need not coincide with FFT bins. The caller must assess
sampling and record length, and provide constant RPM and pressure already in Pa
(not a kinematic pressure field). Full numerical rank alone does not establish
freedom from aliasing, good conditioning, or physical adequacy. No pressure
coefficient K_p is computed without a separately qualified normalization basis.

Examples:
    Prescribed synthetic values: 60 RPM and five blades give 5 Hz. A mean of
    100 Pa plus cosine/sine coefficients of 3 and 4 Pa gives amplitudes 3, 4,
    and 0 Pa at the three orders by the angle-addition identity above.

    >>> t = np.arange(200) / 200
    >>> p = 100 + 3*np.cos(2*np.pi*5*t) + 4*np.sin(2*np.pi*10*t)
    >>> fit = fit_blade_harmonics(t, p, rpm=60, blade_count=5)
    >>> round(fit.mean_pa, 1)
    100.0
    >>> np.round(fit.amplitudes_pa, 1).tolist()
    [3.0, 4.0, 0.0]
"""

from __future__ import annotations

from dataclasses import dataclass
from numbers import Integral, Real

import numpy as np
from numpy.typing import ArrayLike, NDArray


@dataclass(frozen=True)
class BladeHarmonicFit:
    """Fitted mean and ordered first, second and third harmonic components.

    Attributes:
        mean_pa: Fitted intercept in Pa.
        blade_passage_hz: Blade-passage frequency in Hz.
        frequencies_hz: Three harmonic frequencies in Hz, in increasing order.
        amplitudes_pa: Three nonnegative peak pressure amplitudes in Pa.
        phases_rad: Corresponding cosine phases, relative to time zero, in rad.
    """

    mean_pa: float
    blade_passage_hz: float
    frequencies_hz: NDArray[np.float64]
    amplitudes_pa: NDArray[np.float64]
    phases_rad: NDArray[np.float64]


def fit_blade_harmonics(
    times: ArrayLike,
    pressure: ArrayLike,
    *,
    rpm: float,
    blade_count: int,
) -> BladeHarmonicFit:
    """Fit one pressure trace with an intercept and three sine/cosine pairs.

    Args:
        times: Finite, strictly increasing one-dimensional times in seconds.
        pressure: Matching one-dimensional pressure samples in Pa. Call once
            per tap for multiple traces; unit conversion is the caller's task.
        rpm: Finite, positive, constant rotational speed in revolutions/minute.
        blade_count: Positive integer number of blades; booleans are rejected.

    Returns:
        Fitted mean, blade-passage frequency, amplitudes and cosine phases.

    Raises:
        ValueError: If inputs are invalid, fewer than seven samples are given,
            derived frequencies or angles are nonfinite, the seven-column
            design matrix is numerically rank deficient, or fitting fails.
    """
    if isinstance(blade_count, bool) or not isinstance(blade_count, Integral):
        raise ValueError("blade_count must be a positive integer.")
    if blade_count <= 0:
        raise ValueError("blade_count must be a positive integer.")
    if (
        isinstance(rpm, bool)
        or not isinstance(rpm, Real)
        or not np.isfinite(rpm)
        or rpm <= 0
    ):
        raise ValueError("rpm must be finite and positive.")

    times = np.asarray(times, dtype=np.float64)
    pressure = np.asarray(pressure, dtype=np.float64)
    if times.ndim != 1 or pressure.ndim != 1:
        raise ValueError("times and pressure must be one-dimensional.")
    if times.size != pressure.size:
        raise ValueError("times and pressure must have matching length.")
    # One intercept plus two coefficients at each of the three orders.
    orders = np.array([1, 2, 3], dtype=np.float64)
    coefficient_count = 1 + 2 * orders.size
    if times.size < coefficient_count:
        raise ValueError("At least seven samples are required.")
    if not np.all(np.isfinite(times)) or not np.all(np.isfinite(pressure)):
        raise ValueError("times and pressure must contain only finite samples.")
    if np.any(times[1:] <= times[:-1]):
        raise ValueError("times must be strictly increasing.")

    # NIST's minute-to-second conversion is cited in the module docstring.
    try:
        blade_passage_hz = float(rpm) / 60.0 * int(blade_count)
    except OverflowError as exc:
        raise ValueError("Derived blade-passage frequency must be finite.") from exc
    with np.errstate(over="ignore", invalid="ignore"):
        frequencies = blade_passage_hz * orders
        # Fit from the first sample for smaller angles, then translate phases
        # back to time zero using cos(omega*(t-t0) + phi_local).
        angular_frequencies = 2 * np.pi * frequencies
        angles = (times - times[0])[:, None] * angular_frequencies
        origin_angles = times[0] * angular_frequencies
    if (
        blade_passage_hz <= 0
        or not np.all(np.isfinite(frequencies))
        or not np.all(np.isfinite(angles))
        or not np.all(np.isfinite(origin_angles))
    ):
        raise ValueError("Derived frequencies and phase angles must be finite.")

    design = np.column_stack((np.ones(times.size), np.cos(angles), np.sin(angles)))
    try:
        # Default rank cutoff is floating-point epsilon * max(matrix dimensions):
        # https://numpy.org/doc/stable/reference/generated/numpy.linalg.lstsq.html
        coefficients, _, rank, _ = np.linalg.lstsq(design, pressure, rcond=None)
    except np.linalg.LinAlgError as exc:
        raise ValueError("Harmonic least-squares fit failed.") from exc
    if rank != coefficient_count:
        raise ValueError("Harmonic samples are rank deficient.")
    if not np.all(np.isfinite(coefficients)):
        raise ValueError("Harmonic fit coefficients must be finite.")

    cosine = coefficients[1 : 1 + orders.size]
    sine = coefficients[1 + orders.size :]
    phases = np.arctan2(-sine, cosine) - origin_angles
    phases = np.arctan2(np.sin(phases), np.cos(phases))
    return BladeHarmonicFit(
        mean_pa=float(coefficients[0]),
        blade_passage_hz=blade_passage_hz,
        frequencies_hz=frequencies,
        amplitudes_pa=np.hypot(cosine, sine),
        phases_rad=phases,
    )
